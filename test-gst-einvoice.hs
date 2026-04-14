#!/usr/bin/env cabal

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{- cabal:
build-depends: base, aeson, text, bytestring, http-client, http-client-tls, time, servant, servant-client, http-types
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import GHC.Generics
import qualified Network.HTTP.Client.TLS as HttpTLS
import Servant.API
import Servant.Client

-- ============================================================
-- Types (mirror of CharteredInfo/Types.hs)
-- ============================================================

data AuthResp = AuthResp
  { arStatus :: Int,
    arData :: Maybe AuthData,
    arErrorDetails :: Maybe [EInvoiceError]
  }
  deriving (Show)

instance FromJSON AuthResp where
  parseJSON = withObject "AuthResp" $ \v ->
    AuthResp <$> v .: "Status" <*> v .:? "Data" <*> v .:? "ErrorDetails"

data AuthData = AuthData
  { adClientId :: Text,
    adUserName :: Text,
    adAuthToken :: Text,
    adSek :: Text,
    adTokenExpiry :: Text
  }
  deriving (Show)

instance FromJSON AuthData where
  parseJSON = withObject "AuthData" $ \v ->
    AuthData <$> v .: "ClientId" <*> v .: "UserName" <*> v .: "AuthToken" <*> v .: "Sek" <*> v .: "TokenExpiry"

data InvoiceResp = InvoiceResp
  { irStatus :: Text,
    irData :: Maybe InvoiceRespData,
    irErrorDetails :: Maybe [EInvoiceError],
    irInfoDtls :: Maybe [InfoDtl]
  }
  deriving (Show)

instance FromJSON InvoiceResp where
  parseJSON = withObject "InvoiceResp" $ \v ->
    InvoiceResp <$> v .: "Status" <*> v .:? "Data" <*> v .:? "ErrorDetails" <*> v .:? "InfoDtls"

data InvoiceRespData = InvoiceRespData
  { irdAckNo :: Text,
    irdAckDt :: Text,
    irdIrn :: Text,
    irdSignedInvoice :: Maybe Text,
    irdSignedQRCode :: Maybe Text
  }
  deriving (Show)

instance FromJSON InvoiceRespData where
  parseJSON v = case v of
    String jsonStr ->
      case eitherDecodeStrict (TE.encodeUtf8 jsonStr) of
        Left err -> fail $ "Failed to parse Data JSON string: " <> err
        Right obj -> parseObj obj
    Object _ -> parseObj v
    _ -> fail "Expected String or Object for Data"
    where
      parseObj obj = flip (withObject "InvoiceRespData") obj $ \o ->
        InvoiceRespData
          <$> (o .: "AckNo" <|> (T.pack . show <$> (o .: "AckNo" :: Parser Integer)))
          <*> o .: "AckDt"
          <*> o .: "Irn"
          <*> o .:? "SignedInvoice"
          <*> o .:? "SignedQRCode"

data EInvoiceError = EInvoiceError
  { eeErrorCode :: Text,
    eeErrorMessage :: Text
  }
  deriving (Show)

instance FromJSON EInvoiceError where
  parseJSON = withObject "EInvoiceError" $ \v ->
    EInvoiceError <$> v .: "ErrorCode" <*> v .: "ErrorMessage"

data InfoDtl = InfoDtl
  { idInfCd :: Text,
    idDesc :: Maybe Value
  }
  deriving (Show)

instance FromJSON InfoDtl where
  parseJSON = withObject "InfoDtl" $ \v ->
    InfoDtl <$> v .: "InfCd" <*> v .:? "Desc"

-- ============================================================
-- Servant API types (same as Flow.hs)
-- ============================================================

type AuthAPI =
  "eivital" :> "dec" :> "v1.04" :> "auth"
    :> QueryParam "aspid" Text
    :> QueryParam "password" Text
    :> QueryParam "Gstin" Text
    :> QueryParam "user_name" Text
    :> QueryParam "eInvPwd" Text
    :> Get '[JSON] AuthResp

type GenerateInvoiceAPI =
  "eicore" :> "dec" :> "v1.03" :> "Invoice"
    :> QueryParam "aspid" Text
    :> QueryParam "password" Text
    :> QueryParam "Gstin" Text
    :> QueryParam "AuthToken" Text
    :> QueryParam "user_name" Text
    :> QueryParam "eInvPwd" Text
    :> ReqBody '[JSON] Value
    :> Post '[JSON] InvoiceResp

-- Servant clients
authClientFn :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ClientM AuthResp
authClientFn = client (Proxy @AuthAPI)

generateInvoiceClientFn :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Value -> ClientM InvoiceResp
generateInvoiceClientFn = client (Proxy @GenerateInvoiceAPI)

-- ============================================================
-- authenticateIO / generateInvoiceIO (same as Flow.hs exports)
-- ============================================================

authenticateIO :: Text -> Text -> Text -> Text -> Text -> BaseUrl -> IO (Either ClientError AuthResp)
authenticateIO aspId' password' gstin' userName' eInvPwd' authUrl' = do
  manager <- HttpTLS.newTlsManager
  let env = mkClientEnv manager authUrl'
  runClientM (authClientFn (Just aspId') (Just password') (Just gstin') (Just userName') (Just eInvPwd')) env

generateInvoiceIO :: Text -> Text -> Text -> Text -> Text -> Text -> BaseUrl -> Value -> IO (Either ClientError InvoiceResp)
generateInvoiceIO aspId' password' gstin' authToken' userName' eInvPwd' invoiceUrl' payload = do
  manager <- HttpTLS.newTlsManager
  let env = mkClientEnv manager invoiceUrl'
  runClientM (generateInvoiceClientFn (Just aspId') (Just password') (Just gstin') (Just authToken') (Just userName') (Just eInvPwd') payload) env

-- ============================================================
-- Sandbox credentials
-- ============================================================

sandboxUrl :: BaseUrl
sandboxUrl = BaseUrl Https "gstsandbox.charteredinfo.com" 443 ""

aspId, passwordParam, gstinParam, userNameParam, eInvPwdParam :: Text
aspId = "1650601821"
passwordParam = "Pmpl@123"
gstinParam = "34AACCC1596Q002"
userNameParam = "TaxProEnvPON"
eInvPwdParam = "abc34*"

mkInvoicePayload :: Text -> Value
mkInvoicePayload docNo =
  object
    [ "Version" .= ("1.1" :: Text),
      "TranDtls" .= object ["TaxSch" .= ("GST" :: Text), "SupTyp" .= ("B2B" :: Text)],
      "DocDtls" .= object ["Typ" .= ("INV" :: Text), "No" .= docNo, "Dt" .= ("14/04/2026" :: Text)],
      "SellerDtls"
        .= object
          [ "Gstin" .= ("34AACCC1596Q002" :: Text),
            "LglNm" .= ("NIC company pvt ltd" :: Text),
            "Addr1" .= ("5th block, kuvempu layout" :: Text),
            "Loc" .= ("GANDHINAGAR" :: Text),
            "Pin" .= (605001 :: Int),
            "Stcd" .= ("34" :: Text)
          ],
      "BuyerDtls"
        .= object
          [ "Gstin" .= ("27AABCB5730G2ZW" :: Text),
            "LglNm" .= ("Bajaj General Insurance Limited" :: Text),
            "Addr1" .= ("1st floor, G E Plaza, Airport Road" :: Text),
            "Loc" .= ("Maharashtra" :: Text),
            "Pin" .= (411006 :: Int),
            "Stcd" .= ("27" :: Text),
            "Pos" .= ("27" :: Text)
          ],
      "ItemList"
        .= [ object
               [ "SlNo" .= ("1" :: Text),
                 "IsServc" .= ("Y" :: Text),
                 "HsnCd" .= ("997161" :: Text),
                 "Qty" .= (1 :: Int),
                 "Unit" .= ("NOS" :: Text),
                 "UnitPrice" .= (1000 :: Double),
                 "TotAmt" .= (1000 :: Double),
                 "Discount" .= (0 :: Double),
                 "PreTaxVal" .= (0 :: Double),
                 "AssAmt" .= (1000 :: Double),
                 "GstRt" .= (18 :: Double),
                 "IgstAmt" .= (180 :: Double),
                 "CgstAmt" .= (0 :: Double),
                 "SgstAmt" .= (0 :: Double),
                 "TotItemVal" .= (1180 :: Double)
               ]
           ],
      "ValDtls"
        .= object
          [ "AssVal" .= (1000 :: Double),
            "CgstVal" .= (0 :: Double),
            "SgstVal" .= (0 :: Double),
            "IgstVal" .= (180 :: Double),
            "TotInvVal" .= (1180 :: Double)
          ],
      "EwbDtls" .= object ["Distance" .= (0 :: Int)]
    ]

-- ============================================================
-- Main
-- ============================================================

main :: IO ()
main = do
  putStrLn "============================================"
  putStrLn "  TEST 1: JSON Parsing (offline)"
  putStrLn "============================================"
  testJsonParsing

  putStrLn ""
  putStrLn "============================================"
  putStrLn "  TEST 2: authenticateIO (live sandbox)"
  putStrLn "============================================"
  token <- testAuthenticate

  putStrLn ""
  putStrLn "============================================"
  putStrLn "  TEST 3: generateInvoiceIO (live sandbox)"
  putStrLn "============================================"
  testGenerateInvoice token

  putStrLn ""
  putStrLn "ALL TESTS PASSED"

testJsonParsing :: IO ()
testJsonParsing = do
  let authJson = "{\"Status\":1,\"Data\":{\"ClientId\":\"AACCC29GSPR5CM0\",\"UserName\":\"TaxProEnvPON\",\"AuthToken\":\"testtoken123\",\"Sek\":\"\",\"TokenExpiry\":\"2026-04-14 13:44:00\"},\"ErrorDetails\":null,\"InfoDtls\":null}"
  case eitherDecode authJson :: Either String AuthResp of
    Left err -> error $ "FAIL: Auth JSON parse: " <> err
    Right r -> do
      assert "Auth status == 1" (arStatus r == 1)
      case arData r of
        Nothing -> error "FAIL: Auth data is Nothing"
        Just d -> do
          assert "AuthToken == testtoken123" (adAuthToken d == "testtoken123")
          putStrLn $ "  AuthToken: " <> T.unpack (adAuthToken d)
      putStrLn "  [OK] Auth response parsing"

  let errJson = "{\"Status\":\"0\",\"Data\":null,\"ErrorDetails\":[{\"ErrorCode\":\"2150\",\"ErrorMessage\":\"Duplicate IRN\"}],\"InfoDtls\":[{\"InfCd\":\"DUPIRN\",\"Desc\":{\"AckNo\":152610027386922,\"AckDt\":\"2026-04-06 17:06:00\",\"Irn\":\"adc6855e\"}}]}"
  case eitherDecode errJson :: Either String InvoiceResp of
    Left err -> error $ "FAIL: Invoice error JSON parse: " <> err
    Right r -> do
      assert "Status == 0" (irStatus r == "0")
      assert "Data is Nothing" (case irData r of Nothing -> True; _ -> False)
      case irErrorDetails r of
        Just (e : _) -> assert "ErrorCode == 2150" (eeErrorCode e == "2150")
        _ -> error "FAIL: Expected error details"
      putStrLn "  [OK] Invoice error response parsing"

testAuthenticate :: IO Text
testAuthenticate = do
  result <- authenticateIO aspId passwordParam gstinParam userNameParam eInvPwdParam sandboxUrl
  case result of
    Left err -> error $ "FAIL: authenticateIO: " <> show err
    Right r -> do
      assert "Auth status == 1" (arStatus r == 1)
      case arData r of
        Nothing -> error "FAIL: Auth returned no Data"
        Just d -> do
          putStrLn $ "  Status: " <> show (arStatus r)
          putStrLn $ "  AuthToken: " <> T.unpack (adAuthToken d)
          putStrLn $ "  TokenExpiry: " <> T.unpack (adTokenExpiry d)
          putStrLn "  [OK] authenticateIO"
          return (adAuthToken d)

testGenerateInvoice :: Text -> IO ()
testGenerateInvoice token = do
  now <- getCurrentTime
  let ts = filter (`notElem` [' ', ':', '.', '-']) (show now)
      docNo = T.pack $ "TST" <> take 13 ts
  putStrLn $ "  DocNo: " <> T.unpack docNo

  result <- generateInvoiceIO aspId passwordParam gstinParam token userNameParam eInvPwdParam sandboxUrl (mkInvoicePayload docNo)
  case result of
    Left err -> error $ "FAIL: generateInvoiceIO: " <> show err
    Right r -> do
      putStrLn $ "  Status: " <> T.unpack (irStatus r)
      putStrLn $ "  Data: " <> show (irData r)
      putStrLn $ "  irErrorDetails: " <> show (irErrorDetails r)
      putStrLn $ "  irInfoDtls: " <> show (irInfoDtls r)
      case irData r of
        Just d -> do
          putStrLn $ "  IRN: " <> T.unpack (irdIrn d)
          putStrLn $ "  AckNo: " <> T.unpack (irdAckNo d)
          putStrLn $ "  AckDt: " <> T.unpack (irdAckDt d)
          putStrLn $ "  SignedInvoice: " <> maybe "N/A" (T.unpack . T.take 50) (irdSignedInvoice d) <> "..."
          putStrLn "  [OK] generateInvoiceIO - IRN generated"
        Nothing -> do
          case irErrorDetails r of
            Just errs -> mapM_ (\e -> putStrLn $ "  Error: " <> T.unpack (eeErrorCode e) <> " - " <> T.unpack (eeErrorMessage e)) errs
            Nothing -> pure ()
          putStrLn "  [OK] generateInvoiceIO - response parsed"

assert :: String -> Bool -> IO ()
assert _ True = pure ()
assert msg False = error $ "ASSERTION FAILED: " <> msg
