module GSTEInvoiceTest (gstEInvoiceTests) where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.External.GSTEInvoice.CharteredInfo.Flow
  ( authenticateIO,
    generateInvoiceIO,
  )
import Kernel.External.GSTEInvoice.CharteredInfo.Types
import Kernel.Prelude
import Servant.Client (BaseUrl (..), Scheme (..))
import Test.Tasty
import Test.Tasty.HUnit

-- Sandbox credentials
aspId :: Text
aspId = "1650601821"

passwordParam :: Text
passwordParam = "Pmpl@123"

gstinParam :: Text
gstinParam = "34AACCC1596Q002"

userNameParam :: Text
userNameParam = "TaxProEnvPON"

eInvPwdParam :: Text
eInvPwdParam = "abc34*"

sandboxAuthUrl :: BaseUrl
sandboxAuthUrl = BaseUrl Https "gstsandbox.charteredinfo.com" 443 ""

sandboxInvoiceUrl :: BaseUrl
sandboxInvoiceUrl = BaseUrl Https "gstsandbox.charteredinfo.com" 443 ""

gstEInvoiceTests :: TestTree
gstEInvoiceTests =
  testGroup
    "GST E-Invoice Tests"
    [ testCase "AuthResp JSON parsing (offline)" testAuthRespParsing,
      testCase "InvoiceResp JSON parsing - error case (offline)" testInvoiceRespErrorParsing,
      testCase "authenticateIO - live sandbox" testAuthenticateIO,
      testCase "generateInvoiceIO - live sandbox" testGenerateInvoiceIO
    ]

-- | Test: call authenticateIO (same Servant client + types as the real authenticate)
testAuthenticateIO :: Assertion
testAuthenticateIO = do
  result <- authenticateIO aspId passwordParam gstinParam userNameParam eInvPwdParam sandboxAuthUrl
  case result of
    Left err -> assertFailure $ "authenticateIO failed: " <> show err
    Right authResp -> do
      assertEqual "Auth status should be 1" 1 authResp.status
      assertBool "Auth data should be present" (isJust authResp.authData)
      let Just ad = authResp.authData
      assertBool "AuthToken should not be empty" (not $ T.null ad.authToken)
      putStrLn $ "  AuthToken: " <> T.unpack ad.authToken
      putStrLn $ "  TokenExpiry: " <> T.unpack ad.tokenExpiry
      putStrLn "  [OK] authenticateIO works"

-- | Test: call authenticateIO then generateInvoiceIO (full flow)
testGenerateInvoiceIO :: Assertion
testGenerateInvoiceIO = do
  -- Step 1: Authenticate
  authResult <- authenticateIO aspId passwordParam gstinParam userNameParam eInvPwdParam sandboxAuthUrl
  authResp <- case authResult of
    Left err -> assertFailure ("authenticateIO failed: " <> show err) >> error "unreachable"
    Right r -> return r
  let Just ad = authResp.authData
      token = ad.authToken
  putStrLn $ "  Authenticated, token: " <> T.unpack token

  -- Step 2: Generate invoice with unique doc number
  now <- getCurrentTime
  let ts = T.filter (`notElem` [' ', ':', '.', '-']) (T.pack $ show now)
      uniqueDocNo = "TST" <> T.take 13 ts
  putStrLn $ "  DocNo: " <> T.unpack uniqueDocNo

  let payload =
        EInvoicePayload
          { version = "1.1",
            tranDtls = TranDtls {taxSch = "GST", supTyp = "B2B"},
            docDtls = DocDtls {typ = "INV", no = uniqueDocNo, dt = "14/04/2026"},
            sellerDtls =
              SellerDtls
                { gstin = "34AACCC1596Q002",
                  lglNm = "NIC company pvt ltd",
                  addr1 = "5th block, kuvempu layout",
                  loc = "GANDHINAGAR",
                  pin = 605001,
                  stcd = "34"
                },
            buyerDtls =
              BuyerDtls
                { gstin = "27AABCB5730G2ZW",
                  lglNm = "Bajaj General Insurance Limited",
                  addr1 = "1st floor, G E Plaza, Airport Road",
                  loc = "Maharashtra",
                  pin = 411006,
                  stcd = "27",
                  pos = Just "27"
                },
            itemList =
              [ ItemEntry
                  { slNo = "1",
                    isServc = "Y",
                    hsnCd = "997161",
                    qty = 1,
                    unit = "NOS",
                    unitPrice = 1000,
                    totAmt = 1000,
                    discount = 0,
                    preTaxVal = 0,
                    assAmt = 1000,
                    gstRt = 18,
                    igstAmt = 180,
                    cgstAmt = 0,
                    sgstAmt = 0,
                    totItemVal = 1180
                  }
              ],
            valDtls =
              ValDtls
                { assVal = 1000,
                  cgstVal = 0,
                  sgstVal = 0,
                  igstVal = 180,
                  totInvVal = 1180
                },
            ewbDtls = Just EwbDtls {distance = 0}
          }

  invoiceResult <- generateInvoiceIO aspId passwordParam gstinParam token userNameParam eInvPwdParam sandboxInvoiceUrl payload
  case invoiceResult of
    Left err -> assertFailure $ "generateInvoiceIO failed: " <> show err
    Right invResp -> do
      putStrLn $ "  Status: " <> T.unpack invResp.status
      case invResp.invoiceData of
        Just d -> do
          putStrLn $ "  IRN: " <> T.unpack d.irn
          putStrLn $ "  AckNo: " <> T.unpack d.ackNo
          putStrLn $ "  AckDt: " <> T.unpack d.ackDt
          putStrLn "  [OK] generateInvoiceIO works - IRN generated"
        Nothing -> do
          case invResp.errorDetails of
            Just errs -> mapM_ (\e -> putStrLn $ "  Error: " <> T.unpack e.errorCode <> " - " <> T.unpack e.errorMessage) errs
            Nothing -> pure ()
          case invResp.infoDtls of
            Just infos -> mapM_ (\i -> putStrLn $ "  Info: " <> T.unpack i.infCd) infos
            Nothing -> pure ()
          putStrLn "  [OK] generateInvoiceIO works - response parsed (with errors/info)"

-- | Offline: parse a known auth response
testAuthRespParsing :: Assertion
testAuthRespParsing = do
  let json =
        "{\"Status\":1,\"Data\":{\"ClientId\":\"AACCC29GSPR5CM0\",\"UserName\":\"TaxProEnvPON\",\"AuthToken\":\"1mCazpbRUissKllE9k5dQUblK\",\"Sek\":\"\",\"TokenExpiry\":\"2026-04-14 13:44:00\"},\"ErrorDetails\":null,\"InfoDtls\":null}"
  case eitherDecode json :: Either String AuthResp of
    Left err -> assertFailure $ "Parse failed: " <> err
    Right resp -> do
      assertEqual "Status" 1 resp.status
      let Just d = resp.authData
      assertEqual "AuthToken" "1mCazpbRUissKllE9k5dQUblK" d.authToken
      assertEqual "UserName" "TaxProEnvPON" d.userName

-- | Offline: parse a known error response (Duplicate IRN)
testInvoiceRespErrorParsing :: Assertion
testInvoiceRespErrorParsing = do
  let json =
        BSL.fromStrict $
          TE.encodeUtf8
            "{\"Status\":\"0\",\"Data\":null,\"ErrorDetails\":[{\"ErrorCode\":\"2150\",\"ErrorMessage\":\"Duplicate IRN\"}],\"InfoDtls\":[{\"InfCd\":\"DUPIRN\",\"Desc\":{\"AckNo\":152610027386922,\"AckDt\":\"2026-04-06 17:06:00\",\"Irn\":\"adc6855e7e3507a8891ae4e972dd88c9ebd68716df7f5201041167c374f0041b\"}}]}"
  case eitherDecode json :: Either String InvoiceResp of
    Left err -> assertFailure $ "Parse failed: " <> err
    Right resp -> do
      assertEqual "Status" "0" resp.status
      assertBool "Data should be Nothing" (isNothing resp.invoiceData)
      let Just errs = resp.errorDetails
      assertEqual "Error count" 1 (length errs)
      assertEqual "ErrorCode" "2150" ((.errorCode) (head errs))
      assertEqual "ErrorMessage" "Duplicate IRN" ((.errorMessage) (head errs))
      let Just infos = resp.infoDtls
      assertEqual "Info count" 1 (length infos)
      assertEqual "InfCd" "DUPIRN" ((.infCd) (head infos))
