module Kernel.External.Insurance.IffcoTokio.Flow where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as DT
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import EulerHS.Types (EulerClient, ManagerSelector (..), client)
import Kernel.External.Encryption
import Kernel.External.Insurance.IffcoTokio.Config (iffcoTokioHttpManagerKey)
import Kernel.External.Insurance.IffcoTokio.Types
import Kernel.External.Insurance.Interface.Types (HomeDeclarationReq)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (GenericError (..))
import Kernel.Utils.Common
import qualified Network.HTTP.Media as M
import Servant hiding (throwError)
import Text.XML (def, parseText)
import Text.XML.Cursor (content, fromDocument, laxElement, ($//), (&/))
import qualified Text.XML.Cursor as XC

-- ---------------------------------------------------------------------------
-- Servant content type for SOAP / XML

data IffcoApplicationXML deriving (Typeable)

instance Accept IffcoApplicationXML where
  contentType _ = "text" M.// "xml"

-- Render: pass raw BSL bytes directly as the request body
instance MimeRender IffcoApplicationXML BSL.ByteString where
  mimeRender _ bs = bs

-- Unrender: decode UTF-8 bytes and parse the SOAP response envelope
instance MimeUnrender IffcoApplicationXML HomeDeclarationResp where
  mimeUnrender _ bs =
    case TE.decodeUtf8' (BSL.toStrict bs) of
      Left err -> Left (show err)
      Right txt -> case parseHomeDeclarationResponse txt of
        Left e -> Left (T.unpack e)
        Right r -> Right r

-- ---------------------------------------------------------------------------
-- Servant API type

-- The IFFCO Tokio endpoint takes fixed query params that identify the operation.
-- SOAPAction value: "document/http://siebel.com/CustomUI:RegisterHomeDeclaration_New"
-- (Servant QueryParam handles URL-encoding automatically.)
type RegisterHomeDeclarationAPI =
  "eai_ws_enu"
    :> "start.swe"
    :> QueryParam "SWEExtSource" Text
    :> QueryParam "SWEExtCmd" Text
    :> QueryParam "SOAPAction" Text
    :> ReqBody '[IffcoApplicationXML] BSL.ByteString
    :> Post '[IffcoApplicationXML] HomeDeclarationResp

registerHomeDeclarationClient ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  BSL.ByteString ->
  EulerClient HomeDeclarationResp
registerHomeDeclarationClient = client (Proxy :: Proxy RegisterHomeDeclarationAPI)

-- ---------------------------------------------------------------------------
-- Main call function

-- | Make the SOAP call and throw on failure (standard throwing variant).
registerHomeDeclaration ::
  ( HasCallStack,
    EncFlow m r,
    MonadFlow m,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  IffcoTokioConfig ->
  HomeDeclarationReq ->
  m HomeDeclarationResp
registerHomeDeclaration config req =
  registerHomeDeclarationEither config req
    >>= either (\err -> throwError $ InternalError $ "IFFCO Tokio API call failed: " <> err) return

-- | Make the SOAP call and return 'Either' instead of throwing.
-- Used by the async interface layer so that the callback always fires.
registerHomeDeclarationEither ::
  ( HasCallStack,
    EncFlow m r,
    MonadFlow m,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  IffcoTokioConfig ->
  HomeDeclarationReq ->
  m (Either Text HomeDeclarationResp)
registerHomeDeclarationEither config req = do
  decryptedPassword <- decrypt config.password
  baseUrl <- parseBaseUrl (T.strip config.url)
  let soapBody = buildSoapEnvelope config.username decryptedPassword config req
      eulerClient =
        registerHomeDeclarationClient
          (Just "SecureWebService")
          (Just "Execute")
          -- SOAPAction value includes surrounding double-quotes as per SOAP spec
          (Just "\"document/http://siebel.com/CustomUI:RegisterHomeDeclaration_New\"")
          soapBody
  result <- callAPI' (Just $ ManagerSelector $ DT.pack iffcoTokioHttpManagerKey) baseUrl eulerClient "IFFCO-Tokio-register-home-declaration" (Proxy @RegisterHomeDeclarationAPI)
  return $ case result of
    Left err -> Left (T.pack (show err))
    Right resp -> Right resp

-- ---------------------------------------------------------------------------
-- SOAP envelope builder

buildSoapEnvelope :: Text -> Text -> IffcoTokioConfig -> HomeDeclarationReq -> BSL.ByteString
buildSoapEnvelope username password cfg req =
  BSL.fromStrict . TE.encodeUtf8 $
    T.concat
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
        "<soapenv:Envelope",
        " xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"",
        " xmlns:cus=\"http://siebel.com/CustomUI\"",
        " xmlns:ins=\"http://www.siebel.com/xml/INSDeclarationsWebServiceRequestIO\">",
        "<soapenv:Header>",
        "<wsse:Security xmlns:wsse=\"http://schemas.xmlsoap.org/ws/2002/07/secext\">",
        "<wsse:UsernameToken xmlns:wsu=\"http://schemas.xmlsoap.org/ws/2002/07/utility\">",
        "<wsse:Username>",
        xmlEscape username,
        "</wsse:Username>",
        "<wsse:Password Type=\"wsse:PasswordText\">",
        xmlEscape password,
        "</wsse:Password>",
        "</wsse:UsernameToken>",
        "</wsse:Security>",
        "</soapenv:Header>",
        "<soapenv:Body>",
        "<cus:RegisterHomeDeclaration_New_Input>",
        "<ins:INSDeclarationsWebServiceRequestIO>",
        "<ins:DeclarationDetails>",
        "<ins:MasterPolicyClient>",
        xmlEscape (fromMaybe cfg.masterPolicyClient req.masterPolicyClient),
        "</ins:MasterPolicyClient>",
        "<ins:InsurancePlan>",
        xmlEscape (fromMaybe cfg.insurancePlan req.insurancePlan),
        "</ins:InsurancePlan>",
        "<ins:InsuredAddress>",
        maybe "" xmlEscape req.insuredAddress,
        "</ins:InsuredAddress>",
        "<ins:InsuredEmail>",
        maybe "" xmlEscape req.insuredEmail,
        "</ins:InsuredEmail>",
        "<ins:InsuredMobile>",
        xmlEscape req.insuredMobile,
        "</ins:InsuredMobile>",
        "<ins:InsuredName>",
        xmlEscape req.insuredName,
        "</ins:InsuredName>",
        "<ins:InvoiceDate>",
        xmlEscape req.invoiceDate,
        "</ins:InvoiceDate>",
        "<ins:InvoiceRequestNumber>",
        xmlEscape req.invoiceRequestNumber,
        "</ins:InvoiceRequestNumber>",
        "<ins:EWCommencesOn>",
        maybe "" xmlEscape req.ewCommencesOn,
        "</ins:EWCommencesOn>",
        "<ins:ExtraAttrib01>",
        maybe "" xmlEscape req.extraAttrib01,
        "</ins:ExtraAttrib01>",
        "<ins:ExtraAttrib02>",
        maybe "" xmlEscape req.extraAttrib02,
        "</ins:ExtraAttrib02>",
        "<ins:RiskEndDate>",
        maybe "" xmlEscape req.riskEndDate,
        "</ins:RiskEndDate>",
        "<ins:RiskStartDate>",
        maybe "" xmlEscape req.riskStartDate,
        "</ins:RiskStartDate>",
        "</ins:DeclarationDetails>",
        "</ins:INSDeclarationsWebServiceRequestIO>",
        "</cus:RegisterHomeDeclaration_New_Input>",
        "</soapenv:Body>",
        "</soapenv:Envelope>"
      ]

-- Escape special XML characters in text node values to prevent injection
xmlEscape :: Text -> Text
xmlEscape =
  T.replace "&" "&amp;"
    . T.replace "<" "&lt;"
    . T.replace ">" "&gt;"
    . T.replace "\"" "&quot;"
    . T.replace "'" "&apos;"

-- SOAP response XML parser (uses xml-conduit cursor navigation)

lookupSoapField :: XC.Cursor -> Text -> Maybe Text
lookupSoapField cursor localName =
  case cursor $// laxElement localName &/ content of
    [] -> Nothing
    ts -> Just $ T.strip $ T.concat ts

parseHomeDeclarationResponse :: Text -> Either Text HomeDeclarationResp
parseHomeDeclarationResponse xmlText = do
  doc <- case parseText def (TL.fromStrict xmlText) of
    Left err -> Left (T.pack (show err))
    Right d -> Right d
  let cursor = fromDocument doc
      require fieldName =
        maybe (Left $ "IFFCO Tokio response missing field: " <> fieldName) Right $
          lookupSoapField cursor fieldName
  certNum <- require "Certificate_spcNumber"
  declId <- require "Declaration_spcId"
  st <- require "Status"
  return HomeDeclarationResp {certificateNumber = certNum, declarationId = declId, status = st}
