{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Kernel.External.GSTEInvoice.DigiGST.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Aeson.Encoding as AE
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import Kernel.External.Encryption
import Kernel.External.GSTEInvoice.CharteredInfo.Types (fixedDouble)
import Kernel.Prelude

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for the EY DigiGST e-invoice API.
data DigiGSTConfig = DigiGSTConfig
  { url :: BaseUrl, -- e.g. https://eapi.digigstey.in/bcappapi (generateAccessToken.do and generateEinvoice.do)
    userName :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    companyCode :: Maybe Text,
    sourceId :: Maybe Text,
    gstin :: Text,
    sellerGstinLegalName :: Text,
    sellerGstinTradeName :: Maybe Text,
    sellerGstinAddr :: Text,
    sellerGstinAddr2 :: Maybe Text,
    sellerGstinLocation :: Text,
    sellerGstinPinCode :: Int,
    sellerGstinStcd :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Access token response
-- ---------------------------------------------------------------------------

-- | Response of generateAccessToken.do. A failed login also comes back with
-- HTTP 200 and hdr.status "S"; it is told apart by resp.error being set.
data DigiGSTAuthResp = DigiGSTAuthResp
  { hdr :: DigiGSTRespHdr,
    resp :: Maybe DigiGSTAuthRespData
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data DigiGSTAuthRespData = DigiGSTAuthRespData
  { idToken :: Maybe Text,
    expiresIn :: Maybe Int,
    authError :: Maybe Text,
    authErrorDescription :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON DigiGSTAuthRespData where
  parseJSON = withObject "DigiGSTAuthRespData" $ \v ->
    DigiGSTAuthRespData
      <$> v .:? "id_token"
      <*> v .:? "expires_in"
      <*> v .:? "error"
      <*> v .:? "error_description"

-- | callAPI logs the response through this instance, so the token is left out.
instance ToJSON DigiGSTAuthRespData where
  toJSON DigiGSTAuthRespData {..} =
    object
      [ "id_token" .= (("[REDACTED]" :: Text) <$ idToken),
        "expires_in" .= expiresIn,
        "error" .= authError,
        "error_description" .= authErrorDescription
      ]

-- ---------------------------------------------------------------------------
-- Generate e-invoice request
-- ---------------------------------------------------------------------------

newtype DigiGSTInvoiceReq = DigiGSTInvoiceReq
  { req :: DigiGSTInvoice
  }
  deriving (Show, Eq, Generic, ToJSON)

data DigiGSTInvoice = DigiGSTInvoice
  { returnPeriod :: Maybe Text,
    taxScheme :: Text,
    docCat :: Text,
    docType :: Text,
    docNo :: Text,
    docDate :: Text,
    reverseCharge :: Text,
    suppGstin :: Text,
    supTradeName :: Maybe Text,
    supLegalName :: Text,
    supBuildingNo :: Text,
    supBuildingName :: Maybe Text,
    supLocation :: Text,
    supPincode :: Int,
    supStateCode :: Text,
    custGstin :: Text,
    custOrSupName :: Text,
    custOrSupAddr1 :: Text,
    custOrSupAddr4 :: Text,
    custPincode :: Int,
    billToState :: Text,
    pos :: Text,
    sec7OfIgstFlag :: Text,
    tcsFlag :: Text,
    claimRefundFlag :: Text,
    autoPopToRefundFlag :: Text,
    invAssessableAmt :: Double,
    invIgstAmt :: Double,
    invCgstAmt :: Double,
    invSgstAmt :: Double,
    docAmt :: Double,
    lineItems :: [DigiGSTLineItem]
  }
  deriving (Show, Eq, Generic)

instance ToJSON DigiGSTInvoice where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}
  toEncoding DigiGSTInvoice {..} =
    pairs $
      "returnPeriod" .=? returnPeriod
        <> "taxScheme" .= taxScheme
        <> "docCat" .= docCat
        <> "docType" .= docType
        <> "docNo" .= docNo
        <> "docDate" .= docDate
        <> "reverseCharge" .= reverseCharge
        <> "suppGstin" .= suppGstin
        <> "supTradeName" .=? supTradeName
        <> "supLegalName" .= supLegalName
        <> "supBuildingNo" .= supBuildingNo
        <> "supBuildingName" .=? supBuildingName
        <> "supLocation" .= supLocation
        <> "supPincode" .= supPincode
        <> "supStateCode" .= supStateCode
        <> "custGstin" .= custGstin
        <> "custOrSupName" .= custOrSupName
        <> "custOrSupAddr1" .= custOrSupAddr1
        <> "custOrSupAddr4" .= custOrSupAddr4
        <> "custPincode" .= custPincode
        <> "billToState" .= billToState
        <> "pos" .= pos
        <> "sec7OfIgstFlag" .= sec7OfIgstFlag
        <> "tcsFlag" .= tcsFlag
        <> "claimRefundFlag" .= claimRefundFlag
        <> "autoPopToRefundFlag" .= autoPopToRefundFlag
        <> AE.pair "invAssessableAmt" (fixedDouble 2 invAssessableAmt)
        <> AE.pair "invIgstAmt" (fixedDouble 2 invIgstAmt)
        <> AE.pair "invCgstAmt" (fixedDouble 2 invCgstAmt)
        <> AE.pair "invSgstAmt" (fixedDouble 2 invSgstAmt)
        <> AE.pair "docAmt" (fixedDouble 2 docAmt)
        <> "lineItems" .= lineItems
    where
      key .=? mbValue = maybe mempty (key .=) mbValue

data DigiGSTLineItem = DigiGSTLineItem
  { itemNo :: Int,
    supplyType :: Text,
    hsnsacCode :: Text,
    isService :: Text,
    itemUqc :: Text,
    itemQty :: Int,
    unitPrice :: Double,
    itemAmt :: Double,
    taxableVal :: Double,
    igstRt :: Double,
    igstAmt :: Double,
    cgstRt :: Double,
    cgstAmt :: Double,
    sgstRt :: Double,
    sgstAmt :: Double,
    totalItemAmt :: Double
  }
  deriving (Show, Eq, Generic)

instance ToJSON DigiGSTLineItem where
  toJSON = genericToJSON defaultOptions
  toEncoding DigiGSTLineItem {..} =
    pairs $
      "itemNo" .= itemNo
        <> "supplyType" .= supplyType
        <> "hsnsacCode" .= hsnsacCode
        <> "isService" .= isService
        <> "itemUqc" .= itemUqc
        <> "itemQty" .= itemQty
        <> AE.pair "unitPrice" (fixedDouble 2 unitPrice)
        <> AE.pair "itemAmt" (fixedDouble 2 itemAmt)
        <> AE.pair "taxableVal" (fixedDouble 2 taxableVal)
        <> AE.pair "igstRt" (fixedDouble 3 igstRt)
        <> AE.pair "igstAmt" (fixedDouble 2 igstAmt)
        <> AE.pair "cgstRt" (fixedDouble 3 cgstRt)
        <> AE.pair "cgstAmt" (fixedDouble 2 cgstAmt)
        <> AE.pair "sgstRt" (fixedDouble 3 sgstRt)
        <> AE.pair "sgstAmt" (fixedDouble 2 sgstAmt)
        <> AE.pair "totalItemAmt" (fixedDouble 2 totalItemAmt)

-- ---------------------------------------------------------------------------
-- Generate e-invoice response
-- ---------------------------------------------------------------------------

-- | Response envelope. hdr.status is "S" on success and "E" on failure; resp
-- carries the IRN details on success and the error fields on failure.
data DigiGSTInvoiceResp = DigiGSTInvoiceResp
  { hdr :: DigiGSTRespHdr,
    resp :: Maybe DigiGSTInvoiceRespData
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype DigiGSTRespHdr = DigiGSTRespHdr
  { status :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data DigiGSTInvoiceRespData = DigiGSTInvoiceRespData
  { ackNo :: Maybe Text,
    ackDt :: Maybe Text,
    irn :: Maybe Text,
    signedInvoice :: Maybe Text,
    signedQRCode :: Maybe Text,
    status :: Maybe Text,
    errorCode :: Maybe Text,
    errorMessage :: Maybe Text,
    infoErrorCode :: Maybe Text,
    infoErrorMessage :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON DigiGSTInvoiceRespData where
  parseJSON = withObject "DigiGSTInvoiceRespData" $ \v ->
    DigiGSTInvoiceRespData
      <$> (v .:? "AckNo" <|> (fmap (T.pack . show) <$> (v .:? "AckNo" :: Parser (Maybe Integer))))
      <*> v .:? "AckDt"
      <*> v .:? "Irn"
      <*> v .:? "SignedInvoice"
      <*> v .:? "SignedQRCode"
      <*> v .:? "Status"
      <*> v .:? "errorCode"
      <*> v .:? "errorMessage"
      <*> v .:? "infoErrorCode"
      <*> v .:? "infoErrorMessage"
