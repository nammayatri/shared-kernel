{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Kernel.External.GSTEInvoice.CharteredInfo.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.External.Encryption
import Kernel.Prelude

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for the Chartered Information Systems GSP e-invoice API.
data CharteredInfoConfig = CharteredInfoConfig
  { authUrl :: BaseUrl, -- e.g. https://gstsandbox.charteredinfo.com (for /eivital/dec/v1.04/auth)
    invoiceUrl :: BaseUrl, -- e.g. https://gstsandbox.charteredinfo.com (for /eicore/dec/v1.03/Invoice)
    aspId :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    gstin :: Text,
    userName :: Text,
    eInvPwd :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Auth API response
-- ---------------------------------------------------------------------------

-- | Top-level response envelope from /auth.
data AuthResp = AuthResp
  { status :: Int,
    authData :: Maybe AuthData,
    errorDetails :: Maybe [EInvoiceError],
    infoDtls :: Maybe [Value]
  }
  deriving (Show, Eq, Generic)

instance FromJSON AuthResp where
  parseJSON = withObject "AuthResp" $ \v ->
    AuthResp
      <$> v .: "Status"
      <*> v .:? "Data"
      <*> v .:? "ErrorDetails"
      <*> v .:? "InfoDtls"

instance ToJSON AuthResp where
  toJSON AuthResp {..} =
    object
      [ "Status" .= status,
        "Data" .= authData,
        "ErrorDetails" .= errorDetails,
        "InfoDtls" .= infoDtls
      ]

data AuthData = AuthData
  { clientId :: Text,
    userName :: Text,
    authToken :: Text,
    sek :: Text,
    tokenExpiry :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON AuthData where
  parseJSON = withObject "AuthData" $ \v ->
    AuthData
      <$> v .: "ClientId"
      <*> v .: "UserName"
      <*> v .: "AuthToken"
      <*> v .: "Sek"
      <*> v .: "TokenExpiry"

instance ToJSON AuthData where
  toJSON AuthData {..} =
    object
      [ "ClientId" .= clientId,
        "UserName" .= userName,
        "AuthToken" .= authToken,
        "Sek" .= sek,
        "TokenExpiry" .= tokenExpiry
      ]

-- ---------------------------------------------------------------------------
-- Invoice API request (e-invoice payload)
-- ---------------------------------------------------------------------------

data EInvoicePayload = EInvoicePayload
  { version :: Text,
    tranDtls :: TranDtls,
    docDtls :: DocDtls,
    sellerDtls :: SellerDtls,
    buyerDtls :: BuyerDtls,
    itemList :: [ItemEntry],
    valDtls :: ValDtls,
    ewbDtls :: Maybe EwbDtls
  }
  deriving (Show, Eq, Generic)

instance FromJSON EInvoicePayload where
  parseJSON = withObject "EInvoicePayload" $ \v ->
    EInvoicePayload
      <$> v .: "Version"
      <*> v .: "TranDtls"
      <*> v .: "DocDtls"
      <*> v .: "SellerDtls"
      <*> v .: "BuyerDtls"
      <*> v .: "ItemList"
      <*> v .: "ValDtls"
      <*> v .:? "EwbDtls"

instance ToJSON EInvoicePayload where
  toJSON EInvoicePayload {..} =
    object
      [ "Version" .= version,
        "TranDtls" .= tranDtls,
        "DocDtls" .= docDtls,
        "SellerDtls" .= sellerDtls,
        "BuyerDtls" .= buyerDtls,
        "ItemList" .= itemList,
        "ValDtls" .= valDtls,
        "EwbDtls" .= ewbDtls
      ]

data TranDtls = TranDtls
  { taxSch :: Text,
    supTyp :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON TranDtls where
  parseJSON = withObject "TranDtls" $ \v ->
    TranDtls <$> v .: "TaxSch" <*> v .: "SupTyp"

instance ToJSON TranDtls where
  toJSON TranDtls {..} =
    object ["TaxSch" .= taxSch, "SupTyp" .= supTyp]

data DocDtls = DocDtls
  { typ :: Text,
    no :: Text,
    dt :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON DocDtls where
  parseJSON = withObject "DocDtls" $ \v ->
    DocDtls <$> v .: "Typ" <*> v .: "No" <*> v .: "Dt"

instance ToJSON DocDtls where
  toJSON DocDtls {..} =
    object ["Typ" .= typ, "No" .= no, "Dt" .= dt]

data SellerDtls = SellerDtls
  { gstin :: Text,
    lglNm :: Text,
    addr1 :: Text,
    loc :: Text,
    pin :: Int,
    stcd :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SellerDtls where
  parseJSON = withObject "SellerDtls" $ \v ->
    SellerDtls
      <$> v .: "Gstin"
      <*> v .: "LglNm"
      <*> v .: "Addr1"
      <*> v .: "Loc"
      <*> v .: "Pin"
      <*> v .: "Stcd"

instance ToJSON SellerDtls where
  toJSON SellerDtls {..} =
    object
      [ "Gstin" .= gstin,
        "LglNm" .= lglNm,
        "Addr1" .= addr1,
        "Loc" .= loc,
        "Pin" .= pin,
        "Stcd" .= stcd
      ]

data BuyerDtls = BuyerDtls
  { gstin :: Text,
    lglNm :: Text,
    addr1 :: Text,
    loc :: Text,
    pin :: Int,
    stcd :: Text,
    pos :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON BuyerDtls where
  parseJSON = withObject "BuyerDtls" $ \v ->
    BuyerDtls
      <$> v .: "Gstin"
      <*> v .: "LglNm"
      <*> v .: "Addr1"
      <*> v .: "Loc"
      <*> v .: "Pin"
      <*> v .: "Stcd"
      <*> v .:? "Pos"

instance ToJSON BuyerDtls where
  toJSON BuyerDtls {..} =
    object
      [ "Gstin" .= gstin,
        "LglNm" .= lglNm,
        "Addr1" .= addr1,
        "Loc" .= loc,
        "Pin" .= pin,
        "Stcd" .= stcd,
        "Pos" .= pos
      ]

data ItemEntry = ItemEntry
  { slNo :: Text,
    isServc :: Text,
    hsnCd :: Text,
    qty :: Int,
    unit :: Text,
    unitPrice :: Double,
    totAmt :: Double,
    discount :: Double,
    preTaxVal :: Double,
    assAmt :: Double,
    gstRt :: Double,
    igstAmt :: Double,
    cgstAmt :: Double,
    sgstAmt :: Double,
    totItemVal :: Double
  }
  deriving (Show, Eq, Generic)

instance FromJSON ItemEntry where
  parseJSON = withObject "ItemEntry" $ \v ->
    ItemEntry
      <$> v .: "SlNo"
      <*> v .: "IsServc"
      <*> v .: "HsnCd"
      <*> v .: "Qty"
      <*> v .: "Unit"
      <*> v .: "UnitPrice"
      <*> v .: "TotAmt"
      <*> v .: "Discount"
      <*> v .: "PreTaxVal"
      <*> v .: "AssAmt"
      <*> v .: "GstRt"
      <*> v .: "IgstAmt"
      <*> v .: "CgstAmt"
      <*> v .: "SgstAmt"
      <*> v .: "TotItemVal"

instance ToJSON ItemEntry where
  toJSON ItemEntry {..} =
    object
      [ "SlNo" .= slNo,
        "IsServc" .= isServc,
        "HsnCd" .= hsnCd,
        "Qty" .= qty,
        "Unit" .= unit,
        "UnitPrice" .= unitPrice,
        "TotAmt" .= totAmt,
        "Discount" .= discount,
        "PreTaxVal" .= preTaxVal,
        "AssAmt" .= assAmt,
        "GstRt" .= gstRt,
        "IgstAmt" .= igstAmt,
        "CgstAmt" .= cgstAmt,
        "SgstAmt" .= sgstAmt,
        "TotItemVal" .= totItemVal
      ]

data ValDtls = ValDtls
  { assVal :: Double,
    cgstVal :: Double,
    sgstVal :: Double,
    igstVal :: Double,
    totInvVal :: Double
  }
  deriving (Show, Eq, Generic)

instance FromJSON ValDtls where
  parseJSON = withObject "ValDtls" $ \v ->
    ValDtls
      <$> v .: "AssVal"
      <*> v .: "CgstVal"
      <*> v .: "SgstVal"
      <*> v .: "IgstVal"
      <*> v .: "TotInvVal"

instance ToJSON ValDtls where
  toJSON ValDtls {..} =
    object
      [ "AssVal" .= assVal,
        "CgstVal" .= cgstVal,
        "SgstVal" .= sgstVal,
        "IgstVal" .= igstVal,
        "TotInvVal" .= totInvVal
      ]

data EwbDtls = EwbDtls
  { distance :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON EwbDtls where
  parseJSON = withObject "EwbDtls" $ \v ->
    EwbDtls <$> v .: "Distance"

instance ToJSON EwbDtls where
  toJSON EwbDtls {..} =
    object ["Distance" .= distance]

-- ---------------------------------------------------------------------------
-- Invoice API response
-- ---------------------------------------------------------------------------

-- | Top-level response envelope from /Invoice.
-- On success (Status "1"), Data is a JSON-encoded string containing IRN details.
-- On failure (Status "0"), Data is null and ErrorDetails/InfoDtls contain error info.
data InvoiceResp = InvoiceResp
  { status :: Text,
    invoiceData :: Maybe InvoiceRespData,
    errorDetails :: Maybe [EInvoiceError],
    infoDtls :: Maybe [InfoDtl]
  }
  deriving (Show, Eq, Generic)

instance FromJSON InvoiceResp where
  parseJSON = withObject "InvoiceResp" $ \v ->
    InvoiceResp
      <$> v .: "Status"
      <*> v .:? "Data"
      <*> v .:? "ErrorDetails"
      <*> v .:? "InfoDtls"

instance ToJSON InvoiceResp where
  toJSON InvoiceResp {..} =
    object
      [ "Status" .= status,
        "Data" .= invoiceData,
        "ErrorDetails" .= errorDetails,
        "InfoDtls" .= infoDtls
      ]

-- | Parsed invoice response data. The API returns Data as a JSON string,
-- so we parse the string into this structured type.
data InvoiceRespData = InvoiceRespData
  { ackNo :: Text,
    ackDt :: Text,
    irn :: Text,
    signedInvoice :: Maybe Text,
    signedQRCode :: Maybe Text,
    status :: Maybe Text,
    rawData :: Value
  }
  deriving (Show, Eq, Generic)

instance FromJSON InvoiceRespData where
  parseJSON v = case v of
    String jsonStr ->
      case eitherDecodeStrict (TE.encodeUtf8 jsonStr) of
        Left err -> fail $ "Failed to parse Data JSON string: " <> err
        Right obj -> parseFromObject obj v
    Object _ -> parseFromObject v v
    _ -> fail "Expected String or Object for InvoiceResp.Data"
    where
      parseFromObject obj raw = do
        flip (withObject "InvoiceRespData") obj $ \o ->
          InvoiceRespData
            <$> (o .: "AckNo" <|> (T.pack . show <$> (o .: "AckNo" :: Parser Int)))
            <*> o .: "AckDt"
            <*> o .: "Irn"
            <*> o .:? "SignedInvoice"
            <*> o .:? "SignedQRCode"
            <*> o .:? "Status"
            <*> pure raw

instance ToJSON InvoiceRespData where
  toJSON InvoiceRespData {..} = rawData

-- ---------------------------------------------------------------------------
-- Shared types
-- ---------------------------------------------------------------------------

data EInvoiceError = EInvoiceError
  { errorCode :: Text,
    errorMessage :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON EInvoiceError where
  parseJSON = withObject "EInvoiceError" $ \v ->
    EInvoiceError
      <$> v .: "ErrorCode"
      <*> v .: "ErrorMessage"

instance ToJSON EInvoiceError where
  toJSON EInvoiceError {..} =
    object
      [ "ErrorCode" .= errorCode,
        "ErrorMessage" .= errorMessage
      ]

data InfoDtl = InfoDtl
  { infCd :: Text,
    desc :: Maybe Value
  }
  deriving (Show, Eq, Generic)

instance FromJSON InfoDtl where
  parseJSON = withObject "InfoDtl" $ \v ->
    InfoDtl
      <$> v .: "InfCd"
      <*> v .:? "Desc"

instance ToJSON InfoDtl where
  toJSON InfoDtl {..} =
    object
      [ "InfCd" .= infCd,
        "Desc" .= desc
      ]
