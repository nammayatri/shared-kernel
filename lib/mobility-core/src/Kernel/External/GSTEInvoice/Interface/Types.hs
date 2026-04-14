module Kernel.External.GSTEInvoice.Interface.Types where

import Data.Aeson (Value)
import qualified Kernel.External.GSTEInvoice.CharteredInfo.Types as CharteredInfo
import Kernel.Prelude

-- | Provider-agnostic configuration sum type.
data GSTEInvoiceConfig
  = CharteredInfoEInvoiceConfig CharteredInfo.CharteredInfoConfig
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Provider-agnostic auth response.
data EInvoiceAuthResp = EInvoiceAuthResp
  { authToken :: Text,
    tokenExpiry :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Provider-agnostic invoice generation response.
data EInvoiceGenerateResp = EInvoiceGenerateResp
  { status :: Text,
    irn :: Maybe Text,
    ackNo :: Maybe Text,
    ackDt :: Maybe Text,
    signedInvoice :: Maybe Text,
    signedQRCode :: Maybe Text,
    errors :: Maybe [EInvoiceErrorDetail],
    infoDtls :: Maybe [EInvoiceInfoDetail]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Provider-agnostic error detail.
data EInvoiceErrorDetail = EInvoiceErrorDetail
  { errorCode :: Text,
    errorMessage :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Provider-agnostic info detail (e.g. DUPIRN with IRN/AckNo, EWBERR).
data EInvoiceInfoDetail = EInvoiceInfoDetail
  { infoCode :: Text,
    description :: Maybe Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
