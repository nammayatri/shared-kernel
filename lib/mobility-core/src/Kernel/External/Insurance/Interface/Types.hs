module Kernel.External.Insurance.Interface.Types where

import Data.Aeson
import qualified Kernel.External.Insurance.Acko.Types as Acko
import qualified Kernel.External.Insurance.IffcoTokio.Types as IffcoTokio
import Kernel.Prelude

data InsuranceRequest = InsuranceRequest
  { trip :: Trip,
    category :: Text,
    customer :: Customer,
    planType :: Int,
    referenceId :: Text,
    plan :: Text,
    partnerId :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Trip = Trip
  { journey :: [Journey],
    endDate :: UTCTime,
    bookingId :: Text,
    startDate :: UTCTime,
    bookingDate :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Journey = Journey
  { mode :: Maybe Text,
    origin :: Location,
    person :: [Person],
    destination :: Location
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Location = Location
  { city :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Person = Person
  { insured :: Insured
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Insured = Insured
  { name :: Text,
    phone :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Customer = Customer
  { id :: Text,
    name :: Text,
    phone :: Text,
    state :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data InsuranceConfig
  = AckoInsuranceConfig Acko.AckoInsuranceConfig
  | IffcoTokioInsuranceConfig IffcoTokio.IffcoTokioConfig
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data InsuranceResponse = InsuranceResponse
  { policyId :: Text,
    referenceId :: Text,
    planType :: Maybe Int,
    startDate :: Maybe UTCTime,
    endDate :: Maybe UTCTime,
    policyNumber :: Text,
    certificatePdf :: Maybe Text,
    certificateUrl :: Maybe Text,
    person :: Maybe [PersonResponse],
    premium :: Maybe Premium
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PersonResponse = PersonResponse
  { insured :: InsuredResponse
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data InsuredResponse = InsuredResponse
  { name :: Maybe Text,
    phone :: Maybe Text,
    policyNumber :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Premium = Premium
  { amount :: Double,
    gst :: Maybe Double,
    sgst :: Maybe Double,
    cgst :: Maybe Double,
    igst :: Maybe Double,
    breakup :: Maybe [Value]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- IFFCO Tokio home declaration types

-- | Interface-level request for RegisterHomeDeclaration.
-- |invoiceRequestNumber| is the correlation key: it is returned immediately
-- in |HomeDeclarationInstantResp| and later echoed in |HomeDeclarationAsyncResp|
-- so the caller can match the async result to the original request.
data HomeDeclarationReq = HomeDeclarationReq
  { insuredAddress :: Text,
    insuredEmail :: Text,
    insuredMobile :: Text,
    insuredName :: Text,
    -- | Date in MM/DD/YYYY format as required by IFFCO Tokio
    invoiceDate :: Text,
    -- | Caller-generated correlation ID
    invoiceRequestNumber :: Text,
    ewCommencesOn :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Returned immediately after firing the async API call.
newtype HomeDeclarationInstantResp = HomeDeclarationInstantResp
  { invoiceRequestNumber :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Delivered to the callback once the IFFCO Tokio API responds.
data HomeDeclarationAsyncResp = HomeDeclarationAsyncResp
  { -- | Echoed back so callers can correlate with the instant response
    invoiceRequestNumber :: Text,
    certificateNumber :: Text,
    declarationId :: Text,
    status :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
