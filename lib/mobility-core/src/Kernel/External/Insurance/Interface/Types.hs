module Kernel.External.Insurance.Interface.Types where

import Data.Aeson
import qualified Kernel.External.Insurance.Acko.Types as Acko
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

data InsuranceConfig = AckoInsuranceConfig Acko.AckoInsuranceConfig deriving (Show, Eq, Generic, ToJSON, FromJSON)

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
