module Kernel.External.Insurance.Acko.Types where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude

data AckoInsuranceRequest = AckoInsuranceRequest
  { trip :: Trip,
    category :: Text,
    customer :: Customer,
    plan_type :: Int,
    reference_id :: Text,
    plan :: Text,
    partner_id :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Trip = Trip
  { journey :: [Journey],
    end_date :: UTCTime,
    booking_id :: Text,
    start_date :: UTCTime,
    booking_date :: UTCTime
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

data AckoInsuranceResponse = AckoInsuranceResponse
  { policy_id :: Text,
    reference_id :: Text,
    plan_type :: Int,
    start_date :: Maybe UTCTime,
    end_date :: Maybe UTCTime,
    policy_number :: Text,
    certificate_pdf :: Text,
    certificate_url :: Text,
    person :: [PersonResponse],
    premium :: Premium
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Premium = Premium
  { amount :: Double,
    gst :: Double,
    sgst :: Double,
    cgst :: Double,
    igst :: Double,
    breakup :: [Value]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PersonResponse = PersonResponse
  { insured :: InsuredResponse
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data InsuredResponse = InsuredResponse
  { name :: Text,
    phone :: Text,
    policy_number :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AckoInsuranceConfig = AckoInsuranceConfig
  { url :: BaseUrl,
    partnerId :: Text,
    username :: Text,
    state :: Text,
    apiKey :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
