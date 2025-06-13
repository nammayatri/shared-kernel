{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Insurance.Types where

import Data.Aeson.Types
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Storage.Esqueleto (derivePersistField)

data InsuranceService = Acko
  deriving (Show, Read, Eq, Ord, Generic)

$(mkBeamInstancesForEnumAndList ''InsuranceService)
derivePersistField "InsuranceService"

-- Generic instances for type with single value will not work
instance FromJSON InsuranceService where
  parseJSON (String "Acko") = pure Acko
  parseJSON (String _) = parseFail "Expected \"Acko\""
  parseJSON e = typeMismatch "String" e

instance ToJSON InsuranceService where
  toJSON = String . show
