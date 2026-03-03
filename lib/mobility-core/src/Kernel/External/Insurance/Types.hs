{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Insurance.Types where

import Data.Aeson.Types
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Storage.Esqueleto (derivePersistField)

data InsuranceService = Acko | IffcoTokio
  deriving (Show, Read, Eq, Ord, Generic)

$(mkBeamInstancesForEnumAndList ''InsuranceService)
derivePersistField "InsuranceService"

instance FromJSON InsuranceService where
  parseJSON (String "Acko") = pure Acko
  parseJSON (String "IffcoTokio") = pure IffcoTokio
  parseJSON (String _) = parseFail "Expected \"Acko\" or \"IffcoTokio\""
  parseJSON e = typeMismatch "String" e

instance ToJSON InsuranceService where
  toJSON = String . show
