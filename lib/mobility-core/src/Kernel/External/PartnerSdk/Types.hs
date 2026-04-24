{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.PartnerSdk.Types where

import Data.Aeson.Types
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Storage.Esqueleto (derivePersistField)

data PartnerSdkService = Aarokya
  deriving (Show, Read, Eq, Ord, Generic)

$(mkBeamInstancesForEnumAndList ''PartnerSdkService)
derivePersistField "PartnerSdkService"

instance FromJSON PartnerSdkService where
  parseJSON (String "Aarokya") = pure Aarokya
  parseJSON (String _) = parseFail "Expected \"Aarokya\""
  parseJSON e = typeMismatch "String" e

instance ToJSON PartnerSdkService where
  toJSON = String . show
