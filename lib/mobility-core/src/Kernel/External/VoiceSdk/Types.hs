{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.VoiceSdk.Types where

import Data.Aeson.Types
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Storage.Esqueleto (derivePersistField)

data VoiceSdkService = BreezeBuddy
  deriving (Show, Read, Eq, Ord, Generic)

$(mkBeamInstancesForEnumAndList ''VoiceSdkService)
derivePersistField "VoiceSdkService"

instance FromJSON VoiceSdkService where
  parseJSON (String "BreezeBuddy") = pure BreezeBuddy
  parseJSON (String _) = parseFail "Expected \"BreezeBuddy\""
  parseJSON e = typeMismatch "String" e

instance ToJSON VoiceSdkService where
  toJSON = String . show
