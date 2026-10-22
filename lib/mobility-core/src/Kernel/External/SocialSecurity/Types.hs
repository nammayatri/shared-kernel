{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.SocialSecurity.Types where

import Data.Aeson.Types
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Storage.Esqueleto (derivePersistField)

data SocialSecurityService = EShram
  deriving (Show, Read, Eq, Ord, Generic)

$(mkBeamInstancesForEnumAndList ''SocialSecurityService)
derivePersistField "SocialSecurityService"

instance FromJSON SocialSecurityService where
  parseJSON (String "EShram") = pure EShram
  parseJSON (String _) = parseFail "Expected \"EShram\""
  parseJSON e = typeMismatch "String" e

instance ToJSON SocialSecurityService where
  toJSON = String . show
