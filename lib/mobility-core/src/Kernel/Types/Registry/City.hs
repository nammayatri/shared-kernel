module Kernel.Types.Registry.City (City (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)

data City = City
  { name :: Maybe Text,
    code :: Maybe Text
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON City where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON City where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
