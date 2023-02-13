module Kernel.Types.Registry.Country (Country (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)

data Country = Country
  { name :: Maybe Text,
    code :: Maybe Text
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON Country where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Country where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
