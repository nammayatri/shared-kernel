{-# LANGUAGE DerivingVia #-}

module Kernel.Types.Beckn.Domain (Domain (..)) where

import Data.Aeson
import Data.OpenApi hiding (Example)
import EulerHS.Prelude
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.Example
import Kernel.Utils.GenericPretty
import Kernel.Utils.JSON (replaceUnderscoresString)

data Domain
  = MOBILITY
  | LOCAL_RETAIL
  | FOOD_AND_BEVERAGE
  | HEALTHCARE
  | METRO
  | PARKING
  | PUBLIC_TRANSPORT
  | LOGISTICS
  deriving (Eq, Generic, Show, Read, FromDhall)
  deriving (PrettyShow) via Showable Domain

instance Example Domain where
  example = MOBILITY

customAesonOptions :: Options
customAesonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "MOBILITY" -> "nic2004:60221"
        "LOCAL_RETAIL" -> "nic2004:52110"
        "METRO" -> "nic2004:60212"
        "PARKING" -> "nic2004:63031"
        "PUBLIC_TRANSPORT" -> "nic2004:63032"
        "LOGISTICS" -> "nic2004:60232"
        val -> replaceUnderscoresString val, -- TODO: update remaining domains with codes
      sumEncoding = UntaggedValue
    }

instance ToSchema Domain where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions customAesonOptions

instance ToJSON Domain where
  toJSON = genericToJSON customAesonOptions

instance FromJSON Domain where
  parseJSON = genericParseJSON customAesonOptions
