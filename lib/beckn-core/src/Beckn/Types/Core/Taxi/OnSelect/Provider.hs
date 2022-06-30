module Beckn.Types.Core.Taxi.OnSelect.Provider
  ( Provider (..),
    ProviderTags (..),
  )
where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.OnSelect.Category
import Beckn.Types.Core.Taxi.OnSelect.Descriptor
import Beckn.Types.Core.Taxi.OnSelect.Item
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.Aeson
import Data.OpenApi (ToSchema (..), fromAesonOptions)

data Provider = Provider
  { id :: Text,
    descriptor :: Descriptor,
    categories :: [Category],
    items :: [Item], --FIXME this should be list of only RENTAL or only ONE_WAY items
    contacts :: Text,
    tags :: ProviderTags
  }
  deriving (Generic, Show)

instance ToJSON Provider where
  toJSON = genericToJSON providerJSONOptions

instance FromJSON Provider where
  parseJSON = genericParseJSON providerJSONOptions

instance ToSchema Provider where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions providerJSONOptions

providerJSONOptions :: Options
providerJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "contacts" -> "./komn/contacts"
        a -> a
    }

data ProviderTags = ProviderTags
  { rides_inprogress :: Int,
    rides_completed :: Int,
    rides_confirmed :: Int,
    distance_to_nearest_driver :: Maybe DecimalValue
  }
  deriving (Generic, Show)

instance ToJSON ProviderTags where
  toJSON = genericToJSON providerTagsJSONOptions

instance FromJSON ProviderTags where
  parseJSON = genericParseJSON providerTagsJSONOptions

instance ToSchema ProviderTags where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions providerTagsJSONOptions

providerTagsJSONOptions :: Options
providerTagsJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "rides_inprogress" -> "./komn/rides_inprogress"
        "rides_confirmed" -> "./komn/rides_confirmed"
        "rides_completed" -> "./komn/rides_completed"
        "distance_to_nearest_driver" -> "./komn/distance_to_nearest_driver"
        a -> a
    }
