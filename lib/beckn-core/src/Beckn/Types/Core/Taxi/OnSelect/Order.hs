module Beckn.Types.Core.Taxi.OnSelect.Order where

import Beckn.Types.Core.Taxi.OnSelect.Provider
import Beckn.Utils.JSON (slashedRecordFields)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), fromAesonOptions)
import Beckn.Prelude

newtype Order = Order
  { provider :: Provider
  }
  deriving (Generic, Show)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions slashedRecordFields

instance FromJSON Order where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Order where
  toJSON = genericToJSON slashedRecordFields
