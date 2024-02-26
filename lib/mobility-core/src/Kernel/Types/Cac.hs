module Kernel.Types.Cac where

import Data.Aeson
import qualified Data.Aeson.Key as DAK
import Data.Maybe
import qualified Data.Text as Text

data CACValue a = CACValue
  { cacValue :: Value
  }

class FromJSONCAC a where
  fromJSONCAC :: FromJSON a => CACValue a -> Result a

dropKeyFromConfig :: Text.Text -> Key -> Key
dropKeyFromConfig key config =
  case Text.stripPrefix key (DAK.toText config) of
    Just a -> DAK.fromText a
    Nothing -> config
