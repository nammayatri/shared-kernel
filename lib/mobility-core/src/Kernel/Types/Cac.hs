module Kernel.Types.Cac where

import Data.Aeson

data CACValue a = CACValue
  { cacValue :: Value
  }

class FromJSONCAC a where
  fromJSONCAC :: FromJSON a => CACValue a -> Result a
