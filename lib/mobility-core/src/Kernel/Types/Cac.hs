module Kernel.Types.Cac where

data CACValue a = CACValue
  { cacValue :: Value
  }

class FromJSONCAC a where
  fromJSONCAC :: FromJSON a => CACValue a -> a
