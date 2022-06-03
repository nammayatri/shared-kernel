module Beckn.Types.Core.Taxi.Common.FareProductType where

import Beckn.Prelude hiding (show)

data FareProductType
  = ONE_WAY_TRIP
  | RENTAL_TRIP
  deriving
    ( Eq,
      Ord,
      Generic,
      ToSchema,
      Show,
      FromJSON,
      ToJSON,
      Read
    )
