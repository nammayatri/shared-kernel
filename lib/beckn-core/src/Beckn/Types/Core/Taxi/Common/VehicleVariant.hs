module Beckn.Types.Core.Taxi.Common.VehicleVariant where

import Beckn.Prelude

data VehicleVariant = SEDAN | SUV | HATCHBACK
  deriving
    ( Show,
      Eq,
      Read,
      Generic,
      ToJSON,
      FromJSON,
      ToSchema,
      ToParamSchema,
      Enum,
      Bounded
    )
