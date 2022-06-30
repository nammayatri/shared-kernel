module Beckn.Types.Core.Taxi.Select
  ( module Beckn.Types.Core.Taxi.Select,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Select.Order as Reexport
import Beckn.Prelude

newtype SelectMessage = SelectMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
