module Beckn.Types.Core.Taxi.OnSelect
  ( module Beckn.Types.Core.Taxi.OnSelect,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.OnSelect.Descriptor as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Category as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Item as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Order as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Provider as Reexport
import Beckn.Prelude

newtype OnSelectMessage = OnSelectMessage
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
