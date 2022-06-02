module Beckn.Types.Core.Metro.OnSearch.Location where

import Beckn.Types.Core.Metro.OnSearch.Descriptor (Descriptor)
import Beckn.Types.Core.Metro.OnSearch.Gps (Gps)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data Location = Location
  { id :: Text,
    descriptor :: Descriptor,
    gps :: Gps,
    station_code :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
