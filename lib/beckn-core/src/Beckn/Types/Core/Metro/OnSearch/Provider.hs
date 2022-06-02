module Beckn.Types.Core.Metro.OnSearch.Provider where

import Beckn.Types.Core.Metro.OnSearch.Descriptor (Descriptor)
import Beckn.Types.Core.Metro.OnSearch.Item
import Beckn.Types.Core.Metro.OnSearch.Location
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (exp, id)

data Provider = Provider
  { id :: Text,
    descriptor :: Descriptor,
    locations :: [Location],
    items :: [Item]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
