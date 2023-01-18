{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Registry
  ( module Kernel.Types.Registry,
    module E,
  )
where

import Kernel.Prelude
import Kernel.Types.Registry.Subscriber as E

class Registry m where
  registryLookup :: SimpleLookupRequest -> m (Maybe Subscriber)

data SimpleLookupRequest = SimpleLookupRequest
  { unique_key_id :: Text,
    subscriber_id :: Text
  }
  deriving (Eq, Ord)

lookupRequestToRedisKey :: SimpleLookupRequest -> Text
lookupRequestToRedisKey SimpleLookupRequest {..} = unique_key_id <> "|" <> subscriber_id
