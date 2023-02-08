module Kernel.Types.Registry
  ( module Kernel.Types.Registry,
    module E,
  )
where

import Kernel.Prelude
import Kernel.Types.Registry.Subscriber as E

class Registry m where
  registryLookup :: BaseUrl -> SimpleLookupRequest -> m (Maybe Subscriber)

data SimpleLookupRequest = SimpleLookupRequest
  { unique_key_id :: Text,
    subscriber_id :: Text
  }
  deriving (Eq, Ord)

lookupRequestToRedisKey :: SimpleLookupRequest -> Text
lookupRequestToRedisKey SimpleLookupRequest {..} = unique_key_id <> "|" <> subscriber_id
