module Kernel.Storage.InMem.Management.Types where

import Data.Aeson (Value)
import Kernel.Prelude
import Kernel.Types.App ()
import Kernel.Types.Common (Seconds)

data InMemKeyEntry = InMemKeyEntry
  { keyName :: Text,
    keySchema :: Maybe Value
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype InMemKeysResponse = InMemKeysResponse
  { keyList :: [InMemKeyEntry]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype InMemGetRequest = InMemGetRequest
  { key :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data InMemGetResponse = InMemGetResponse
  { found :: Bool,
    value :: Maybe Value
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype InMemRefreshRequest = InMemRefreshRequest
  { keyPrefix :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data InMemRefreshResponse = InMemRefreshResponse
  { deletedKeys :: Int,
    remainingKeys :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data InMemServerInfoResponse = InMemServerInfoResponse
  { serviceName :: Text,
    podName :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RegisterKeyRequest = RegisterKeyRequest
  { keyName :: Text,
    keySchema :: Maybe Value,
    ttlInSeconds :: Maybe Seconds
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RegisterKeyResponse = RegisterKeyResponse
  { registered :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
