module Kernel.Storage.InMem.Management.Types where

import Data.Aeson (Value)
import qualified Data.Aeson as Ae
import Kernel.Prelude
import Kernel.Types.App ()
import Kernel.Types.Common (Seconds)

data InMemKeyEntry = InMemKeyEntry
  { keyName :: Text,
    keySchema :: Maybe Value
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data InMemKeysResponse = InMemKeysResponse
  { keyList :: [InMemKeyEntry],
    -- | Total number of keys matching the (optional) pattern, before
    -- limit/offset are applied. Lets callers paginate without guessing.
    totalKeys :: Int
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
  { keyInfix :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data InMemRefreshResponse = InMemRefreshResponse
  { deletedKeys :: Int,
    remainingKeys :: Int,
    redisNotified :: Bool
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

data SidecarRefreshRequest = SidecarRefreshRequest
  { serviceName :: Text,
    keyInfix :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PodCleanupAck = PodCleanupAck
  { podName :: Text,
    success :: Bool,
    errorMessage :: Maybe Text
  }
  deriving (Generic, Show, ToSchema)

podCleanupAckJsonOptions :: Ae.Options
podCleanupAckJsonOptions = Ae.defaultOptions {Ae.fieldLabelModifier = \f -> if f == "errorMessage" then "error" else f}

instance ToJSON PodCleanupAck where
  toJSON = Ae.genericToJSON podCleanupAckJsonOptions

instance FromJSON PodCleanupAck where
  parseJSON = Ae.genericParseJSON podCleanupAckJsonOptions

data SidecarRefreshResponse = SidecarRefreshResponse
  { service :: Text,
    total :: Int,
    confirmed :: Int,
    pods :: [PodCleanupAck]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
