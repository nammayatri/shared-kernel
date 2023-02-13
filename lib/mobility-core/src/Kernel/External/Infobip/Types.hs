module Kernel.External.Infobip.Types where

import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)

newtype SMSRes = SMSRes
  { messages :: [Message]
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

data Message = Message
  { to :: Text,
    status :: SMSStatus,
    messageId :: Text,
    smsCount :: Integer
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

data SMSStatus = SMSStatus
  { description :: Text,
    groupId :: Int,
    groupName :: Text,
    id :: Int,
    name :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

data WebengageRes = WebengageRes
  { version :: Text,
    messageId :: Text,
    toNumber :: Text,
    status :: Text,
    statusCode :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data InfoBIPConfig = InfoBIPConfig
  { username :: Text,
    password :: Text,
    url :: BaseUrl,
    sender :: Text,
    token :: Text,
    webhookurl :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

newtype WebengageConfig = WebengageConfig
  { url :: BaseUrl
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)
