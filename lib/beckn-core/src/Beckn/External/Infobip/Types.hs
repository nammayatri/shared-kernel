module Beckn.External.Infobip.Types where

import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude hiding (encodeUtf8, fromStrict, id, toStrict)
import Servant.Client (BaseUrl)

newtype SMSReq = SMSReq {messages :: [MessageReq]}
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

data SMSRes = SMSRes
  { bulkId :: Maybe Text,
    messages :: Maybe [MessageRes],
    requestError :: Maybe SMSErr
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

data SMSErr = SMSErr
  { messageId :: Text,
    text :: Text,
    validationErrors :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

data MessageReq = MessageReq
  { destinations :: [SMSDestination],
    from :: Text,
    text :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

newtype SMSDestination = SMSDestination {to :: Text}
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

data MessageRes = MessageRes
  { messageId :: Text,
    status :: SMSStatus,
    to :: Text
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

data InfoBIPConfig = InfoBIPConfig
  { username :: Text,
    password :: Text,
    url :: BaseUrl,
    sender :: Text,
    token :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)
