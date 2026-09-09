{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Kernel.External.SMS.CerfSms.Types where

import Data.Aeson
import EulerHS.Prelude

-- | Both CERF transports answer with the same object shape; the JSON bulk API
-- just wraps a list of them. Error responses carry only @code@, @desc@ and
-- @totalMessageParts@, so everything but @code@ is optional.
data CerfSmsResponse = CerfSmsResponse
  { code :: Text,
    desc :: Maybe Text,
    reqId :: Maybe Text,
    time :: Maybe Text,
    custRef :: Maybe Text,
    partMessageIds :: Maybe [Text],
    totalMessageParts :: Maybe Int,
    campaignName :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | One entry of the @smslist@ array. CERF expects the lowercase keys verbatim.
data CerfSmsListItem = CerfSmsListItem
  { text :: Text,
    -- | Comma separated destinations. We always send exactly one.
    mobiles :: Text,
    messagetype :: Text,
    custref :: Maybe Text,
    templateid :: Maybe Text
  }
  deriving (Generic, Show, Eq)

instance ToJSON CerfSmsListItem where
  toJSON = genericToJSON cerfJsonOptions

instance FromJSON CerfSmsListItem where
  parseJSON = genericParseJSON cerfJsonOptions

data CerfSmsJsonReq = CerfSmsJsonReq
  { username :: Text,
    -- | The api key; CERF names this field @password@ on the JSON API.
    password :: Text,
    senderid :: Text,
    campaignname :: Maybe Text,
    entityid :: Maybe Text,
    smslist :: [CerfSmsListItem]
  }
  deriving (Generic, Show, Eq)

instance ToJSON CerfSmsJsonReq where
  toJSON = genericToJSON cerfJsonOptions

instance FromJSON CerfSmsJsonReq where
  parseJSON = genericParseJSON cerfJsonOptions

-- | The JSON bulk API responds with one element per destination. The manual
-- documents a bare array, but the live endpoint wraps it in a @responseList@
-- object, so accept either shape.
newtype CerfSmsJsonRes = CerfSmsJsonRes
  { responses :: [CerfSmsResponse]
  }
  deriving (Generic, Show, Eq)

instance FromJSON CerfSmsJsonRes where
  parseJSON v =
    (CerfSmsJsonRes <$> withObject "CerfSmsJsonRes" (.: "responseList") v)
      <|> (CerfSmsJsonRes <$> parseJSON v)

instance ToJSON CerfSmsJsonRes where
  toJSON (CerfSmsJsonRes rs) = object ["responseList" .= rs]

-- | CERF rejects requests carrying explicit @null@s for the optional fields.
cerfJsonOptions :: Options
cerfJsonOptions = defaultOptions {omitNothingFields = True}
