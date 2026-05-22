{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kernel.External.SMS.KaleyraSms.Types where

import Control.Applicative ((<$>), (<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson.Types (Options (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Kernel.Prelude (Eq, Int, Maybe, Show)

data KaleyraSmsReq = KaleyraSmsReq
  { to :: Text,
    messageType :: Text,
    sender :: Text,
    body :: Text
  }
  deriving (Generic, Eq, Show)

instance ToJSON KaleyraSmsReq where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "messageType" -> "type"
            other -> other
        }

instance FromJSON KaleyraSmsReq where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "messageType" -> "type"
            other -> other
        }

-- Success response from Kaleyra
data KaleyraSmsData = KaleyraSmsData
  { message_id :: Text,
    recipient :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data KaleyraSmsSuccessRes = KaleyraSmsSuccessRes
  { id :: Text,
    totalCount :: Maybe Int,
    dataField :: Maybe [KaleyraSmsData]
  }
  deriving (Show, Eq, Generic)

instance FromJSON KaleyraSmsSuccessRes where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "dataField" -> "data"
            other -> other
        }

instance ToJSON KaleyraSmsSuccessRes where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "dataField" -> "data"
            other -> other
        }

-- Error response from Kaleyra
data KaleyraSmsErrorRes = KaleyraSmsErrorRes
  { code :: Text,
    message :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- Combined response type
data KaleyraSmsRes
  = KaleyraSmsSuccess KaleyraSmsSuccessRes
  | KaleyraSmsError KaleyraSmsErrorRes
  deriving (Show, Generic)

instance FromJSON KaleyraSmsRes where
  parseJSON v =
    (KaleyraSmsSuccess <$> parseJSON v) <|> (KaleyraSmsError <$> parseJSON v)

instance ToJSON KaleyraSmsRes where
  toJSON (KaleyraSmsSuccess res) = toJSON res
  toJSON (KaleyraSmsError err) = toJSON err
