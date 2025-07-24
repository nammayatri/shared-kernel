module Kernel.External.ConversionEvent.Meta.Types
  ( module Kernel.External.ConversionEvent.Meta.Types,
  )
where

import Data.Aeson
import Data.Text
import EulerHS.Prelude
import Kernel.Prelude

data MetaConversionReqType = MetaConversionReqType
  { data1 :: [MetaConversionDataType]
  }
  deriving (Generic, FromJSON, ToJSON)

data MetaConversionDataType = MetaConversionDataType
  { eventName :: Text,
    eventTime :: Int,
    userData :: UserDataType,
    customData :: Maybe CustomDataType,
    eventSourceUrl :: Maybe Text,
    optOut :: Maybe Bool,
    eventId :: Maybe Text,
    actionSource :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON)

data UserDataType = UserDataType
  { em :: Maybe Text,
    ph :: Maybe Text,
    fn :: Maybe Text,
    ln :: Maybe Text,
    db :: Maybe Text,
    ge :: Maybe Text,
    ct :: Maybe Text,
    st :: Maybe Text,
    zp :: Maybe Text,
    country :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON)

data CustomDataType = CustomDataType
  { value :: Maybe Double,
    currency :: Maybe Text,
    contentName :: Maybe Text,
    contentCategory :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON)

data MetaConfig = MetaConfig
  { url :: BaseUrl,
    apiVersion :: Text,
    pixelId :: Text,
    accessToken :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
