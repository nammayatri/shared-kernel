{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.External.Whatsapp.Interface.Types
  ( module Kernel.External.Whatsapp.Interface.Types
  )
where

import qualified Kernel.External.Whatsapp.GupShup.Config as GupShup
import qualified Kernel.External.Whatsapp.Types as T
import Kernel.Prelude
import Deriving.Aeson
import Kernel.Utils.JSON

data WhatsappServiceConfig = GupShupConfig GupShup.GupShupCfg
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[SumTaggedObject "tag" "content"] WhatsappServiceConfig


data OptApiMethods = OPT_IN | OPT_OUT
  deriving (Show, Eq, Read, Generic, ToSchema, FromJSON, ToJSON, Enum)

data WhatsappHandler m = WhatsappHandler
  { getProvidersPriorityList :: m [T.WhatsappService],
    getProviderConfig :: T.WhatsappService -> m WhatsappServiceConfig
  }

data OptApiReq = OptApiReq
  { phoneNumber :: Text,
    method :: OptApiMethods
  }
  deriving (Generic, Show)

instance FromJSON OptApiReq where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON OptApiReq where
  toJSON = genericToJSON constructorsWithSnakeCase

data SendOtpApiReq = SendOtpApiReq
  {
    sendTo :: Text,
    var1 :: Text
  }
  deriving (Generic, Eq, Show, ToSchema)

instance FromJSON SendOtpApiReq where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON SendOtpApiReq where
  toJSON = genericToJSON constructorsWithSnakeCase

type SendOtpApiResp = OptApiResp

data OptApiResp = OptApiResp
  { _response :: OptApiResponse,
    _data :: Maybe OptApiRespData
  }
  deriving (Generic, Eq, Show, ToSchema)

instance FromJSON OptApiResp where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON OptApiResp where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny


newtype OptApiRespData = OptApiRespData
  { responseMessages :: [OptApiResponse] 
  }
  deriving (Generic, Eq, Show, ToSchema)

instance FromJSON OptApiRespData where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON OptApiRespData where
  toJSON = genericToJSON constructorsWithSnakeCase

data OptApiResponse = OptApiResponse
  { id :: Text,
    phone :: Text,
    details :: Text,
    status :: Text
  }
  deriving (Generic, Eq, Show, ToSchema, FromJSON, ToJSON)
