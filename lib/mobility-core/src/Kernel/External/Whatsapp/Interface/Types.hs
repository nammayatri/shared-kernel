{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.External.Whatsapp.Interface.Types
  ( module Kernel.External.Whatsapp.Interface.Types,
  )
where

import Deriving.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import qualified Kernel.External.Whatsapp.GupShup.Config as GupShup
import qualified Kernel.External.Whatsapp.Types as T
import Kernel.Prelude
import Kernel.Utils.JSON

newtype WhatsappServiceConfig = GupShupConfig GupShup.GupShupCfg
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[SumTaggedObject "tag" "content"] WhatsappServiceConfig

data OptApiMethods = OPT_IN | OPT_OUT
  deriving (Show, Eq, Read, Ord, Generic, ToSchema, FromJSON, ToJSON, Enum)

$(mkBeamInstancesForEnum ''OptApiMethods)

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
  { sendTo :: Text,
    var1 :: Text
  }
  deriving (Generic, Eq, Show, ToSchema)

instance FromJSON SendOtpApiReq where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON SendOtpApiReq where
  toJSON = genericToJSON constructorsWithSnakeCase

type SendOtpApiResp = OptApiResp

type SendWhatsAppMessageApiResp = OptApiResp

data SendWhatsAppMessageWithTemplateIdApIReq = SendWhatsAppMessageWithTemplateIdApIReq
  { sendTo :: Text,
    templateId :: Text,
    var1 :: Maybe Text,
    var2 :: Maybe Text,
    var3 :: Maybe Text,
    containsUrlButton :: Maybe Bool
  }
  deriving (Generic, Eq, Show, ToSchema)

instance FromJSON SendWhatsAppMessageWithTemplateIdApIReq where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON SendWhatsAppMessageWithTemplateIdApIReq where
  toJSON = genericToJSON constructorsWithSnakeCase

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
