{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.PaytmEDC.Checksum where

import Data.Aeson
import Data.Map (Map)
import Kernel.External.Payment.PaytmEDC.Types (PaytmEDCResponseHead, PaytmEDCResultInfo)
import Kernel.Prelude

-- Checksum Request Head
data ChecksumRequestHead = ChecksumRequestHead
  { mid :: Text,
    tid :: Maybe Text,
    clientId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Checksum Request Body
data ChecksumRequestBody = ChecksumRequestBody
  { params :: Map Text Text,
    mapParams :: Maybe (Map Text (Map Text Text))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Generate Checksum Request
data GenerateChecksumReq = GenerateChecksumReq
  { gcReqHead :: ChecksumRequestHead,
    gcReqBody :: ChecksumRequestBody
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GenerateChecksumReq where
  toJSON req = object ["head" .= gcReqHead req, "body" .= gcReqBody req]

instance FromJSON GenerateChecksumReq where
  parseJSON = withObject "GenerateChecksumReq" $ \v ->
    GenerateChecksumReq
      <$> v .: "head"
      <*> v .: "body"

-- Checksum Response Body
data ChecksumResponseBody = ChecksumResponseBody
  { checksum :: Maybe Text,
    resultInfo :: PaytmEDCResultInfo
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Generate Checksum Response
data GenerateChecksumResp = GenerateChecksumResp
  { gcRespHead :: PaytmEDCResponseHead,
    gcRespBody :: ChecksumResponseBody
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GenerateChecksumResp where
  toJSON resp = object ["head" .= gcRespHead resp, "body" .= gcRespBody resp]

instance FromJSON GenerateChecksumResp where
  parseJSON = withObject "GenerateChecksumResp" $ \v ->
    GenerateChecksumResp
      <$> v .: "head"
      <*> v .: "body"
