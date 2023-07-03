{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.AadhaarVerification.Gridline.Types where

import Data.Aeson
import EulerHS.Prelude hiding (error, state)
import qualified Network.HTTP.Media as M
import qualified Servant.API as S

data AADHAARJSON deriving (Typeable)

instance S.Accept AADHAARJSON where
  contentType _ = "application" M.// "json"

instance S.MimeRender AADHAARJSON GridlineAadhaarOtpReq where
  mimeRender _ = encode

instance S.MimeUnrender AADHAARJSON GridlineAadhaarOtpReq where
  mimeUnrender _ = eitherDecode

instance S.MimeRender AADHAARJSON GridlineSubmitResponse where
  mimeRender _ = encode

instance S.MimeUnrender AADHAARJSON GridlineSubmitResponse where
  mimeUnrender _ = eitherDecode

instance S.MimeRender AADHAARJSON GridlineVerifyAadhaarResp where
  mimeRender _ = encode

instance S.MimeUnrender AADHAARJSON GridlineVerifyAadhaarResp where
  mimeUnrender _ = eitherDecode

instance S.MimeRender AADHAARJSON GridlineAadhaarOtpVerifyReq where
  mimeRender _ = encode

instance S.MimeUnrender AADHAARJSON GridlineAadhaarOtpVerifyReq where
  mimeUnrender _ = eitherDecode

data GridlineAadhaarOtpReq = GridlineAadhaarOtpReq
  { aadhaar_number :: Text,
    consent :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data GridlineVerifyAadhaarResp = GridlineVerifyAadhaarResp
  { request_id :: Text,
    status :: Int,
    _data :: GridlineVerifyAadhaarData,
    timestamp :: Int,
    path :: Text
  }
  deriving (Show, Generic, ToJSON)

data GridlineVerifyAadhaarData = GridlineVerifyAadhaarData
  { code :: Text,
    message :: Text,
    transaction_id :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

instance FromJSON GridlineVerifyAadhaarResp where
  parseJSON = withObject "GridlineVerifyAadhaarResp" $ \v -> do
    request_id <- v .: "request_id"
    path <- v .: "path"
    status <- v .: "status"
    timestamp <- v .: "timestamp"
    _data <- v .: "data"
    return (GridlineVerifyAadhaarResp request_id status _data timestamp path)

data GridlineAadhaarOtpVerifyReq = GridlineAadhaarOtpVerifyReq
  { otp :: Int,
    include_xml :: Bool,
    share_code :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data GridlineSubmitResponse = GridlineSubmitResponse
  { _data :: GridlineSubmitAadhaarData,
    request_id :: Text
  }
  deriving (Show, Generic, ToJSON)

data GridlineSubmitAadhaarData = GridlineSubmitAadhaarData
  { aadhaar_data :: AadhaarData,
    code :: Text,
    transaction_id :: Text,
    message :: Text,
    share_code :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data AadhaarData = AadhaarData
  { name :: Text,
    gender :: Text,
    date_of_birth :: Text,
    image :: Text
  }
  deriving (Show, Generic, ToJSON)

instance FromJSON GridlineSubmitResponse where
  parseJSON = withObject "GridlineSubmitResponse" $ \v -> do
    _data <- v .: "data"
    request_id <- v .: "request_id"
    return (GridlineSubmitResponse _data request_id)

instance FromJSON AadhaarData where
  parseJSON = withObject "AadhaarData" $ \v -> do
    name <- v .: "name"
    gender <- v .: "gender"
    date_of_birth <- v .: "date_of_birth"
    image <- v .: "photo_base64"
    return (AadhaarData name gender date_of_birth image)
