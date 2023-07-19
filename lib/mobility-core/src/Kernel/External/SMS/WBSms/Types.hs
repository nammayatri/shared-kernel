{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Kernel.External.SMS.WBSms.Types where

import Data.ByteString.Lazy (toStrict)
import qualified Data.Text.Encoding as T
import EulerHS.Prelude hiding (encodeUtf8, fromStrict, toStrict)
import Kernel.Utils.Servant.HTML (HTML)
import Servant (MimeUnrender (mimeUnrender))
import Web.FormUrlEncoded (ToForm, toForm)
import qualified Web.Internal.FormUrlEncoded
import Web.Internal.HttpApiData

data SubmitSms = SubmitSms
  { -- | Phone number.
    mobile :: Text,
    -- | SMS content.
    message :: Text,
    -- | Template of the SMS
    templateid :: Text,
    -- | For authorization
    passkey :: Text
  }
  deriving (Generic, Eq, Show)

data SubmitSmsRes = Sent
  deriving (Generic, Show, ToJSON, FromJSON)

instance MimeUnrender HTML SubmitSmsRes where
  mimeUnrender _ = Right . parseSubmitSmsRes . T.decodeLatin1 . toStrict

parseSubmitSmsRes :: Text -> SubmitSmsRes
parseSubmitSmsRes _ = Sent

instance ToForm SubmitSms where
  toForm :: SubmitSms -> Web.Internal.FormUrlEncoded.Form
  toForm (SubmitSms mobile message templateid passkey) =
    [ ("mobile", toQueryParam mobile),
      ("message", toQueryParam message),
      ("templateid", toQueryParam templateid),
      ("passkey", toQueryParam passkey)
    ]
