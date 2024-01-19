{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Tools.Slack.Internal
  ( sendSlackMessage,
    createSlackConfig,
    SlackEnv (..),
    HasSlackEnv,
  )
where

import Data.Aeson
import qualified Data.Aeson as A
import EulerHS.Prelude
import Kernel.Prelude
import qualified Web.Slack as Slack
import qualified Web.Slack.Chat as Chat

type HasSlackEnv f = HasField "slackEnv" f SlackEnv

data SlackEnv = SlackEnv
  { channel :: Text,
    slackConfig :: Slack.SlackConfig
  }
  deriving (Generic)

data FileUploadResp = FileUploadResp
  { ok :: Bool,
    uploadError :: Maybe Text,
    file :: Maybe A.Value
  }
  deriving (Generic, ToJSON, Show)

instance FromJSON FileUploadResp where
  parseJSON = withObject "FileUploadResp" $ \v -> do
    ok <- v .: "ok"
    uploadError <- v .: "error"
    file <- v .: "file"
    return (FileUploadResp ok uploadError file)

createSlackConfig :: Text -> Text -> IO SlackEnv
createSlackConfig token channel = SlackEnv channel <$> Slack.mkSlackConfig token

sendSlackMessage :: Slack.SlackConfig -> Text -> Text -> Maybe Text -> Maybe Text -> IO (Slack.Response Chat.PostMsgRsp)
sendSlackMessage slackConfig channel message threadTs attachments = do
  Slack.chatPostMessage slackConfig $ (Chat.mkPostMsgReq channel message) {Chat.postMsgReqThreadTs = threadTs, Chat.postMsgReqAttachments = attachments}
