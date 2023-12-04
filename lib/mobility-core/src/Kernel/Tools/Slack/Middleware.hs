{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-error=deprecations #-}

module Kernel.Tools.Slack.Middleware where

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Map as M
import Data.String.Conversions
import EulerHS.Prelude
import GHC.Records.Extra (HasField)
import qualified Kernel.Tools.Slack as Slack
import qualified Kernel.Tools.Slack.Internal as SI
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Servant
import qualified Web.Slack as Slack
import qualified Web.Slack.Chat as Chat

notifyOnSlackMiddleware :: (SI.HasSlackEnv f) => f -> Application -> Application
notifyOnSlackMiddleware appEnv app req respF = do
  let headers = Wai.requestHeaders req
      notifySlackChannel = cs . snd <$> find (\(headerKey, _) -> headerKey == "NotifySlack") headers
  case notifySlackChannel of
    Just slackChannel -> do
      body <- Wai.consumeRequestBodyStrict req
      let path = Wai.rawPathInfo req
          queryStrings = Wai.rawQueryString req
      called :: IORef Int <- newIORef 0
      let returnBody = do
            calledTimes <- readIORef called
            modifyIORef called (+ 1)
            pure $
              if calledTimes > 0
                then B.empty
                else BL.toStrict body
      slackThread <- notifyApiCallOnSlack appEnv (BL.toStrict body) (path <> queryStrings) slackChannel
      app (req {Wai.requestBody = returnBody}) (updateResponseOnThread slackThread)
    Nothing -> app req respF
  where
    updateResponseOnThread (Right slackThread) resp = notifyApiCallResponseOnSlack appEnv resp (Just slackThread) *> respF resp
    updateResponseOnThread (Left err) resp = print ("Slack SendMessageError: " <> show err :: Text) *> respF resp

notifyApiCallOnSlack :: (HasField "slackEnv" env SI.SlackEnv) => env -> ByteString -> ByteString -> Text -> IO (Slack.Response Chat.PostMsgRsp)
notifyApiCallOnSlack appEnv body path sc = do
  let title = "*API TRIGGERED: *" <> cs path
      body' = "```" <> cs body <> "```"
      slackEnv = bool (appEnv.slackEnv {SI.channel = sc}) appEnv.slackEnv (sc == "default")
  Slack.notifyOnSlackIO slackEnv title body' Nothing Nothing

notifyApiCallResponseOnSlack :: (HasField "slackEnv" env SI.SlackEnv) => env -> Wai.Response -> Maybe Chat.PostMsgRsp -> IO (Slack.Response Chat.PostMsgRsp)
notifyApiCallResponseOnSlack appEnv resp st = do
  let (status, _, _) = Wai.responseToStream resp
      code = HTTP.statusCode status
      respInfo :: Text = "statusCode: `" <> show code <> "` and message: `" <> cs (HTTP.statusMessage status) <> "`"
  Slack.notifyOnSlackIO appEnv.slackEnv "" "" (Chat.postMsgRspTs <$> st) (attachment code respInfo)
  where
    createColoredAttachement color text = cs $ A.encode ([M.insert "author" "Response" . M.insert "text" text $ M.singleton "color" color] :: [M.Map Text Text])

    attachment code respInfo =
      Just $
        createColoredAttachement
          ( case div code 100 of
              2 -> "#51f436"
              4 -> "#f4bd36"
              5 -> "#f44336"
              _ -> "#36b8f4"
          )
          respInfo
