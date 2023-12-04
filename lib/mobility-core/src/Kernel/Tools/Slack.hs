{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Tools.Slack
  ( notifyOnSlack,
    notifyOnSlackIO,
  )
where

import Data.String.Conversions
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Kernel.Tools.Slack.Internal as SI
import Kernel.Utils.Common
import qualified Web.Slack as Slack
import qualified Web.Slack.Chat as Chat

-- Just "[{\"color\" : \"#e7700d\", \"text\":\"this is test\"}]"
notifyOnSlackIO :: SI.SlackEnv -> Text -> Text -> Maybe Text -> Maybe Text -> IO (Slack.Response Chat.PostMsgRsp)
notifyOnSlackIO slackEnv title body threadId attachments = do
  SI.sendSlackMessage slackEnv.slackConfig slackEnv.channel constructMessage threadId attachments
  where
    nl = "\n"
    constructMessage = do
      let fTitle = title
      fTitle <> nl <> body

notifyOnSlack :: (HasField "slackEnv" r SI.SlackEnv, Log m, L.MonadFlow m, MonadReader r m) => Text -> Text -> Maybe Text -> m (Slack.Response Chat.PostMsgRsp)
notifyOnSlack title body threadId = do
  slackEnv <- asks (.slackEnv)
  L.runIO $ notifyOnSlackIO slackEnv title body threadId Nothing
