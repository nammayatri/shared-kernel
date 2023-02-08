module Kernel.External.Slack.Flow where

import Kernel.External.Slack.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common hiding (Error)
import Data.Maybe
import qualified Data.Text as T
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant hiding (throwError)
import Servant.Client

-- | Slack API interface
type SlackConnectAPI =
  Header "Authorization" Text
    :> ReqBody '[JSON] SlackRequest
    :> Post '[JSON] SlackResponse

slackConnectAPI :: Proxy SlackConnectAPI
slackConnectAPI = Proxy

defaultBaseUrl :: BaseUrl
defaultBaseUrl =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "slack.com",
      baseUrlPort = 443,
      baseUrlPath = T.unpack "/api/chat.postMessage"
    }

postMessage ::
  ( CoreMetrics m,
    HasFlowEnv m r '["slackCfg" ::: SlackConfig]
  ) =>
  T.Text ->
  m SlackResponse
postMessage message = do
  withLogTag "Slack" $ do
    SlackConfig {..} <- asks (.slackCfg)
    let slackRequest =
          SlackRequest
            { channel = channelName,
              blocks = Just [Block {_type = "section", _text = Block {_type = "plain_text", _text = message}}]
            }
    callSlackAPI
      defaultBaseUrl
      (callSlack slackToken slackRequest)
      "PostMessage"
  where
    callSlack token slackRequest = ET.client slackConnectAPI (Just $ "Bearer " <> token) slackRequest

callSlackAPI :: CallAPI env a
callSlackAPI =
  callApiUnwrappingApiError
    (identity @Error)
    Nothing
    Nothing
