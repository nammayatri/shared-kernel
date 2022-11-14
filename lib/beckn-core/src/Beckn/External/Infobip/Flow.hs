module Beckn.External.Infobip.Flow where

import qualified Beckn.External.Infobip.API as API
import Beckn.External.Infobip.Types
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import EulerHS.Prelude

sendSms ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  InfoBIPConfig ->
  Text ->
  Text ->
  m SMSRes
sendSms smsCfg smsTemplate phoneNumber = do
  let url = smsCfg.url
  let req =
        SMSReq
          { messages =
              [ MessageReq
                  { destinations = [SMSDestination phoneNumber],
                    from = smsCfg.sender,
                    text = smsTemplate
                  }
              ]
          }
  let userpass = fromString $ (T.unpack smsCfg.username) <> ":" <> (T.unpack smsCfg.password)
  let auth = "Basic " <> T.decodeUtf8 (Base64.encode userpass)
  submitSms url auth req

submitSms ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  SMSReq ->
  m SMSRes
submitSms url auth req = do
  callAPI url (API.sendSms auth req) "sendSms"
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_SEND_SMS") url)
