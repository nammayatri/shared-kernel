module Beckn.External.Infobip.Flow where

import qualified Beckn.External.Infobip.API.SendSms as APISend
import qualified Beckn.External.Infobip.API.WebengageWebhook as APIStatus
import Beckn.External.Infobip.Types
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import EulerHS.Prelude

sendSms ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  InfoBIPConfig ->
  Text ->
  Text ->
  Text ->
  Text ->
  m SMSRes
sendSms smsCfg smsTemplate phoneNumber entityId templetId = do
  let url = smsCfg.url
  let userName = smsCfg.username
  let password = smsCfg.password
  let from = smsCfg.sender
  let webhookurl = smsCfg.webhookurl
  submitSms url userName password from phoneNumber smsTemplate entityId templetId webhookurl

submitSms ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  m SMSRes
submitSms url userName password from phoneNumber smsTemplate entityId templetId webhookurl = do
  callAPI url (APISend.sendSms userName password from phoneNumber smsTemplate entityId templetId webhookurl) "sendSms"
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_SEND_SMS") url)

callWebengageWebhook ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  WebengageConfig ->
  WebengageRes ->
  m APISuccess
callWebengageWebhook webCfg req = do
  let url = webCfg.url
  callAPI url (APIStatus.sendStatus req) "success"
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_WEBHOOK") url)
