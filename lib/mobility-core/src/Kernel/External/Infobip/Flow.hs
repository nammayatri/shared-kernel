module Kernel.External.Infobip.Flow where

import EulerHS.Prelude
import qualified Kernel.External.Infobip.API.SendSms as APISend
import qualified Kernel.External.Infobip.API.WebengageWebhook as APIStatus
import Kernel.External.Infobip.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common

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
