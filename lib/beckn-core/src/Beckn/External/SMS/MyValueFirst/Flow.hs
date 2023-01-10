module Beckn.External.SMS.MyValueFirst.Flow where

import qualified Beckn.External.SMS.MyValueFirst.API as API
import Beckn.External.SMS.MyValueFirst.Types (SubmitSms (..), SubmitSmsRes (..))
import Beckn.Sms.Config (SmsConfig)
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import qualified Data.Text as T
import EulerHS.Prelude

submitSms ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  SubmitSms ->
  m SubmitSmsRes
submitSms url params = do
  callAPI url (API.submitSms params) "submitSms"
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_SEND_SMS") url)

type OtpTemplate = Text

type OrgName = Text

type InviteTemplate = Text

constructInviteSms :: OrgName -> InviteTemplate -> Text
constructInviteSms = T.replace "{#org#}"

sendOTPApi ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  m SubmitSmsRes
sendOTPApi url username password otpSmsTemplate phoneNumber sender = do
  submitSms
    url
    SubmitSms
      { username = username,
        password = password,
        from = sender,
        to = phoneNumber,
        text = otpSmsTemplate
      }

sendSms ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  SmsConfig ->
  Text ->
  Text ->
  m SubmitSmsRes
sendSms smsCfg smsTemplate phoneNumber = do
  let smsCred = smsCfg.credConfig
      url = smsCfg.url
  submitSms
    url
    SubmitSms
      { username = smsCred.username,
        password = smsCred.password,
        from = smsCfg.sender,
        to = phoneNumber,
        text = smsTemplate
      }

checkSmsResult :: (Log m, MonadThrow m) => SubmitSmsRes -> m ()
checkSmsResult =
  \case
    Sent -> pure ()
    BadNumber -> throwError SMSInvalidNumber
    InvalidReceiver -> throwError SMSInvalidNumber
    err -> throwError $ SMSError err
