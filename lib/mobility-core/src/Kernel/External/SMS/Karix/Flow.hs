{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Kernel.External.SMS.Karix.Flow where

import EulerHS.Prelude
import EulerHS.Types as ET
import Kernel.External.SMS.Karix.Api as API
import Kernel.External.SMS.Karix.Config
import Kernel.External.SMS.Karix.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common as B
import Servant.Client

-- | Send OTP via Karix JSON API
sendOTPApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    B.Log m
  ) =>
  Text -> --  SMS text
  Text -> --  Phone number
  Text -> --  Sender
  Text -> --  Decrypted API key
  KarixCfg ->
  m KarixSubmitRes
sendOTPApi otpSmsTemplate phoneNumber karixSender karixKey KarixCfg {..} = do
  -- Build message
  logDebug $ "faltu" <> show otpSmsTemplate <> show karixSender
  let msg =
        KarixMessage
          { dest = [phoneNumber],
            text = "1234 is your OTP for login to Namma Yatri App. test -Namma Yatri", --otpSmsTemplate
            send = "NMAYTI", --karixSender
            type_ = "PM"
          }

      req =
        KarixRequest
          { ver = "1.0",
            key = karixKey,
            messages = [msg]
          }

  logDebug $ "Karix request body: " <> encodeToText req

  let eulerClient = ET.client API.karixConnectAPI
  rawRes <- callAPI url (eulerClient req) "sendOTPApi" API.karixConnectAPI
  checkKarixOptError url rawRes

checkKarixOptError :: (MonadThrow m, B.Log m) => BaseUrl -> Either ClientError KarixSubmitRes -> m KarixSubmitRes
checkKarixOptError url res =
  fromEitherM (karixOptError url) res >>= validateKarixResponse

validateKarixResponse :: (MonadThrow m, B.Log m) => KarixSubmitRes -> m KarixSubmitRes
validateKarixResponse submitRes =
  case submitRes.response.status.code of
    "200" -> pure submitRes
    -- Additional code handling can be added here if needed
    _ -> throwError KarixSmsServerError

karixOptError :: BaseUrl -> ClientError -> ExternalAPICallError
karixOptError = ExternalAPICallError (Just "KARIX_JSON_API_ERROR")
