{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Kernel.External.SMS.CerfSms.Flow where

import qualified Data.Aeson as A
import EulerHS.Prelude
import EulerHS.Types as ET
import Kernel.External.SMS.CerfSms.API as API
import Kernel.External.SMS.CerfSms.Config
import Kernel.External.SMS.CerfSms.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common as B
import Servant.Client

type CerfSmsFlow m r =
  ( CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m,
    B.Log m
  )

-- | @GET \/pushapi\/sendmsg@. Carries no DLT template id.
sendPushSms ::
  CerfSmsFlow m r =>
  -- | SMS text
  Text ->
  -- | Phone number
  Text ->
  -- | Sender id (@signature@)
  Text ->
  -- | Decrypted api key
  Text ->
  -- | Message type: @PM@ or @UC@
  Text ->
  CerfSmsCfg ->
  m CerfSmsResponse
sendPushSms smsBody phoneNumber sender cerfApiKey messageType CerfSmsCfg {..} = do
  let eulerClient = ET.client API.cerfSmsPushAPI
  rawRes <-
    callAPI
      url
      ( eulerClient
          username
          phoneNumber
          cerfApiKey
          sender
          messageType
          smsBody
          custRef
          campaignName
      )
      "sendPushSms"
      API.cerfSmsPushAPI
  handleCerfResponse url rawRes >>= validateCerfResponse

-- | @POST \/pushapi\/json\/sendbulkmsg@ with a single-element @smslist@.
sendJsonSms ::
  CerfSmsFlow m r =>
  -- | SMS text
  Text ->
  -- | Phone number
  Text ->
  -- | Sender id
  Text ->
  -- | Decrypted api key
  Text ->
  -- | Message type: @PM@ or @UC@
  Text ->
  -- | DLT template id
  Maybe Text ->
  CerfSmsCfg ->
  m CerfSmsResponse
sendJsonSms smsBody phoneNumber sender cerfApiKey messageType mbTemplateId CerfSmsCfg {..} = do
  let smsItem =
        CerfSmsListItem
          { text = smsBody,
            mobiles = phoneNumber,
            messagetype = messageType,
            custref = custRef,
            templateid = mbTemplateId
          }
      req =
        CerfSmsJsonReq
          { username = username,
            password = cerfApiKey,
            senderid = sender,
            campaignname = campaignName,
            entityid = entityId,
            smslist = [smsItem]
          }
      eulerClient = ET.client API.cerfSmsJsonAPI
  rawRes <- callAPI url (eulerClient req) "sendJsonSms" API.cerfSmsJsonAPI
  jsonRes <- handleCerfResponse url rawRes
  case jsonRes.responses of
    (resp : _) -> validateCerfResponse resp
    [] -> throwError $ CerfSmsError "EMPTY_RESPONSE" (Just "CERF returned an empty response list.")

-- | CERF answers with a JSON body but does not always label it @application\/json@,
-- so decode the payload ourselves before giving up on a content type mismatch.
handleCerfResponse ::
  (MonadThrow m, B.Log m, A.FromJSON a) =>
  BaseUrl ->
  Either ClientError a ->
  m a
handleCerfResponse url res = case res of
  Left (UnsupportedContentType _ response)
    | Right decoded <- A.eitherDecode (responseBody response) -> pure decoded
  _ -> fromEitherM (cerfSmsApiError url) res

validateCerfResponse :: (MonadThrow m, B.Log m) => CerfSmsResponse -> m CerfSmsResponse
validateCerfResponse resp@CerfSmsResponse {..}
  | code == cerfSuccessCode = pure resp
  | otherwise = throwError $ CerfSmsError code (desc <|> descriptionForCode code)

cerfSuccessCode :: Text
cerfSuccessCode = "6001"

-- | Fallback descriptions for the codes documented in the CERF push API manual,
-- used when the response omits @desc@.
descriptionForCode :: Text -> Maybe Text
descriptionForCode = \case
  "-1" -> Just "Invalid credentials"
  "-2" -> Just "Invalid api key"
  "-3" -> Just "Invalid destination"
  "-4" -> Just "Invalid signature"
  "-5" -> Just "Invalid message type"
  "-6" -> Just "Invalid message text"
  "-10" -> Just "Internal error occurred"
  _ -> Nothing

cerfSmsApiError :: BaseUrl -> ClientError -> ExternalAPICallError
cerfSmsApiError = ExternalAPICallError (Just "CERF_SMS_API_ERROR")
