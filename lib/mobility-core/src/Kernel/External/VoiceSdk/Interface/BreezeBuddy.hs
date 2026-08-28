module Kernel.External.VoiceSdk.Interface.BreezeBuddy where

import Kernel.External.Encryption
import qualified Kernel.External.VoiceSdk.BreezeBuddy.Flow as BreezeBuddy
import qualified Kernel.External.VoiceSdk.BreezeBuddy.Types as BreezeBuddyTypes
import Kernel.External.VoiceSdk.Interface.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Common

createLead ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  BreezeBuddyTypes.BreezeBuddySdkConfig ->
  CreateLeadReq ->
  m CreateLeadResp
createLead config req = do
  apiKey <- decrypt config.apiKey
  resp <- BreezeBuddy.createLead config.url apiKey (toBreezeBuddyLeadRequest req)
  pure $ fromBreezeBuddyLeadResponse resp

connect ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  BreezeBuddyTypes.BreezeBuddySdkConfig ->
  ConnectReq ->
  m ConnectResp
connect config req = do
  apiKey <- decrypt config.apiKey
  resp <- BreezeBuddy.connect config.url apiKey (toBreezeBuddyConnectRequest req)
  pure $ fromBreezeBuddyConnectResponse resp

toBreezeBuddyLeadRequest :: CreateLeadReq -> BreezeBuddyTypes.BreezeBuddyLeadRequest
toBreezeBuddyLeadRequest req =
  BreezeBuddyTypes.BreezeBuddyLeadRequest
    { request_id = req.requestId,
      template = req.template,
      payload = req.payload,
      reseller_id = req.resellerId,
      merchant_id = req.merchantId,
      reporting_webhook_url = req.reportingWebhookUrl,
      execution_mode = req.executionMode,
      is_playground = req.isPlayground,
      configurations_override = req.configurationsOverride
    }

fromBreezeBuddyLeadResponse :: BreezeBuddyTypes.BreezeBuddyLeadResponse -> CreateLeadResp
fromBreezeBuddyLeadResponse resp =
  CreateLeadResp
    { status = resp.status,
      leadCallTrackerId = resp.lead_call_tracker_id,
      orderId = resp.order_id,
      message = resp.message
    }

toBreezeBuddyConnectRequest :: ConnectReq -> BreezeBuddyTypes.BreezeBuddyConnectRequest
toBreezeBuddyConnectRequest req =
  BreezeBuddyTypes.BreezeBuddyConnectRequest
    { lead_id = req.leadId
    }

fromBreezeBuddyConnectResponse :: BreezeBuddyTypes.BreezeBuddyConnectResponse -> ConnectResp
fromBreezeBuddyConnectResponse resp =
  ConnectResp
    { roomUrl = resp.room_url,
      token = resp.token,
      sessionId = resp.session_id,
      leadId = resp.lead_id
    }
