{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.EventTracking.Breeze.Flow
  ( pushEvent,
  )
where

import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import EulerHS.Types (client)
import Kernel.External.Encryption (decrypt)
import Kernel.External.EventTracking.Breeze.API
import Kernel.External.EventTracking.Breeze.Config
import Kernel.External.EventTracking.Breeze.Types
import Kernel.External.EventTracking.Interface.Types (EventTrackingReq (..))
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common

pushEvent ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BreezeCfg ->
  EventTrackingReq ->
  m ()
pushEvent cfg req = do
  token <- decrypt cfg.collectorToken
  eventId <- generateGUIDText
  now <- getCurrentTime
  let breezeClient = client (Proxy :: Proxy BreezeEventAPI)
      event = toBreezeEvent cfg eventId now req
  resp <-
    callAPI cfg.baseUrl (breezeClient ("Bearer " <> token) cfg.tenantId cfg.workspaceId event) "breezeEvent" breezeEventAPI
      >>= fromEitherM (\err -> InternalError $ "Failed to call Breeze event collector: " <> redactToken token (show err))
  case (resp.collected, resp.filtered) of
    (_, Just n)
      | n > 0 ->
        throwError . InternalError $
          "Breeze collector filtered event " <> req.eventName <> ": the name is not allowed by the collector's configuration"
    (Just 0, _) ->
      throwError . InternalError $ "Breeze collector accepted no events for " <> req.eventName
    _ ->
      logDebug $ "Breeze event " <> req.eventName <> " queued (collected: " <> show resp.collected <> ", total: " <> show resp.total <> ")"

toBreezeEvent :: BreezeCfg -> Text -> UTCTime -> EventTrackingReq -> BreezeEvent
toBreezeEvent cfg eventId now req =
  BreezeEvent
    { envelop_version = "1.0",
      _id = eventId,
      name = T.toLower req.eventName,
      tenant_id = cfg.tenantId,
      workspace_id = cfg.workspaceId,
      anon_id = req.customerId,
      occured_at = formatRfc3339 (fromMaybe now req.timestamp),
      properties = req.attributes
    }

formatRfc3339 :: UTCTime -> Text
formatRfc3339 = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

redactToken :: Text -> Text -> Text
redactToken token t
  | T.null token = t
  | otherwise = T.replace token "[REDACTED]" t
