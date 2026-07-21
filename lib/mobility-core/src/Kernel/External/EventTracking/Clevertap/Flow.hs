{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.EventTracking.Clevertap.Flow
  ( pushEvent,
  )
where

import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import EulerHS.Types (client)
import Kernel.External.Encryption (decrypt)
import Kernel.External.EventTracking.Clevertap.API
import Kernel.External.EventTracking.Clevertap.Config
import Kernel.External.EventTracking.Clevertap.Types
import Kernel.External.EventTracking.Interface.Types (EventTrackingReq (..))
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common

-- | Push an event to the Clevertap upload API.
--
-- The @enabled@ check lives in "Kernel.External.EventTracking.Interface", not
-- here, so every provider gets it without having to remember it.
pushEvent ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  ClevertapCfg ->
  EventTrackingReq ->
  m ()
pushEvent cfg req = do
  passcode <- decrypt cfg.passcode
  let clevertapClient = client (Proxy :: Proxy ClevertapUploadAPI)
  resp <-
    callAPI cfg.baseUrl (clevertapClient cfg.accountId passcode (toClevertapReq req)) "clevertapUpload" clevertapUploadAPI
      >>= fromEitherM (\err -> InternalError $ "Failed to call Clevertap Upload API: " <> show err)
  -- Clevertap answers HTTP 200 even when individual events are rejected, so a
  -- non-empty `unprocessed` is a real failure that would otherwise be silent.
  --
  -- We send one event per call, so any entry here means this event was not
  -- delivered. Throw rather than log-and-return: returning success would make
  -- the caller report the event as sent, which is simply untrue.
  case resp.unprocessed of
    Just failures@(_ : _) ->
      throwError . InternalError $
        "Clevertap rejected event " <> req.eventName <> " (status: " <> resp.status <> "): "
          <> T.intercalate "; " (map describeFailure failures)
    _ ->
      logDebug $
        "Clevertap event " <> req.eventName <> " accepted (status: " <> resp.status <> ", processed: " <> show resp.processed <> ")"

-- | Render a rejected event as @code: message@ for the log line. The record
-- itself is omitted: it is the payload we just sent and may carry user data.
describeFailure :: ClevertapUnprocessed -> Text
describeFailure failure =
  maybe "unknown" show failure.code <> ": " <> fromMaybe "no error message" failure._error

-- | Map the provider-agnostic request onto Clevertap's wire format.
toClevertapReq :: EventTrackingReq -> ClevertapUploadReq
toClevertapReq req =
  ClevertapUploadReq
    { d =
        [ ClevertapEvent
            { _identity = req.customerId,
              _type = "event",
              evtName = req.eventName,
              evtData = req.attributes,
              ts = toEpoch <$> req.timestamp
            }
        ]
    }
  where
    toEpoch = floor . utcTimeToPOSIXSeconds
