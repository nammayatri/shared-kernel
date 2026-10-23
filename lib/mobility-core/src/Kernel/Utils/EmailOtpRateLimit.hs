{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.EmailOtpRateLimit
  ( EmailOtpLimitWindow (..),
    enforceEmailOtpLimit,
  )
where

import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (DbHash (..), EncFlow, getDbHash)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error (AuthError (..))
import Kernel.Types.SlidingWindowCounters (SlidingWindowOptions (..))
import Kernel.Types.Time (MonadClock)
import Kernel.Types.TryException
import Kernel.Utils.Error.Throwing (throwError)
import Kernel.Utils.Logging (Log, logInfo)
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import Text.Hex (encodeHex)

data EmailOtpLimitWindow = EmailOtpLimitWindow
  { window :: Maybe SlidingWindowOptions,
    threshold :: Maybe Int
  }

enforceEmailOtpLimit ::
  ( L.MonadFlow m,
    MonadClock m,
    Redis.HedisFlow m r,
    TryException m,
    EncFlow m r,
    MonadThrow m,
    Log m
  ) =>
  Text ->
  [EmailOtpLimitWindow] ->
  Text ->
  m ()
enforceEmailOtpLimit keyPrefix limitWindows email = do
  emailHash <- encodeHex . unDbHash <$> getDbHash (T.toLower (T.strip email))
  let counterKey tag = keyPrefix <> emailHash <> ":" <> tag
      taggedWindows = zip (map (\i -> "W" <> show i) [1 :: Int ..]) limitWindows
  mbResetSeconds <- Redis.withNonCriticalCrossAppRedis $ firstExceeded counterKey taggedWindows
  whenJust mbResetSeconds $ throwError . HitsLimitError
  Redis.withNonCriticalCrossAppRedis $
    forM_ taggedWindows $ \(tag, limitWindow) ->
      whenJust limitWindow.window $ SWC.incrementWindowCount (counterKey tag)

firstExceeded ::
  ( L.MonadFlow m,
    MonadClock m,
    Redis.HedisFlow m r,
    TryException m,
    Log m
  ) =>
  (Text -> Text) ->
  [(Text, EmailOtpLimitWindow)] ->
  m (Maybe Int)
firstExceeded _ [] = pure Nothing
firstExceeded counterKey ((tag, limitWindow) : rest) =
  case (limitWindow.window, limitWindow.threshold) of
    (Just swo, Just limit) -> do
      count <- SWC.getCurrentWindowCount (counterKey tag) swo
      if count >= fromIntegral limit
        then do
          logInfo $ "Email OTP rate limit hit for window " <> tag <> " with count " <> show count
          pure $ Just (fromIntegral (swo.period * SWC.convertPeriodTypeToSeconds swo.periodType))
        else firstExceeded counterKey rest
    _ -> firstExceeded counterKey rest
