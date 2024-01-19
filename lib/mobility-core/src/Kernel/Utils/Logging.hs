{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.Logging
  ( Log (..),
    LogLevel (..),
    log,
    logTagDebug,
    logTagInfo,
    logTagWarning,
    logTagError,
    logDebug,
    logInfo,
    logWarning,
    logError,
    withRandomId,
    withTransactionIdLogTag,
    withTransactionIdLogTag',
    withPersonIdLogTag,
    makeLogSomeException,
    logPretty,
    HasPrettyLogger,
  )
where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.Generics.Product (HasField' (..))
import EulerHS.Prelude hiding (id, (^.))
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.GuidLike (generateGUID)
import Kernel.Types.Id
import Kernel.Types.Logging
import Kernel.Types.MonadGuid
import Kernel.Utils.GenericPretty (PrettyShow, textPretty)

log :: Log m => LogLevel -> Text -> m ()
log = logOutput

logTagDebug :: Log m => Text -> Text -> m ()
logTagDebug tag = withLogTag tag . logOutput DEBUG

logTagInfo :: Log m => Text -> Text -> m ()
logTagInfo tag = withLogTag tag . logOutput INFO

logTagWarning :: Log m => Text -> Text -> m ()
logTagWarning tag = withLogTag tag . logOutput WARNING

logTagError :: Log m => Text -> Text -> m ()
logTagError tag = withLogTag tag . logOutput ERROR

logDebug :: Log m => Text -> m ()
logDebug = logOutput DEBUG

logInfo :: Log m => Text -> m ()
logInfo = logOutput INFO

logWarning :: Log m => Text -> m ()
logWarning = logOutput WARNING

logError :: Log m => Text -> m ()
logError = logOutput ERROR

withRandomId :: (MonadGuid m, Log m) => m b -> m b
withRandomId f = do
  id <- generateGUID
  withLogTag id f

withPersonIdLogTag :: Log m => Id b -> m a -> m a
withPersonIdLogTag personId = do
  withLogTag ("actor-" <> getId personId)

withTransactionIdLogTag :: (HasField' "context" b c, HasField' "transaction_id" c (Maybe Text), Log m) => b -> m a -> m a
withTransactionIdLogTag req =
  withTransactionIdLogTag' $ fromMaybe "Unknown" (req ^. field' @"context" . #transaction_id)

withTransactionIdLogTag' :: Log m => Text -> m a -> m a
withTransactionIdLogTag' txnId =
  withLogTag ("txnId-" <> txnId)

makeLogSomeException :: SomeException -> Text
makeLogSomeException someExc
  | Just (HTTPException err) <- fromException someExc = logHTTPError err
  | Just (BaseException err) <- fromException someExc =
    fromMaybe (show err) $ toMessage err
  | otherwise = show someExc
  where
    logHTTPError err =
      show (toHttpCode err)
        <> " "
        <> toErrorCode err
        <> maybe "" (": " <>) (toMessage err)

renderViaShow :: (Show a) => Text -> a -> Text
renderViaShow description val = description <> ": " <> show val

renderViaPrettyShow :: (PrettyShow a) => Text -> a -> Text
renderViaPrettyShow description val = description <> "\n" <> textPretty val

type HasPrettyLogger m env =
  ( Log m,
    MonadReader env m,
    HasField' "loggerConfig" env LoggerConfig
  )

logPretty ::
  ( PrettyShow a,
    Show a,
    HasPrettyLogger m env
  ) =>
  LogLevel ->
  Text ->
  a ->
  m ()
logPretty logLevel description val = do
  pretty <- asks (^. field' @"loggerConfig" . #prettyPrinting)
  let render =
        if pretty
          then renderViaPrettyShow
          else renderViaShow
  logOutput logLevel $ render description val
