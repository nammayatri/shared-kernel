module Beckn.Utils.Service
  ( startService,
  )
where

import Beckn.Prelude
import Beckn.Utils.Common as CoreCommon
import Beckn.Utils.Shutdown

startService ::
  ( Forkable m,
    MonadIO m,
    MonadReader r m,
    HasField "isShuttingDown" r Shutdown,
    MonadCatch m,
    Log m
  ) =>
  Text ->
  m () ->
  m ()
startService name f =
  fork name . untilShutdown $
    f `catch` \e -> do
      log ERROR $ makeLogSomeException e
      threadDelay 1000000
