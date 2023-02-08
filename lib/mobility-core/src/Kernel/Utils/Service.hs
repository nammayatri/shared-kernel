module Kernel.Utils.Service
  ( startService,
  )
where

import Kernel.Prelude
import Kernel.Utils.Common as CoreCommon
import Kernel.Utils.Shutdown

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
