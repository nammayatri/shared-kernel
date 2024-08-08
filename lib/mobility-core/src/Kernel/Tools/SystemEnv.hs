module Kernel.Tools.SystemEnv where

import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Storage.Beam.SystemConfigs as BeamSC
import Kernel.Storage.Queries.SystemConfigs
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)
import qualified System.Environment as Env

data SystemEnv = SystemEnv
  { systemEnv :: [(String, String)]
  }
  deriving (Generic, Show, ToJSON, FromJSON, FromDhall)

updateSystemEnv :: (CacheFlow m r, EsqDBFlow m r, HasSchemaName BeamSC.SystemConfigsT) => m ()
updateSystemEnv = do
  systemEnv <- findById "system_env" >>= pure . decodeFromText' @SystemEnv
  case systemEnv of
    Just env -> liftIO $ mapM_ (uncurry Env.setEnv) (env.systemEnv)
    Nothing -> do
      incrementSystemConfigsFailedCounter ("system_env_decode_failed_" <> schemaName (Proxy :: Proxy BeamSC.SystemConfigsT) <> "_system_env")
      throwError (InternalError "system_env_decode_failed")

updateSystemEnvInLoopFork :: (CacheFlow m r, EsqDBFlow m r, HasSchemaName BeamSC.SystemConfigsT) => Integer -> m ()
updateSystemEnvInLoopFork interval = updateSystemEnv >> (fork "updateSystemEnvInLoopFork" $ forever $ updateSystemEnv >> liftIO (threadDelay $ fromIntegral interval * 1000000))
