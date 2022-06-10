module Beckn.Storage.Esqueleto.Transactionable where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Esqueleto.Logger
import Beckn.Storage.Esqueleto.SqlDB
import Beckn.Types.Logging
import Beckn.Types.Time (getCurrentTime)
import Database.Esqueleto.Experimental (SqlBackend, runSqlPool)

class (MonadThrow m, Log m) => Transactionable m where
  runTransaction :: SqlDB a -> m a

instance {-# OVERLAPPING #-} Transactionable (ReaderT SqlDBEnv (ReaderT SqlBackend LoggerIO)) where
  runTransaction = identity

-- We need INCOHERENT here because in next case:
-- create :: a -> m ()
-- create = runTransaction  . create'
-- compiler cannot figure out which instance to use since in some cases it might be SqlDB
-- and in another it might be not.
-- But with INCOHERENT it will always use Transactionable m instance.
-- It's fine since Transactionable SqlDB instance should be used only in find... functions,
-- which have proper Transactionable instance.
instance {-# INCOHERENT #-} (HasEsqEnv m r, MonadThrow m, Log m) => Transactionable m where
  runTransaction = runTransactionImpl

runTransactionImpl ::
  (HasEsqEnv m r) =>
  SqlDB a ->
  m a
runTransactionImpl run = do
  logEnv <- asks (.loggerEnv)
  dbEnv <- asks (.esqDBEnv)
  now <- getCurrentTime
  let sqlDBEnv =
        SqlDBEnv
          { currentTime = now
          }
  liftIO . runLoggerIO logEnv $ runSqlPool (runReaderT run sqlDBEnv) dbEnv.connPool
