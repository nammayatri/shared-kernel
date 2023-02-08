{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kernel.Storage.Esqueleto.SqlDB
  ( SqlDBEnv (..),
    SqlDB (..),
    SelectSqlDB (..),
    FullEntitySqlDB,
    liftToFullEntitySqlDB,
    withFullEntity,
    withFullEntities,
  )
where

import Kernel.Storage.Esqueleto.Class
import Kernel.Storage.Esqueleto.Logger (LoggerIO)
import Kernel.Types.GuidLike
import Kernel.Types.MonadGuid
import Kernel.Types.Time (MonadTime (..))
import Kernel.Utils.Logging
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental (SqlBackend)
import EulerHS.Prelude

newtype SqlDBEnv = SqlDBEnv
  { currentTime :: UTCTime
  }

newtype SqlDB a = SqlDB {unSqlDB :: ReaderT SqlDBEnv (ReaderT SqlBackend LoggerIO) a}
  deriving newtype (Functor, Applicative, Monad, MonadTime, MonadGuid, Log, MonadThrow)

instance Monad m => MonadTime (ReaderT SqlDBEnv m) where
  getCurrentTime = asks (.currentTime)

instance MonadGuid (ReaderT SqlDBEnv (ReaderT SqlBackend LoggerIO)) where
  generateGUIDText = lift $ lift generateGUID

instance Log (ReaderT SqlDBEnv (ReaderT SqlBackend LoggerIO)) where
  logOutput a b = lift . lift $ logOutput a b
  withLogTag a (ReaderT f1) = ReaderT $ \env1 -> do
    let (ReaderT f2) = f1 env1
    ReaderT $ \env2 ->
      withLogTag a $ f2 env2

newtype SelectSqlDB a = SelectSqlDB {unSelectSqlDB :: SqlDB a}
  deriving newtype (Functor, Applicative, Monad, MonadTime, MonadGuid, Log, MonadThrow)

newtype FullEntitySqlDB t = FullEntitySqlDB
  { getSqlDB :: SqlDB t
  }
  deriving newtype (Functor, Applicative, Monad, MonadTime, MonadGuid)

liftToFullEntitySqlDB :: SqlDB t -> FullEntitySqlDB t
liftToFullEntitySqlDB = FullEntitySqlDB

withFullEntity' :: TType t a => a -> (t -> b) -> b
withFullEntity' dtype func = func $ toTType dtype

withFullEntity :: TType t a => a -> (t -> FullEntitySqlDB b) -> SqlDB b
withFullEntity dtype func = getSqlDB $ withFullEntity' dtype func

withFullEntities' :: TType t a => [a] -> ([t] -> b) -> b
withFullEntities' [] f = f []
withFullEntities' (x : xs) f =
  withFullEntity' x $ \y ->
    withFullEntities' xs \ys -> f (y : ys)

withFullEntities :: TType t a => [a] -> ([t] -> FullEntitySqlDB b) -> SqlDB b
withFullEntities dtypes func = getSqlDB $ withFullEntities' dtypes func
