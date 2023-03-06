{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
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
    finalize,
  )
where

import Data.Time (UTCTime)
import Data.Typeable (cast)
import Database.Esqueleto.Experimental (SqlBackend)
import EulerHS.Prelude
import Kernel.Storage.Esqueleto.Class
import Kernel.Storage.Esqueleto.Logger (LoggerIO)
import Kernel.Types.GuidLike
import Kernel.Types.MonadGuid
import Kernel.Types.Time (MonadTime (..))
import Kernel.Utils.Logging

data SqlDBEnv = forall m.
  Finalize m =>
  SqlDBEnv
  { currentTime :: UTCTime,
    actions :: m ()
  }

newtype SqlDB a = SqlDB {unSqlDB :: StateT SqlDBEnv (ReaderT SqlBackend LoggerIO) a}
  deriving newtype (Functor, Applicative, Monad, MonadTime, MonadGuid, Log, MonadThrow, MonadState SqlDBEnv)

instance Monad m => MonadTime (StateT SqlDBEnv m) where
  getCurrentTime = gets (.currentTime)

instance MonadGuid (StateT SqlDBEnv (ReaderT SqlBackend LoggerIO)) where
  generateGUIDText = lift $ lift generateGUID

instance Log (StateT SqlDBEnv (ReaderT SqlBackend LoggerIO)) where
  logOutput a b = lift . lift $ logOutput a b
  withLogTag a (StateT f1) = StateT $ \env1 -> do
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

withFullEntity' :: ToTType t a => a -> (t -> b) -> b
withFullEntity' dtype func = func $ toTType dtype

withFullEntity :: ToTType t a => a -> (t -> FullEntitySqlDB b) -> SqlDB b
withFullEntity dtype func = getSqlDB $ withFullEntity' dtype func

withFullEntities' :: ToTType t a => [a] -> ([t] -> b) -> b
withFullEntities' [] f = f []
withFullEntities' (x : xs) f =
  withFullEntity' x $ \y ->
    withFullEntities' xs \ys -> f (y : ys)

withFullEntities :: ToTType t a => [a] -> ([t] -> FullEntitySqlDB b) -> SqlDB b
withFullEntities dtypes func = getSqlDB $ withFullEntities' dtypes func

finalize :: forall m. Finalize m => m () -> SqlDB ()
finalize someAction = do
  SqlDBEnv {..} <- get
  let mbPrevActions = cast @_ @(m ()) actions
  case mbPrevActions of
    Nothing -> do
      logWarning $
        "Couldn't append finalizer action in \""
          <> monadType someAction
          <> "\"monad. It caused because previous action was created in \""
          <> monadType actions
          <> "\"monad."
    Just _ -> pure ()
  let prevActions = fromMaybe (pure ()) mbPrevActions
  put $ SqlDBEnv {actions = prevActions >> someAction, currentTime}
