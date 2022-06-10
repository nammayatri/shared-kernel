{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Storage.Esqueleto.Class
  ( TType (..),
    TEntityKey (..),
    extractSolidType,
    QEntity (..),
    TEntity (..),
    SolidType,
  )
where

import Beckn.Types.Logging (Log)
import Database.Esqueleto.Experimental
import qualified Database.Esqueleto.Internal.Internal as EsqInt
import EulerHS.Prelude hiding (Key)

class
  TType t a
    | t -> a,
      a -> t
  where
  toTType :: a -> t
  fromTType :: (MonadThrow m, Log m) => t -> m a

class
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  TEntityKey t
  where
  type DomainKey t
  fromKey :: Key t -> DomainKey t
  toKey :: DomainKey t -> Key t

-- Use this type to bind returns of different tables/values.
-- It will allow to build combined type of them (i.e. (Entity a1, Entity a2) -> b )
extractSolidType :: forall solidt a. TType a solidt => a -> SolidType a
extractSolidType = SolidType

--------------------------------------------------------------------------------
--------------------Internal classes and instnces-------------------------------
--------------------------------------------------------------------------------

newtype SolidType a = SolidType {unSolidType :: a}

instance (EsqInt.SqlSelect a b) => EsqInt.SqlSelect (SolidType a) (SolidType b) where
  sqlSelectCols a (SolidType b) = EsqInt.sqlSelectCols a b
  sqlSelectColCount _ = EsqInt.sqlSelectColCount (Proxy @a)
  sqlSelectProcessRow a = SolidType <$> EsqInt.sqlSelectProcessRow a

class QEntity t a where
  toResult :: (MonadThrow m, Log m) => t -> m a

instance TType a b => QEntity (Entity a) b where
  toResult = fromTType . extractTType

instance QEntity a b => QEntity [a] [b] where
  toResult = traverse toResult

instance QEntity a b => QEntity (Maybe a) (Maybe b) where
  toResult = mapM toResult

instance ((b ~ DomainKey a), TEntityKey a) => QEntity (Value (Key a)) b where
  toResult = return . fromKey . unValue

instance QEntity (Value a) a where
  toResult = return . unValue

instance (TType a b) => QEntity (SolidType a) b where
  toResult = fromTType . unSolidType

instance
  ( QEntity a1 b1,
    QEntity a2 b2
  ) =>
  QEntity (a1, a2) (b1, b2)
  where
  toResult (a1, a2) =
    (,) <$> toResult a1
      <*> toResult a2

instance
  ( QEntity a1 b1,
    QEntity a2 b2,
    QEntity a3 b3
  ) =>
  QEntity (a1, a2, a3) (b1, b2, b3)
  where
  toResult (a1, a2, a3) =
    (,,)
      <$> toResult a1
      <*> toResult a2
      <*> toResult a3

instance
  ( QEntity a1 b1,
    QEntity a2 b2,
    QEntity a3 b3,
    QEntity a4 b4
  ) =>
  QEntity (a1, a2, a3, a4) (b1, b2, b3, b4)
  where
  toResult (a1, a2, a3, a4) =
    (,,,)
      <$> toResult a1
      <*> toResult a2
      <*> toResult a3
      <*> toResult a4

instance
  ( QEntity a1 b1,
    QEntity a2 b2,
    QEntity a3 b3,
    QEntity a4 b4,
    QEntity a5 b5
  ) =>
  QEntity (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
  where
  toResult (a1, a2, a3, a4, a5) =
    (,,,,)
      <$> toResult a1
      <*> toResult a2
      <*> toResult a3
      <*> toResult a4
      <*> toResult a5

instance
  ( QEntity a1 b1,
    QEntity a2 b2,
    QEntity a3 b3,
    QEntity a4 b4,
    QEntity a5 b5,
    QEntity a6 b6
  ) =>
  QEntity (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
  where
  toResult (a1, a2, a3, a4, a5, a6) =
    (,,,,,)
      <$> toResult a1
      <*> toResult a2
      <*> toResult a3
      <*> toResult a4
      <*> toResult a5
      <*> toResult a6

instance
  ( QEntity a1 b1,
    QEntity a2 b2,
    QEntity a3 b3,
    QEntity a4 b4,
    QEntity a5 b5,
    QEntity a6 b6,
    QEntity a7 b7
  ) =>
  QEntity (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
  where
  toResult (a1, a2, a3, a4, a5, a6, a7) =
    (,,,,,,)
      <$> toResult a1
      <*> toResult a2
      <*> toResult a3
      <*> toResult a4
      <*> toResult a5
      <*> toResult a6
      <*> toResult a7

class TEntity te a where
  extractTType :: te -> a

instance TEntity (Entity a) a where
  extractTType = entityVal

instance TEntity (Value a) a where
  extractTType = unValue

instance TEntity a b => TEntity [a] [b] where
  extractTType = fmap extractTType

instance TEntity a b => TEntity (Maybe a) (Maybe b) where
  extractTType = fmap extractTType

instance
  ( TEntity a1 b1,
    TEntity a2 b2
  ) =>
  TEntity (a1, a2) (b1, b2)
  where
  extractTType (a1, a2) =
    (,)
      (extractTType a1)
      (extractTType a2)

instance
  ( TEntity a1 b1,
    TEntity a2 b2,
    TEntity a3 b3
  ) =>
  TEntity (a1, a2, a3) (b1, b2, b3)
  where
  extractTType (a1, a2, a3) =
    (,,)
      (extractTType a1)
      (extractTType a2)
      (extractTType a3)

instance
  ( TEntity a1 b1,
    TEntity a2 b2,
    TEntity a3 b3,
    TEntity a4 b4
  ) =>
  TEntity (a1, a2, a3, a4) (b1, b2, b3, b4)
  where
  extractTType (a1, a2, a3, a4) =
    (,,,)
      (extractTType a1)
      (extractTType a2)
      (extractTType a3)
      (extractTType a4)

instance
  ( TEntity a1 b1,
    TEntity a2 b2,
    TEntity a3 b3,
    TEntity a4 b4,
    TEntity a5 b5
  ) =>
  TEntity (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
  where
  extractTType (a1, a2, a3, a4, a5) =
    (,,,,)
      (extractTType a1)
      (extractTType a2)
      (extractTType a3)
      (extractTType a4)
      (extractTType a5)

instance
  ( TEntity a1 b1,
    TEntity a2 b2,
    TEntity a3 b3,
    TEntity a4 b4,
    TEntity a5 b5,
    TEntity a6 b6
  ) =>
  TEntity (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
  where
  extractTType (a1, a2, a3, a4, a5, a6) =
    (,,,,,)
      (extractTType a1)
      (extractTType a2)
      (extractTType a3)
      (extractTType a4)
      (extractTType a5)
      (extractTType a6)

instance
  ( TEntity a1 b1,
    TEntity a2 b2,
    TEntity a3 b3,
    TEntity a4 b4,
    TEntity a5 b5,
    TEntity a6 b6,
    TEntity a7 b7
  ) =>
  TEntity (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
  where
  extractTType (a1, a2, a3, a4, a5, a6, a7) =
    (,,,,,,)
      (extractTType a1)
      (extractTType a2)
      (extractTType a3)
      (extractTType a4)
      (extractTType a5)
      (extractTType a6)
      (extractTType a7)
