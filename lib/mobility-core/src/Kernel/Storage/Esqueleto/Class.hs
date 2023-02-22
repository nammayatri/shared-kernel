 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Storage.Esqueleto.Class
  ( TType (..),
    TEntityKey (..),
    extractSolidType,
    QEntity (..),
    TEntity (..),
    SolidType,
  )
where

import Database.Esqueleto.Experimental
import qualified Database.Esqueleto.Internal.Internal as EsqInt
import EulerHS.Prelude hiding (Key)
import qualified GHC.Generics as Gen
import Kernel.Types.Logging (Log)

class
  TType t a
    | a -> t
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

instance {-# OVERLAPPABLE #-} (Generic t, Generic a, GQEntity (Gen.Rep t) (Gen.Rep a)) => QEntity t a where
  toResult t = Gen.to <$> toResultGen (Gen.from t)

instance {-# OVERLAPPING #-} TType a b => QEntity (Entity a) b where
  toResult = fromTType . extractTType

instance {-# OVERLAPPING #-} QEntity a b => QEntity [a] [b] where
  toResult = traverse toResult

instance {-# OVERLAPPING #-} QEntity a b => QEntity (Maybe a) (Maybe b) where
  toResult = mapM toResult

instance {-# OVERLAPPING #-} ((b ~ DomainKey a), TEntityKey a) => QEntity (Value (Key a)) b where
  toResult = return . fromKey . unValue

instance {-# OVERLAPPING #-} QEntity (Value a) a where
  toResult = return . unValue

instance {-# OVERLAPPING #-} (TType a b) => QEntity (SolidType a) b where
  toResult = fromTType . unSolidType

class GQEntity gt ga where
  toResultGen :: (MonadThrow m, Log m) => gt p -> m (ga p)

instance
  (GQEntity a1 a2, GQEntity b1 b2) =>
  GQEntity ((Gen.:*:) a1 b1) ((Gen.:*:) a2 b2)
  where
  toResultGen (a1 Gen.:*: b1) = do
    a2 <- toResultGen a1
    b2 <- toResultGen b1
    return (a2 Gen.:*: b2)

instance
  (GQEntity a1 a2, GQEntity b1 b2) =>
  GQEntity ((Gen.:+:) a1 b1) ((Gen.:+:) a2 b2)
  where
  toResultGen (Gen.L1 a1) = Gen.L1 <$> toResultGen a1
  toResultGen (Gen.R1 b1) = Gen.R1 <$> toResultGen b1

instance
  (GQEntity a1 a2) =>
  GQEntity (Gen.M1 i t a1) (Gen.M1 i t a2)
  where
  toResultGen (Gen.M1 a1) = Gen.M1 <$> toResultGen a1

instance
  (QEntity a1 a2) =>
  GQEntity (Gen.K1 i a1) (Gen.K1 i a2)
  where
  toResultGen (Gen.K1 a1) = Gen.K1 <$> toResult a1

class TEntity te a where
  extractTType :: te -> a

instance {-# OVERLAPPABLE #-} (Generic te, Generic a, GTEntity (Gen.Rep te) (Gen.Rep a)) => TEntity te a where
  extractTType = Gen.to . extractTTypeGen . Gen.from

instance {-# OVERLAPPING #-} TEntity (Entity a) a where
  extractTType = entityVal

instance {-# OVERLAPPING #-} TEntity (Value a) a where
  extractTType = unValue

instance {-# OVERLAPPING #-} TEntity a b => TEntity [a] [b] where
  extractTType = fmap extractTType

instance {-# OVERLAPPING #-} TEntity a b => TEntity (Maybe a) (Maybe b) where
  extractTType = fmap extractTType

class GTEntity gt ga where
  extractTTypeGen :: gt p -> ga p

instance
  (GTEntity a1 a2, GTEntity b1 b2) =>
  GTEntity ((Gen.:*:) a1 b1) ((Gen.:*:) a2 b2)
  where
  extractTTypeGen (a1 Gen.:*: b1) = do
    let a2 = extractTTypeGen a1
        b2 = extractTTypeGen b1
    a2 Gen.:*: b2

instance
  (GTEntity a1 a2, GTEntity b1 b2) =>
  GTEntity ((Gen.:+:) a1 b1) ((Gen.:+:) a2 b2)
  where
  extractTTypeGen (Gen.L1 a1) = Gen.L1 $ extractTTypeGen a1
  extractTTypeGen (Gen.R1 b1) = Gen.R1 $ extractTTypeGen b1

instance
  (GTEntity a1 a2) =>
  GTEntity (Gen.M1 i t a1) (Gen.M1 i t a2)
  where
  extractTTypeGen (Gen.M1 a1) = Gen.M1 $ extractTTypeGen a1

instance
  (TEntity a1 a2) =>
  GTEntity (Gen.K1 i a1) (Gen.K1 i a2)
  where
  extractTTypeGen (Gen.K1 a1) = Gen.K1 $ extractTType a1
