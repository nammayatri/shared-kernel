 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Storage.Esqueleto.Types
  ( Point (..),
    PostgresList (..),
    PostgresNonEmptyList (..),
    Table,
    MbTable,
    PostgresListField,
  )
where

import Database.Esqueleto.Experimental hiding (Table)
import Kernel.Prelude

data Point = Point
  deriving (Generic, Show, Read, Eq, ToSchema)

instance PersistField Point where
  toPersistValue _ = error "This value should not be used in queries directly."
  fromPersistValue _ = return Point

instance PersistFieldSql Point where
  sqlType _ = SqlOther "geography"

class PostgresListField a

instance PostgresListField (PostgresList a)

instance PostgresListField (PostgresNonEmptyList a)

--
newtype PostgresList a = PostgresList {unPostgresList :: [a]}

-- It works, but what the hell is going on here?
-- toPersistValue -- [a] -> PersistArray
-- fromPersistValue -- PersistList -> [a]
-- we use different PersistValue constructors for marshaling there and back
--
-- This instance isn't currently properly tested, but it works at least for a list of doubles.
instance (PersistField a) => PersistField (PostgresList a) where
  toPersistValue (PostgresList xs) = PersistArray $ map toPersistValue xs
  fromPersistValue (PersistList xs) = PostgresList <$> mapM fromPersistValue xs
  fromPersistValue x = Left $ "Cannot convert " <> show x <> " to PostgresList"

instance (PersistField a) => PersistFieldSql (PostgresList a) where
  sqlType _ = SqlString

--
newtype PostgresNonEmptyList a = PostgresNonEmptyList {unPostgresNonEmptyList :: NonEmpty a}

instance (PersistField a) => PersistField (PostgresNonEmptyList a) where
  toPersistValue (PostgresNonEmptyList xs) = PersistArray $ map toPersistValue $ toList xs
  fromPersistValue (PersistList (s : xs)) = PostgresNonEmptyList <$> mapM fromPersistValue (s :| xs)
  fromPersistValue x = Left $ "Cannot convert " <> show x <> " to PostgresNonEmptyList"

instance (PersistField a) => PersistFieldSql (PostgresNonEmptyList a) where
  sqlType _ = SqlString

type Table a = SqlExpr (Entity a)

type MbTable a = SqlExpr (Maybe (Entity a))
