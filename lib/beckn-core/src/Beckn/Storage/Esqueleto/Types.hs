module Beckn.Storage.Esqueleto.Types where

import Beckn.Prelude
import Database.Esqueleto.Experimental

data Point = Point
  deriving (Generic, Show, Read, Eq, ToSchema)

instance PersistField Point where
  toPersistValue _ = error "This value should not be used in queries directly."
  fromPersistValue _ = return Point

instance PersistFieldSql Point where
  sqlType _ = SqlOther "geography"

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

type Table a = SqlExpr (Entity a)

type MbTable a = SqlExpr (Maybe (Entity a))
