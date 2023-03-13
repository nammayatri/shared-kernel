{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Kernel.Storage.Esqueleto.Queries.Raw
  {-# WARNING
    "Raw queries are not finished, hence not working."
    #-}
  ( rawSql,
    rawSqlFE,
    rawSqlDTB,
    tableName,
    fieldName,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Builder as LBS
import Data.Fixed (Pico)
import qualified Data.String as BBB
import Data.String.Conversions.Monomorphic (fromLazyByteString)
import qualified Database.Esqueleto.Experimental as Esq
import qualified Database.Persist as Esq
import Database.PostgreSQL.Simple.ToField as PGTF
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Kernel.Data.HeterogeneousList as HL
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Class
import Kernel.Storage.Esqueleto.DTypeBuilder
import Kernel.Storage.Esqueleto.SqlDB

renderPersistValue :: Esq.PersistValue -> ByteString
renderPersistValue pv = renderAction $ getAction pv
  where
    getAction = \case
      -- This logic is mostly copypaste from https://hackage.haskell.org/package/persistent-postgresql-2.13.5.1/docs/src/Database.Persist.Postgresql.Internal.html#P
      Esq.PersistText t -> PGTF.toField t
      Esq.PersistByteString bs -> PGTF.toField (PG.Binary bs)
      Esq.PersistInt64 i -> PGTF.toField i
      Esq.PersistDouble d -> PGTF.toField d
      Esq.PersistRational r -> PGTF.Plain $ BBB.fromString $ show (fromRational r :: Pico)
      Esq.PersistBool b -> PGTF.toField b
      Esq.PersistDay d -> PGTF.toField d
      Esq.PersistTimeOfDay t -> PGTF.toField t
      Esq.PersistUTCTime t -> PGTF.toField t
      Esq.PersistNull -> PGTF.toField PG.Null
      Esq.PersistList l -> PGTF.toField $ Esq.listToJSON l
      Esq.PersistMap m -> PGTF.toField $ Esq.mapToJSON m
      Esq.PersistLiteral_ Esq.DbSpecific s -> PGTF.Escape s
      Esq.PersistLiteral_ Esq.Unescaped l -> PGTF.Plain (BB.byteString l)
      Esq.PersistLiteral_ Esq.Escaped e -> PGTF.Escape e
      Esq.PersistArray a -> PGTF.toField $ PG.PGArray $ getAction <$> a
      Esq.PersistObjectId _ -> error "Refusing to serialize a PersistObjectId to a PostgreSQL value"
    renderAction = \case
      PGTF.Plain b -> fromLazyByteString $ LBS.toLazyByteString b
      PGTF.Escape bs -> "\'" <> bs <> "\'"
      PGTF.EscapeByteA bs -> "\'" <> bs <> "\'"
      PGTF.EscapeIdentifier bs -> "\"" <> bs <> "\""
      PGTF.Many actions -> (\bs -> "[" <> bs <> "]") . BS.intercalate ", " $ renderAction <$> actions

class RawSqlValue a where
  toPersistValue :: a -> Esq.PersistValue

instance {-# OVERLAPPABLE #-} (Esq.PersistField a) => RawSqlValue a where
  toPersistValue = Esq.toPersistValue

instance
  {-# OVERLAPS #-}
  ( RawSqlValue a1,
    RawSqlValue a2
  ) =>
  RawSqlValue (a1, a2)
  where
  toPersistValue (a1, a2) = do
    let a1PVal = toPersistValue a1
    let a2PVal = toPersistValue a2
    let valsList = [a1PVal, a2PVal]
    Esq.PersistLiteral_ Esq.Unescaped $ (\bs -> "(" <> bs <> ")") . BS.intercalate ", " $ renderPersistValue <$> valsList

instance
  {-# OVERLAPS #-}
  ( RawSqlValue a1,
    RawSqlValue a2,
    RawSqlValue a3
  ) =>
  RawSqlValue (a1, a2, a3)
  where
  toPersistValue (a1, a2, a3) = do
    let a1PVal = toPersistValue a1
    let a2PVal = toPersistValue a2
    let a3PVal = toPersistValue a3
    let valsList = [a1PVal, a2PVal, a3PVal]
    Esq.PersistLiteral_ Esq.Unescaped $ (\bs -> "(" <> bs <> ")") . BS.intercalate ", " $ renderPersistValue <$> valsList

instance
  {-# OVERLAPS #-}
  ( RawSqlValue a1,
    RawSqlValue a2,
    RawSqlValue a3,
    RawSqlValue a4
  ) =>
  RawSqlValue (a1, a2, a3, a4)
  where
  toPersistValue (a1, a2, a3, a4) = do
    let a1PVal = toPersistValue a1
    let a2PVal = toPersistValue a2
    let a3PVal = toPersistValue a3
    let a4PVal = toPersistValue a4
    let valsList = [a1PVal, a2PVal, a3PVal, a4PVal]
    Esq.PersistLiteral_ Esq.Unescaped $ (\bs -> "(" <> bs <> ")") . BS.intercalate ", " $ renderPersistValue <$> valsList

instance
  {-# OVERLAPS #-}
  ( RawSqlValue a1,
    RawSqlValue a2,
    RawSqlValue a3,
    RawSqlValue a4,
    RawSqlValue a5
  ) =>
  RawSqlValue (a1, a2, a3, a4, a5)
  where
  toPersistValue (a1, a2, a3, a4, a5) = do
    let a1PVal = toPersistValue a1
    let a2PVal = toPersistValue a2
    let a3PVal = toPersistValue a3
    let a4PVal = toPersistValue a4
    let a5PVal = toPersistValue a5
    let valsList = [a1PVal, a2PVal, a3PVal, a4PVal, a5PVal]
    Esq.PersistLiteral_ Esq.Unescaped $ (\bs -> "(" <> bs <> ")") . BS.intercalate ", " $ renderPersistValue <$> valsList

instance
  {-# OVERLAPS #-}
  ( RawSqlValue a1,
    RawSqlValue a2,
    RawSqlValue a3,
    RawSqlValue a4,
    RawSqlValue a5,
    RawSqlValue a6
  ) =>
  RawSqlValue (a1, a2, a3, a4, a5, a6)
  where
  toPersistValue (a1, a2, a3, a4, a5, a6) = do
    let a1PVal = toPersistValue a1
    let a2PVal = toPersistValue a2
    let a3PVal = toPersistValue a3
    let a4PVal = toPersistValue a4
    let a5PVal = toPersistValue a5
    let a6PVal = toPersistValue a6
    let valsList = [a1PVal, a2PVal, a3PVal, a4PVal, a5PVal, a6PVal]
    Esq.PersistLiteral_ Esq.Unescaped $ (\bs -> "(" <> bs <> ")") . BS.intercalate ", " $ renderPersistValue <$> valsList

instance
  {-# OVERLAPS #-}
  ( RawSqlValue a1,
    RawSqlValue a2,
    RawSqlValue a3,
    RawSqlValue a4,
    RawSqlValue a5,
    RawSqlValue a6,
    RawSqlValue a7
  ) =>
  RawSqlValue (a1, a2, a3, a4, a5, a6, a7)
  where
  toPersistValue (a1, a2, a3, a4, a5, a6, a7) = do
    let a1PVal = toPersistValue a1
    let a2PVal = toPersistValue a2
    let a3PVal = toPersistValue a3
    let a4PVal = toPersistValue a4
    let a5PVal = toPersistValue a5
    let a6PVal = toPersistValue a6
    let a7PVal = toPersistValue a7
    let valsList = [a1PVal, a2PVal, a3PVal, a4PVal, a5PVal, a6PVal, a7PVal]
    Esq.PersistLiteral_ Esq.Unescaped $ (\bs -> "(" <> bs <> ")") . BS.intercalate ", " $ renderPersistValue <$> valsList

instance
  {-# OVERLAPS #-}
  ( RawSqlValue a1,
    RawSqlValue a2,
    RawSqlValue a3,
    RawSqlValue a4,
    RawSqlValue a5,
    RawSqlValue a6,
    RawSqlValue a7,
    RawSqlValue a8
  ) =>
  RawSqlValue (a1, a2, a3, a4, a5, a6, a7, a8)
  where
  toPersistValue (a1, a2, a3, a4, a5, a6, a7, a8) = do
    let a1PVal = toPersistValue a1
    let a2PVal = toPersistValue a2
    let a3PVal = toPersistValue a3
    let a4PVal = toPersistValue a4
    let a5PVal = toPersistValue a5
    let a6PVal = toPersistValue a6
    let a7PVal = toPersistValue a7
    let a8PVal = toPersistValue a8
    let valsList = [a1PVal, a2PVal, a3PVal, a4PVal, a5PVal, a6PVal, a7PVal, a8PVal]
    Esq.PersistLiteral_ Esq.Unescaped $ (\bs -> "(" <> bs <> ")") . BS.intercalate ", " $ renderPersistValue <$> valsList

instance
  {-# OVERLAPS #-}
  ( RawSqlValue a1,
    RawSqlValue a2,
    RawSqlValue a3,
    RawSqlValue a4,
    RawSqlValue a5,
    RawSqlValue a6,
    RawSqlValue a7,
    RawSqlValue a8,
    RawSqlValue a9
  ) =>
  RawSqlValue (a1, a2, a3, a4, a5, a6, a7, a8, a9)
  where
  toPersistValue (a1, a2, a3, a4, a5, a6, a7, a8, a9) = do
    let a1PVal = toPersistValue a1
    let a2PVal = toPersistValue a2
    let a3PVal = toPersistValue a3
    let a4PVal = toPersistValue a4
    let a5PVal = toPersistValue a5
    let a6PVal = toPersistValue a6
    let a7PVal = toPersistValue a7
    let a8PVal = toPersistValue a8
    let a9PVal = toPersistValue a9
    let valsList = [a1PVal, a2PVal, a3PVal, a4PVal, a5PVal, a6PVal, a7PVal, a8PVal, a9PVal]
    Esq.PersistLiteral_ Esq.Unescaped $ (\bs -> "(" <> bs <> ")") . BS.intercalate ", " $ renderPersistValue <$> valsList

instance
  {-# OVERLAPS #-}
  ( RawSqlValue a1,
    RawSqlValue a2,
    RawSqlValue a3,
    RawSqlValue a4,
    RawSqlValue a5,
    RawSqlValue a6,
    RawSqlValue a7,
    RawSqlValue a8,
    RawSqlValue a9,
    RawSqlValue a10
  ) =>
  RawSqlValue (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  where
  toPersistValue (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = do
    let a1PVal = toPersistValue a1
    let a2PVal = toPersistValue a2
    let a3PVal = toPersistValue a3
    let a4PVal = toPersistValue a4
    let a5PVal = toPersistValue a5
    let a6PVal = toPersistValue a6
    let a7PVal = toPersistValue a7
    let a8PVal = toPersistValue a8
    let a9PVal = toPersistValue a9
    let a10PVal = toPersistValue a10
    let valsList = [a1PVal, a2PVal, a3PVal, a4PVal, a5PVal, a6PVal, a7PVal, a8PVal, a9PVal, a10PVal]
    Esq.PersistLiteral_ Esq.Unescaped $ (\bs -> "(" <> bs <> ")") . BS.intercalate ", " $ renderPersistValue <$> valsList

instance {-# OVERLAPS #-} RawSqlValue Esq.EntityNameDB where
  toPersistValue (Esq.EntityNameDB name) = Esq.PersistLiteral_ Esq.Unescaped $ "\"" <> encodeUtf8 name <> "\""

instance {-# OVERLAPS #-} RawSqlValue Esq.FieldNameDB where
  toPersistValue (Esq.FieldNameDB field) = Esq.PersistLiteral_ Esq.Unescaped $ "\"" <> encodeUtf8 field <> "\""

tableName :: forall a. Esq.PersistEntity a => Esq.PersistValue
tableName = toPersistValue @Esq.EntityNameDB $ Esq.getEntityDBName $ Esq.entityDef @a @Proxy $ (Proxy :: Proxy a)

fieldName :: forall record typ. Esq.PersistEntity record => Esq.EntityField record typ -> Esq.FieldNameDB
fieldName = Esq.fieldDBName

rawSql ::
  forall t a.
  ( Esq.RawSql t,
    QEntity [t] [a]
  ) =>
  Text ->
  HL.HeterogeneousList '[RawSqlValue] ->
  SqlDB [a]
rawSql raw = buildDType @_ @[t] . liftToBuilder . SqlDB . lift . Esq.rawSql raw . HL.mapToList toPersistValue

rawSqlFE ::
  forall t a.
  ( Esq.RawSql t,
    QEntity [t] [a]
  ) =>
  Text ->
  HL.HeterogeneousList '[RawSqlValue] ->
  FullEntitySqlDB [a]
rawSqlFE raw v = liftToFullEntitySqlDB $ rawSql @t @a raw v

rawSqlDTB ::
  forall t a.
  ( Esq.RawSql t,
    TEntity [t] [a]
  ) =>
  Text ->
  HL.HeterogeneousList '[RawSqlValue] ->
  DTypeBuilder SqlDB [a]
rawSqlDTB raw l = extractTType <$> (liftToBuilder . SqlDB . lift $ Esq.rawSql @t raw $ HL.mapToList toPersistValue l)
