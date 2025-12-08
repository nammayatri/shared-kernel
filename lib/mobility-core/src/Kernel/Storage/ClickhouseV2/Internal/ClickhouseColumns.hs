{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Storage.ClickhouseV2.Internal.ClickhouseColumns where

import qualified Data.Aeson.KeyMap as A
import qualified Data.Aeson.Types as A
import Data.Coerce (coerce)
import qualified Data.List as List
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2.ClickhouseTable
import Kernel.Storage.ClickhouseV2.ClickhouseValue
import Kernel.Storage.ClickhouseV2.Internal.Types

instance (FromJSON (ColumnsType 'NOT_AGG (Columns 'NOT_AGG t)), ClickhouseTable t) => ClickhouseColumns 'NOT_AGG (Columns 'NOT_AGG t) where
  type ColumnsType 'NOT_AGG (Columns 'NOT_AGG t) = t Identity
  showClickhouseColumns _ _ _ = "*"
  parseColumns _ _ val _ = eitherResult . A.fromJSON $ val

-- should be all AGG columns or all NOT_AGG columns
instance (ClickhouseValue v) => ClickhouseColumns a (Column a t v) where
  type ColumnsType a (Column a t v) = v
  showClickhouseColumns _ = zipColumnsWithSynonyms1
  parseColumns _ = parseColumns1

instance (C2 ClickhouseValue v1 v2) => ClickhouseColumns a (T2 (Column a t) v1 v2) where
  type ColumnsType a (T2 (Column a t) v1 v2) = (v1, v2)
  showClickhouseColumns _ = zipColumnsWithSynonyms2
  parseColumns _ = parseColumns2

instance (C3 ClickhouseValue v1 v2 v3) => ClickhouseColumns a (T3 (Column a t) v1 v2 v3) where
  type ColumnsType a (T3 (Column a t) v1 v2 v3) = (v1, v2, v3)
  showClickhouseColumns _ = zipColumnsWithSynonyms3
  parseColumns _ = parseColumns3

instance (C4 ClickhouseValue v1 v2 v3 v4) => ClickhouseColumns a (T4 (Column a t) v1 v2 v3 v4) where
  type ColumnsType a (T4 (Column a t) v1 v2 v3 v4) = (v1, v2, v3, v4)
  showClickhouseColumns _ = zipColumnsWithSynonyms4
  parseColumns _ = parseColumns4

instance (C5 ClickhouseValue v1 v2 v3 v4 v5) => ClickhouseColumns a (T5 (Column a t) v1 v2 v3 v4 v5) where
  type ColumnsType a (T5 (Column a t) v1 v2 v3 v4 v5) = (v1, v2, v3, v4, v5)
  showClickhouseColumns _ = zipColumnsWithSynonyms5
  parseColumns _ = parseColumns5

instance (C6 ClickhouseValue v1 v2 v3 v4 v5 v6) => ClickhouseColumns a (T6 (Column a t) v1 v2 v3 v4 v5 v6) where
  type ColumnsType a (T6 (Column a t) v1 v2 v3 v4 v5 v6) = (v1, v2, v3, v4, v5, v6)
  showClickhouseColumns _ = zipColumnsWithSynonyms6
  parseColumns _ = parseColumns6

instance (C7 ClickhouseValue v1 v2 v3 v4 v5 v6 v7) => ClickhouseColumns a (T7 (Column a t) v1 v2 v3 v4 v5 v6 v7) where
  type ColumnsType a (T7 (Column a t) v1 v2 v3 v4 v5 v6 v7) = (v1, v2, v3, v4, v5, v6, v7)
  showClickhouseColumns _ = zipColumnsWithSynonyms7
  parseColumns _ = parseColumns7

instance (C8 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8) => ClickhouseColumns a (T8 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8) where
  type ColumnsType a (T8 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8) = (v1, v2, v3, v4, v5, v6, v7, v8)
  showClickhouseColumns _ = zipColumnsWithSynonyms8
  parseColumns _ = parseColumns8

instance (C9 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8 v9) => ClickhouseColumns a (T9 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9) where
  type ColumnsType a (T9 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9) = (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  showClickhouseColumns _ = zipColumnsWithSynonyms9
  parseColumns _ = parseColumns9

instance (C10 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8 v9 v10) => ClickhouseColumns a (T10 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10) where
  type ColumnsType a (T10 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10) = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)
  showClickhouseColumns _ = zipColumnsWithSynonyms10
  parseColumns _ = parseColumns10

instance (C11 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11) => ClickhouseColumns a (T11 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11) where
  type ColumnsType a (T11 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11) = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11)
  showClickhouseColumns _ = zipColumnsWithSynonyms11
  parseColumns _ = parseColumns11

instance (C12 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12) => ClickhouseColumns a (T12 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12) where
  type ColumnsType a (T12 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12) = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12)
  showClickhouseColumns _ = zipColumnsWithSynonyms12
  parseColumns _ = parseColumns12

instance (C13 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13) => ClickhouseColumns a (T13 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13) where
  type ColumnsType a (T13 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13) = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13)
  showClickhouseColumns _ = zipColumnsWithSynonyms13
  parseColumns _ = parseColumns13

instance (C14 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14) => ClickhouseColumns a (T14 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14) where
  type ColumnsType a (T14 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14) = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14)
  showClickhouseColumns _ = zipColumnsWithSynonyms14
  parseColumns _ = parseColumns14

instance (C15 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15) => ClickhouseColumns a (T15 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15) where
  type ColumnsType a (T15 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15) = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15)
  showClickhouseColumns _ = zipColumnsWithSynonyms15
  parseColumns _ = parseColumns15

instance (C16 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16) => ClickhouseColumns a (T16 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16) where
  type ColumnsType a (T16 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16) = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16)
  showClickhouseColumns _ = zipColumnsWithSynonyms16
  parseColumns _ = parseColumns16

-- we need to create map of values with different types
data NotSpecified

parseColumns1 ::
  forall a t v1.
  ClickhouseValue v1 =>
  Column a t v1 ->
  A.Value ->
  SubQueryLevel ->
  Either String v1
parseColumns1 c1 json l = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  parseValueFromMap @a @t @v1 1 c1 mapResult l

parseColumns2 ::
  forall a t v1 v2.
  (C2 ClickhouseValue v1 v2) =>
  T2 (Column a t) v1 v2 ->
  A.Value ->
  SubQueryLevel ->
  Either String (v1, v2)
parseColumns2 (c1, c2) json l = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult l
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult l
  pure (v1, v2)

parseColumns3 ::
  forall a t v1 v2 v3.
  (C3 ClickhouseValue v1 v2 v3) =>
  T3 (Column a t) v1 v2 v3 ->
  A.Value ->
  SubQueryLevel ->
  Either String (v1, v2, v3)
parseColumns3 (c1, c2, c3) json l = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult l
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult l
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult l
  pure (v1, v2, v3)

parseColumns4 ::
  forall a t v1 v2 v3 v4.
  (C4 ClickhouseValue v1 v2 v3 v4) =>
  T4 (Column a t) v1 v2 v3 v4 ->
  A.Value ->
  SubQueryLevel ->
  Either String (v1, v2, v3, v4)
parseColumns4 (c1, c2, c3, c4) json l = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult l
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult l
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult l
  v4 <- parseValueFromMap @a @t @v4 4 c4 mapResult l
  pure (v1, v2, v3, v4)

parseColumns5 ::
  forall a t v1 v2 v3 v4 v5.
  (C5 ClickhouseValue v1 v2 v3 v4 v5) =>
  T5 (Column a t) v1 v2 v3 v4 v5 ->
  A.Value ->
  SubQueryLevel ->
  Either String (v1, v2, v3, v4, v5)
parseColumns5 (c1, c2, c3, c4, c5) json l = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult l
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult l
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult l
  v4 <- parseValueFromMap @a @t @v4 4 c4 mapResult l
  v5 <- parseValueFromMap @a @t @v5 5 c5 mapResult l
  pure (v1, v2, v3, v4, v5)

parseColumns6 ::
  forall a t v1 v2 v3 v4 v5 v6.
  (C6 ClickhouseValue v1 v2 v3 v4 v5 v6) =>
  T6 (Column a t) v1 v2 v3 v4 v5 v6 ->
  A.Value ->
  SubQueryLevel ->
  Either String (v1, v2, v3, v4, v5, v6)
parseColumns6 (c1, c2, c3, c4, c5, c6) json l = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult l
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult l
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult l
  v4 <- parseValueFromMap @a @t @v4 4 c4 mapResult l
  v5 <- parseValueFromMap @a @t @v5 5 c5 mapResult l
  v6 <- parseValueFromMap @a @t @v6 6 c6 mapResult l
  pure (v1, v2, v3, v4, v5, v6)

parseColumns7 ::
  forall a t v1 v2 v3 v4 v5 v6 v7.
  (C7 ClickhouseValue v1 v2 v3 v4 v5 v6 v7) =>
  T7 (Column a t) v1 v2 v3 v4 v5 v6 v7 ->
  A.Value ->
  SubQueryLevel ->
  Either String (v1, v2, v3, v4, v5, v6, v7)
parseColumns7 (c1, c2, c3, c4, c5, c6, c7) json l = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult l
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult l
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult l
  v4 <- parseValueFromMap @a @t @v4 4 c4 mapResult l
  v5 <- parseValueFromMap @a @t @v5 5 c5 mapResult l
  v6 <- parseValueFromMap @a @t @v6 6 c6 mapResult l
  v7 <- parseValueFromMap @a @t @v7 7 c7 mapResult l
  pure (v1, v2, v3, v4, v5, v6, v7)

parseColumns8 ::
  forall a t v1 v2 v3 v4 v5 v6 v7 v8.
  (C8 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8) =>
  T8 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 ->
  A.Value ->
  SubQueryLevel ->
  Either String (v1, v2, v3, v4, v5, v6, v7, v8)
parseColumns8 (c1, c2, c3, c4, c5, c6, c7, c8) json l = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult l
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult l
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult l
  v4 <- parseValueFromMap @a @t @v4 4 c4 mapResult l
  v5 <- parseValueFromMap @a @t @v5 5 c5 mapResult l
  v6 <- parseValueFromMap @a @t @v6 6 c6 mapResult l
  v7 <- parseValueFromMap @a @t @v7 7 c7 mapResult l
  v8 <- parseValueFromMap @a @t @v8 8 c8 mapResult l
  pure (v1, v2, v3, v4, v5, v6, v7, v8)

parseColumns9 ::
  forall a t v1 v2 v3 v4 v5 v6 v7 v8 v9.
  (C9 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8 v9) =>
  T9 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 ->
  A.Value ->
  SubQueryLevel ->
  Either String (v1, v2, v3, v4, v5, v6, v7, v8, v9)
parseColumns9 (c1, c2, c3, c4, c5, c6, c7, c8, c9) json l = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult l
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult l
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult l
  v4 <- parseValueFromMap @a @t @v4 4 c4 mapResult l
  v5 <- parseValueFromMap @a @t @v5 5 c5 mapResult l
  v6 <- parseValueFromMap @a @t @v6 6 c6 mapResult l
  v7 <- parseValueFromMap @a @t @v7 7 c7 mapResult l
  v8 <- parseValueFromMap @a @t @v8 8 c8 mapResult l
  v9 <- parseValueFromMap @a @t @v9 9 c9 mapResult l
  pure (v1, v2, v3, v4, v5, v6, v7, v8, v9)

parseColumns10 ::
  forall a t v1 v2 v3 v4 v5 v6 v7 v8 v9 v10.
  (C10 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8 v9 v10) =>
  T10 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 ->
  A.Value ->
  SubQueryLevel ->
  Either String (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)
parseColumns10 (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) json l = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult l
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult l
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult l
  v4 <- parseValueFromMap @a @t @v4 4 c4 mapResult l
  v5 <- parseValueFromMap @a @t @v5 5 c5 mapResult l
  v6 <- parseValueFromMap @a @t @v6 6 c6 mapResult l
  v7 <- parseValueFromMap @a @t @v7 7 c7 mapResult l
  v8 <- parseValueFromMap @a @t @v8 8 c8 mapResult l
  v9 <- parseValueFromMap @a @t @v9 9 c9 mapResult l
  v10 <- parseValueFromMap @a @t @v10 10 c10 mapResult l
  pure (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)

parseColumns11 ::
  forall a t v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11.
  (C11 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11) =>
  T11 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 ->
  A.Value ->
  SubQueryLevel ->
  Either String (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11)
parseColumns11 (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11) json l = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult l
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult l
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult l
  v4 <- parseValueFromMap @a @t @v4 4 c4 mapResult l
  v5 <- parseValueFromMap @a @t @v5 5 c5 mapResult l
  v6 <- parseValueFromMap @a @t @v6 6 c6 mapResult l
  v7 <- parseValueFromMap @a @t @v7 7 c7 mapResult l
  v8 <- parseValueFromMap @a @t @v8 8 c8 mapResult l
  v9 <- parseValueFromMap @a @t @v9 9 c9 mapResult l
  v10 <- parseValueFromMap @a @t @v10 10 c10 mapResult l
  v11 <- parseValueFromMap @a @t @v11 11 c11 mapResult l
  pure (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11)

parseColumns12 ::
  forall a t v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12.
  (C12 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12) =>
  T12 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 ->
  A.Value ->
  SubQueryLevel ->
  Either String (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12)
parseColumns12 (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) json l = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult l
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult l
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult l
  v4 <- parseValueFromMap @a @t @v4 4 c4 mapResult l
  v5 <- parseValueFromMap @a @t @v5 5 c5 mapResult l
  v6 <- parseValueFromMap @a @t @v6 6 c6 mapResult l
  v7 <- parseValueFromMap @a @t @v7 7 c7 mapResult l
  v8 <- parseValueFromMap @a @t @v8 8 c8 mapResult l
  v9 <- parseValueFromMap @a @t @v9 9 c9 mapResult l
  v10 <- parseValueFromMap @a @t @v10 10 c10 mapResult l
  v11 <- parseValueFromMap @a @t @v11 11 c11 mapResult l
  v12 <- parseValueFromMap @a @t @v12 12 c12 mapResult l
  pure (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12)

parseColumns13 ::
  forall a t v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13.
  (C13 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13) =>
  T13 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 ->
  A.Value ->
  SubQueryLevel ->
  Either String (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13)
parseColumns13 (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13) json l = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult l
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult l
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult l
  v4 <- parseValueFromMap @a @t @v4 4 c4 mapResult l
  v5 <- parseValueFromMap @a @t @v5 5 c5 mapResult l
  v6 <- parseValueFromMap @a @t @v6 6 c6 mapResult l
  v7 <- parseValueFromMap @a @t @v7 7 c7 mapResult l
  v8 <- parseValueFromMap @a @t @v8 8 c8 mapResult l
  v9 <- parseValueFromMap @a @t @v9 9 c9 mapResult l
  v10 <- parseValueFromMap @a @t @v10 10 c10 mapResult l
  v11 <- parseValueFromMap @a @t @v11 11 c11 mapResult l
  v12 <- parseValueFromMap @a @t @v12 12 c12 mapResult l
  v13 <- parseValueFromMap @a @t @v13 13 c13 mapResult l
  pure (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13)

parseColumns14 ::
  forall a t v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14.
  (C14 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14) =>
  T14 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 ->
  A.Value ->
  SubQueryLevel ->
  Either String (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14)
parseColumns14 (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14) json l = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult l
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult l
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult l
  v4 <- parseValueFromMap @a @t @v4 4 c4 mapResult l
  v5 <- parseValueFromMap @a @t @v5 5 c5 mapResult l
  v6 <- parseValueFromMap @a @t @v6 6 c6 mapResult l
  v7 <- parseValueFromMap @a @t @v7 7 c7 mapResult l
  v8 <- parseValueFromMap @a @t @v8 8 c8 mapResult l
  v9 <- parseValueFromMap @a @t @v9 9 c9 mapResult l
  v10 <- parseValueFromMap @a @t @v10 10 c10 mapResult l
  v11 <- parseValueFromMap @a @t @v11 11 c11 mapResult l
  v12 <- parseValueFromMap @a @t @v12 12 c12 mapResult l
  v13 <- parseValueFromMap @a @t @v13 13 c13 mapResult l
  v14 <- parseValueFromMap @a @t @v14 14 c14 mapResult l
  pure (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14)

parseColumns15 ::
  forall a t v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15.
  (C15 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15) =>
  T15 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 ->
  A.Value ->
  SubQueryLevel ->
  Either String (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15)
parseColumns15 (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15) json l = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult l
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult l
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult l
  v4 <- parseValueFromMap @a @t @v4 4 c4 mapResult l
  v5 <- parseValueFromMap @a @t @v5 5 c5 mapResult l
  v6 <- parseValueFromMap @a @t @v6 6 c6 mapResult l
  v7 <- parseValueFromMap @a @t @v7 7 c7 mapResult l
  v8 <- parseValueFromMap @a @t @v8 8 c8 mapResult l
  v9 <- parseValueFromMap @a @t @v9 9 c9 mapResult l
  v10 <- parseValueFromMap @a @t @v10 10 c10 mapResult l
  v11 <- parseValueFromMap @a @t @v11 11 c11 mapResult l
  v12 <- parseValueFromMap @a @t @v12 12 c12 mapResult l
  v13 <- parseValueFromMap @a @t @v13 13 c13 mapResult l
  v14 <- parseValueFromMap @a @t @v14 14 c14 mapResult l
  v15 <- parseValueFromMap @a @t @v15 15 c15 mapResult l
  pure (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15)

parseColumns16 ::
  forall a t v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16.
  (C16 ClickhouseValue v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16) =>
  T16 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 ->
  A.Value ->
  SubQueryLevel ->
  Either String (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16)
parseColumns16 (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16) json l = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult l
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult l
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult l
  v4 <- parseValueFromMap @a @t @v4 4 c4 mapResult l
  v5 <- parseValueFromMap @a @t @v5 5 c5 mapResult l
  v6 <- parseValueFromMap @a @t @v6 6 c6 mapResult l
  v7 <- parseValueFromMap @a @t @v7 7 c7 mapResult l
  v8 <- parseValueFromMap @a @t @v8 8 c8 mapResult l
  v9 <- parseValueFromMap @a @t @v9 9 c9 mapResult l
  v10 <- parseValueFromMap @a @t @v10 10 c10 mapResult l
  v11 <- parseValueFromMap @a @t @v11 11 c11 mapResult l
  v12 <- parseValueFromMap @a @t @v12 12 c12 mapResult l
  v13 <- parseValueFromMap @a @t @v13 13 c13 mapResult l
  v14 <- parseValueFromMap @a @t @v14 14 c14 mapResult l
  v15 <- parseValueFromMap @a @t @v15 15 c15 mapResult l
  v16 <- parseValueFromMap @a @t @v16 16 c16 mapResult l
  pure (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16)

-- FIXME should parse Numbers also
parseValueFromMap ::
  forall a t v.
  (ClickhouseValue v) =>
  ColumnNumber ->
  Column a t v ->
  A.KeyMap (Value NotSpecified) ->
  SubQueryLevel ->
  Either String v
parseValueFromMap n column mapResult l = do
  let columnName = showColumn column
  val <- case A.lookup (fromString $ getColumnSynonym n l) mapResult of
    Nothing -> Left $ "Key \"" <> getColumnSynonym n l <> "\" for column \"" <> columnName <> "\" did not found"
    Just val -> pure $ coerce @(Value NotSpecified) @(Value v) val
  either (\err -> Left $ "Failed to parse key \"" <> getColumnSynonym n l <> "\" for column \"" <> columnName <> "\": " <> err) pure $
    getExcept $ fromClickhouseValue @v val

zipColumnsWithSynonyms1 :: Column a t v1 -> SubQueryLevel -> String
zipColumnsWithSynonyms1 c1 = zipColumns [showColumn c1]

zipColumnsWithSynonyms2 :: T2 (Column a t) v1 v2 -> SubQueryLevel -> String
zipColumnsWithSynonyms2 (c1, c2) = zipColumns [showColumn c1, showColumn c2]

zipColumnsWithSynonyms3 :: T3 (Column a t) v1 v2 v3 -> SubQueryLevel -> String
zipColumnsWithSynonyms3 (c1, c2, c3) = zipColumns [showColumn c1, showColumn c2, showColumn c3]

zipColumnsWithSynonyms4 :: T4 (Column a t) v1 v2 v3 v4 -> SubQueryLevel -> String
zipColumnsWithSynonyms4 (c1, c2, c3, c4) = zipColumns [showColumn c1, showColumn c2, showColumn c3, showColumn c4]

zipColumnsWithSynonyms5 :: T5 (Column a t) v1 v2 v3 v4 v5 -> SubQueryLevel -> String
zipColumnsWithSynonyms5 (c1, c2, c3, c4, c5) = zipColumns [showColumn c1, showColumn c2, showColumn c3, showColumn c4, showColumn c5]

zipColumnsWithSynonyms6 :: T6 (Column a t) v1 v2 v3 v4 v5 v6 -> SubQueryLevel -> String
zipColumnsWithSynonyms6 (c1, c2, c3, c4, c5, c6) = zipColumns [showColumn c1, showColumn c2, showColumn c3, showColumn c4, showColumn c5, showColumn c6]

zipColumnsWithSynonyms7 :: T7 (Column a t) v1 v2 v3 v4 v5 v6 v7 -> SubQueryLevel -> String
zipColumnsWithSynonyms7 (c1, c2, c3, c4, c5, c6, c7) = zipColumns [showColumn c1, showColumn c2, showColumn c3, showColumn c4, showColumn c5, showColumn c6, showColumn c7]

zipColumnsWithSynonyms8 :: T8 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 -> SubQueryLevel -> String
zipColumnsWithSynonyms8 (c1, c2, c3, c4, c5, c6, c7, c8) = zipColumns [showColumn c1, showColumn c2, showColumn c3, showColumn c4, showColumn c5, showColumn c6, showColumn c7, showColumn c8]

zipColumnsWithSynonyms9 :: T9 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 -> SubQueryLevel -> String
zipColumnsWithSynonyms9 (c1, c2, c3, c4, c5, c6, c7, c8, c9) = zipColumns [showColumn c1, showColumn c2, showColumn c3, showColumn c4, showColumn c5, showColumn c6, showColumn c7, showColumn c8, showColumn c9]

zipColumnsWithSynonyms10 :: T10 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 -> SubQueryLevel -> String
zipColumnsWithSynonyms10 (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) = zipColumns [showColumn c1, showColumn c2, showColumn c3, showColumn c4, showColumn c5, showColumn c6, showColumn c7, showColumn c8, showColumn c9, showColumn c10]

zipColumnsWithSynonyms11 :: T11 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 -> SubQueryLevel -> String
zipColumnsWithSynonyms11 (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11) = zipColumns [showColumn c1, showColumn c2, showColumn c3, showColumn c4, showColumn c5, showColumn c6, showColumn c7, showColumn c8, showColumn c9, showColumn c10, showColumn c11]

zipColumnsWithSynonyms12 :: T12 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 -> SubQueryLevel -> String
zipColumnsWithSynonyms12 (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) = zipColumns [showColumn c1, showColumn c2, showColumn c3, showColumn c4, showColumn c5, showColumn c6, showColumn c7, showColumn c8, showColumn c9, showColumn c10, showColumn c11, showColumn c12]

zipColumnsWithSynonyms13 :: T13 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 -> SubQueryLevel -> String
zipColumnsWithSynonyms13 (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13) = zipColumns [showColumn c1, showColumn c2, showColumn c3, showColumn c4, showColumn c5, showColumn c6, showColumn c7, showColumn c8, showColumn c9, showColumn c10, showColumn c11, showColumn c12, showColumn c13]

zipColumnsWithSynonyms14 :: T14 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 -> SubQueryLevel -> String
zipColumnsWithSynonyms14 (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14) = zipColumns [showColumn c1, showColumn c2, showColumn c3, showColumn c4, showColumn c5, showColumn c6, showColumn c7, showColumn c8, showColumn c9, showColumn c10, showColumn c11, showColumn c12, showColumn c13, showColumn c14]

zipColumnsWithSynonyms15 :: T15 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 -> SubQueryLevel -> String
zipColumnsWithSynonyms15 (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15) = zipColumns [showColumn c1, showColumn c2, showColumn c3, showColumn c4, showColumn c5, showColumn c6, showColumn c7, showColumn c8, showColumn c9, showColumn c10, showColumn c11, showColumn c12, showColumn c13, showColumn c14, showColumn c15]

zipColumnsWithSynonyms16 :: T16 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 -> SubQueryLevel -> String
zipColumnsWithSynonyms16 (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16) = zipColumns [showColumn c1, showColumn c2, showColumn c3, showColumn c4, showColumn c5, showColumn c6, showColumn c7, showColumn c8, showColumn c9, showColumn c10, showColumn c11, showColumn c12, showColumn c13, showColumn c14, showColumn c15, showColumn c16]

zipColumns :: [String] -> SubQueryLevel -> String
zipColumns columns l = List.intercalate ", " $ zipWith (\n column -> column <> " as " <> getColumnSynonym n l) [1 ..] columns
