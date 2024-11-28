{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
-- TODO remove
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

-- data Select a db table cols gr ord where
--   Select :: (ClickhouseTable table, ClickhouseColumns a cols) => cols -> GroupBy a gr -> Q db table cols ord -> Select a db table cols gr ord

-- class ClickhouseColumns (a :: IsAggregated) cols where
--   type ColumnsType a cols
--   showClickhouseColumns :: Proxy a -> cols -> String
--   parseColumns :: Proxy a -> cols -> A.Value -> Either String (ColumnsType a cols)

instance (FromJSON (ColumnsType 'NOT_AGG (AvailableColumns db t (AllColumns db t))), ClickhouseTable t) => ClickhouseColumns 'NOT_AGG (AvailableColumns db t (AllColumns db t)) where
  type ColumnsType 'NOT_AGG (AvailableColumns db t (AllColumns db t)) = t Identity
  showClickhouseColumns _ _ = "*"
  parseColumns _ _ = eitherResult . A.fromJSON

-- instance (FromJSON (ColumnsType 'NOT_AGG (Columns 'NOT_AGG t)), ClickhouseTable t) => ClickhouseColumns 'NOT_AGG (AvailableColumns 'SUB_SELECT db t) where
--   type ColumnsType 'NOT_AGG (AvailableColumns 'SUB_SELECT db t) = t Identity
--   showClickhouseColumns _ _ = "*"
--   parseColumns _ _ = eitherResult . A.fromJSON

-- ClickhouseColumns 'NOT_AGG (AvailableColumns db table)

-- instance (FromJSON (ColumnsType 'NOT_AGG (Columns 'NOT_AGG t)), ClickhouseTable t) => ClickhouseColumns 'NOT_AGG (AvailableColumns db t) where

-- type ColumnsType 'NOT_AGG (Columns 'NOT_AGG t) = t Identity
-- showClickhouseColumns _ _ = "*"
-- parseColumns _ _ = eitherResult . A.fromJSON

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

-- we need to create map of values with different types
data NotSpecified

parseColumns1 ::
  forall a t v1.
  ClickhouseValue v1 =>
  Column a t v1 ->
  A.Value ->
  Either String v1
parseColumns1 c1 json = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  parseValueFromMap @a @t @v1 1 c1 mapResult

parseColumns2 ::
  forall a t v1 v2.
  (C2 ClickhouseValue v1 v2) =>
  T2 (Column a t) v1 v2 ->
  A.Value ->
  Either String (v1, v2)
parseColumns2 (c1, c2) json = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult
  pure (v1, v2)

parseColumns3 ::
  forall a t v1 v2 v3.
  (C3 ClickhouseValue v1 v2 v3) =>
  T3 (Column a t) v1 v2 v3 ->
  A.Value ->
  Either String (v1, v2, v3)
parseColumns3 (c1, c2, c3) json = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult
  pure (v1, v2, v3)

parseColumns4 ::
  forall a t v1 v2 v3 v4.
  (C4 ClickhouseValue v1 v2 v3 v4) =>
  T4 (Column a t) v1 v2 v3 v4 ->
  A.Value ->
  Either String (v1, v2, v3, v4)
parseColumns4 (c1, c2, c3, c4) json = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult
  v4 <- parseValueFromMap @a @t @v4 4 c4 mapResult
  pure (v1, v2, v3, v4)

parseColumns5 ::
  forall a t v1 v2 v3 v4 v5.
  (C5 ClickhouseValue v1 v2 v3 v4 v5) =>
  T5 (Column a t) v1 v2 v3 v4 v5 ->
  A.Value ->
  Either String (v1, v2, v3, v4, v5)
parseColumns5 (c1, c2, c3, c4, c5) json = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult
  v4 <- parseValueFromMap @a @t @v4 4 c4 mapResult
  v5 <- parseValueFromMap @a @t @v5 5 c5 mapResult
  pure (v1, v2, v3, v4, v5)

parseColumns6 ::
  forall a t v1 v2 v3 v4 v5 v6.
  (C6 ClickhouseValue v1 v2 v3 v4 v5 v6) =>
  T6 (Column a t) v1 v2 v3 v4 v5 v6 ->
  A.Value ->
  Either String (v1, v2, v3, v4, v5, v6)
parseColumns6 (c1, c2, c3, c4, c5, c6) json = do
  mapResult <- eitherResult . A.fromJSON @(A.KeyMap (Value NotSpecified)) $ json
  v1 <- parseValueFromMap @a @t @v1 1 c1 mapResult
  v2 <- parseValueFromMap @a @t @v2 2 c2 mapResult
  v3 <- parseValueFromMap @a @t @v3 3 c3 mapResult
  v4 <- parseValueFromMap @a @t @v4 4 c4 mapResult
  v5 <- parseValueFromMap @a @t @v5 5 c5 mapResult
  v6 <- parseValueFromMap @a @t @v6 6 c6 mapResult
  pure (v1, v2, v3, v4, v5, v6)

-- FIXME should parse Numbers also
parseValueFromMap ::
  forall a t v.
  (ClickhouseValue v) =>
  Int ->
  Column a t v ->
  A.KeyMap (Value NotSpecified) ->
  Either String v
parseValueFromMap n column mapResult = do
  let columnName = showColumn column
  val <- case A.lookup (fromString $ getColumnSynonym n) mapResult of
    Nothing -> Left $ "Key \"" <> getColumnSynonym n <> "\" for column \"" <> columnName <> "\" did not found"
    Just val -> pure $ coerce @(Value NotSpecified) @(Value v) val
  either (\err -> Left $ "Failed to parse key \"" <> getColumnSynonym n <> "\" for column \"" <> columnName <> "\": " <> err) pure $
    getExcept $ fromClickhouseValue @v val

zipColumnsWithSynonyms1 :: Column a t v1 -> String
zipColumnsWithSynonyms1 c1 = zipColumns [showColumn c1]

zipColumnsWithSynonyms2 :: T2 (Column a t) v1 v2 -> String
zipColumnsWithSynonyms2 (c1, c2) = zipColumns [showColumn c1, showColumn c2]

zipColumnsWithSynonyms3 :: T3 (Column a t) v1 v2 v3 -> String
zipColumnsWithSynonyms3 (c1, c2, c3) = zipColumns [showColumn c1, showColumn c2, showColumn c3]

zipColumnsWithSynonyms4 :: T4 (Column a t) v1 v2 v3 v4 -> String
zipColumnsWithSynonyms4 (c1, c2, c3, c4) = zipColumns [showColumn c1, showColumn c2, showColumn c3, showColumn c4]

zipColumnsWithSynonyms5 :: T5 (Column a t) v1 v2 v3 v4 v5 -> String
zipColumnsWithSynonyms5 (c1, c2, c3, c4, c5) = zipColumns [showColumn c1, showColumn c2, showColumn c3, showColumn c4, showColumn c5]

zipColumnsWithSynonyms6 :: T6 (Column a t) v1 v2 v3 v4 v5 v6 -> String
zipColumnsWithSynonyms6 (c1, c2, c3, c4, c5, c6) = zipColumns [showColumn c1, showColumn c2, showColumn c3, showColumn c4, showColumn c5, showColumn c6]

zipColumns :: [String] -> String
zipColumns columns = List.intercalate ", " $ zipWith (\n column -> column <> " as " <> getColumnSynonym n) [1 ..] columns

getColumnSynonym :: Int -> String
getColumnSynonym n = "res" <> show n
