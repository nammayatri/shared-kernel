{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Storage.ClickhouseV2.Internal.ClickhouseQuery
  ( ClickhouseQuery (toClickhouseQuery),
    RawQuery (..),
  )
where

import qualified Data.Char as C
import qualified Data.List as List
import Data.Typeable (typeRep)
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2.ClickhouseDb
import Kernel.Storage.ClickhouseV2.ClickhouseTable
import Kernel.Storage.ClickhouseV2.ClickhouseValue
import Kernel.Storage.ClickhouseV2.Internal.Types
import Kernel.Utils.JSON (camelToSnakeCase)

instance
  ( ClickhouseDb db,
    ClickhouseTable t,
    ClickhouseColumns a cols,
    ClickhouseQuery gr,
    ClickhouseQuery (OrderBy ord),
    ClickhouseQuery (AvailableColumns db t acols)
  ) =>
  ClickhouseQuery (Select a db t cols gr ord acols)
  where
  toClickhouseQuery (Select cols groupBy q) = do
    let selectModifier = case fromMaybe (getSelectModifier (Proxy @t)) q.selectModifierOverrideQ of
          NO_SELECT_MODIFIER -> ""
          SELECT_FINAL_MODIFIER -> " FINAL "
    "SELECT "
      <> RawQuery (showClickhouseColumns @a @cols (Proxy @a) cols q.subQueryLevelQ)
      <> " FROM "
      <> toClickhouseQuery @(AvailableColumns db t acols) q.tableQ
      <> selectModifier
      <> mkMaybeClause @(Where t) q.whereQ
      <> toClickhouseQuery @(GroupBy a gr) groupBy
      <> mkMaybeClause @(OrderBy ord) (q.orderByQ <&> ($ cols))
      <> mkMaybeClause @Limit q.limitQ
      <> mkMaybeClause @Offset q.offsetQ

mkMaybeClause :: forall expr. ClickhouseQuery expr => Maybe expr -> RawQuery
mkMaybeClause = maybe mempty (toClickhouseQuery @expr)

dropBeforeDot :: String -> String
dropBeforeDot str = case dropWhile (/= '.') str of
  [] -> str
  (_ : rest) -> rest

instance ClickhouseTable t => ClickhouseQuery (Where t) where
  toClickhouseQuery (Where clause) = " WHERE " <> addBrackets (toClickhouseQuery @(Clause t) clause)

instance (ClickhouseTable t) => ClickhouseQuery (Clause t) where
  toClickhouseQuery (And clause1 clause2) = addBrackets (toClickhouseQuery @(Clause t) clause1) <> " AND " <> addBrackets (toClickhouseQuery @(Clause t) clause2)
  toClickhouseQuery (Or clause1 clause2) = addBrackets (toClickhouseQuery @(Clause t) clause1) <> " OR " <> addBrackets (toClickhouseQuery @(Clause t) clause2)
  toClickhouseQuery (Not clause) = "NOT " <> addBrackets (toClickhouseQuery @(Clause t) clause)
  toClickhouseQuery (Is column term) = toClickhouseQuery @(Column _ t _) column <> toClickhouseQuery @(Term _) term
  toClickhouseQuery (Val b) = RawQuery $ C.toLower <$> show @String @Bool b

instance ClickhouseValue value => ClickhouseQuery (Term value) where
  toClickhouseQuery (In valList) = " IN " <> (addBrackets . intercalate "," . (valToClickhouseQuery @value <$>) $ toList valList)
  toClickhouseQuery (Eq term) = "=" <> valToClickhouseQuery @value term
  toClickhouseQuery NullTerm = " IS NULL"
  toClickhouseQuery NotNullTerm = " IS NOT NULL"
  toClickhouseQuery (NotEq term) = "!=" <> valToClickhouseQuery @value term
  toClickhouseQuery (GreaterThan term) = ">" <> valToClickhouseQuery @value term
  toClickhouseQuery (GreaterOrEqualThan term) = ">=" <> valToClickhouseQuery @value term
  toClickhouseQuery (LessThan term) = "<" <> valToClickhouseQuery @value term
  toClickhouseQuery (LessOrEqualThan term) = "<=" <> valToClickhouseQuery @value term
  toClickhouseQuery (Like term) = " LIKE " <> valToClickhouseQuery @value term

-- do we need quotes for each datatype?
valToClickhouseQuery :: forall value. ClickhouseValue value => value -> RawQuery
valToClickhouseQuery = toClickhouseQuery @(Value value) . toClickhouseValue @value

instance ClickhouseValue value => ClickhouseQuery (Value value) where
  toClickhouseQuery = RawQuery . valToString

intercalate :: RawQuery -> [RawQuery] -> RawQuery
intercalate x = RawQuery . List.intercalate (getRawQuery x) . (getRawQuery <$>)

addBrackets :: RawQuery -> RawQuery
addBrackets rq = "(" <> rq <> ")"

instance ClickhouseQuery Limit where
  toClickhouseQuery (Limit val) = " LIMIT " <> show val

instance ClickhouseQuery Offset where
  toClickhouseQuery (Offset val) = " OFFSET " <> show val

instance ClickhouseQuery (OrderBy 'NOT_ORDERED) where
  toClickhouseQuery _ = mempty

instance ClickhouseQuery (OrderBy 'ORDERED) where
  toClickhouseQuery (OrderBy Asc column) = " ORDER BY " <> toClickhouseQuery column <> " ASC"
  toClickhouseQuery (OrderBy Desc column) = " ORDER BY " <> toClickhouseQuery column <> " DESC"

instance ClickhouseQuery NotGrouped where
  toClickhouseQuery _ = mempty

instance ClickhouseQuery NoColumns where
  toClickhouseQuery _ = mempty

instance ClickhouseQuery gr => ClickhouseQuery (GroupBy a gr) where
  toClickhouseQuery (GroupBy gr) = " GROUP BY " <> toClickhouseQuery gr
  toClickhouseQuery Aggregate = mempty
  toClickhouseQuery NotGrouped = mempty

instance ClickhouseTable t => ClickhouseQuery (Column a t value) where
  toClickhouseQuery = fromString . showColumn

instance ClickhouseTable t => ClickhouseQuery (T2 (Column a t) v1 v2) where
  toClickhouseQuery (c1, c2) = intercalate ", " [toClickhouseQuery c1, toClickhouseQuery c2]

instance ClickhouseTable t => ClickhouseQuery (T3 (Column a t) v1 v2 v3) where
  toClickhouseQuery (c1, c2, c3) = intercalate ", " [toClickhouseQuery c1, toClickhouseQuery c2, toClickhouseQuery c3]

instance ClickhouseTable t => ClickhouseQuery (T4 (Column a t) v1 v2 v3 v4) where
  toClickhouseQuery (c1, c2, c3, c4) = intercalate ", " [toClickhouseQuery c1, toClickhouseQuery c2, toClickhouseQuery c3, toClickhouseQuery c4]

instance ClickhouseTable t => ClickhouseQuery (T5 (Column a t) v1 v2 v3 v4 v5) where
  toClickhouseQuery (c1, c2, c3, c4, c5) = intercalate ", " [toClickhouseQuery c1, toClickhouseQuery c2, toClickhouseQuery c3, toClickhouseQuery c4, toClickhouseQuery c5]

instance ClickhouseTable t => ClickhouseQuery (T6 (Column a t) v1 v2 v3 v4 v5 v6) where
  toClickhouseQuery (c1, c2, c3, c4, c5, c6) = intercalate ", " [toClickhouseQuery c1, toClickhouseQuery c2, toClickhouseQuery c3, toClickhouseQuery c4, toClickhouseQuery c5, toClickhouseQuery c6]

instance ClickhouseTable t => ClickhouseQuery (T7 (Column a t) v1 v2 v3 v4 v5 v6 v7) where
  toClickhouseQuery (c1, c2, c3, c4, c5, c6, c7) = intercalate ", " [toClickhouseQuery c1, toClickhouseQuery c2, toClickhouseQuery c3, toClickhouseQuery c4, toClickhouseQuery c5, toClickhouseQuery c6, toClickhouseQuery c7]

instance ClickhouseTable t => ClickhouseQuery (T8 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8) where
  toClickhouseQuery (c1, c2, c3, c4, c5, c6, c7, c8) = intercalate ", " [toClickhouseQuery c1, toClickhouseQuery c2, toClickhouseQuery c3, toClickhouseQuery c4, toClickhouseQuery c5, toClickhouseQuery c6, toClickhouseQuery c7, toClickhouseQuery c8]

instance ClickhouseTable t => ClickhouseQuery (T9 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9) where
  toClickhouseQuery (c1, c2, c3, c4, c5, c6, c7, c8, c9) = intercalate ", " [toClickhouseQuery c1, toClickhouseQuery c2, toClickhouseQuery c3, toClickhouseQuery c4, toClickhouseQuery c5, toClickhouseQuery c6, toClickhouseQuery c7, toClickhouseQuery c8, toClickhouseQuery c9]

instance ClickhouseTable t => ClickhouseQuery (T10 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10) where
  toClickhouseQuery (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) = intercalate ", " [toClickhouseQuery c1, toClickhouseQuery c2, toClickhouseQuery c3, toClickhouseQuery c4, toClickhouseQuery c5, toClickhouseQuery c6, toClickhouseQuery c7, toClickhouseQuery c8, toClickhouseQuery c9, toClickhouseQuery c10]

instance ClickhouseTable t => ClickhouseQuery (T11 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11) where
  toClickhouseQuery (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11) = intercalate ", " [toClickhouseQuery c1, toClickhouseQuery c2, toClickhouseQuery c3, toClickhouseQuery c4, toClickhouseQuery c5, toClickhouseQuery c6, toClickhouseQuery c7, toClickhouseQuery c8, toClickhouseQuery c9, toClickhouseQuery c10, toClickhouseQuery c11]

instance ClickhouseTable t => ClickhouseQuery (T12 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12) where
  toClickhouseQuery (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) = intercalate ", " [toClickhouseQuery c1, toClickhouseQuery c2, toClickhouseQuery c3, toClickhouseQuery c4, toClickhouseQuery c5, toClickhouseQuery c6, toClickhouseQuery c7, toClickhouseQuery c8, toClickhouseQuery c9, toClickhouseQuery c10, toClickhouseQuery c11, toClickhouseQuery c12]

instance ClickhouseTable t => ClickhouseQuery (T13 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13) where
  toClickhouseQuery (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13) = intercalate ", " [toClickhouseQuery c1, toClickhouseQuery c2, toClickhouseQuery c3, toClickhouseQuery c4, toClickhouseQuery c5, toClickhouseQuery c6, toClickhouseQuery c7, toClickhouseQuery c8, toClickhouseQuery c9, toClickhouseQuery c10, toClickhouseQuery c11, toClickhouseQuery c12, toClickhouseQuery c13]

instance ClickhouseTable t => ClickhouseQuery (T14 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14) where
  toClickhouseQuery (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14) = intercalate ", " [toClickhouseQuery c1, toClickhouseQuery c2, toClickhouseQuery c3, toClickhouseQuery c4, toClickhouseQuery c5, toClickhouseQuery c6, toClickhouseQuery c7, toClickhouseQuery c8, toClickhouseQuery c9, toClickhouseQuery c10, toClickhouseQuery c11, toClickhouseQuery c12, toClickhouseQuery c13, toClickhouseQuery c14]

instance ClickhouseTable t => ClickhouseQuery (T15 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15) where
  toClickhouseQuery (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15) = intercalate ", " [toClickhouseQuery c1, toClickhouseQuery c2, toClickhouseQuery c3, toClickhouseQuery c4, toClickhouseQuery c5, toClickhouseQuery c6, toClickhouseQuery c7, toClickhouseQuery c8, toClickhouseQuery c9, toClickhouseQuery c10, toClickhouseQuery c11, toClickhouseQuery c12, toClickhouseQuery c13, toClickhouseQuery c14, toClickhouseQuery c15]

instance ClickhouseTable t => ClickhouseQuery (T16 (Column a t) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16) where
  toClickhouseQuery (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16) = intercalate ", " [toClickhouseQuery c1, toClickhouseQuery c2, toClickhouseQuery c3, toClickhouseQuery c4, toClickhouseQuery c5, toClickhouseQuery c6, toClickhouseQuery c7, toClickhouseQuery c8, toClickhouseQuery c9, toClickhouseQuery c10, toClickhouseQuery c11, toClickhouseQuery c12, toClickhouseQuery c13, toClickhouseQuery c14, toClickhouseQuery c15, toClickhouseQuery c16]

instance ClickhouseTable t => ClickhouseQuery (AvailableAllColumns db t) where
  toClickhouseQuery _ = do
    let tableName = dropBeforeDot $ camelToSnakeCase . dropTSuffix . show $ typeRep (Proxy @t)
    fromString tableName
    where
      dropTSuffix str = take (length str - 1) str

instance ClickhouseQuery (AvailableSubSelectColumns db t subcols) where
  toClickhouseQuery (AvailableColumns (SubSelectColumns subSelect)) = addBrackets . toClickhouseQuery $ subSelect

instance (ClickhouseTable t, ClickhouseColumns 'NOT_AGG cols, ClickhouseInsertValues values) => ClickhouseQuery (Insert db t cols values) where
  toClickhouseQuery (Insert cols values) =
    let tableName = dropBeforeDot $ camelToSnakeCase . dropTSuffix . show $ typeRep (Proxy @t)
        colsStr = RawQuery $ showClickhouseColumns @'NOT_AGG @cols (Proxy @'NOT_AGG) cols 0
        valsStr = toInsertValues values
     in "INSERT INTO " <> fromString tableName <> " (" <> colsStr <> ") VALUES (" <> valsStr <> ")"
    where
      dropTSuffix str = take (length str - 1) str

class ClickhouseInsertValues values where
  toInsertValues :: values -> RawQuery

instance ClickhouseValue v => ClickhouseInsertValues v where
  toInsertValues = valToClickhouseQuery

instance (ClickhouseValue v1, ClickhouseValue v2) => ClickhouseInsertValues (v1, v2) where
  toInsertValues (v1, v2) = intercalate ", " [valToClickhouseQuery v1, valToClickhouseQuery v2]

instance (ClickhouseValue v1, ClickhouseValue v2, ClickhouseValue v3) => ClickhouseInsertValues (v1, v2, v3) where
  toInsertValues (v1, v2, v3) = intercalate ", " [valToClickhouseQuery v1, valToClickhouseQuery v2, valToClickhouseQuery v3]

instance (ClickhouseValue v1, ClickhouseValue v2, ClickhouseValue v3, ClickhouseValue v4) => ClickhouseInsertValues (v1, v2, v3, v4) where
  toInsertValues (v1, v2, v3, v4) = intercalate ", " [valToClickhouseQuery v1, valToClickhouseQuery v2, valToClickhouseQuery v3, valToClickhouseQuery v4]

instance (ClickhouseValue v1, ClickhouseValue v2, ClickhouseValue v3, ClickhouseValue v4, ClickhouseValue v5) => ClickhouseInsertValues (v1, v2, v3, v4, v5) where
  toInsertValues (v1, v2, v3, v4, v5) = intercalate ", " [valToClickhouseQuery v1, valToClickhouseQuery v2, valToClickhouseQuery v3, valToClickhouseQuery v4, valToClickhouseQuery v5]

instance (ClickhouseValue v1, ClickhouseValue v2, ClickhouseValue v3, ClickhouseValue v4, ClickhouseValue v5, ClickhouseValue v6) => ClickhouseInsertValues (v1, v2, v3, v4, v5, v6) where
  toInsertValues (v1, v2, v3, v4, v5, v6) = intercalate ", " [valToClickhouseQuery v1, valToClickhouseQuery v2, valToClickhouseQuery v3, valToClickhouseQuery v4, valToClickhouseQuery v5, valToClickhouseQuery v6]

instance (ClickhouseValue v1, ClickhouseValue v2, ClickhouseValue v3, ClickhouseValue v4, ClickhouseValue v5, ClickhouseValue v6, ClickhouseValue v7) => ClickhouseInsertValues (v1, v2, v3, v4, v5, v6, v7) where
  toInsertValues (v1, v2, v3, v4, v5, v6, v7) = intercalate ", " [valToClickhouseQuery v1, valToClickhouseQuery v2, valToClickhouseQuery v3, valToClickhouseQuery v4, valToClickhouseQuery v5, valToClickhouseQuery v6, valToClickhouseQuery v7]

instance (ClickhouseValue v1, ClickhouseValue v2, ClickhouseValue v3, ClickhouseValue v4, ClickhouseValue v5, ClickhouseValue v6, ClickhouseValue v7, ClickhouseValue v8) => ClickhouseInsertValues (v1, v2, v3, v4, v5, v6, v7, v8) where
  toInsertValues (v1, v2, v3, v4, v5, v6, v7, v8) = intercalate ", " [valToClickhouseQuery v1, valToClickhouseQuery v2, valToClickhouseQuery v3, valToClickhouseQuery v4, valToClickhouseQuery v5, valToClickhouseQuery v6, valToClickhouseQuery v7, valToClickhouseQuery v8]

instance (ClickhouseValue v1, ClickhouseValue v2, ClickhouseValue v3, ClickhouseValue v4, ClickhouseValue v5, ClickhouseValue v6, ClickhouseValue v7, ClickhouseValue v8, ClickhouseValue v9) => ClickhouseInsertValues (v1, v2, v3, v4, v5, v6, v7, v8, v9) where
  toInsertValues (v1, v2, v3, v4, v5, v6, v7, v8, v9) = intercalate ", " [valToClickhouseQuery v1, valToClickhouseQuery v2, valToClickhouseQuery v3, valToClickhouseQuery v4, valToClickhouseQuery v5, valToClickhouseQuery v6, valToClickhouseQuery v7, valToClickhouseQuery v8, valToClickhouseQuery v9]

instance (ClickhouseValue v1, ClickhouseValue v2, ClickhouseValue v3, ClickhouseValue v4, ClickhouseValue v5, ClickhouseValue v6, ClickhouseValue v7, ClickhouseValue v8, ClickhouseValue v9, ClickhouseValue v10) => ClickhouseInsertValues (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) where
  toInsertValues (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) = intercalate ", " [valToClickhouseQuery v1, valToClickhouseQuery v2, valToClickhouseQuery v3, valToClickhouseQuery v4, valToClickhouseQuery v5, valToClickhouseQuery v6, valToClickhouseQuery v7, valToClickhouseQuery v8, valToClickhouseQuery v9, valToClickhouseQuery v10]

instance (ClickhouseValue v1, ClickhouseValue v2, ClickhouseValue v3, ClickhouseValue v4, ClickhouseValue v5, ClickhouseValue v6, ClickhouseValue v7, ClickhouseValue v8, ClickhouseValue v9, ClickhouseValue v10, ClickhouseValue v11) => ClickhouseInsertValues (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) where
  toInsertValues (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) = intercalate ", " [valToClickhouseQuery v1, valToClickhouseQuery v2, valToClickhouseQuery v3, valToClickhouseQuery v4, valToClickhouseQuery v5, valToClickhouseQuery v6, valToClickhouseQuery v7, valToClickhouseQuery v8, valToClickhouseQuery v9, valToClickhouseQuery v10, valToClickhouseQuery v11]

instance (ClickhouseValue v1, ClickhouseValue v2, ClickhouseValue v3, ClickhouseValue v4, ClickhouseValue v5, ClickhouseValue v6, ClickhouseValue v7, ClickhouseValue v8, ClickhouseValue v9, ClickhouseValue v10, ClickhouseValue v11, ClickhouseValue v12) => ClickhouseInsertValues (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) where
  toInsertValues (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) = intercalate ", " [valToClickhouseQuery v1, valToClickhouseQuery v2, valToClickhouseQuery v3, valToClickhouseQuery v4, valToClickhouseQuery v5, valToClickhouseQuery v6, valToClickhouseQuery v7, valToClickhouseQuery v8, valToClickhouseQuery v9, valToClickhouseQuery v10, valToClickhouseQuery v11, valToClickhouseQuery v12]

instance (ClickhouseValue v1, ClickhouseValue v2, ClickhouseValue v3, ClickhouseValue v4, ClickhouseValue v5, ClickhouseValue v6, ClickhouseValue v7, ClickhouseValue v8, ClickhouseValue v9, ClickhouseValue v10, ClickhouseValue v11, ClickhouseValue v12, ClickhouseValue v13) => ClickhouseInsertValues (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) where
  toInsertValues (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) = intercalate ", " [valToClickhouseQuery v1, valToClickhouseQuery v2, valToClickhouseQuery v3, valToClickhouseQuery v4, valToClickhouseQuery v5, valToClickhouseQuery v6, valToClickhouseQuery v7, valToClickhouseQuery v8, valToClickhouseQuery v9, valToClickhouseQuery v10, valToClickhouseQuery v11, valToClickhouseQuery v12, valToClickhouseQuery v13]

instance (ClickhouseValue v1, ClickhouseValue v2, ClickhouseValue v3, ClickhouseValue v4, ClickhouseValue v5, ClickhouseValue v6, ClickhouseValue v7, ClickhouseValue v8, ClickhouseValue v9, ClickhouseValue v10, ClickhouseValue v11, ClickhouseValue v12, ClickhouseValue v13, ClickhouseValue v14) => ClickhouseInsertValues (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) where
  toInsertValues (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) = intercalate ", " [valToClickhouseQuery v1, valToClickhouseQuery v2, valToClickhouseQuery v3, valToClickhouseQuery v4, valToClickhouseQuery v5, valToClickhouseQuery v6, valToClickhouseQuery v7, valToClickhouseQuery v8, valToClickhouseQuery v9, valToClickhouseQuery v10, valToClickhouseQuery v11, valToClickhouseQuery v12, valToClickhouseQuery v13, valToClickhouseQuery v14]

instance (ClickhouseValue v1, ClickhouseValue v2, ClickhouseValue v3, ClickhouseValue v4, ClickhouseValue v5, ClickhouseValue v6, ClickhouseValue v7, ClickhouseValue v8, ClickhouseValue v9, ClickhouseValue v10, ClickhouseValue v11, ClickhouseValue v12, ClickhouseValue v13, ClickhouseValue v14, ClickhouseValue v15) => ClickhouseInsertValues (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) where
  toInsertValues (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) = intercalate ", " [valToClickhouseQuery v1, valToClickhouseQuery v2, valToClickhouseQuery v3, valToClickhouseQuery v4, valToClickhouseQuery v5, valToClickhouseQuery v6, valToClickhouseQuery v7, valToClickhouseQuery v8, valToClickhouseQuery v9, valToClickhouseQuery v10, valToClickhouseQuery v11, valToClickhouseQuery v12, valToClickhouseQuery v13, valToClickhouseQuery v14, valToClickhouseQuery v15]

instance (ClickhouseValue v1, ClickhouseValue v2, ClickhouseValue v3, ClickhouseValue v4, ClickhouseValue v5, ClickhouseValue v6, ClickhouseValue v7, ClickhouseValue v8, ClickhouseValue v9, ClickhouseValue v10, ClickhouseValue v11, ClickhouseValue v12, ClickhouseValue v13, ClickhouseValue v14, ClickhouseValue v15, ClickhouseValue v16) => ClickhouseInsertValues (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16) where
  toInsertValues (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16) = intercalate ", " [valToClickhouseQuery v1, valToClickhouseQuery v2, valToClickhouseQuery v3, valToClickhouseQuery v4, valToClickhouseQuery v5, valToClickhouseQuery v6, valToClickhouseQuery v7, valToClickhouseQuery v8, valToClickhouseQuery v9, valToClickhouseQuery v10, valToClickhouseQuery v11, valToClickhouseQuery v12, valToClickhouseQuery v13, valToClickhouseQuery v14, valToClickhouseQuery v15, valToClickhouseQuery v16]
