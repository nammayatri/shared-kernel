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
import Kernel.Storage.ClickhouseV2.Internal.ClickhouseColumns
import Kernel.Storage.ClickhouseV2.Internal.Types
import Kernel.Utils.JSON (camelToSnakeCase)

newtype RawQuery = RawQuery {getRawQuery :: String}
  deriving newtype (IsString, Semigroup, Monoid)

class ClickhouseQuery expr where
  toClickhouseQuery :: expr -> RawQuery

instance (ClickhouseDb db, ClickhouseTable t, ClickhouseColumns a cols, ClickhouseQuery gr, ClickhouseQuery ord) => ClickhouseQuery (Select a db t cols gr ord) where
  toClickhouseQuery (Select cols groupBy q) = do
    -- should we add table name modifier?
    let tableName = dropBeforeDot $ camelToSnakeCase . dropTSuffix . show $ typeRep (Proxy @t)
    "SELECT "
      <> RawQuery (showClickhouseColumns @a @cols (Proxy @a) cols)
      <> " FROM "
      <> fromString tableName
      <> toClickhouseQuery @(Where t) (q.whereQ cols)
      <> toClickhouseQuery @(GroupBy a gr) groupBy
      <> mkMaybeClause @(OrderBy ord) (q.orderByQ <&> ($ cols))
      <> mkMaybeClause @Limit q.limitQ
      <> mkMaybeClause @Offset q.offsetQ
    where
      dropTSuffix str = take (length str - 1) str

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
  toClickhouseQuery (In valList) = " IN " <> (addBrackets . intercalate "," . (valToClickhouseQuery @value <$>) $ valList)
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

instance ClickhouseQuery NotOrdered where
  toClickhouseQuery _ = mempty

instance ClickhouseQuery ord => ClickhouseQuery (OrderBy ord) where
  toClickhouseQuery (OrderBy Asc column) = " ORDER BY " <> toClickhouseQuery @ord column <> " ASC"
  toClickhouseQuery (OrderBy Desc column) = " ORDER BY " <> toClickhouseQuery @ord column <> " DESC"

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
