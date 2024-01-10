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
    RawQuery (getRawQuery),
  )
where

import qualified Data.List as List
import Data.Typeable (typeRep)
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2.ClickhouseDb
import Kernel.Storage.ClickhouseV2.ClickhouseTable
import Kernel.Storage.ClickhouseV2.ClickhouseValue
import Kernel.Storage.ClickhouseV2.Internal.Types
import Kernel.Utils.JSON (camelToSnakeCase)

newtype RawQuery = RawQuery {getRawQuery :: String}
  deriving newtype (IsString, Semigroup, Monoid)

class ClickhouseQuery expr where
  toClickhouseQuery :: expr -> RawQuery

instance (ClickhouseDb db, ClickhouseTable t) => ClickhouseQuery (Select db t) where
  toClickhouseQuery (Select q) = do
    -- should we add table name modifier?
    let tableName = dropBeforeDot $ camelToSnakeCase . dropTSuffix . show $ typeRep (Proxy @t)
    -- FIXME cols not supported in Select expression
    -- let selectClause = if null cols then "SELECT *" else T.unpack $ "SELECT " <> intercalate ", " cols
    "SELECT *"
      <> " FROM "
      <> fromString tableName
      <> toClickhouseQuery @(Where t) q.whereQ
      <> mkMaybeClause @(Aggregate t) q.aggregateQ
      <> mkMaybeClause @(OrderBy t) q.orderByQ
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
  toClickhouseQuery (Is column term) = toClickhouseQuery @(Column t _) column <> toClickhouseQuery @(Term _) term

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
  toClickhouseQuery (String str) = addQuotes . RawQuery $ str
  toClickhouseQuery Null = "null"

intercalate :: RawQuery -> [RawQuery] -> RawQuery
intercalate x = RawQuery . List.intercalate (getRawQuery x) . (getRawQuery <$>)

addBrackets :: RawQuery -> RawQuery
addBrackets rq = "(" <> rq <> ")"

addQuotes :: RawQuery -> RawQuery
addQuotes rq = "'" <> rq <> "'"

instance ClickhouseQuery Limit where
  toClickhouseQuery (Limit val) = " LIMIT " <> show val

instance ClickhouseQuery Offset where
  toClickhouseQuery (Offset val) = " OFFSET " <> show val

-- FIXME Separate module ClickhouseTable
instance ClickhouseTable t => ClickhouseQuery (OrderBy t) where
  toClickhouseQuery (OrderBy Asc column) = " ORDER BY " <> toClickhouseQuery @(Column t _) column <> " ASC"
  toClickhouseQuery (OrderBy Desc column) = " ORDER BY " <> toClickhouseQuery @(Column t _) column <> " DESC"

instance ClickhouseTable t => ClickhouseQuery (Aggregate t) where
  toClickhouseQuery (GroupByAggregate groupBy) = toClickhouseQuery @(GroupBy t) groupBy

instance ClickhouseTable t => ClickhouseQuery (GroupBy t) where
  toClickhouseQuery (GroupBy column) = " GROUP BY " <> toClickhouseQuery @(Column t _) column

instance ClickhouseTable t => ClickhouseQuery (Column t value) where
  toClickhouseQuery column = fromString $ getFieldModification column.getColumn
