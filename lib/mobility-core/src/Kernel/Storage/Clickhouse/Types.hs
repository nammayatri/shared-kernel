module Kernel.Storage.Clickhouse.Types where

import Kernel.Prelude

type ChCol a b = a -> b

data CHExpr a = CHExpr a (a -> a -> String) a

instance Eq (CHExpr a) where
  (==) (CHExpr a fn b) (CHExpr c fn2 d) = fn a b == fn2 c d

data ClickhouseExpr = Nil | ExprStr String deriving (Eq)

class ToClickhouseQuery a where
  toClickhouseQuery :: a -> String

instance ToClickhouseQuery ClickhouseExpr where
  toClickhouseQuery expr = "(" <> go expr
    where
      go :: ClickhouseExpr -> String
      go e =
        case e of
          Nil -> ")"
          ExprStr str -> str <> ")"

data Order = Asc String | Desc String

instance ToClickhouseQuery Order where
  toClickhouseQuery (Asc colName) = " ORDER BY " <> colName <> " ASC"
  toClickhouseQuery (Desc colName) = " ORDER BY " <> colName <> " DESC"

newtype Offset = Offset Int

instance ToClickhouseQuery Offset where
  toClickhouseQuery (Offset val) = " OFFSET " <> show val

newtype Limit = Limit Int

instance ToClickhouseQuery Limit where
  toClickhouseQuery (Limit val) = " LIMIT " <> show val

dropBeforeDot :: String -> String
dropBeforeDot str = case dropWhile (/= '.') str of
  [] -> str
  (_ : rest) -> rest
