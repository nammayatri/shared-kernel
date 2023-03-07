{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Storage.Clickhouse.Operators where

import Data.List (intercalate)
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Types

type ValueToExprStr = String -> String -> ClickhouseExpr

(=.=) :: ValueToExprStr -- equal to operator :: usage ("columnName" =.= "expectedValue")
(=!=) :: ValueToExprStr -- not equal to operator :: usage ("columnName" =!= "unexpectedValue")
(<<.) :: ValueToExprStr -- less than operator :: usage ("columnName" <<. "highestValue")
(>>.) :: ValueToExprStr -- greater than operator :: usage ("columnName" >>. "lowestValue")

infixl 6 <<., >>., =.=, =!=

(=.=) col val = ExprStr (col <> "=" <> "'" <> val <> "'")

(=!=) col val = ExprStr (col <> "!" <> val)

(<<.) col val = ExprStr (col <> "<" <> val)

(>>.) col val = ExprStr (col <> ">" <> val)

__like :: ValueToExprStr
__like col val = ExprStr (col <> " LIKE " <> val)

instance ToClickhouseQuery [String] where
  toClickhouseQuery ls = "(" <> intercalate "," ls <> ")"

__in :: String -> [String] -> ClickhouseExpr
__in col valList = ExprStr (col <> " IN " <> toClickhouseQuery valList)

type ExprsToExprStr = ClickhouseExpr -> ClickhouseExpr -> ClickhouseExpr

(|.|) :: ExprsToExprStr
(&.&) :: ExprsToExprStr

infixr 8 |.|, &.&

(|.|) a b = ExprStr (toClickhouseQuery a <> " OR " <> toClickhouseQuery b)

(&.&) a b = ExprStr (toClickhouseQuery a <> " AND " <> toClickhouseQuery b)
