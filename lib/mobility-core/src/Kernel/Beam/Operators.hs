{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kernel.Beam.Operators
  ( (+.),
    (-.),
    (*.),
    extractSqlSyntax,
  )
where

import Data.ByteString (ByteString)
import Data.Semigroup ((<>))
import qualified Database.Beam as B
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgExpressionSyntax (..), emit)
import qualified Database.Beam.Query as BQ
import Database.Beam.Query.Internal (TablePrefix)
import Prelude (($))

infixl 6 +.

(+.) :: B.QGenExpr ctx Postgres s a -> B.QGenExpr ctx Postgres s a -> B.QGenExpr ctx Postgres s a
(+.) = binaryOp "+"

infixl 6 -.

(-.) :: B.QGenExpr ctx Postgres s a -> B.QGenExpr ctx Postgres s a -> B.QGenExpr ctx Postgres s a
(-.) = binaryOp "-"

infixl 7 *.

(*.) :: B.QGenExpr ctx Postgres s a -> B.QGenExpr ctx Postgres s a -> B.QGenExpr ctx Postgres s a
(*.) = binaryOp "*"

binaryOp :: ByteString -> B.QGenExpr ctx Postgres s a -> B.QGenExpr ctx Postgres s a -> B.QGenExpr ctx Postgres s a
binaryOp op left right = BQ.QExpr $ \tablePrefix ->
  let PgExpressionSyntax leftSyntax = extractSqlSyntax tablePrefix left
      PgExpressionSyntax rightSyntax = extractSqlSyntax tablePrefix right
   in PgExpressionSyntax $ emit "(" <> leftSyntax <> emit (" " <> op <> " ") <> rightSyntax <> emit ")"

extractSqlSyntax :: TablePrefix -> B.QGenExpr ctx Postgres s a -> PgExpressionSyntax
extractSqlSyntax tablePrefix (BQ.QExpr f) = f tablePrefix
