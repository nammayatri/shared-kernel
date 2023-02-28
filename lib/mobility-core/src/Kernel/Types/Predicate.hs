{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Predicate
  ( module Kernel.Types.Predicate,
    module Kleene,
    (\/),
  )
where

import Algebra.Lattice
import qualified Data.Foldable as F
import Data.Ix (inRange)
import Data.List (nub)
import qualified Data.Text as T
import EulerHS.Prelude
import GHC.Records.Extra
import GHC.TypeLits (KnownSymbol, symbolVal)
import Kleene
import qualified Kleene.DFA as KDFA
import Kleene.Internal.Pretty (pretty)

type PredShow p = p -> Text -> Text

type PredFun p a = p -> a -> Bool

class Predicate a p where
  pFun :: PredFun p a

class ShowablePredicate p where
  pShow :: PredShow p

type Regex = RE Char

anyOf :: [Char] -> Regex
anyOf = joins . map (fromString . (: []))

instance Predicate String Regex where
  pFun = match . KDFA.fromRE

instance Predicate Text Regex where
  pFun re a = match (KDFA.fromRE re) (T.unpack a)

instance ShowablePredicate Regex where
  pShow regex var = var <> " matches regex /" <> T.pack (pretty regex) <> "/"

data And p1 p2 = And p1 p2

instance (Predicate x p1, Predicate x p2) => Predicate x (And p1 p2) where
  pFun (And p1 p2) = liftBinPredFun (&&) p1 p2

instance (ShowablePredicate p1, ShowablePredicate p2) => ShowablePredicate (And p1 p2) where
  pShow (And p1 p2) = liftBinPredShow "and" p1 p2

data Or p1 p2 = Or p1 p2

instance (Predicate x p1, Predicate x p2) => Predicate x (Or p1 p2) where
  pFun (Or p1 p2) = liftBinPredFun (||) p1 p2

instance (ShowablePredicate p1, ShowablePredicate p2) => ShowablePredicate (Or p1 p2) where
  pShow (Or p1 p2) = liftBinPredShow "or" p1 p2

newtype Not p = Not p

instance (Predicate x p) => Predicate x (Not p) where
  pFun (Not p) = not . pFun p

instance (ShowablePredicate p) => ShowablePredicate (Not p) where
  pShow (Not p) = liftPredShow "not" p

newtype InMaybe p = InMaybe p

instance (Predicate x p) => Predicate (Maybe x) (InMaybe p) where
  pFun (InMaybe p) = maybe True (pFun p)

instance (ShowablePredicate p) => ShowablePredicate (InMaybe p) where
  pShow (InMaybe p) = liftPredShow "ifNotNull" p

newtype ExactLength = ExactLength Int

instance ShowablePredicate ExactLength where
  pShow (ExactLength len) name = showFunction "length" name <> " == " <> show len

instance F.Foldable l => Predicate (l a) ExactLength where
  pFun (ExactLength len) l = F.length l == len

instance Predicate Text ExactLength where
  pFun (ExactLength len) l = T.length l == len

data LengthInRange = LengthInRange Int Int

instance ShowablePredicate LengthInRange where
  pShow (LengthInRange a b) name = show a <> " <= " <> showFunction "length" name <> " <= " <> show b

instance F.Foldable l => Predicate (l a) LengthInRange where
  pFun (LengthInRange a b) l = inRange (a, b) $ F.length l

instance Predicate Text LengthInRange where
  pFun (LengthInRange a b) l = inRange (a, b) $ T.length l

newtype MinLength = MinLength Int

instance ShowablePredicate MinLength where
  pShow (MinLength m) name = showFunction "length" name <> " >= " <> show m

instance F.Foldable l => Predicate (l a) MinLength where
  pFun (MinLength m) l = F.length l >= m

instance Predicate Text MinLength where
  pFun (MinLength m) l = T.length l >= m

newtype MaxLength = MaxLength Int

instance ShowablePredicate MaxLength where
  pShow (MaxLength m) name = showFunction "length" name <> " <= " <> show m

instance F.Foldable l => Predicate (l a) MaxLength where
  pFun (MaxLength m) l = F.length l <= m

instance Predicate Text MaxLength where
  pFun (MaxLength m) l = T.length l <= m

data NotEmpty = NotEmpty

instance ShowablePredicate NotEmpty where
  pShow NotEmpty name = name <> " is not empty"

instance F.Foldable l => Predicate (l a) NotEmpty where
  pFun NotEmpty l = not (F.null l)

instance Predicate Text NotEmpty where
  pFun NotEmpty l = not (T.null l)

data InRange n = InRange n n

instance Show n => ShowablePredicate (InRange n) where
  pShow (InRange a b) name = show a <> " <= " <> name <> " <= " <> show b

instance Ord n => Predicate n (InRange n) where
  pFun (InRange a b) n = a <= n && n <= b

newtype Min n = Min n

instance Show n => ShowablePredicate (Min n) where
  pShow (Min m) name = name <> " >= " <> show m

instance Ord n => Predicate n (Min n) where
  pFun (Min m) = (>= m)

newtype Max n = Max n

instance Show n => ShowablePredicate (Max n) where
  pShow (Max m) name = name <> " <= " <> show m

instance Ord n => Predicate n (Max n) where
  pFun (Max m) = (<= m)

data UniqueField f = UniqueField

instance KnownSymbol f => ShowablePredicate (UniqueField f) where
  pShow _ name = T.pack (symbolVal (Proxy @f)) <> " field must be unique for each element of the " <> name <> " list."

instance (Container c, Ord a, HasField n (Element c) a) => Predicate c (UniqueField n) where
  pFun _ list = length list == (length . nub . map (getField @n) $ toList list)

liftPredShow :: ShowablePredicate p => Text -> p -> Text -> Text
liftPredShow fname p text = showFunction fname (pShow p text)

showFunction :: Text -> Text -> Text
showFunction fname arg = fname <> parenthesized arg

liftBinPredShow ::
  (ShowablePredicate p1, ShowablePredicate p2) =>
  Text ->
  p1 ->
  p2 ->
  Text ->
  Text
liftBinPredShow str a b text = parenthesized $ pShow a text <> " " <> str <> " " <> pShow b text

liftBinPredFun ::
  (Predicate x p1, Predicate x p2) =>
  (Bool -> Bool -> Bool) ->
  p1 ->
  p2 ->
  x ->
  Bool
liftBinPredFun op a b x = pFun a x `op` pFun b x

parenthesized :: Text -> Text
parenthesized x = "(" <> x <> ")"

newtype Exact n = Exact n

instance Show n => ShowablePredicate (Exact n) where
  pShow (Exact m) name = name <> " == " <> show m

instance Eq n => Predicate n (Exact n) where
  pFun (Exact m) = (== m)

data PredicateFunc a = PredicateFunc (Text -> Text) (a -> Bool)

instance Predicate a (PredicateFunc a) where
  pFun (PredicateFunc _ func) a = func a

instance ShowablePredicate (PredicateFunc a) where
  pShow (PredicateFunc mkMessage _) field = mkMessage field
