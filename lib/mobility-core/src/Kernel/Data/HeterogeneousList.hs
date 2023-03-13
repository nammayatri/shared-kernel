{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Kernel.Data.HeterogeneousList
  ( HeterogeneousList (..),
    mapToList,
    map,
  )
where

import Data.Kind
import Prelude hiding (map)

type family MkConstr (cs :: [Type -> Constraint]) (a :: Type) :: Constraint where
  MkConstr '[] a = () :: Constraint
  MkConstr (c ': cs) a = (c a, MkConstr cs a)

infixr 5 :<+>

data HeterogeneousList (constr :: [Type -> Constraint]) where
  HLEnd :: HeterogeneousList constr
  (:<+>) :: MkConstr constr val => val -> HeterogeneousList constr -> HeterogeneousList constr

instance Semigroup (HeterogeneousList constr) where
  (<>) :: HeterogeneousList constr -> HeterogeneousList constr -> HeterogeneousList constr
  (<>) (a :<+> l) b = a :<+> (l <> b)
  (<>) HLEnd b = b

instance Monoid (HeterogeneousList constr) where
  mempty = HLEnd

mapToList :: forall constr b. (forall a. (MkConstr constr a) => a -> b) -> HeterogeneousList constr -> [b]
mapToList _ HLEnd = []
mapToList f (a :<+> l) = f a : mapToList @constr @b f l

map :: forall constr. (forall a. (MkConstr constr a) => a -> a) -> HeterogeneousList constr -> HeterogeneousList constr
map _ HLEnd = HLEnd
map f (a :<+> l) = do
  f a :<+> map @constr f l
