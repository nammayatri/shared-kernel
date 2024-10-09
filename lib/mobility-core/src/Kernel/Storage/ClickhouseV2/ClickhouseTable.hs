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

module Kernel.Storage.ClickhouseV2.ClickhouseTable where

import Kernel.Prelude

class (Typeable t, FromJSON (t Identity)) => ClickhouseTable (t :: (Type -> Type) -> Type) where
  tableModification :: FieldModifications t
  mapTable :: forall f g. (forall a. C f a -> C g a) -> t f -> t g

-- | A type family that we use to "tag" columns in our table datatypes.
type family Columnar (f :: Type -> Type) x where
  Columnar Identity x = x
  Columnar f x = f x

-- | A short type-alias for 'Columnar'. May shorten your schema definitions
type C f a = Columnar f a

newtype FieldModification table value = FieldModification String

instance IsString (FieldModification table value) where
  fromString = FieldModification

-- record field did not work with TH
getFieldModification :: FieldModification table value -> String
getFieldModification (FieldModification str) = str

type FieldModifications table = table (FieldModification table)
