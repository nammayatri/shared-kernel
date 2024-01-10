{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Storage.ClickhouseV2.ClickhouseValue
  ( Value (getValue),
    ClickhouseValue (..),
  )
where

import Data.Coerce (coerce)
import qualified Data.Text as T
import qualified Data.Time as Time
import Kernel.Prelude
import Kernel.Types.Id
import qualified Text.Read as T

newtype Value a = Value {getValue :: String}
  deriving newtype (FromJSON)

class (Show a, Read a) => ClickhouseValue a where
  toClickhouseValue :: a -> Value a
  fromClickhouseValue :: Value a -> Either String a
  toClickhouseValue = Value . show
  fromClickhouseValue = T.readEither . getValue

instance ClickhouseValue Double

instance ClickhouseValue Bool where
  fromClickhouseValue (Value "1") = Right True
  fromClickhouseValue (Value "0") = Right False
  fromClickhouseValue (Value "True") = Right True
  fromClickhouseValue (Value "False") = Right False
  fromClickhouseValue (Value "true") = Right True
  fromClickhouseValue (Value "false") = Right False
  fromClickhouseValue _ = Left "Supported format for Bool: 0, 1, false, true, False, True"

instance ClickhouseValue Time.Day

instance ClickhouseValue UTCTime where
  toClickhouseValue = Value . Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"
  fromClickhouseValue = getExcept . Time.parseTimeM @Except True Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" . getValue

-- No instance for (MonadFail (Either String))
newtype Except a = Except {getExcept :: Either String a}
  deriving newtype (Monad, Applicative, Functor)

instance MonadFail Except where
  fail = Except . Left

instance ClickhouseValue (Id a) where
  toClickhouseValue = Value . T.unpack . getId
  fromClickhouseValue = Right . Id . T.pack . getValue

instance ClickhouseValue a => ClickhouseValue (Maybe a) where
  toClickhouseValue (Just a) = coerce @(Value a) @(Value (Maybe a)) (toClickhouseValue a)
  toClickhouseValue Nothing = Value "null"
  fromClickhouseValue :: ClickhouseValue a => Value (Maybe a) -> Either String (Maybe a)
  fromClickhouseValue (Value "null") = Right Nothing
  fromClickhouseValue str = Just <$> fromClickhouseValue @a (coerce @(Value (Maybe a)) @(Value a) str)
