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

module Kernel.Storage.ClickhouseV2.ClickhouseValue where

import Data.Coerce (coerce)
import qualified Data.Text as T
import qualified Data.Time as Time
import Kernel.Prelude
import Kernel.Types.Id

newtype Value a = Value {getValue :: String}
  deriving newtype (FromJSON)

class (Show a, Read a) => ClickhouseValue a where
  toClickhouseValue :: a -> Value a
  fromClickhouseValue :: Value a -> Maybe a -- Either String a
  toClickhouseValue = Value . show
  fromClickhouseValue = readMaybe . getValue

instance ClickhouseValue Double

instance ClickhouseValue Bool

instance ClickhouseValue Time.Day

instance ClickhouseValue UTCTime where
  toClickhouseValue = Value . Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"
  fromClickhouseValue = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" . getValue

instance ClickhouseValue (Id a) where
  toClickhouseValue = Value . T.unpack . getId
  fromClickhouseValue = Just . Id . T.pack . getValue

instance ClickhouseValue a => ClickhouseValue (Maybe a) where
  toClickhouseValue (Just a) = coerce @(Value a) @(Value (Maybe a)) (toClickhouseValue a)
  toClickhouseValue Nothing = Value "null"
  fromClickhouseValue :: ClickhouseValue a => Value (Maybe a) -> Maybe (Maybe a)
  fromClickhouseValue (Value "null") = Just Nothing
  fromClickhouseValue str = Just <$> fromClickhouseValue @a (coerce @(Value (Maybe a)) @(Value a) str)
