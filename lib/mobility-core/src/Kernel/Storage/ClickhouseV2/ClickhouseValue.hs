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

import qualified Data.Aeson as A
import Data.Coerce (coerce)
import qualified Data.Text as T
import qualified Data.Time as Time
import Kernel.Prelude
import Kernel.Types.Id
import qualified Text.Read as T

data Value a = Null | String String

instance FromJSON (Value a) where
  parseJSON val@(A.String _) = String <$> parseJSON @String val
  parseJSON A.Null = pure Null
  parseJSON _ = fail "Expected String or Null for clickhouse value"

class (Show a, Read a) => ClickhouseValue a where
  toClickhouseValue :: a -> Value a
  fromClickhouseValue :: Value a -> Except a
  toClickhouseValue = String . show
  fromClickhouseValue (String str) = Except $ T.readEither str
  fromClickhouseValue Null = fail "Unexpected Null"

instance ClickhouseValue Double

instance ClickhouseValue Bool where
  fromClickhouseValue (String "1") = pure True
  fromClickhouseValue (String "0") = pure False
  fromClickhouseValue (String "True") = pure True
  fromClickhouseValue (String "False") = pure False
  fromClickhouseValue (String "true") = pure True
  fromClickhouseValue (String "false") = pure False
  fromClickhouseValue (String _) = fail "Supported format for Bool: 0, 1, false, true, False, True"
  fromClickhouseValue _ = fail "Unexpected Null"

instance ClickhouseValue Time.Day

instance ClickhouseValue UTCTime where
  toClickhouseValue = String . Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"
  fromClickhouseValue (String str) = Time.parseTimeM @Except True Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" str
  fromClickhouseValue Null = fail "Unexpected Null"

-- No instance for (MonadFail (Either String))
newtype Except a = Except {getExcept :: Either String a}
  deriving newtype (Monad, Applicative, Functor)

instance MonadFail Except where
  fail = Except . Left

instance ClickhouseValue (Id a) where
  toClickhouseValue = String . T.unpack . getId
  fromClickhouseValue (String str) = pure . Id . T.pack $ str
  fromClickhouseValue Null = fail "Unexpected Null"

instance ClickhouseValue a => ClickhouseValue (Maybe a) where
  toClickhouseValue (Just a) = coerce @(Value a) @(Value (Maybe a)) (toClickhouseValue a)
  toClickhouseValue Nothing = Null
  fromClickhouseValue :: ClickhouseValue a => Value (Maybe a) -> Except (Maybe a)
  fromClickhouseValue Null = pure Nothing
  fromClickhouseValue str = Just <$> fromClickhouseValue @a (coerce @(Value (Maybe a)) @(Value a) str)
