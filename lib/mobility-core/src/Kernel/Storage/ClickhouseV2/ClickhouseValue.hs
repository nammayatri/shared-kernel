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
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Time as Time
import Kernel.Prelude
import Kernel.Types.Common (Centesimal, HighPrecMeters, HighPrecMoney)
import Kernel.Types.Id
import qualified Text.Read as T

data Value a = Null | String String | Number Sci.Scientific

-- FIXME
-- data Value a where
--   Null :: Value a
--   String :: constraint => a -> Value a
--   Number :: constraint => a -> Value a

instance FromJSON (Value a) where
  parseJSON val@(A.String _) = String <$> parseJSON @String val
  parseJSON val@(A.Number _) = Number <$> parseJSON @Sci.Scientific val
  parseJSON A.Null = pure Null
  parseJSON _ = fail "Expected String or Null for clickhouse value"

class (Show a, Read a) => ClickhouseValue a where
  toClickhouseValue :: a -> Value a
  fromClickhouseValue :: Value a -> Except a
  toClickhouseValue = String . show
  fromClickhouseValue (String str) = parseAsString @a str
  fromClickhouseValue (Number _) = fail "Unexpected Number"
  fromClickhouseValue Null = fail "Unexpected Null"

-- For ATLAS_KAFKA env we store numbers as String, for APP_SERVICE_CLICKHOUSE env we store as Number. So we should be able to parse both
instance ClickhouseValue HighPrecMoney where
  fromClickhouseValue = parseAsStringOrNumber @HighPrecMoney

instance ClickhouseValue HighPrecMeters where
  fromClickhouseValue = parseAsStringOrNumber @HighPrecMeters

instance ClickhouseValue Double where
  fromClickhouseValue = parseAsStringOrNumber @Double

instance ClickhouseValue Centesimal where
  fromClickhouseValue = parseAsStringOrNumber @Centesimal

instance ClickhouseValue Int where
  fromClickhouseValue = parseAsStringOrNumber @Int

parseAsStringOrNumber :: forall a. (Read a, Num a, FromJSON a) => Value a -> Except a
parseAsStringOrNumber (String str) = parseAsString @a str
parseAsStringOrNumber (Number num) = parseAsNumber @a num
parseAsStringOrNumber Null = fail "Unexpected Null"

parseAsString :: forall a. Read a => String -> Except a
parseAsString = Except . T.readEither @a

parseAsNumber :: forall a. (Num a, FromJSON a) => Sci.Scientific -> Except a
parseAsNumber = Except . eitherResult . A.fromJSON @a . A.Number

instance ClickhouseValue Bool where
  fromClickhouseValue (String "1") = pure True
  fromClickhouseValue (String "0") = pure False
  fromClickhouseValue (String "True") = pure True
  fromClickhouseValue (String "False") = pure False
  fromClickhouseValue (String "true") = pure True
  fromClickhouseValue (String "false") = pure False
  fromClickhouseValue (String _) = fail "Supported format for Bool: 0, 1, false, true, False, True"
  fromClickhouseValue _ = fail "Unexpected Null"

instance ClickhouseValue Text

instance ClickhouseValue Time.Day

instance ClickhouseValue UTCTime where
  toClickhouseValue = String . Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"
  fromClickhouseValue (String str) = Time.parseTimeM @Except True Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" str
  fromClickhouseValue (Number _) = fail "Unexpected Number"
  fromClickhouseValue Null = fail "Unexpected Null"

newtype DateTime = DateTime {getDateTime :: UTCTime}
  deriving newtype (Show, Read)

instance ClickhouseValue DateTime where
  toClickhouseValue = String . Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S" . getDateTime
  fromClickhouseValue (String str) = DateTime <$> Time.parseTimeM @Except True Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S" str
  fromClickhouseValue (Number _) = fail "Unexpected Number"
  fromClickhouseValue Null = fail "Unexpected Null"

-- No instance for (MonadFail (Either String))
newtype Except a = Except {getExcept :: Either String a}
  deriving newtype (Monad, Applicative, Functor)

instance MonadFail Except where
  fail = Except . Left

instance ClickhouseValue (Id a) where
  toClickhouseValue = String . T.unpack . getId
  fromClickhouseValue (String str) = pure . Id . T.pack $ str
  fromClickhouseValue (Number _) = fail "Unexpected Number"
  fromClickhouseValue Null = fail "Unexpected Null"

instance ClickhouseValue (ShortId a) where
  toClickhouseValue = String . T.unpack . getShortId
  fromClickhouseValue (String str) = pure . ShortId . T.pack $ str
  fromClickhouseValue (Number _) = fail "Unexpected Number"
  fromClickhouseValue Null = fail "Unexpected Null"

instance ClickhouseValue a => ClickhouseValue (Maybe a) where
  toClickhouseValue (Just a) = coerce @(Value a) @(Value (Maybe a)) (toClickhouseValue a)
  toClickhouseValue Nothing = Null
  fromClickhouseValue :: ClickhouseValue a => Value (Maybe a) -> Except (Maybe a)
  fromClickhouseValue Null = pure Nothing
  fromClickhouseValue str = Just <$> fromClickhouseValue @a (coerce @(Value (Maybe a)) @(Value a) str)

eitherResult :: A.Result a -> Either String a
eitherResult (A.Error err) = Left err
eitherResult (A.Success a) = Right a

valToString :: Value value -> String
valToString (String str) = addQuotes $ str
valToString (Number num) = addQuotes $ show num -- working both with quotes and without quotes
valToString Null = "null"

addQuotes :: String -> String
addQuotes rq = "'" <> rq <> "'"
