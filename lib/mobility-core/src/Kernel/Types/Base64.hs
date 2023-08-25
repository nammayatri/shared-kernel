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
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}

module Kernel.Types.Base64 where

import qualified Data.Aeson as A
import Data.Bifunctor (bimap)
import Data.ByteString
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Queries
  ( PersistField (..),
    PersistFieldSql,
    PersistValue (PersistText),
  )
import Kernel.Utils.Dhall

newtype Base64 = Base64 ByteString
  deriving (Show, Eq, Read, Ord)
  deriving newtype (PersistFieldSql)

deriving newtype instance FromField Base64

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Base64 where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Base64

instance FromBackendRow Postgres Base64

instance PersistField Base64 where
  toPersistValue (Base64 t) = PersistText . decodeUtf8 $ Base64.encode t
  fromPersistValue (PersistText t) = bimap T.pack Base64 . Base64.decode $ encodeUtf8 t
  fromPersistValue x = Left $ "When trying to deserialize an Base64: expected PersistText, received: " <> T.pack (show x)

instance FromDhall Base64 where
  autoWith = customDecoder T.pack (fmap Base64 . Base64.decode . encodeUtf8 @Text) . autoWith

instance ToJSON Base64 where
  toJSON (Base64 bs) = A.String $ decodeUtf8 $ Base64.encode bs

instance FromJSON Base64 where
  parseJSON = A.withText "Base64" decodeBase64
    where
      decodeBase64 txt =
        Base64.decode (encodeUtf8 txt)
          & either fail (pure . Base64)

-- Use only for constant values in test code
instance IsString Base64 where
  fromString = Base64 . Base64.decodeLenient . encodeUtf8
