{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.FromField where

import Data.Aeson
import Data.ByteString.Internal (ByteString, unpackChars)
import Data.ByteString.Lazy (fromStrict)
import Data.Generics.Labels ()
import Database.Beam
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import Kernel.Prelude as KP

fromFieldJSON ::
  (Typeable a, FromJSON a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion a
fromFieldJSON f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just value' -> case decode $ fromStrict value' of
    Just res -> pure res
    Nothing -> DPSF.returnError ConversionFailed f ("Could not 'read'" <> KP.show value')

fromFieldEnum ::
  (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion a
fromFieldEnum f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just value' ->
    case readMaybe (unpackChars value') of
      Just val -> pure val
      _ -> DPSF.returnError ConversionFailed f ("Could not 'read'" <> KP.show value')

fromFieldDefault ::
  (Typeable a, Read a, DPSF.FromField a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion a
fromFieldDefault f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> DPSF.fromField f mbValue
