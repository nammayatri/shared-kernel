{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.SMS.Types
  ( module Kernel.External.SMS.Types,
  )
where

import Data.OpenApi
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import EulerHS.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.Common (fromFieldEnum)

data SmsService = MyValueFirst | ExotelSms | GupShup
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

fromFieldSmsService ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion [SmsService]
fromFieldSmsService f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> V.toList <$> fromField f mbValue

instance FromField [SmsService] where
  fromField = fromFieldSmsService

instance FromField SmsService where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be (V.Vector Text) => HasSqlValueSyntax be [SmsService] where
  sqlValueSyntax x = sqlValueSyntax (V.fromList (T.pack . show <$> x))

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [SmsService]

instance FromBackendRow Postgres [SmsService]

availableSmsServices :: [SmsService]
availableSmsServices = [MyValueFirst, ExotelSms, GupShup]

derivePersistField "SmsService"
