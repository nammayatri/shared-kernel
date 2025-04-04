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

module Kernel.External.Whatsapp.Types
  ( module Kernel.External.Whatsapp.Types,
  )
where

import Data.Aeson as A
import Data.OpenApi
import qualified Data.Text as T
import Database.Beam.Backend
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForList)
import qualified Kernel.Prelude as KP
import Kernel.Storage.Esqueleto (derivePersistField)

data WhatsappService = GupShup | TataCommunications
  deriving (Show, Read, Eq, Ord, Generic, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be WhatsappService where
  sqlValueSyntax = autoSqlValueSyntax

instance FromJSON WhatsappService where -- remove this instance once you add more constructors to WhatsappService type.
  parseJSON (A.String val) = maybe (fail ("failed to parse String " <> show val <> " in WhatsappService type")) pure (KP.readMaybe $ T.unpack val)
  parseJSON (A.Array _) = pure GupShup
  parseJSON e = fail $ "unexpected type, expected String for WhatsappService " <> show e

instance ToJSON WhatsappService where
  toJSON GupShup = A.String (show GupShup)
  toJSON TataCommunications = A.String (show TataCommunications)

availableWhatsappServices :: [WhatsappService]
availableWhatsappServices = [GupShup, TataCommunications]

$(mkBeamInstancesForList ''WhatsappService)

derivePersistField "WhatsappService"
