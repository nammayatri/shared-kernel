{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Ticket.Types
  ( module Kernel.External.Ticket.Types,
  )
where

import qualified Data.Aeson as A
import Data.OpenApi
import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import qualified Kernel.Prelude as KP
import Kernel.Storage.Esqueleto (derivePersistField)

data IssueTicketService = Kapture
  deriving (Show, Read, Eq, Ord, Generic, ToSchema)

instance FromJSON IssueTicketService where -- remove this instance once you add more constructors to IssueTicketService type.
  parseJSON (A.String val) = pure $ fromMaybe (error $ "failed to parse String " <> val <> " in IssueTicketService type") (KP.readMaybe $ T.unpack val)
  parseJSON e = error $ "unexpected type, expected String for IssueTicketService " <> show e

instance ToJSON IssueTicketService where
  toJSON Kapture = A.String (show Kapture)

$(mkBeamInstancesForEnum ''IssueTicketService)

derivePersistField "IssueTicketService"
