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

import Data.OpenApi
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import EulerHS.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.Common (fromFieldEnum)

data IssueTicketService = Kapture
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

instance FromField IssueTicketService where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be IssueTicketService where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be IssueTicketService

instance FromBackendRow Postgres IssueTicketService

instance IsString IssueTicketService where
  fromString = show

derivePersistField "IssueTicketService"
