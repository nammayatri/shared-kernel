{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Kernel.External.IncidentReport.Interface.Types
  ( module Reexport,
    module Kernel.External.IncidentReport.Interface.Types,
  )
where

import Deriving.Aeson
import qualified Kernel.External.IncidentReport.ERSS.Config as ERSS
import Kernel.External.IncidentReport.Types as Reexport
import Kernel.Prelude

newtype IncidentReportServiceConfig = ERSSConfig ERSS.ERSSCfg
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data IncidentReportReq = IncidentReportReq
  { contactNo :: Text,
    latitude :: Double,
    longitude :: Double,
    mpin :: Text,
    token :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data IncidentReportData = ReportData
  { jmCode :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data IncidentReportRes = IncidentReportRes
  { version :: Text,
    status :: Int,
    message :: Text,
    incidentData :: IncidentReportData
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data IncidentReportUpdateReq = IncidentReportUpdateReq
  { jmCode :: Text,
    latitude :: Double,
    longitude :: Double,
    token :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data IncidentReportUpdateRes = IncidentReportUpdateRes
  { version :: Text,
    status :: Int,
    message :: Text,
    incidentData :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
