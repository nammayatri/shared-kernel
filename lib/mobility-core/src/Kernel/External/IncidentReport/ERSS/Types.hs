{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.IncidentReport.ERSS.Types
  ( module Kernel.External.IncidentReport.ERSS.Types,
  )
where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.JSON

data IncidentReportReq = IncidentReportReq
  { contactNo :: Text,
    latitude :: Double,
    longitude :: Double,
    userName :: Text,
    mpin :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON IncidentReportReq where
  toJSON = genericToJSON constructorsWithSnakeCase

data IncidentReportData = IncidentReportData
  { jmCode :: Text
  }
  deriving (Show, Eq, Generic)

data IncidentReportRes = IncidentReportRes
  { version :: Text,
    status :: Int,
    message :: Text,
    incidentData :: IncidentReportData
  }
  deriving (Show, Eq, Generic)

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "incidentData" -> "data"
        other -> other
    }

jsonOptionsData :: Options
jsonOptionsData =
  defaultOptions
    { fieldLabelModifier = \case
        "jmCode" -> "jm_code"
        other -> other
    }

instance FromJSON IncidentReportData where
  parseJSON = genericParseJSON jsonOptionsData

instance ToJSON IncidentReportData where
  toJSON = genericToJSON jsonOptionsData

instance FromJSON IncidentReportRes where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON IncidentReportRes where
  toJSON = genericToJSON jsonOptions

data IncidentReportUpdateReq = IncidentReportUpdateReq
  { jmCode :: Text,
    latitude :: Double,
    longitude :: Double
  }
  deriving (Show, Eq, Generic)

instance ToJSON IncidentReportUpdateReq where
  toJSON = genericToJSON constructorsWithSnakeCase

data IncidentReportUpdateRes = IncidentReportUpdateRes
  { version :: Text,
    status :: Int,
    message :: Text,
    incidentData :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON IncidentReportUpdateRes where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON IncidentReportUpdateRes where
  toJSON = genericToJSON jsonOptions
