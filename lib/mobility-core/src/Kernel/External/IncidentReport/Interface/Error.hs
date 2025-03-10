{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.IncidentReport.Interface.Error where

import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Utils.Error.TH (mkOpenAPIError)

data IncidentReportError
  = FailedToCallIncidentReportAPI Text
  | FailedToCallIncidentReportUpdateAPI Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''IncidentReportError

instance IsBaseError IncidentReportError where
  toMessage = \case
    FailedToCallIncidentReportAPI err -> Just $ "Failed to incident report API: " <> err
    FailedToCallIncidentReportUpdateAPI err -> Just $ "Failed to incident report update API: " <> err

instance IsHTTPError IncidentReportError where
  toErrorCode = \case
    FailedToCallIncidentReportAPI _ -> "FAILED_TO_CALL_INCIDENT_REPORT_API"
    FailedToCallIncidentReportUpdateAPI _ -> "FAILED_TO_CALL_INCIDENT_REPORT_UPDATE_API"
  toHttpCode = \case
    FailedToCallIncidentReportAPI _ -> E400
    FailedToCallIncidentReportUpdateAPI _ -> E400

instance IsAPIError IncidentReportError

mkOpenAPIError ''IncidentReportError
