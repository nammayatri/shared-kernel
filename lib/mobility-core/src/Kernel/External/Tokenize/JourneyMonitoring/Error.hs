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

module Kernel.External.Tokenize.JourneyMonitoring.Error where

import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Utils.Error.TH (mkOpenAPIError)

data JourneyMonitoringError
  = JMUnauthorizedError
  | JMError Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''JourneyMonitoringError

instance IsBaseError JourneyMonitoringError where
  toMessage = \case
    JMUnauthorizedError -> Just "Invalid Credentaials, Please provide valid username and password."
    JMError msg -> Just $ "JourneyMonitoring Error with message: " <> msg

instance IsHTTPError JourneyMonitoringError where
  toErrorCode = \case
    JMUnauthorizedError -> "JM_UNAUTHORIZED"
    JMError _ -> "JM_ERROR"

  toHttpCode = \case
    JMUnauthorizedError -> E401
    JMError _ -> E400

instance IsAPIError JourneyMonitoringError

mkOpenAPIError ''JourneyMonitoringError
