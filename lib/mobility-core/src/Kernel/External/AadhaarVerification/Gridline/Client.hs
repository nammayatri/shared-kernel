{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.AadhaarVerification.Gridline.Client where

import qualified Data.Text as T
import qualified EulerHS.Types as ET
import Kernel.External.AadhaarVerification.Gridline.API as API
import Kernel.External.AadhaarVerification.Gridline.Config
import Kernel.External.AadhaarVerification.Gridline.Error
import qualified Kernel.External.AadhaarVerification.Gridline.Types as Gridline
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Common
import Kernel.Utils.Common hiding (Error)

generateAadhaarOtp ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Gridline.GridlineAadhaarOtpReq ->
  m Gridline.GridlineVerifyAadhaarResp
generateAadhaarOtp url apiKey authType req = do
  let client = API.generateAadhaarOtp (Just apiKey) (Just authType) req
  callGridlineAPI url client "generateAadhaarOtp" API.generateAadhaarOtpAPI
    >>= validateGenerateResponseStatus

verifyAadhaarOtp ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Gridline.GridlineAadhaarOtpVerifyReq ->
  m Gridline.GridlineSubmitResponse
verifyAadhaarOtp url apiKey authType transactionId req = do
  let client = API.verifyAadhaarOtp (Just apiKey) (Just authType) (Just transactionId) req
  callGridlineAPI url client "verifyAadhaarOtp" API.verifyAadhaarOtpAPI
    >>= validateVerifyResponseStatus

callGridlineAPI :: CallAPI env api res
callGridlineAPI = callApiUnwrappingApiError (identity @GridlineError) (Just $ ET.ManagerSelector $ T.pack gridlineHttpManagerKey) (Just "GRIDLINE_ERROR") Nothing

validateGenerateResponseStatus :: (MonadThrow m, Log m) => Gridline.GridlineVerifyAadhaarResp -> m Gridline.GridlineVerifyAadhaarResp
validateGenerateResponseStatus response = do
  let code = response._data.code
  case code of
    "1008" -> throwError NoMobileNumberRegistered
    "1011" -> throwError ExceedOtpGenerationLimit
    "1012" -> throwError AadhaarNumberNotExist
    _ -> pure response

validateVerifyResponseStatus :: (MonadThrow m, Log m) => Gridline.GridlineSubmitResponse -> m Gridline.GridlineSubmitResponse
validateVerifyResponseStatus response = do
  let code = response._data.code
  case code of
    "1003" -> throwError SessionExpired
    "1005" -> throwError OtpAttemptExceeded
    _ -> pure response
