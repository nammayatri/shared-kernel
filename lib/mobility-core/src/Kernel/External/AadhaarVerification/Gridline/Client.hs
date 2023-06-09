{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.AadhaarVerification.Gridline.Client where

import Kernel.External.AadhaarVerification.Gridline.API as API
import qualified Kernel.External.AadhaarVerification.Gridline.Types as Gridline
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common hiding (Error)
import Servant.Client.Core (ClientError)

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
  callAPI url (API.generateAadhaarOtp (Just apiKey) (Just authType) req) "generateAadhaarOtp" API.generateAadhaarOtpAPI
    >>= checkVerifyGridlineError url

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
  callAPI url (API.verifyAadhaarOtp (Just apiKey) (Just authType) (Just transactionId) req) "verifyAadhaarOtp" API.verifyAadhaarOtpAPI
    >>= checkSubmitGridlineError url

checkSubmitGridlineError :: (MonadThrow m, Log m) => BaseUrl -> Either ClientError Gridline.GridlineSubmitResponse -> m Gridline.GridlineSubmitResponse
checkSubmitGridlineError url res = do
  fromEitherM (gridlineError url) res >>= validateSubmitResponseStatus

gridlineError :: BaseUrl -> ClientError -> ExternalAPICallError
gridlineError = ExternalAPICallError (Just "GRIDLINE_API_ERROR")

validateSubmitResponseStatus :: (MonadThrow m, Log m) => Gridline.GridlineSubmitResponse -> m Gridline.GridlineSubmitResponse
validateSubmitResponseStatus response =
  case (response._data, response._error) of
    (Just _, Nothing) -> pure response
    (Nothing, Just _) -> pure response
    _ -> throwError GridlineInvalidRequest

checkVerifyGridlineError :: (MonadThrow m, Log m) => BaseUrl -> Either ClientError Gridline.GridlineVerifyAadhaarResp -> m Gridline.GridlineVerifyAadhaarResp
checkVerifyGridlineError url res = do
  fromEitherM (gridlineError url) res >>= validateVerifyResponseStatus

validateVerifyResponseStatus :: (MonadThrow m, Log m) => Gridline.GridlineVerifyAadhaarResp -> m Gridline.GridlineVerifyAadhaarResp
validateVerifyResponseStatus response =
  case (response._data, response._error) of
    (Just _, Nothing) -> pure response
    (Nothing, Just _) -> pure response
    _ -> throwError GridlineInvalidRequest
