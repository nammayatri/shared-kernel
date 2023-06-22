{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.AadhaarVerification.Gridline.API
  ( GenerateAadhaarOtpAPI,
    generateAadhaarOtpAPI,
    generateAadhaarOtp,
    VerifyAadhaarOtpAPI,
    verifyAadhaarOtpAPI,
    verifyAadhaarOtp,
  )
where

import qualified EulerHS.Types as T
import qualified Kernel.External.AadhaarVerification.Gridline.Types as Gridline
import Kernel.Prelude
import Servant (Header, Post, ReqBody, (:>))

type GenerateAadhaarOtpAPI =
  "aadhaar-api"
    :> "boson"
    :> "generate-otp"
    :> Header "X-API-Key" Text
    :> Header "X-Auth-Type" Text
    :> ReqBody '[Gridline.AADHAARJSON] Gridline.GridlineAadhaarOtpReq
    :> Post '[Gridline.AADHAARJSON] Gridline.GridlineVerifyAadhaarResp

generateAadhaarOtpAPI :: Proxy GenerateAadhaarOtpAPI
generateAadhaarOtpAPI = Proxy

generateAadhaarOtp ::
  Maybe Text ->
  Maybe Text ->
  Gridline.GridlineAadhaarOtpReq ->
  T.EulerClient Gridline.GridlineVerifyAadhaarResp
generateAadhaarOtp = T.client generateAadhaarOtpAPI

type VerifyAadhaarOtpAPI =
  "aadhaar-api"
    :> "boson"
    :> "submit-otp"
    :> Header "X-API-Key" Text
    :> Header "X-Auth-Type" Text
    :> Header "X-Transaction-ID" Text
    :> ReqBody '[Gridline.AADHAARJSON] Gridline.GridlineAadhaarOtpVerifyReq
    :> Post '[Gridline.AADHAARJSON] Gridline.GridlineSubmitResponse

verifyAadhaarOtpAPI :: Proxy VerifyAadhaarOtpAPI
verifyAadhaarOtpAPI = Proxy

verifyAadhaarOtp ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Gridline.GridlineAadhaarOtpVerifyReq ->
  T.EulerClient Gridline.GridlineSubmitResponse
verifyAadhaarOtp = T.client verifyAadhaarOtpAPI
