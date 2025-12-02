{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.AadhaarVerification.Interface.Gridline where

import Kernel.External.AadhaarVerification.Gridline.Client as Gridline
import Kernel.External.AadhaarVerification.Gridline.Config
import qualified Kernel.External.AadhaarVerification.Gridline.Types as GT
import Kernel.External.AadhaarVerification.Interface.Types
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Servant.Client

generateAadhaarOtp ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  GridlineCfg ->
  AadhaarOtpReq ->
  m AadhaarVerificationResp
generateAadhaarOtp cfg req = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  let authType = cfg.authType
  let reqData =
        GT.GridlineAadhaarOtpReq
          { aadhaar_number = req.aadhaarNumber,
            consent = req.consent
          }
  resp <- Gridline.generateAadhaarOtp url apiKey authType reqData
  return $
    AadhaarVerificationResp
      { statusCode = resp._data.code,
        message = resp._data.message,
        transactionId = resp._data.transaction_id,
        requestId = resp.request_id
      }

verifyAadhaarOtp ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  GridlineCfg ->
  AadhaarOtpVerifyReq ->
  m AadhaarOtpVerifyRes
verifyAadhaarOtp cfg req = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  let authType = cfg.authType
  let transactionId = req.transactionId
  let reqData =
        GT.GridlineAadhaarOtpVerifyReq
          { otp = req.otp,
            include_xml = False,
            share_code = req.shareCode
          }
  resp <- Gridline.verifyAadhaarOtp url apiKey authType transactionId reqData
  return $
    AadhaarOtpVerifyRes
      { transactionId = resp._data.transaction_id,
        message = resp._data.message,
        code = resp._data.code,
        name = resp._data.aadhaar_data.name,
        gender = resp._data.aadhaar_data.gender,
        date_of_birth = resp._data.aadhaar_data.date_of_birth,
        share_code = resp._data.share_code,
        image = resp._data.aadhaar_data.image,
        request_id = resp.request_id
      }
