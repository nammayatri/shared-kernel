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

generateAadhaarOtp ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  GridlineCfg ->
  AadhaarOtpReq ->
  m AadhaarVerificationResp
generateAadhaarOtp cfg req = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  authType <- decrypt cfg.authType
  let reqData =
        GT.GridlineAadhaarOtpReq
          { aadhaar_number = req.aadhaarNumber,
            consent = req.consent
          }
  gridLineVerificationSuccess <- Gridline.generateAadhaarOtp url apiKey authType reqData
  case (gridLineVerificationSuccess._data, gridLineVerificationSuccess._error) of
    (Just _data, Nothing) -> do
      pure $
        AadhaarVerificationResp
          { statusCode = _data.code,
            message = _data.message,
            transactionId = _data.transaction_id,
            requestId = gridLineVerificationSuccess.request_id
          }
    (_, Just _error) -> do
      pure $
        AadhaarVerificationResp
          { statusCode = _error.code,
            message = _error.message,
            transactionId = " ",
            requestId = gridLineVerificationSuccess.request_id
          }
    (_, _) -> do
      pure $
        AadhaarVerificationResp
          { statusCode = " ",
            message = "error occurred in the interface side",
            transactionId = " ",
            requestId = " "
          }

verifyAadhaarOtp ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  GridlineCfg ->
  AadhaarOtpVerifyReq ->
  m AadhaarOtpVerifyRes
verifyAadhaarOtp cfg req = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  authType <- decrypt cfg.authType
  let transactionId = req.transactionId
  let reqData =
        GT.GridlineAadhaarOtpVerifyReq
          { otp = req.otp,
            include_xml = req.includeXml,
            share_code = req.shareCode
          }
  gridLineVerificationSuccess <- Gridline.verifyAadhaarOtp url apiKey authType transactionId reqData
  case (gridLineVerificationSuccess._data, gridLineVerificationSuccess._error) of
    (Just _data, Nothing) -> do
      pure $
        AadhaarOtpVerifyRes
          { transactionId = _data.transaction_id,
            message = _data.message,
            code = _data.code,
            name = _data.aadhaar_data.name,
            gender = _data.aadhaar_data.gender,
            date_of_birth = _data.aadhaar_data.date_of_birth,
            share_code = _data.share_code,
            image = _data.aadhaar_data.photo_base64,
            request_id = gridLineVerificationSuccess.request_id
          }
    (_, Just _error) -> do
      pure $
        AadhaarOtpVerifyRes
          { transactionId = " ",
            message = _error.message,
            code = " ",
            name = " ",
            gender = " ",
            date_of_birth = " ",
            share_code = " ",
            image = " ",
            request_id = gridLineVerificationSuccess.request_id
          }
    (_, _) -> do
      pure $
        AadhaarOtpVerifyRes
          { transactionId = " ",
            message = "error occured in interface side",
            code = " ",
            name = " ",
            gender = " ",
            date_of_birth = " ",
            share_code = " ",
            image = " ",
            request_id = " "
          }
