{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PackageImports #-}

module Kernel.External.Payment.PaytmEDC.Flow where

import qualified Data.Text as T
import Data.Time (addUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import EulerHS.Types as Euler
import Kernel.External.Payment.PaytmEDC.Checksum
import Kernel.External.Payment.PaytmEDC.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (fromEitherM)
import Kernel.Utils.Servant.Client
import Servant hiding (throwError)

-- IST offset: UTC+5:30 = 5 hours 30 minutes = 19800 seconds
istOffset :: NominalDiffTime
istOffset = 60 * 330 -- 19800 seconds = 5.5 hours

-- Convert UTC time to IST (Indian Standard Time)
utcToIST :: UTCTime -> UTCTime
utcToIST = addUTCTime istOffset

-- Format timestamp for Paytm API (yyyy-MM-dd HH:mm:ss) in IST
formatPaytmTimestamp :: UTCTime -> Text
formatPaytmTimestamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" . utcToIST

-- Generate Checksum API
type GenerateChecksumAPI =
  "ecr" :> "generateChecksum"
    :> ReqBody '[JSON] GenerateChecksumReq
    :> Post '[JSON] GenerateChecksumResp

-- Sale API
type SaleAPI =
  "ecr" :> "payment" :> "request"
    :> ReqBody '[JSON] PaytmEDCSaleRequest
    :> Post '[JSON] PaytmEDCResponse

-- Status Enquiry API
type StatusEnquiryAPI =
  "ecr" :> "payment" :> "status"
    :> ReqBody '[JSON] PaytmEDCStatusRequest
    :> Post '[JSON] PaytmEDCResponse

-- Call Generate Checksum API
generateChecksum ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  GenerateChecksumReq ->
  m GenerateChecksumResp
generateChecksum url req = do
  let proxy = Proxy @GenerateChecksumAPI
      eulerClient = Euler.client proxy req
  callPaytmEDCAPI url eulerClient "generateChecksum" proxy

-- Initiate Sale on EDC terminal
initiateSale ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  PaytmEDCSaleRequest ->
  m PaytmEDCResponse
initiateSale url req = do
  let proxy = Proxy @SaleAPI
      eulerClient = Euler.client proxy req
  callPaytmEDCAPI url eulerClient "sale" proxy

-- Check transaction status
statusEnquiry ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  PaytmEDCStatusRequest ->
  m PaytmEDCResponse
statusEnquiry url req = do
  let proxy = Proxy @StatusEnquiryAPI
      eulerClient = Euler.client proxy req
  callPaytmEDCAPI url eulerClient "status-enquiry" proxy

-- Common API call helper
callPaytmEDCAPI :: CallAPI' m r api res res
callPaytmEDCAPI url eulerClient description proxy = do
  callAPI url eulerClient description proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call PaytmEDC " <> description <> " API: " <> show err)
