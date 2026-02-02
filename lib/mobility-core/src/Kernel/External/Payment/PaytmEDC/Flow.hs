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

import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import qualified Data.Aeson as A
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import "base64-bytestring" Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import EulerHS.Types as Euler
import Kernel.External.Payment.PaytmEDC.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (fromEitherM)
import Kernel.Utils.Servant.Client
import Servant hiding (throwError)

-- HMAC-SHA256 using cryptonite
hmacSHA256 :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmacSHA256 key msg =
  let hmacResult :: HMAC SHA256
      hmacResult = hmac key msg
   in BA.convert $ hmacGetDigest hmacResult

-- Build checksum: Base64( HMAC-SHA256( merchantKey, JSON(body) ) )
buildChecksum :: Text -> PaytmEDCSaleRequestBody -> Text
buildChecksum merchantKey reqBody =
  let bodyJson = LBS.toStrict $ A.encode reqBody
      keyBytes = encodeUtf8 merchantKey
      hmacResult = hmacSHA256 keyBytes bodyJson
      checksum = Base64.encode hmacResult
   in decodeUtf8 checksum

-- Build checksum for status request
buildStatusChecksum :: Text -> PaytmEDCStatusRequestBody -> Text
buildStatusChecksum merchantKey reqBody =
  let bodyJson = LBS.toStrict $ A.encode reqBody
      keyBytes = encodeUtf8 merchantKey
      hmacResult = hmacSHA256 keyBytes bodyJson
      checksum = Base64.encode hmacResult
   in decodeUtf8 checksum

-- Verify checksum from Paytm response
verifyChecksum :: Text -> PaytmEDCResponseBody -> Text -> Bool
verifyChecksum merchantKey respBody receivedChecksum =
  let bodyJson = LBS.toStrict $ A.encode respBody
      keyBytes = encodeUtf8 merchantKey
      hmacResult = hmacSHA256 keyBytes bodyJson
      expectedChecksum = decodeUtf8 $ Base64.encode hmacResult
   in expectedChecksum == receivedChecksum

-- Format timestamp for Paytm API (yyyy-MM-dd HH:mm:ss)
formatPaytmTimestamp :: UTCTime -> Text
formatPaytmTimestamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

-- Sale API
type SaleAPI =
  "sale"
    :> ReqBody '[JSON] PaytmEDCSaleRequest
    :> Post '[JSON] PaytmEDCResponse

-- Status Enquiry API
type StatusEnquiryAPI =
  "status"
    :> ReqBody '[JSON] PaytmEDCStatusRequest
    :> Post '[JSON] PaytmEDCResponse

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
