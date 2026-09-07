{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant clients for HDFC CBX.
--
-- Every body is a JOSE compact string rather than JSON, but the @Content-Type@ is still
-- @application/json@ -- see 'JoseBody'. Encoding and decoding of the envelope happens one
-- layer up, in "Kernel.External.Payout.Interface.HdfcCbx"; this module only moves opaque
-- text over a mutually-authenticated connection.
module Kernel.External.Payout.HdfcCbx.Flow where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import EulerHS.Types as Euler
import qualified EulerHS.Types as ET
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (fromEitherM)
import Kernel.Utils.Servant.Client
import Network.HTTP.Media ((//))
import Servant hiding (throwError)

-- | A body that travels as @application/json@ but is a JOSE compact serialisation, not a
-- JSON document. Servant's own 'JSON' would try to parse it and fail on the first dot.
data JoseBody

instance Accept JoseBody where
  contentType _ = "application" // "json"

instance MimeRender JoseBody Text where
  mimeRender _ = BL.fromStrict . TE.encodeUtf8

instance MimeUnrender JoseBody Text where
  mimeUnrender _ = Right . TE.decodeUtf8 . BL.toStrict

type BulkPaymentAPI =
  "cbx-nodal-bulkPayment"
    :> Header "apikey" Text
    :> Header "Authorization" Text
    :> ReqBody '[JoseBody] Text
    :> Post '[JoseBody] Text

type BulkPaymentInquiryAPI =
  "cbx-nodal-bulkPaymentInq"
    :> Header "apikey" Text
    :> Header "Authorization" Text
    :> ReqBody '[JoseBody] Text
    :> Post '[JoseBody] Text

type BatchNumInquiryAPI =
  "cbx-nodal-batchnuminq"
    :> Header "apikey" Text
    :> Header "Authorization" Text
    :> ReqBody '[JoseBody] Text
    :> Post '[JoseBody] Text

type BeneRegAPI =
  "cbx-nodal-beneReg"
    :> Header "apikey" Text
    :> Header "Authorization" Text
    :> ReqBody '[JoseBody] Text
    :> Post '[JoseBody] Text

type CallCtx m r = (Metrics.CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m)

-- All four share a shape: select the mutually-authenticated manager, attach the api key and
-- bearer token, send an envelope, receive an envelope. Written out rather than abstracted --
-- the polymorphic version needs a Client type equality that costs more than it saves.

bulkPayment :: (CallCtx m r) => Text -> BaseUrl -> Text -> Text -> Text -> m Text
bulkPayment mgr url apiKey token envelope = do
  let proxy = Proxy @BulkPaymentAPI
      eulerClient = Euler.client proxy (Just apiKey) (Just $ "Bearer " <> token) envelope
  callAPI' (Just $ ET.ManagerSelector mgr) url eulerClient "hdfc-bulk-payment" proxy
    >>= fromEitherM (\err -> InternalError $ "HDFC CBX bulkPayment failed: " <> show err)

bulkPaymentInquiry :: (CallCtx m r) => Text -> BaseUrl -> Text -> Text -> Text -> m Text
bulkPaymentInquiry mgr url apiKey token envelope = do
  let proxy = Proxy @BulkPaymentInquiryAPI
      eulerClient = Euler.client proxy (Just apiKey) (Just $ "Bearer " <> token) envelope
  callAPI' (Just $ ET.ManagerSelector mgr) url eulerClient "hdfc-bulk-payment-inquiry" proxy
    >>= fromEitherM (\err -> InternalError $ "HDFC CBX bulkPaymentInq failed: " <> show err)

batchNumInquiry :: (CallCtx m r) => Text -> BaseUrl -> Text -> Text -> Text -> m Text
batchNumInquiry mgr url apiKey token envelope = do
  let proxy = Proxy @BatchNumInquiryAPI
      eulerClient = Euler.client proxy (Just apiKey) (Just $ "Bearer " <> token) envelope
  callAPI' (Just $ ET.ManagerSelector mgr) url eulerClient "hdfc-batchnum-inquiry" proxy
    >>= fromEitherM (\err -> InternalError $ "HDFC CBX batchnuminq failed: " <> show err)

beneReg :: (CallCtx m r) => Text -> BaseUrl -> Text -> Text -> Text -> m Text
beneReg mgr url apiKey token envelope = do
  let proxy = Proxy @BeneRegAPI
      eulerClient = Euler.client proxy (Just apiKey) (Just $ "Bearer " <> token) envelope
  callAPI' (Just $ ET.ManagerSelector mgr) url eulerClient "hdfc-bene-reg" proxy
    >>= fromEitherM (\err -> InternalError $ "HDFC CBX beneReg failed: " <> show err)
