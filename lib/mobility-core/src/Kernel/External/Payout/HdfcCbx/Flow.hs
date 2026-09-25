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
-- Every body is a JOSE compact string sent as @Content-Type: application/jose@ -- see
-- 'JoseBody'. Encoding and decoding of the envelope happens one layer up, in
-- "Kernel.External.Payout.Interface.HdfcCbx"; this module only moves opaque text over a
-- mutually-authenticated connection.
--
-- Headers per HDFC's Postman guide, confirmed against UAT on 2026-08-31: @apikey@ (the
-- consumer key), @Scope@, @transactionId@ (a caller-generated trace id) and the bearer
-- token.
module Kernel.External.Payout.HdfcCbx.Flow where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as TE
import EulerHS.Types as Euler
import qualified EulerHS.Types as ET
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Error.Throwing (throwError)
import Kernel.Utils.Servant.Client
import Network.HTTP.Media ((//))
import qualified Network.HTTP.Types as HttpTypes
import Servant hiding (throwError)
import Servant.Client.Core (ClientError (..), Response, responseBody, responseStatusCode)

-- | A JOSE compact serialisation travelling as @application/jose@ (RFC 7515's media
-- type, and what HDFC's own sample requests send). Servant's 'JSON' would both mislabel
-- it and try to parse it, failing on the first dot.
--
-- Requests go out with the head content type; the tail widens what a /response/ may be
-- labelled as, because the gateway's label for the returned envelope is not documented
-- and a mismatch would fail the call after it succeeded at the bank.
data JoseBody

instance Accept JoseBody where
  contentType _ = "application" // "jose"
  contentTypes _ = ("application" // "jose") NE.:| ["application" // "json", "text" // "plain"]

instance MimeRender JoseBody Text where
  mimeRender _ = BL.fromStrict . TE.encodeUtf8

instance MimeUnrender JoseBody Text where
  mimeUnrender _ = Right . TE.decodeUtf8 . BL.toStrict

-- | 'HdfcCbxConfig.url' is the bare host (e.g. @https://api.hdfcuat.bank.in@, no path) --
-- every version segment lives here instead, because the four operations do not share one:
-- bulk payment and both inquiries are v1, but the batch-number lookup is v2 (@cbx-getBatchNo-v2@
-- on the API portal, distinct from the older @cbx-nodal-batchnuminq@ name in HDFC's bulk
-- spec sheet).
type BulkPaymentAPI =
  "api" :> "v1" :> "cbx-nodal-bulkPayment"
    :> Header "apikey" Text
    :> Header "Scope" Text
    :> Header "transactionId" Text
    :> Header "Authorization" Text
    :> ReqBody '[JoseBody] Text
    :> Post '[JoseBody] Text

type BulkPaymentInquiryAPI =
  "api" :> "v1" :> "cbx-nodal-bulkPaymentInq"
    :> Header "apikey" Text
    :> Header "Scope" Text
    :> Header "transactionId" Text
    :> Header "Authorization" Text
    :> ReqBody '[JoseBody] Text
    :> Post '[JoseBody] Text

-- | @cbx-getBatchNo-v2@ on the API portal. Not @cbx-nodal-batchnuminq@ (the name in the
-- bulk spec sheet) and not v1 -- the portal lists this one a version ahead of the other
-- three. Calling the old v1 name is consistent with the 401 TH99401 "Invalid API Key"
-- observed against UAT on 2026-09-02: the app's key was never subscribed to a v1 product
-- that doesn't exist under this name.
type BatchNumInquiryAPI =
  "api" :> "v2" :> "cbx-getBatchNo"
    :> Header "apikey" Text
    :> Header "Scope" Text
    :> Header "transactionId" Text
    :> Header "Authorization" Text
    :> ReqBody '[JoseBody] Text
    :> Post '[JoseBody] Text

type CallCtx m r = (Metrics.CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m)

-- | A reply the gateway sends OUTSIDE the JOSE tunnel. Interim inquiry answers, token
-- refusals and some validation failures arrive as bare @application/problem+json@ with
-- statuses like 202 or 412 -- e.g. a first inquiry on a just-submitted batch answers
-- @202 {"title":"Accepted","errors":[{"code":"0","reason":"We have accepted your
-- request. Please enquire again after sometime"}]}@ (observed against UAT, 2026-09-02).
-- The adapter maps these to canonical outcomes; anything that is neither an envelope
-- nor a problem document stays an error.
data GatewayNote = GatewayNote
  { noteStatus :: Int,
    noteCode :: Text,
    noteReason :: Text
  }
  deriving stock (Show, Eq, Generic)

data ProblemErr = ProblemErr
  { code :: Maybe Text,
    reason :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype ProblemBody = ProblemBody
  { errors :: Maybe [ProblemErr]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

noteFromClientError :: ClientError -> Maybe GatewayNote
noteFromClientError = \case
  -- A refusal: the status is outside 2xx, so servant hands the response back untouched.
  FailureResponse _ resp -> noteFromResponse resp
  -- An acceptance that is not yet an answer. Servant only consults the response content
  -- type once the status says success, so a note carried on a 2xx surfaces here instead:
  -- the interim inquiry reply is @202 application/problem+json@ ("please enquire again
  -- after sometime"), which is neither a failure nor a JOSE envelope. Reading it as a note
  -- is what lets 'classifyInquiryNote' answer InquiryNotReady rather than the poll erroring.
  UnsupportedContentType _ resp -> noteFromResponse resp
  -- Correctly labelled, but not an envelope -- same document, same treatment.
  DecodeFailure _ resp -> noteFromResponse resp
  _ -> Nothing

-- | A problem document, if that is what the body is. Anything else is 'Nothing' and stays
-- an error: a note is only a note when the gateway actually sent one.
noteFromResponse :: Response -> Maybe GatewayNote
noteFromResponse resp = do
  prob :: ProblemBody <- A.decode (responseBody resp)
  firstErr <- listToMaybe =<< prob.errors
  pure
    GatewayNote
      { noteStatus = HttpTypes.statusCode (responseStatusCode resp),
        noteCode = fromMaybe "" firstErr.code,
        noteReason = fromMaybe "" firstErr.reason
      }

-- | An envelope, a gateway note, or -- for anything unrecognisable -- an error.
joseResult :: (CallCtx m r) => Text -> Either ClientError Text -> m (Either GatewayNote Text)
joseResult what = \case
  Right t -> pure (Right t)
  Left err
    | Just note <- noteFromClientError err -> pure (Left note)
    | otherwise -> throwError (InternalError $ "HDFC CBX " <> what <> " failed: " <> show err)

-- All four share a shape: select the mutually-authenticated manager, attach the api key and
-- bearer token, send an envelope, receive an envelope. Written out rather than abstracted --
-- the polymorphic version needs a Client type equality that costs more than it saves.

bulkPayment :: (CallCtx m r) => Text -> BaseUrl -> Text -> Text -> Text -> Text -> Text -> m (Either GatewayNote Text)
bulkPayment mgr url apiKey scope txnId token envelope = do
  let proxy = Proxy @BulkPaymentAPI
      eulerClient = Euler.client proxy (Just apiKey) (Just scope) (Just txnId) (Just $ "Bearer " <> token) envelope
  callAPI' (Just $ ET.ManagerSelector mgr) url eulerClient "hdfc-bulk-payment" proxy
    >>= joseResult "bulkPayment"

bulkPaymentInquiry :: (CallCtx m r) => Text -> BaseUrl -> Text -> Text -> Text -> Text -> Text -> m (Either GatewayNote Text)
bulkPaymentInquiry mgr url apiKey scope txnId token envelope = do
  let proxy = Proxy @BulkPaymentInquiryAPI
      eulerClient = Euler.client proxy (Just apiKey) (Just scope) (Just txnId) (Just $ "Bearer " <> token) envelope
  callAPI' (Just $ ET.ManagerSelector mgr) url eulerClient "hdfc-bulk-payment-inquiry" proxy
    >>= joseResult "bulkPaymentInq"

batchNumInquiry :: (CallCtx m r) => Text -> BaseUrl -> Text -> Text -> Text -> Text -> Text -> m (Either GatewayNote Text)
batchNumInquiry mgr url apiKey scope txnId token envelope = do
  let proxy = Proxy @BatchNumInquiryAPI
      eulerClient = Euler.client proxy (Just apiKey) (Just scope) (Just txnId) (Just $ "Bearer " <> token) envelope
  callAPI' (Just $ ET.ManagerSelector mgr) url eulerClient "hdfc-batchnum-inquiry" proxy
    >>= joseResult "batchnuminq"
