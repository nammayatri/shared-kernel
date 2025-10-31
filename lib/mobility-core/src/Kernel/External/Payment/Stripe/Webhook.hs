{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Payment.Stripe.Webhook
  ( StripeWebhookAPI,
    serviceEventWebhook,
    RawByteString (..),
  )
where

import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import qualified Data.Aeson as A
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.OpenApi hiding (Header, get)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Tuple.Extra (both)
import Kernel.External.Encryption
import Kernel.External.Payment.Interface.Types
import qualified Kernel.External.Payment.Stripe.Types as Stripe
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.HideSecrets
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified Servant.API as S

type StripeWebhookAPI =
  "service" :> "stripe" :> "payment"
    :> Header "Stripe-Signature" Text
    :> ReqBody '[OctetStream] RawByteString -- we need raw bytes for proper signature check
    :> Post '[JSON] AckResponse

-- openapi lib does not provide instance for ByteString, so need to define newtype
newtype RawByteString = RawByteString {getRawByteString :: LBS.ByteString}

instance ToSchema RawByteString where
  declareNamedSchema _ = pure $ NamedSchema (Just "RawByteString") byteSchema

instance S.MimeRender OctetStream RawByteString where
  mimeRender _ = getRawByteString

instance S.MimeUnrender OctetStream RawByteString where
  mimeUnrender _ = Right . RawByteString

serviceEventWebhook ::
  EncFlow m r =>
  PaymentServiceConfig ->
  (Id Stripe.Event -> m Bool) ->
  (Stripe.WebhookReq -> Text -> m AckResponse) ->
  Maybe Text ->
  RawByteString ->
  m AckResponse
serviceEventWebhook paymentConfig checkDuplicatedEvent serviceEventHandler mbSigHeader rawBytes = do
  withLogTag "stripeWebhook" $ do
    let mResp = A.eitherDecode rawBytes.getRawByteString
    case mResp of
      Right (resp :: Stripe.WebhookReq) -> withLogTag ("eventId-" <> resp.id.getId) $ do
        sigHeader <- mbSigHeader & fromMaybeM (InvalidRequest "Stripe-Signature header did not found")
        void $ verifyAuth paymentConfig sigHeader rawBytes
        -- according to docs run heavy logic asynchronically and return 200 quickly
        fork "stripe webhook" $ do
          isDuplicatedEvent <- checkDuplicatedEvent resp.id
          if not isDuplicatedEvent
            then do
              let respDump = encodeToText $ hideSecrets @Stripe.WebhookReq resp
              void $ serviceEventHandler resp respDump
            else do
              logInfo $ "Duplicated Stripe webhook event found; skipping"
        pure Ack
      Left err -> do
        logInfo $ "Stripe webhook parsing failed: " <> show err
        throwError $ InvalidRequest "STRIPE_WEBHOOK_PARSING_FAILED"

verifyAuth ::
  EncFlow m r =>
  PaymentServiceConfig ->
  Text ->
  RawByteString ->
  m ()
verifyAuth config sigHeader (RawByteString rawBody) = do
  (secret, tolerance) <- case config of
    StripeConfig cfg -> do
      webhookEndpointSecret <- cfg.webhookEndpointSecret & fromMaybeM (InternalError "STRIPE_WEBHOOK_SECRET_NOT_FOUND")
      s <- decrypt webhookEndpointSecret
      pure (s, fromMaybe 300 cfg.webhookToleranceSeconds)
    _ -> throwError (InternalError "NOT_STRIPE_CONFIG")

  (ts, sigsV1) <- parseStripeSignature sigHeader
  now <- getCurrentTime
  let tsUtc = posixSecondsToUTCTime (fromIntegral ts)
  when (diffUTCTime now tsUtc > fromIntegral tolerance) $
    throwError (InvalidRequest "STRIPE_SIGNATURE_TIMESTAMP_OUT_OF_TOLERANCE")

  let rawStrictBody = LBS.toStrict rawBody
  let signedPayload = BS.concat [BSC.pack (show ts), BSC.pack ".", rawStrictBody]
      expected = hmacSHA256Hex (encodeUtf8 secret) signedPayload

  unless (any (secureEqHex expected) sigsV1) $
    throwError (InvalidRequest "INVALID_STRIPE_SIGNATURE")

parseStripeSignature :: (MonadThrow m, Log m) => Text -> m (Int, [BS.ByteString])
parseStripeSignature hdr = do
  -- format: t=1697040000, v1=abcdef..., v1=...
  let parts = T.splitOn "," hdr
      kvs = map (both T.strip . second (T.drop 1) . T.breakOn "=") parts
      get k = [v | (kk, v) <- kvs, kk == k]

  tsTxt <- listToMaybe (get "t") & fromMaybeM (InvalidRequest "STRIPE_SIGNATURE_MISSING_TIMESTAMP")
  ts <- readMaybe (T.unpack tsTxt) & fromMaybeM (InvalidRequest "STRIPE_SIGNATURE_BAD_TIMESTAMP")

  let v1s = map encodeUtf8 (get "v1")
  when (null v1s) $
    throwError $ InvalidRequest "STRIPE_SIGNATURE_MISSING_V1"

  pure (ts, v1s)

hmacSHA256Hex :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmacSHA256Hex key msg =
  let mac = hmac key msg :: HMAC SHA256
   in convertToBase Base16 (hmacGetDigest mac)

secureEqHex :: BS.ByteString -> BS.ByteString -> Bool
secureEqHex a b =
  case ( convertFromBase Base16 a :: Either String BS.ByteString,
         convertFromBase Base16 b :: Either String BS.ByteString
       ) of
    (Right da, Right db) -> constEq da db -- const time
    _ -> False -- not valid hex
