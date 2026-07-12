{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Helpers for the inbound Meta WhatsApp Cloud API webhook:
--   * capture the raw request body (@RawByteString@) for signature verification,
--   * verify @X-Hub-Signature-256@ (hex @HMAC_SHA256(rawBody, appSecret)@) with a
--     constant-time comparison,
--   * answer the GET @hub.challenge@ verification handshake.
-- Self-contained duplicate of the Xyne webhook primitives (do not import across
-- the Ticket namespace).
module Kernel.External.Meta.Webhook where

import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.Aeson (eitherDecode)
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import Data.OpenApi (NamedSchema (NamedSchema), ToSchema (..), byteSchema)
import qualified Data.Text as T
import Kernel.External.Meta.Types (MetaWebhookEnvelope)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Network.HTTP.Media ((//))
import Servant hiding (throwError)
import qualified Servant.API as S

-- | OpenApi/Servant wrapper so the inbound webhook route can receive raw bytes
-- for signature verification (JSON parsing happens after verify).
newtype RawByteString = RawByteString {getRawByteString :: LBS.ByteString}

instance ToSchema RawByteString where
  declareNamedSchema _ = pure $ NamedSchema (Just "RawByteString") byteSchema

instance S.MimeRender OctetStream RawByteString where
  mimeRender _ = getRawByteString

instance S.MimeUnrender OctetStream RawByteString where
  mimeUnrender _ = Right . RawByteString

-- | Content type that advertises @application/json@ on the wire (so Servant
-- routes the request when Meta sends @Content-Type: application/json@) but keeps
-- the body as raw bytes for HMAC verification. Defining a fresh 'Accept'-tagged
-- type avoids the overlap with Servant's generic
-- @FromJSON a => MimeUnrender JSON a@ instance.
data RawJson

instance Accept RawJson where
  contentTypes _ = NE.fromList ["application" // "json", "application" // "*"]

instance S.MimeRender RawJson RawByteString where
  mimeRender _ = getRawByteString

instance S.MimeUnrender RawJson RawByteString where
  mimeUnrender _ = Right . RawByteString

-- | Pure X-Hub-Signature-256 check. Header format is @sha256=<hex>@; anything
-- else (missing header, missing prefix, malformed/odd-length hex) is False and
-- never throws (secureEqHex handles undecodable hex).
verifyMetaSignaturePure :: Text -> Maybe Text -> LBS.ByteString -> Bool
verifyMetaSignaturePure appSecret mSigHeader raw =
  case mSigHeader >>= T.stripPrefix "sha256=" of
    Nothing -> False
    Just hex ->
      secureEqHex
        (hmacSHA256Hex (encodeUtf8 appSecret) (LBS.toStrict raw))
        (encodeUtf8 hex)

-- | Throwing wrapper for the webhook route. Throws @InvalidRequest@ on mismatch.
verifyMetaSignature ::
  (MonadThrow m, Log m) =>
  Text ->
  Maybe Text ->
  RawByteString ->
  m ()
verifyMetaSignature appSecret mSigHeader (RawByteString rawBody) =
  unless (verifyMetaSignaturePure appSecret mSigHeader rawBody) $
    throwError (InvalidRequest "INVALID_META_SIGNATURE")

-- | GET verification handshake. Given the configured verify token and the
-- @hub.mode@ / @hub.verify_token@ / @hub.challenge@ query params, echo the
-- challenge VERBATIM iff mode is "subscribe" and the token matches (constant
-- time). The caller serves the result as text/plain.
metaVerifyChallenge :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text
metaVerifyChallenge cfgToken mMode mToken mChallenge =
  case (mMode, mToken, mChallenge) of
    (Just "subscribe", Just t, Just c)
      | constEq (encodeUtf8 t :: BS.ByteString) (encodeUtf8 cfgToken :: BS.ByteString) -> Just c
    _ -> Nothing

decodeWebhookEnvelope :: RawByteString -> Either String MetaWebhookEnvelope
decodeWebhookEnvelope (RawByteString raw) = eitherDecode raw

hmacSHA256Hex :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmacSHA256Hex key msg =
  let mac = hmac key msg :: HMAC SHA256
   in convertToBase Base16 (hmacGetDigest mac)

-- | Compare two hex strings in constant time. Base16-decodes both sides then
-- constEq; malformed/odd-length hex yields False and never throws.
secureEqHex :: BS.ByteString -> BS.ByteString -> Bool
secureEqHex a b =
  case ( convertFromBase Base16 a :: Either String BS.ByteString,
         convertFromBase Base16 b :: Either String BS.ByteString
       ) of
    (Right da, Right db) -> constEq da db
    _ -> False
