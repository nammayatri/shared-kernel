{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Helpers for verifying inbound Xyne Spaces @DESK_REPLY@ webhooks.
-- Mirrors the Stripe webhook pattern: capture the raw request body with a
-- @RawByteString@ MimeUnrender and HMAC-SHA256 verify against the configured
-- signing secret using constant-time comparison.
module Kernel.External.Ticket.XyneSpaces.Webhook
  ( RawByteString (..),
    verifyXyneSignature,
  )
where

import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.OpenApi (NamedSchema (NamedSchema), ToSchema (..), byteSchema)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified Servant.API as S

-- | OpenApi/Servant wrapper so the inbound webhook route can receive raw
-- bytes for signature verification (JSON parsing happens after verify).
newtype RawByteString = RawByteString {getRawByteString :: LBS.ByteString}

instance ToSchema RawByteString where
  declareNamedSchema _ = pure $ NamedSchema (Just "RawByteString") byteSchema

instance S.MimeRender OctetStream RawByteString where
  mimeRender _ = getRawByteString

instance S.MimeUnrender OctetStream RawByteString where
  mimeUnrender _ = Right . RawByteString

-- | Verify the @X-Xyne-Signature@ header for a raw webhook body using the
-- shared signing secret. Throws @InvalidRequest@ on mismatch.
--
-- Signature format: hex-encoded @HMAC_SHA256(rawBody, signingSecret)@.
verifyXyneSignature ::
  (MonadThrow m, Log m) =>
  Text ->
  Text ->
  RawByteString ->
  m ()
verifyXyneSignature signingSecret sigHeader (RawByteString rawBody) = do
  let rawStrict = LBS.toStrict rawBody
      expected = hmacSHA256Hex (encodeUtf8 signingSecret) rawStrict
      provided = encodeUtf8 sigHeader
  unless (secureEqHex expected provided) $
    throwError (InvalidRequest "INVALID_XYNE_SIGNATURE")

hmacSHA256Hex :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmacSHA256Hex key msg =
  let mac = hmac key msg :: HMAC SHA256
   in convertToBase Base16 (hmacGetDigest mac)

secureEqHex :: BS.ByteString -> BS.ByteString -> Bool
secureEqHex a b =
  case ( convertFromBase Base16 a :: Either String BS.ByteString,
         convertFromBase Base16 b :: Either String BS.ByteString
       ) of
    (Right da, Right db) -> constEq da db
    _ -> False
