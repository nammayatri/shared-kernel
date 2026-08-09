{-# LANGUAGE PackageImports #-}

{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.BillDesk.BillDeskJose
  ( createBillDeskRequest,
    parseBillDeskResponse,
    createJwe,
    decryptJwe,
    createJws,
    verifyJws,
  )
where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (AEAD, AEADMode (..), AuthTag (..), aeadInit, aeadSimpleDecrypt, aeadSimpleEncrypt, cipherInit)
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson as A
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified "base64-bytestring" Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.Prelude

-- | AES-GCM authentication tag length in bytes.
gcmTagLen :: Int
gcmTagLen = 16

-- | AES-GCM initialization vector length in bytes.
gcmIvLen :: Int
gcmIvLen = 12

-- ---------------------------------------------------------------------------
-- Helper functions
-- ---------------------------------------------------------------------------

-- | Convert a Maybe to an Either, using the supplied value for Nothing.
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither l Nothing = Left l
maybeToEither _ (Just r) = Right r

-- | Initialize an AES-256-GCM AEAD cipher from a 32-byte key and a 12-byte IV.
initAesGcm :: BS.ByteString -> BS.ByteString -> Either Text (AEAD AES256)
initAesGcm key iv =
  case cipherInit key :: CryptoFailable AES256 of
    CryptoFailed e -> Left ("AES cipher init failed: " <> show e)
    CryptoPassed cipher ->
      case aeadInit AEAD_GCM cipher iv of
        CryptoFailed e -> Left ("AEAD init failed: " <> show e)
        CryptoPassed aead -> Right aead

-- | Compute HMAC-SHA256 and return the raw digest bytes.
computeHmacSha256 :: BS.ByteString -> BS.ByteString -> BS.ByteString
computeHmacSha256 key msg =
  let mac = hmac key msg :: HMAC SHA256
   in convert (hmacGetDigest mac)

-- | Base64url-encode without padding.
b64UrlEncodeNoPad :: BS.ByteString -> Text
b64UrlEncodeNoPad bs =
  let encoded = TE.decodeUtf8 (B64URL.encode bs)
   in T.dropWhileEnd (== '=') encoded

-- | Base64url-decode, adding padding back as needed.
b64UrlDecodeNoPad :: Text -> Either String BS.ByteString
b64UrlDecodeNoPad t =
  let bs = TE.encodeUtf8 t
      padLen = (4 - BS.length bs `mod` 4) `mod` 4
      padded = bs <> BS.replicate padLen (fromIntegral (fromEnum '='))
   in B64URL.decode padded

-- ---------------------------------------------------------------------------
-- JWE operations
-- ---------------------------------------------------------------------------

-- | Create a JWE compact serialization using AES-256-GCM direct key agreement.
--
-- Compact format: @headerB64..ivB64.ciphertextB64.tagB64@
-- The empty second segment is because \"dir\" algorithm has no wrapped key.
createJwe ::
  BS.ByteString ->
  BS.ByteString ->
  Text ->
  Text ->
  BS.ByteString ->
  Either Text Text
createJwe plaintext encryptionKey encryptionKeyId clientId iv = do
  let header =
        A.object
          [ "alg" A..= ("dir" :: Text),
            "enc" A..= ("A256GCM" :: Text),
            "kid" A..= encryptionKeyId,
            "clientid" A..= clientId
          ]
      headerB64 = b64UrlEncodeNoPad (LBS.toStrict (A.encode header))
      aadBytes = TE.encodeUtf8 headerB64
  aead <- initAesGcm encryptionKey iv
  let (AuthTag tagBA, ct) = aeadSimpleEncrypt aead aadBytes plaintext gcmTagLen
      tag = convert tagBA :: BS.ByteString
      ivB64 = b64UrlEncodeNoPad iv
      ctB64 = b64UrlEncodeNoPad ct
      tagB64 = b64UrlEncodeNoPad tag
  pure $ headerB64 <> ".." <> ivB64 <> "." <> ctB64 <> "." <> tagB64

-- | Decrypt a JWE compact serialization using AES-256-GCM.
decryptJwe :: BS.ByteString -> Text -> Maybe BS.ByteString
decryptJwe encryptionKey jweString =
  case T.splitOn "." jweString of
    [headerB64, _, ivB64, ctB64, tagB64] -> do
      iv <- rightToMaybe (b64UrlDecodeNoPad ivB64)
      ct <- rightToMaybe (b64UrlDecodeNoPad ctB64)
      tagBytes <- rightToMaybe (b64UrlDecodeNoPad tagB64)
      let aadBytes = TE.encodeUtf8 headerB64
          authTag = AuthTag (convert tagBytes)
      aead <- rightToMaybe (initAesGcm encryptionKey iv)
      aeadSimpleDecrypt aead aadBytes ct authTag
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- JWS operations
-- ---------------------------------------------------------------------------

-- | Create a JWS compact serialization using HMAC-SHA256.
createJws :: Text -> BS.ByteString -> Text -> Text
createJws payload signingKey clientId =
  let header =
        A.object
          [ "alg" A..= ("HS256" :: Text),
            "kid" A..= ("HMAC" :: Text),
            "clientid" A..= clientId
          ]
      headerB64 = b64UrlEncodeNoPad (LBS.toStrict (A.encode header))
      payloadB64 = b64UrlEncodeNoPad (TE.encodeUtf8 payload)
      signingInput = headerB64 <> "." <> payloadB64
      sig = computeHmacSha256 signingKey (TE.encodeUtf8 signingInput)
      sigB64 = b64UrlEncodeNoPad sig
   in signingInput <> "." <> sigB64

-- | Verify a JWS compact serialization and return the decoded payload on success.
verifyJws :: BS.ByteString -> Text -> Maybe Text
verifyJws signingKey jwsString =
  case T.splitOn "." jwsString of
    [headerB64, payloadB64, sigB64] -> do
      let signingInput = headerB64 <> "." <> payloadB64
          expectedSig = computeHmacSha256 signingKey (TE.encodeUtf8 signingInput)
          expectedSigB64 = b64UrlEncodeNoPad expectedSig
      if sigB64 == expectedSigB64
        then do
          payloadBytes <- rightToMaybe (b64UrlDecodeNoPad payloadB64)
          Just (TE.decodeUtf8 payloadBytes)
        else Nothing
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- High-level API
-- ---------------------------------------------------------------------------

-- | Create a BillDesk API request by JWE-encrypting and JWS-signing the payload.
createBillDeskRequest ::
  Text ->
  BS.ByteString ->
  Text ->
  BS.ByteString ->
  A.Value ->
  IO (Either Text Text)
createBillDeskRequest clientId encryptionKey encryptionKeyId signingKey payload = do
  iv <- getRandomBytes gcmIvLen
  let plaintext = LBS.toStrict (A.encode payload)
  pure $ do
    jweToken <- createJwe plaintext encryptionKey encryptionKeyId clientId iv
    pure $ createJws jweToken signingKey clientId

-- | Parse a BillDesk API response by verifying the JWS signature and decrypting the JWE payload.
parseBillDeskResponse ::
  BS.ByteString ->
  BS.ByteString ->
  Text ->
  Either Text LBS.ByteString
parseBillDeskResponse signingKey encryptionKey response = do
  jwsPayload <- maybeToEither "JWS verification failed" (verifyJws signingKey response)
  decrypted <- maybeToEither "JWE decryption failed" (decryptJwe encryptionKey jwsPayload)
  pure $ LBS.fromStrict decrypted
