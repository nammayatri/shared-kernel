{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PackageImports #-}

-- | AES-GCM + PBKDF2 envelope encryption for the Parivahan ntrsearchservice (MoRTH v2.1).
--
-- Wire format matches the Java reference supplied in the API docs:
--
--   Base64 ( IV (12 B) ‖ Salt (16 B) ‖ Ciphertext ‖ AuthTag (16 B) )
--
-- Key derivation: PBKDF2-HMAC-SHA256, 65 536 iterations, 256-bit output.
module Kernel.External.Verification.MorthV2.Crypto
  ( CryptoError (..),
    encryptEnvelopeData,
    decryptEnvelopeData,
  )
where

import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Cipher.Types as CT
import qualified Crypto.Error as CE
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Crypto.Random as CR
import qualified Data.Bifunctor as Bi
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.Text as T
import Kernel.Prelude

data CryptoError
  = CryptoBase64DecodeError Text
  | CryptoPayloadTooShort Int
  | CryptoCipherInitFailed Text
  | CryptoDecryptionFailed
  deriving stock (Show, Eq)

ivLen, saltLen, tagLen, pbkdf2Iters, keyLen :: Int
ivLen = 12
saltLen = 16
tagLen = 16 -- 128-bit tag
pbkdf2Iters = 65536
keyLen = 32 -- 256-bit key

deriveKey :: Text -> ByteString -> ByteString
deriveKey apiKey salt =
  PBKDF2.fastPBKDF2_SHA256
    PBKDF2.Parameters {PBKDF2.iterCounts = pbkdf2Iters, PBKDF2.outputLength = keyLen}
    (encodeUtf8 apiKey :: ByteString)
    salt

initAead :: ByteString -> ByteString -> Either Text (CT.AEAD AES.AES256)
initAead key iv = CE.onCryptoFailure (Left . T.pack . show) Right $ do
  cipher <- CT.cipherInit key
  CT.aeadInit CT.AEAD_GCM cipher iv

-- | Encrypt a JSON payload (or any bytes) into the Base64 wire envelope.
encryptEnvelopeData :: MonadIO m => Text -> ByteString -> m Text
encryptEnvelopeData apiKey plaintext = liftIO $ do
  iv <- CR.getRandomBytes ivLen
  salt <- CR.getRandomBytes saltLen
  let key = deriveKey apiKey salt
  case initAead key iv of
    Left err ->
      -- Unreachable with a correctly-sized key + IV, but surface as a runtime error
      -- rather than silently corrupting the payload.
      error $ "MorthV2.Crypto.encryptEnvelopeData: AEAD init failed: " <> err
    Right aead -> do
      let (tag, ct) = CT.aeadSimpleEncrypt aead BS.empty plaintext tagLen
          wire = iv <> salt <> ct <> BA.convert tag
      pure (decodeUtf8 (B64.encode wire) :: Text)

-- | Decrypt the Base64 wire envelope back to the inner plaintext bytes.
decryptEnvelopeData :: Text -> Text -> Either CryptoError ByteString
decryptEnvelopeData apiKey wireB64 = do
  wire <- Bi.first (CryptoBase64DecodeError . T.pack) $ B64.decode (encodeUtf8 wireB64 :: ByteString)
  let minLen = ivLen + saltLen + tagLen
  when (BS.length wire < minLen) $ Left (CryptoPayloadTooShort (BS.length wire))
  let (iv, rest1) = BS.splitAt ivLen wire
      (salt, rest2) = BS.splitAt saltLen rest1
      ctLen = BS.length rest2 - tagLen
      (ct, tagBS) = BS.splitAt ctLen rest2
      key = deriveKey apiKey salt
      tag = CT.AuthTag (BA.convert tagBS)
  aead <- Bi.first CryptoCipherInitFailed $ initAead key iv
  case CT.aeadSimpleDecrypt aead BS.empty ct tag of
    Nothing -> Left CryptoDecryptionFailed
    Just pt -> Right pt
