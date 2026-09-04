{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.CCAvenue.Crypto
  ( ccaEncrypt,
    ccaDecrypt,
  )
where

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (IV, cbcDecrypt, cbcEncrypt, cipherInit, makeIV)
import Crypto.Error (CryptoFailable (..))
import qualified Crypto.Hash as Hash
import Crypto.Hash.Algorithms (MD5)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.Prelude

-- | Encrypt plaintext using CCAvenue's AES-128-CBC scheme.
--
-- Key derivation: MD5-hash the working key, hex-encode the digest,
-- then interpret each hex byte-pair as a raw byte (16 bytes total).
-- Uses a fixed IV [0x00..0x0f] and PKCS5 padding, matching the
-- CCAvenue PHP SDK.
ccaEncrypt :: Text -> Text -> Either Text Text
ccaEncrypt workingKey plaintext = do
  let key = deriveKey workingKey
  cipher <- initCipher key
  iv <- maybe (Left "Failed to create IV") Right fixedIV
  let padded = pkcs5Pad (TE.encodeUtf8 plaintext)
      encrypted = cbcEncrypt cipher iv padded
  Right (bytesToHex encrypted)

-- | Decrypt hex-encoded ciphertext using CCAvenue's AES-128-CBC scheme.
ccaDecrypt :: Text -> Text -> Either Text Text
ccaDecrypt workingKey hexCiphertext = do
  let key = deriveKey workingKey
  cipher <- initCipher key
  iv <- maybe (Left "Failed to create IV") Right fixedIV
  let ciphertextBytes = hexToBytes' (TE.encodeUtf8 hexCiphertext)
      decrypted = cbcDecrypt cipher iv ciphertextBytes
      unpadded = pkcs5Unpad decrypted
  case TE.decodeUtf8' unpadded of
    Left err -> Left ("UTF-8 decode error: " <> show err)
    Right txt -> Right txt

-- | Derive the 16-byte AES key from a CCAvenue working key.
--
-- Steps: MD5 hash -> hex encode -> interpret hex pairs as bytes.
deriveKey :: Text -> BS.ByteString
deriveKey workingKey =
  let md5Digest = Hash.hash (TE.encodeUtf8 workingKey) :: Hash.Digest MD5
      md5Hex = bytesToHex (convert md5Digest :: BS.ByteString)
   in hexToBytes' (TE.encodeUtf8 (T.toLower md5Hex))

-- | Fixed IV used by CCAvenue: [0x00, 0x01, ..., 0x0f].
fixedIV :: Maybe (IV AES128)
fixedIV = makeIV (BS.pack [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f])

-- | Initialise an AES128 cipher from a raw key, returning Left on failure.
initCipher :: BS.ByteString -> Either Text AES128
initCipher key =
  case cipherInit key of
    CryptoPassed c -> Right c
    CryptoFailed e -> Left ("Cipher init failed: " <> show e)

-- | PKCS5/PKCS7 padding to 16-byte block boundary.
pkcs5Pad :: BS.ByteString -> BS.ByteString
pkcs5Pad bs =
  let blockSize = 16
      padLen = blockSize - (BS.length bs `mod` blockSize)
      padding = BS.replicate padLen (fromIntegral padLen)
   in bs <> padding

-- | Remove PKCS5/PKCS7 padding.
pkcs5Unpad :: BS.ByteString -> BS.ByteString
pkcs5Unpad bs
  | BS.null bs = bs
  | otherwise =
    let padLen = fromIntegral (BS.last bs)
     in BS.take (BS.length bs - padLen) bs

-- | Convert a ByteString to its lowercase hex Text representation.
bytesToHex :: BS.ByteString -> Text
bytesToHex = T.pack . concatMap toHexPair . BS.unpack
  where
    toHexPair w =
      let (hi, lo) = w `divMod` 16
       in [hexChar hi, hexChar lo]
    hexChar n
      | n < 10 = toEnum (fromEnum '0' + fromIntegral n)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)

-- | Convert a hex-encoded ByteString (ASCII) to raw bytes.
-- Each pair of hex characters becomes one byte.
hexToBytes' :: BS.ByteString -> BS.ByteString
hexToBytes' hex =
  let pairs = chunksOf2 (BSC.unpack hex)
   in BS.pack (map decodePair pairs)
  where
    chunksOf2 [] = []
    chunksOf2 [x] = [[x]]
    chunksOf2 (x : y : rest) = [x, y] : chunksOf2 rest

    decodePair [hi, lo] = fromIntegral (hexVal hi * 16 + hexVal lo)
    decodePair [c] = fromIntegral (hexVal c)
    decodePair _ = 0

    hexVal c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
      | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
      | otherwise = 0
