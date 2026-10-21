{-# LANGUAGE PackageImports #-}

module Kernel.External.SocialSecurity.EShram.Encryption
  ( encryptPayload,
    decryptPayload,
  )
where

import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Cipher.Types as CT
import qualified Crypto.Data.Padding as Padding
import qualified Crypto.Error as CE
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Data.ByteString as BS
import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.Prelude

initCipher :: Text -> Text -> Either Text AES.AES256
initCipher secretCode saltValue =
  case CT.cipherInit key of
    CE.CryptoPassed cipher -> Right cipher
    CE.CryptoFailed err -> Left $ "eShram cipher init failed: " <> show err
  where
    key :: BS.ByteString
    key =
      PBKDF2.fastPBKDF2_SHA256
        PBKDF2.Parameters {PBKDF2.iterCounts = 65536, PBKDF2.outputLength = 32}
        (TE.encodeUtf8 secretCode)
        (TE.encodeUtf8 saltValue)

-- | Encrypt a plain-text payload (compact JSON) to Base64 with the given secret code and salt.
encryptPayload :: Text -> Text -> Text -> Either Text Text
encryptPayload secretCode saltValue plainText = do
  cipher <- initCipher secretCode saltValue
  pure . TE.decodeUtf8 . B64.encode . CT.cbcEncrypt cipher CT.nullIV $ Padding.pad (Padding.PKCS7 16) (TE.encodeUtf8 plainText)

-- | Decrypt a Base64 cipher text with the given secret code and salt.
decryptPayload :: Text -> Text -> Text -> Either Text Text
decryptPayload secretCode saltValue cipherText = do
  cipher <- initCipher secretCode saltValue
  raw <- either (Left . ("eShram: Base64 decode failed: " <>) . T.pack) Right $ B64.decode (TE.encodeUtf8 cipherText)
  when (BS.length raw `mod` 16 /= 0) $ Left "eShram: cipher text length is not a multiple of 16"
  plain <- maybe (Left "eShram: invalid PKCS7 padding") Right $ Padding.unpad (Padding.PKCS7 16) (CT.cbcDecrypt cipher CT.nullIV raw)
  either (Left . ("eShram: invalid UTF-8 in plaintext: " <>) . show) Right $ TE.decodeUtf8' plain
