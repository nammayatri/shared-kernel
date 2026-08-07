module Kernel.Utils.DESEncryption where

import Crypto.Cipher.TripleDES
import Crypto.Cipher.Types
import Crypto.Data.Padding
import Crypto.Error (CryptoFailable (..))
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as TE
import Kernel.Prelude
import Kernel.Types.Base64

encryptDES :: Base64 -> Text -> Either String Text
encryptDES (Base64 cipherKey) plainText = do
  let cipherEither = cipherInit cipherKey :: CryptoFailable DES_EDE3
  case cipherEither of
    CryptoPassed cipher ->
      let paddedPlainText = pad (PKCS7 (blockSize cipher)) (TE.encodeUtf8 plainText)
       in Right $ TE.decodeUtf8 $ Base64.encode $ ecbEncrypt cipher paddedPlainText
    CryptoFailed err -> Left $ show err

decryptDES :: Base64 -> Text -> Either String Text
decryptDES (Base64 cipherKey) encryptedText = do
  let cipherEither = cipherInit cipherKey :: CryptoFailable DES_EDE3
  case cipherEither of
    CryptoPassed cipher -> do
      let decodedCipherText = Base64.decodeLenient (TE.encodeUtf8 encryptedText)
          decryptedPaddedText = ecbDecrypt cipher decodedCipherText
          decryptedText = unpad (PKCS7 (blockSize cipher)) decryptedPaddedText
      case decryptedText of
        Just decryptText -> Right $ TE.decodeUtf8 decryptText
        Nothing -> Left "Could not decrypt!"
    CryptoFailed err -> Left $ show err
