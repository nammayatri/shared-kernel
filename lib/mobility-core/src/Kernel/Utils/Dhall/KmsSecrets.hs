{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PackageImports #-}

-- | Support for KMS-encrypted values inside dhall configs.
--
-- A text value written as @"kms:<base64 ciphertext>"@ is replaced by its decrypted value
-- while the config is loaded. Values without the prefix are left untouched, so plain and
-- encrypted values can live side by side and be migrated one field at a time.
--
-- No encryption context is used, so a ciphertext decrypts wherever it appears in a config.
-- That keeps it independent of how the secrets file is imported (the path of a value changes
-- with the @let@ names of every file that imports it).
--
-- This module only knows how to find and rewrite the values. The actual call to a KMS
-- is injected as a 'KmsDecryptor' (see "Kernel.Utils.Dhall.AwsKms").
module Kernel.Utils.Dhall.KmsSecrets
  ( KmsDecryptor,
    KmsSecretError (..),
    kmsPrefix,
    decryptKmsSecrets,
  )
where

import qualified Data.ByteString as BS
import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Dhall.Core (Chunks (..), Expr (..), subExpressions)
import Kernel.Prelude

-- | Decrypts one ciphertext. Gets the raw (already base64-decoded) ciphertext, returns the raw plaintext.
type KmsDecryptor = BS.ByteString -> IO BS.ByteString

-- | Raised when an encrypted config value can not be decrypted. The message never contains
-- the ciphertext or the plaintext.
newtype KmsSecretError = KmsSecretError Text
  deriving (Show)

instance Exception KmsSecretError

-- | Marker for encrypted values.
kmsPrefix :: Text
kmsPrefix = "kms:"

decryptKmsSecrets :: KmsDecryptor -> Expr s a -> IO (Expr s a)
decryptKmsSecrets decryptor = go
  where
    go expr = case expr of
      TextLit (Chunks [] txt)
        | Just payload <- T.stripPrefix kmsPrefix txt ->
          TextLit . Chunks [] <$> decryptValue payload
      _ -> subExpressions go expr

    decryptValue payload = do
      let failWith :: Text -> IO x
          failWith reason = throwIO . KmsSecretError $ "Can't decrypt kms: config value: " <> reason
      ciphertext <- either (const $ failWith "value after 'kms:' is not valid base64") pure $ B64.decode (TE.encodeUtf8 (T.strip payload))
      plaintext <- decryptor ciphertext
      either (const $ failWith "decrypted value is not valid UTF-8") pure $ TE.decodeUtf8' plaintext
