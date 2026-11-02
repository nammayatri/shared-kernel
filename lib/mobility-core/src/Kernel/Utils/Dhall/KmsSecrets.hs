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
-- The decryption is bound to an encryption context made of a single label:
--
--   * @field@ - the path of the value inside the secrets file, built from the record keys
--     and @let@ names that lead to it, joined with dots (e.g. @dbPassword@ or
--     @ondcTokenMap.mapValue.token@)
--
-- so a ciphertext copied into another field fails to decrypt. The application name is
-- deliberately not part of it: one secrets file is imported by several apps (for example the
-- driver app, its allocator and its drainer), and all of them must be able to decrypt it.
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
import Dhall.Core (Binding (variable), Chunks (..), Expr (..), RecordField (..), bindingExprs, subExpressions)
import qualified Dhall.Map
import Kernel.Prelude

-- | Decrypts one ciphertext. Gets the encryption context as key/value pairs and the raw
-- (already base64-decoded) ciphertext, returns the raw plaintext.
type KmsDecryptor = [(Text, Text)] -> BS.ByteString -> IO BS.ByteString

-- | Raised when an encrypted config value can not be decrypted. The message never contains
-- the ciphertext or the plaintext.
newtype KmsSecretError = KmsSecretError Text
  deriving (Show)

instance Exception KmsSecretError

-- | Marker for encrypted values.
kmsPrefix :: Text
kmsPrefix = "kms:"

decryptKmsSecrets :: KmsDecryptor -> Expr s a -> IO (Expr s a)
decryptKmsSecrets decryptor = go []
  where
    -- @path@ holds the record keys and let names leading to the current expression.
    go path expr = case expr of
      TextLit (Chunks [] txt)
        | Just payload <- T.stripPrefix kmsPrefix txt ->
          TextLit . Chunks [] <$> decryptValue path payload
      RecordLit fields ->
        RecordLit
          <$> Dhall.Map.traverseWithKey
            (\key recordField -> (\v -> recordField {recordFieldValue = v}) <$> go (path ++ [key]) (recordFieldValue recordField))
            fields
      Let binding body ->
        Let <$> bindingExprs (go (path ++ [variable binding])) binding <*> go path body
      _ -> subExpressions (go path) expr

    decryptValue path payload = do
      let field = T.intercalate "." path
          failWith :: Text -> IO x
          failWith reason = throwIO . KmsSecretError $ "Can't decrypt config value for field=" <> field <> ": " <> reason
      when (null path) $ failWith "value is not inside a named field"
      ciphertext <- either (const $ failWith "value after 'kms:' is not valid base64") pure $ B64.decode (TE.encodeUtf8 (T.strip payload))
      plaintext <- decryptor [("field", field)] ciphertext
      either (const $ failWith "decrypted value is not valid UTF-8") pure $ TE.decodeUtf8' plaintext
