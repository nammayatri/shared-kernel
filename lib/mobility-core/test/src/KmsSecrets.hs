{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PackageImports #-}

module KmsSecrets (kmsSecretsTests) where

import qualified Data.ByteString as BS
import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.IORef as IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Void as V
import Dhall.Core (Chunks (..), Expr (..), makeBinding, makeRecordField)
import qualified Dhall.Map
import Kernel.Prelude
import Kernel.Utils.Dhall.KmsSecrets
import Test.Tasty
import Test.Tasty.HUnit

type Cfg = Expr () V.Void

-- | A fake "KMS": the ciphertext is the plaintext reversed. Counts how often it is called.
fakeDecryptor :: IORef.IORef Int -> KmsDecryptor
fakeDecryptor calls ciphertext = do
  IORef.modifyIORef calls (+ 1)
  pure (BS.reverse ciphertext)

fakeEncrypt :: Text -> Text
fakeEncrypt plain = kmsPrefix <> TE.decodeUtf8 (B64.encode (BS.reverse (TE.encodeUtf8 plain)))

txt :: Text -> Cfg
txt t = TextLit (Chunks [] t)

record :: [(Text, Cfg)] -> Cfg
record fields = RecordLit . Dhall.Map.fromList $ map (\(k, v) -> (k, makeRecordField v)) fields

kmsSecretsTests :: TestTree
kmsSecretsTests =
  testGroup
    "KMS secrets in dhall configs"
    [ decryptsOnlyPrefixedValues,
      decryptsNestedFields,
      decryptsInsideLetBindings,
      plainConfigNeverCallsKms,
      rejectsInvalidBase64
    ]

decryptsOnlyPrefixedValues :: TestTree
decryptsOnlyPrefixedValues = testCase "decrypts kms: values and leaves plain values untouched" $ do
  seen <- IORef.newIORef 0
  let input = record [("dbPassword", txt (fakeEncrypt "hunter2")), ("host", txt "localhost")]
  result <- decryptKmsSecrets (fakeDecryptor seen) input
  result @?= record [("dbPassword", txt "hunter2"), ("host", txt "localhost")]

decryptsNestedFields :: TestTree
decryptsNestedFields = testCase "decrypts nested record fields" $ do
  seen <- IORef.newIORef 0
  let input = record [("secrets", record [("apiKey", txt (fakeEncrypt "k"))])]
  result <- decryptKmsSecrets (fakeDecryptor seen) input
  result @?= record [("secrets", record [("apiKey", txt "k")])]

plainConfigNeverCallsKms :: TestTree
plainConfigNeverCallsKms = testCase "a config without kms: values never calls KMS" $ do
  seen <- IORef.newIORef 0
  let input = record [("password", txt "plain"), ("port", txt "5432")]
  result <- decryptKmsSecrets (fakeDecryptor seen) input
  result @?= input
  calls <- IORef.readIORef seen
  calls @?= 0

rejectsInvalidBase64 :: TestTree
rejectsInvalidBase64 = testCase "fails clearly when the value after kms: is not base64" $ do
  seen <- IORef.newIORef 0
  let input = record [("dbPassword", txt (kmsPrefix <> "not base64 !!"))]
  result <- try (decryptKmsSecrets (fakeDecryptor seen) input)
  case result of
    Left (KmsSecretError msg) -> assertBool "error mentions base64" ("base64" `T.isInfixOf` msg)
    Right _ -> assertFailure "expected a KmsSecretError"

decryptsInsideLetBindings :: TestTree
decryptsInsideLetBindings = testCase "decrypts values inside let bindings, whatever the binding is called" $ do
  seen <- IORef.newIORef 0
  let input name = Let (makeBinding name (record [("token", txt (fakeEncrypt "t"))])) (txt "body")
  r1 <- decryptKmsSecrets (fakeDecryptor seen) (input "tokens")
  r2 <- decryptKmsSecrets (fakeDecryptor seen) (input "other")
  r1 @?= Let (makeBinding "tokens" (record [("token", txt "t")])) (txt "body")
  r2 @?= Let (makeBinding "other" (record [("token", txt "t")])) (txt "body")
