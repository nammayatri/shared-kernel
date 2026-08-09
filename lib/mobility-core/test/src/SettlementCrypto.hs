{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SettlementCrypto
  ( settlementCryptoTests,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import EulerHS.Prelude
import Kernel.External.Settlement.BillDesk.BillDeskJose
import Kernel.External.Settlement.CCAvenue.Crypto
import Test.Tasty
import Test.Tasty.HUnit

settlementCryptoTests :: TestTree
settlementCryptoTests =
  testGroup
    "Settlement PG Crypto"
    [ testGroup "BillDesk JWE+JWS" billDeskTests,
      testGroup "CCAvenue AES-128-CBC" ccAvenueTests
    ]

-- ---------------------------------------------------------------------------
-- BillDesk test fixtures
-- ---------------------------------------------------------------------------

bdClientId :: Text
bdClientId = "test_client_id"

bdEncryptionKeyId :: Text
bdEncryptionKeyId = "AES_DIR"

-- 32 bytes for AES-256-GCM
bdEncryptionKey :: BS.ByteString
bdEncryptionKey = "01234567890123456789012345678901"

-- HMAC signing key (arbitrary length)
bdSigningKey :: BS.ByteString
bdSigningKey = "my-secret-hmac-signing-key-here!"

bdSamplePayload :: A.Value
bdSamplePayload =
  A.object
    [ "mercid" A..= ("TESTMERC" :: Text),
      "from_date" A..= ("20250601" :: Text),
      "to_date" A..= ("20250607" :: Text)
    ]

-- ---------------------------------------------------------------------------
-- BillDesk tests
-- ---------------------------------------------------------------------------

billDeskTests :: [TestTree]
billDeskTests =
  [ testCase "JWS round-trip: sign then verify recovers payload" $ do
      let payload = "hello world, this is a JWS test payload"
          signed = createJws payload bdSigningKey bdClientId
      -- signed should be 3 dot-separated parts
      let parts = T.splitOn "." signed
      length parts @?= 3

      -- verify should recover the original payload
      let verified = verifyJws bdSigningKey signed
      verified @?= Just payload,
    testCase "JWS verify fails with wrong key" $ do
      let payload = "sensitive data"
          signed = createJws payload bdSigningKey bdClientId
          wrongKey = "wrong-key-wrong-key-wrong-key!!!!"
          verified = verifyJws wrongKey signed
      verified @?= Nothing,
    testCase "JWE round-trip: encrypt then decrypt recovers plaintext" $ do
      let plaintext = TE.encodeUtf8 "{\"test\":\"value\",\"amount\":100}"
          iv = BS.pack [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b]
      case createJwe plaintext bdEncryptionKey bdEncryptionKeyId bdClientId iv of
        Left err -> assertFailure $ "JWE encryption failed: " <> T.unpack err
        Right jweToken -> do
          -- JWE should be 5 dot-separated parts (with empty 2nd part for "dir")
          let parts = T.splitOn "." jweToken
          length parts @?= 5
          case parts of
            (_ : emptyKey : _) -> emptyKey @?= ""
            _ -> assertFailure "JWE should have at least 2 parts"

          case decryptJwe bdEncryptionKey jweToken of
            Nothing -> assertFailure "JWE decryption returned Nothing"
            Just decrypted -> decrypted @?= plaintext,
    testCase "JWE decrypt fails with wrong key" $ do
      let plaintext = TE.encodeUtf8 "secret message"
          iv = BS.pack [0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05]
      case createJwe plaintext bdEncryptionKey bdEncryptionKeyId bdClientId iv of
        Left err -> assertFailure $ "JWE encryption failed: " <> T.unpack err
        Right jweToken -> do
          let wrongKey = "99999999999999999999999999999999"
          decryptJwe wrongKey jweToken @?= Nothing,
    testCase "Full BillDesk request round-trip: createBillDeskRequest then parseBillDeskResponse" $ do
      result <- createBillDeskRequest bdClientId bdEncryptionKey bdEncryptionKeyId bdSigningKey bdSamplePayload
      case result of
        Left err -> assertFailure $ "createBillDeskRequest failed: " <> T.unpack err
        Right signedToken -> do
          -- The token is a JWS wrapping a JWE
          let jwsParts = T.splitOn "." signedToken
          length jwsParts @?= 3

          -- Simulate: server receives this, processes it, and sends back
          -- a response using the same encrypt-then-sign pipeline
          let responsePayload = A.object ["status" A..= ("ok" :: Text), "settlements" A..= ([] :: [A.Value])]
          responseResult <- createBillDeskRequest bdClientId bdEncryptionKey bdEncryptionKeyId bdSigningKey responsePayload
          case responseResult of
            Left err -> assertFailure $ "Response creation failed: " <> T.unpack err
            Right responseToken -> do
              case parseBillDeskResponse bdSigningKey bdEncryptionKey responseToken of
                Left err -> assertFailure $ "parseBillDeskResponse failed: " <> T.unpack err
                Right decryptedBytes -> do
                  let decoded = A.decode decryptedBytes :: Maybe A.Value
                  decoded @?= Just responsePayload,
    testCase "parseBillDeskResponse fails with tampered token" $ do
      responseResult <- createBillDeskRequest bdClientId bdEncryptionKey bdEncryptionKeyId bdSigningKey bdSamplePayload
      case responseResult of
        Left err -> assertFailure $ "Setup failed: " <> T.unpack err
        Right token -> do
          let tampered = token <> "x" -- append garbage to break signature
          case parseBillDeskResponse bdSigningKey bdEncryptionKey tampered of
            Left _ -> pure () -- expected
            Right _ -> assertFailure "Should have failed on tampered token"
  ]

-- ---------------------------------------------------------------------------
-- CCAvenue test fixtures
-- ---------------------------------------------------------------------------

ccaWorkingKey :: Text
ccaWorkingKey = "ABCDEF1234567890ABCDEF1234567890"

ccaSamplePayload :: Text
ccaSamplePayload = "{\"reference_no\":\"109810375484\"}"

-- ---------------------------------------------------------------------------
-- CCAvenue tests
-- ---------------------------------------------------------------------------

ccAvenueTests :: [TestTree]
ccAvenueTests =
  [ testCase "AES-128-CBC round-trip: encrypt then decrypt recovers plaintext" $ do
      case ccaEncrypt ccaWorkingKey ccaSamplePayload of
        Left err -> assertFailure $ "ccaEncrypt failed: " <> T.unpack err
        Right encrypted -> do
          -- encrypted should be a hex string (all hex chars)
          assertBool "Encrypted output should be hex characters" $
            T.all (\c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) encrypted

          case ccaDecrypt ccaWorkingKey encrypted of
            Left err -> assertFailure $ "ccaDecrypt failed: " <> T.unpack err
            Right decrypted -> decrypted @?= ccaSamplePayload,
    testCase "Decrypt fails with wrong key" $ do
      case ccaEncrypt ccaWorkingKey ccaSamplePayload of
        Left err -> assertFailure $ "ccaEncrypt failed: " <> T.unpack err
        Right encrypted -> do
          let wrongKey = "WRONG_KEY_WRONG_KEY_WRONG_KEY_00"
          case ccaDecrypt wrongKey encrypted of
            Left _ -> pure () -- expected: decryption error or garbled output
            Right decrypted ->
              -- even if decryption "succeeds" with wrong key, output should differ
              assertBool "Decrypted with wrong key should not match original" $
                decrypted /= ccaSamplePayload,
    testCase "Encrypting same plaintext twice produces same ciphertext (fixed IV)" $ do
      -- CCAvenue uses a fixed IV, so same input -> same output (deterministic)
      case (ccaEncrypt ccaWorkingKey ccaSamplePayload, ccaEncrypt ccaWorkingKey ccaSamplePayload) of
        (Right enc1, Right enc2) -> enc1 @?= enc2
        _ -> assertFailure "ccaEncrypt failed",
    testCase "Round-trip with JSON settlement request" $ do
      let jsonPayload = "{\"order_no\":\"ORD123\",\"reference_no\":\"REF456\"}"
      case ccaEncrypt ccaWorkingKey jsonPayload of
        Left err -> assertFailure $ "ccaEncrypt failed: " <> T.unpack err
        Right encrypted -> do
          case ccaDecrypt ccaWorkingKey encrypted of
            Left err -> assertFailure $ "ccaDecrypt failed: " <> T.unpack err
            Right decrypted -> do
              decrypted @?= jsonPayload
              -- verify it's valid JSON after round-trip
              let parsed = A.decode (LBS.fromStrict $ TE.encodeUtf8 decrypted) :: Maybe A.Value
              assertBool "Decrypted output should be valid JSON" $ isJust parsed,
    testCase "Round-trip with empty string" $ do
      case ccaEncrypt ccaWorkingKey "" of
        Left err -> assertFailure $ "ccaEncrypt empty failed: " <> T.unpack err
        Right encrypted -> do
          assertBool "Encrypted output should not be empty" $ not (T.null encrypted)
          case ccaDecrypt ccaWorkingKey encrypted of
            Left err -> assertFailure $ "ccaDecrypt empty failed: " <> T.unpack err
            Right decrypted -> decrypted @?= "",
    testCase "Round-trip with large payload" $ do
      let largePayload = T.replicate 10000 "abcdefghij"
      case ccaEncrypt ccaWorkingKey largePayload of
        Left err -> assertFailure $ "ccaEncrypt large failed: " <> T.unpack err
        Right encrypted -> do
          case ccaDecrypt ccaWorkingKey encrypted of
            Left err -> assertFailure $ "ccaDecrypt large failed: " <> T.unpack err
            Right decrypted -> decrypted @?= largePayload
  ]
