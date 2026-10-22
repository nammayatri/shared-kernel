{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PackageImports #-}

module MorthV2CryptoTests (morthV2CryptoTests) where

import qualified Data.ByteString as BS
import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import EulerHS.Prelude
import Kernel.External.Verification.MorthV2.Crypto
import Test.Tasty
import Test.Tasty.HUnit

morthV2CryptoTests :: TestTree
morthV2CryptoTests =
  testGroup
    "MorthV2.Crypto"
    [ testCase "round-trip returns original plaintext" roundTrip,
      testCase "two encrypts of the same plaintext produce different ciphertexts (random IV + salt)" randomness,
      testCase "wire layout is IV(12) + Salt(16) + CT + Tag(16)" wireLayout,
      testCase "wrong apiKey fails decryption" wrongKey,
      testCase "truncated payload surfaces CryptoPayloadTooShort" tooShort,
      testCase "non-base64 input surfaces CryptoBase64DecodeError" badBase64
    ]

apiKey :: Text
apiKey = "test-api-key-0506D4B8user"

plaintext :: ByteString
plaintext = "{\"clientId\":\"0506D4B8user\"}"

roundTrip :: Assertion
roundTrip = do
  wire <- encryptEnvelopeData apiKey plaintext
  decryptEnvelopeData apiKey wire @?= Right plaintext

randomness :: Assertion
randomness = do
  w1 <- encryptEnvelopeData apiKey plaintext
  w2 <- encryptEnvelopeData apiKey plaintext
  assertBool "ciphertexts must differ (random IV/salt)" (w1 /= w2)
  -- both should still decrypt to the same plaintext
  decryptEnvelopeData apiKey w1 @?= Right plaintext
  decryptEnvelopeData apiKey w2 @?= Right plaintext

wireLayout :: Assertion
wireLayout = do
  wire <- encryptEnvelopeData apiKey plaintext
  case B64.decode (encodeUtf8 wire :: ByteString) of
    Left err -> assertFailure $ "base64 decode failed: " <> err
    Right raw -> do
      -- Must contain at least IV + Salt + Tag + 1 byte of CT.
      assertBool "wire >= 45 bytes" (BS.length raw >= 12 + 16 + 16 + BS.length plaintext)
      -- Total length = IV + Salt + CT + Tag, where CT length == plaintext length (GCM has no padding).
      BS.length raw @?= 12 + 16 + BS.length plaintext + 16

wrongKey :: Assertion
wrongKey = do
  wire <- encryptEnvelopeData apiKey plaintext
  case decryptEnvelopeData "a-different-key" wire of
    Left CryptoDecryptionFailed -> pure ()
    other -> assertFailure $ "expected CryptoDecryptionFailed, got: " <> show other

tooShort :: Assertion
tooShort = do
  -- Base64 of 10 bytes — far below the 44-byte minimum (IV + Salt + Tag).
  let shortWire = decodeUtf8 (B64.encode (BS.replicate 10 0)) :: Text
  case decryptEnvelopeData apiKey shortWire of
    Left (CryptoPayloadTooShort n) -> n @?= 10
    other -> assertFailure $ "expected CryptoPayloadTooShort, got: " <> show other

badBase64 :: Assertion
badBase64 =
  case decryptEnvelopeData apiKey "!!! not base64 !!!" of
    Left (CryptoBase64DecodeError _) -> pure ()
    other -> assertFailure $ "expected CryptoBase64DecodeError, got: " <> show other
