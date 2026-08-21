{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Round-trip and negative tests for 'Kernel.Utils.Jose'.
--
-- These prove the implementation is self-consistent. They do /not/ prove we agree with any
-- particular counterparty: for that, a fixture test decrypting the partner's own published
-- sample envelope with their sample key is required, and should be added here once the
-- sample material is checked in.
module Jose (joseTests) where

import qualified Crypto.PubKey.RSA as RSA
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import EulerHS.Prelude
import Kernel.Utils.Jose
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Test.Tasty
import Test.Tasty.HUnit

genKeyPair :: IO (RSA.PublicKey, RSA.PrivateKey)
genKeyPair = RSA.generate 256 65537 -- 2048-bit

payload :: ByteString
payload = "{\"filerefno\":\"204817\",\"nooftran\":2}"

jwsRoundTrip :: TestTree
jwsRoundTrip = testCase "JWS: sign then verify returns the original payload" $ do
  (pub, priv) <- genKeyPair
  case signJWS priv (kidOf pub) payload >>= verifyJWS pub of
    Left err -> assertFailure $ "expected round trip, got " <> show err
    Right out -> out @?= payload

jwsCompactShape :: TestTree
jwsCompactShape = testCase "JWS: compact form has three dot-separated parts" $ do
  (pub, priv) <- genKeyPair
  case signJWS priv (kidOf pub) payload of
    Left err -> assertFailure $ "signing failed: " <> show err
    Right compact -> length (T.splitOn "." compact) @?= 3

jwsWrongKeyRejected :: TestTree
jwsWrongKeyRejected = testCase "JWS: a signature from another key does not verify" $ do
  (_, privA) <- genKeyPair
  (pubB, _) <- genKeyPair
  case signJWS privA "kid-a" payload >>= verifyJWS pubB of
    Left SignatureVerificationFailed -> pure ()
    Left err -> assertFailure $ "expected SignatureVerificationFailed, got " <> show err
    Right _ -> assertFailure "a signature from the wrong key verified"

jwsTamperedRejected :: TestTree
jwsTamperedRejected = testCase "JWS: altering the payload invalidates the signature" $ do
  (pub, priv) <- genKeyPair
  case signJWS priv (kidOf pub) payload of
    Left err -> assertFailure $ "signing failed: " <> show err
    Right compact -> do
      let tampered = T.replace "." ".x" compact -- corrupts the payload segment
      case verifyJWS pub tampered of
        Right _ -> assertFailure "a tampered JWS verified"
        Left _ -> pure ()

jweRoundTrip :: TestTree
jweRoundTrip = testCase "JWE: encrypt then decrypt returns the original plaintext" $ do
  (pub, priv) <- genKeyPair
  eCompact <- encryptJWE pub (kidOf pub) payload
  case eCompact of
    Left err -> assertFailure $ "encryption failed: " <> show err
    Right compact -> case decryptJWE priv compact of
      Left err -> assertFailure $ "decryption failed: " <> show err
      Right out -> out @?= payload

jweCompactShape :: TestTree
jweCompactShape = testCase "JWE: compact form has five dot-separated parts" $ do
  (pub, _) <- genKeyPair
  eCompact <- encryptJWE pub (kidOf pub) payload
  case eCompact of
    Left err -> assertFailure $ "encryption failed: " <> show err
    Right compact -> length (T.splitOn "." compact) @?= 5

jweTamperedTagRejected :: TestTree
jweTamperedTagRejected = testCase "JWE: a corrupted ciphertext fails the authentication tag" $ do
  (pub, priv) <- genKeyPair
  eCompact <- encryptJWE pub (kidOf pub) payload
  case eCompact of
    Left err -> assertFailure $ "encryption failed: " <> show err
    Right compact -> do
      let parts = T.splitOn "." compact
      case parts of
        [h, k, iv, ct, tag] -> do
          let flipped = T.intercalate "." [h, k, iv, T.reverse ct, tag]
          case decryptJWE priv flipped of
            Right _ -> assertFailure "a corrupted JWE decrypted successfully"
            Left _ -> pure ()
        _ -> assertFailure "unexpected compact shape"

nestedRoundTrip :: TestTree
nestedRoundTrip = testCase "Nested: JWS inside JWE survives both layers" $ do
  (ourPub, ourPriv) <- genKeyPair
  (bankPub, bankPriv) <- genKeyPair
  -- what a client sends: sign with ours, encrypt to theirs
  case signJWS ourPriv (kidOf ourPub) payload of
    Left err -> assertFailure $ "signing failed: " <> show err
    Right jws -> do
      eEnvelope <- encryptJWE bankPub (kidOf bankPub) (C8.pack (T.unpack jws))
      case eEnvelope of
        Left err -> assertFailure $ "encryption failed: " <> show err
        Right envelope ->
          -- what the counterparty does: decrypt with theirs, verify with ours
          case decryptJWE bankPriv envelope of
            Left err -> assertFailure $ "decryption failed: " <> show err
            Right innerRaw -> case verifyJWS ourPub (T.pack (C8.unpack innerRaw)) of
              Left err -> assertFailure $ "verification failed: " <> show err
              Right out -> out @?= payload

kidIsStable :: TestTree
kidIsStable = testCase "kid: same key gives the same id, different keys differ" $ do
  (pubA, _) <- genKeyPair
  (pubB, _) <- genKeyPair
  kidOf pubA @?= kidOf pubA
  assertBool "distinct keys produced the same kid" (kidOf pubA /= kidOf pubB)

kidIsBase64UrlUnpadded :: TestTree
kidIsBase64UrlUnpadded = testCase "kid: base64url, unpadded, 43 chars for a SHA-256" $ do
  (pub, _) <- genKeyPair
  let k = kidOf pub
  T.length k @?= 43
  assertBool "kid contains padding" (not $ T.isInfixOf "=" k)
  assertBool "kid contains non-url-safe characters" (not $ T.any (\c -> c == '+' || c == '/') k)

-- | Opens an envelope produced by a *different* JOSE implementation.
--
-- Everything above round-trips our own output through our own code, which passes even if
-- the key directions are swapped or the AAD is wrong -- both ends would be wrong the same
-- way. This is the only test here that can detect that class of mistake.
--
-- Skipped unless JOSE_INTEROP_DIR points at a directory containing the fixture; generate it
-- with Backend/app/mocks/hdfc-cbx/make_interop_fixture.py.
interopWithForeignImplementation :: TestTree
interopWithForeignImplementation =
  testCase "Interop: an envelope built by another library opens correctly" $ do
    mbDir <- lookupEnv "JOSE_INTEROP_DIR"
    case mbDir of
      Nothing -> pure () -- not configured; nothing to assert
      Just dir -> do
        let envelopePath = dir <> "/python-to-haskell.jwe"
            payloadPath = dir <> "/payload.json"
        present <- doesFileExist envelopePath
        if not present
          then assertFailure $ "JOSE_INTEROP_DIR set but no fixture at " <> envelopePath
          else do
            envelope <- T.strip <$> TIO.readFile envelopePath
            expected <- C8.pack . T.unpack . T.strip <$> TIO.readFile payloadPath
            bankPriv <- readPriv $ dir <> "/../keys/bank.key"
            clientPub <- readPub $ dir <> "/../keys/client.pub"
            -- the adapter's inbound path in miniature: decrypt with ours, verify with theirs
            case decryptJWE bankPriv envelope of
              Left err -> assertFailure $ "could not decrypt a foreign JWE: " <> show err
              Right innerRaw -> case verifyJWS clientPub (T.pack (C8.unpack innerRaw)) of
                Left err -> assertFailure $ "could not verify a foreign JWS: " <> show err
                Right decoded -> decoded @?= expected
  where
    readPriv path = do
      pem <- TIO.readFile path
      either (\e -> assertFailure ("private key: " <> show e) >> fail "unreachable") pure (parseRsaPrivateKeyPem pem)
    readPub path = do
      pem <- TIO.readFile path
      either (\e -> assertFailure ("public key: " <> show e) >> fail "unreachable") pure (parseRsaPublicKeyPem pem)

joseTests :: TestTree
joseTests =
  testGroup
    "JOSE"
    [ jwsRoundTrip,
      jwsCompactShape,
      jwsWrongKeyRejected,
      jwsTamperedRejected,
      jweRoundTrip,
      jweCompactShape,
      jweTamperedTagRejected,
      nestedRoundTrip,
      kidIsStable,
      kidIsBase64UrlUnpadded,
      interopWithForeignImplementation
    ]
