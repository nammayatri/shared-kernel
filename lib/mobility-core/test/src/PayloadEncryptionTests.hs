{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module PayloadEncryptionTests
  ( payloadEncryptionTests,
  )
where

import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Cipher.Types as Cipher
import Crypto.Error (CryptoFailable, throwCryptoError)
import qualified Crypto.Hash.Algorithms as Hash
import qualified Crypto.KDF.HKDF as HKDF
import qualified Crypto.PubKey.Curve25519 as X25519
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.List (lookup)
import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import EulerHS.Prelude
import Kernel.Utils.PayloadEncryption
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal (ResponseReceived (..))
import Test.Tasty
import Test.Tasty.HUnit

-- | An independent client for the scheme, mirroring what the apps do.
data Client = Client
  { request :: Request,
    sealedBody :: ByteString,
    prk :: HKDF.PRK Hash.SHA256,
    aad :: ByteString
  }

joinAad :: [ByteString] -> ByteString
joinAad = BS.intercalate "\0"

aead :: ByteString -> Cipher.AEAD AES.AES256
aead material =
  let (key, nonce) = BS.splitAt 32 material
   in throwCryptoError (Cipher.aeadInit Cipher.AEAD_GCM (throwCryptoError (Cipher.cipherInit key :: CryptoFailable AES.AES256)) nonce)

seal :: ByteString -> ByteString -> ByteString -> ByteString
seal material aad' plaintext =
  let (Cipher.AuthTag tag, ciphertext) = Cipher.aeadSimpleEncrypt (aead material) aad' plaintext 16
   in ciphertext <> BA.convert tag

open :: ByteString -> ByteString -> ByteString -> Maybe ByteString
open material aad' sealed =
  let (ciphertext, tag) = BS.splitAt (BS.length sealed - 16) sealed
   in Cipher.aeadSimpleDecrypt (aead material) aad' ciphertext (Cipher.AuthTag (BA.convert tag))

expand :: HKDF.PRK Hash.SHA256 -> ByteString -> ByteString
expand prk' info = HKDF.expand prk' info 44

-- | A one-shot body reader that records whether the middleware consumed it.
oneShotBody :: IORef Bool -> ByteString -> IO (IO ByteString)
oneShotBody wasRead body = do
  remaining <- newIORef (Just body)
  pure $ do
    writeIORef wasRead True
    atomicModifyIORef' remaining (\chunk -> (Nothing, fromMaybe BS.empty chunk))

mkClient :: X25519.PublicKey -> Method -> ByteString -> Integer -> ByteString -> Maybe ByteString -> IORef Bool -> IO Client
mkClient = mkClientWith "application/json"

-- | A client sealing a body of the given content type (e.g. a multipart upload with its boundary).
mkClientWith :: ByteString -> X25519.PublicKey -> Method -> ByteString -> Integer -> ByteString -> Maybe ByteString -> IORef Bool -> IO Client
mkClientWith contentType serverPub method path ts body mbToken wasRead = do
  temp <- X25519.generateSecretKey
  let secretA = BA.convert (X25519.dh serverPub temp) :: ByteString
      prk' = HKDF.extract ("ny-enc-v1" :: ByteString) secretA :: HKDF.PRK Hash.SHA256
      tsRaw = BS8.pack (show ts)
      aad' = joinAad ["ny-enc-v1", method, path, tsRaw, contentType]
      bodiless = method `elem` [methodGet, methodHead]
      sealed = if bodiless then BS.empty else seal (expand prk' "request") aad' body
      tokenHeader = maybe [] (\t -> [("x-enc-token", B64.encode (seal (expand prk' "token") aad' t))]) mbToken
  bodyReader <- oneShotBody wasRead sealed
  let req =
        defaultRequest
          { requestBody = bodyReader,
            requestMethod = method,
            rawPathInfo = path,
            requestHeaders =
              [ ("x-enc-version", "v1"),
                ("x-enc-kid", "v1"),
                ("x-enc", B64.encode (BA.convert (X25519.toPublic temp))),
                ("x-enc-ts", tsRaw),
                ("x-enc-content-type", contentType),
                (hContentType, "application/octet-stream"),
                ("x-trace", "kept")
              ]
                <> tokenHeader,
            requestBodyLength = KnownLength (fromIntegral (BS.length sealed))
          }
  pure Client {request = req, sealedBody = sealed, prk = prk', aad = aad'}

type Captured = (Status, ResponseHeaders, ByteString)

run :: Middleware -> Application -> Request -> IO Captured
run middleware app req = do
  out <- newIORef Nothing
  _ <-
    middleware app req $ \response -> do
      let (status, headers, withBody) = responseToStream response
      acc <- newIORef mempty
      withBody (\streamingBody -> streamingBody (\chunk -> modifyIORef' acc (<> chunk)) (pure ()))
      body <- BSL.toStrict . Builder.toLazyByteString <$> readIORef acc
      writeIORef out (Just (status, headers, body))
      pure ResponseReceived
  readIORef out >>= maybe (assertFailure "middleware did not respond") pure

openResponse :: Client -> Captured -> Maybe ByteString
openResponse client (status, headers, body) = do
  encodedNonce <- lookup "x-enc-resp-nonce" headers
  nonce <- either (const Nothing) Just (B64.decode encodedNonce)
  open (expand client.prk ("response" <> nonce)) (joinAad [client.aad, "response", show (statusCode status)]) body

isError :: ByteString -> Captured -> Bool
isError code (status, _, body) = statusCode status == 400 && code `BS.isInfixOf` body

sealedTokenOf :: Client -> IO ByteString
sealedTokenOf client = maybe (assertFailure "client sent no x-enc-token") pure (lookup "x-enc-token" (requestHeaders client.request))

withHeaders :: [Header] -> Request -> Request
withHeaders overrides req =
  req {requestHeaders = overrides <> filter ((`notElem` map fst overrides) . fst) (requestHeaders req)}

-- | A file part with bytes that are not valid UTF-8, plus a text field.
multipartType, multipartBody :: ByteString
multipartType = "multipart/form-data; boundary=ny-test-boundary"
multipartBody =
  BS.concat
    [ "--ny-test-boundary\r\nContent-Disposition: form-data; name=\"file\"; filename=\"voice.m4a\"\r\nContent-Type: audio/mp4\r\n\r\n",
      BS.pack [0 .. 255],
      "\r\n--ny-test-boundary\r\nContent-Disposition: form-data; name=\"fileType\"\r\n\r\nAudio\r\n--ny-test-boundary--\r\n"
    ]

payloadEncryptionTests :: TestTree
payloadEncryptionTests = withResource setup (const pass) $ \getEnv ->
  testGroup
    "PayloadEncryption"
    [ testCase "round trip: handler gets plain body and headers, client opens the response" $ do
        (serverPub, middleware, seen, echo) <- getEnv
        ts <- now
        client <- newIORef False >>= mkClient serverPub methodPost "/ui/driver/ride/accept" ts rideBody Nothing
        resp <- run middleware echo client.request
        (headers, body) <- readIORef seen
        body @?= rideBody
        lookup hContentType headers @?= Just "application/json"
        lookup "x-trace" headers @?= Just "kept"
        assertBool "no x-enc* header reaches the handler" (not (any (("x-enc" `BS.isPrefixOf`) . CI.foldedCase . fst) headers))
        openResponse client resp @?= Just ("{\"echo\":" <> rideBody <> "}"),
      testCase "sealed token is restored into the token header" $ do
        (serverPub, middleware, seen, echo) <- getEnv
        ts <- now
        client <- newIORef False >>= mkClient serverPub methodPost "/ui/driver/ride/accept" ts rideBody (Just "token-abc")
        resp <- run middleware echo client.request
        (headers, _) <- readIORef seen
        lookup "token" headers @?= Just "token-abc"
        assertBool "response still opens" (isJust (openResponse client resp)),
      testCase "tampered token is rejected" $ do
        (serverPub, middleware, _, echo) <- getEnv
        ts <- now
        client <- newIORef False >>= mkClient serverPub methodPost "/ui/driver/ride/accept" ts rideBody (Just "token-abc")
        sealedToken <- sealedTokenOf client >>= either (const $ assertFailure "x-enc-token is not base64") pure . B64.decode
        let flipped = B64.encode (BS.cons (BS.head sealedToken + 1) (BS.tail sealedToken))
        resp <- run middleware echo (withHeaders [("x-enc-token", flipped)] client.request)
        assertBool "PAYLOAD_TOKEN_DECRYPTION_FAILED" (isError "PAYLOAD_TOKEN_DECRYPTION_FAILED" resp),
      testCase "a sealed token cannot be lifted onto another request" $ do
        (serverPub, middleware, _, echo) <- getEnv
        ts <- now
        victim <- newIORef False >>= mkClient serverPub methodPost "/ui/driver/ride/accept" ts rideBody (Just "token-abc")
        attacker <- newIORef False >>= mkClient serverPub methodPost "/ui/driver/payout" ts rideBody Nothing
        stolen <- sealedTokenOf victim
        resp <- run middleware echo (withHeaders [("x-enc-token", stolen)] attacker.request)
        assertBool "PAYLOAD_TOKEN_DECRYPTION_FAILED" (isError "PAYLOAD_TOKEN_DECRYPTION_FAILED" resp),
      testCase "sealed and plain token together are rejected" $ do
        (serverPub, middleware, _, echo) <- getEnv
        ts <- now
        client <- newIORef False >>= mkClient serverPub methodPost "/ui/driver/ride/accept" ts rideBody (Just "token-abc")
        resp <- run middleware echo (withHeaders [("token", "token-other")] client.request)
        assertBool "PAYLOAD_ENCRYPTION_DUPLICATE_TOKEN" (isError "PAYLOAD_ENCRYPTION_DUPLICATE_TOKEN" resp),
      testCase "bad headers are rejected before the body is read" $ do
        (serverPub, middleware, _, echo) <- getEnv
        ts <- now
        forM_
          [ ([("x-enc-kid", "v9")], ts, "PAYLOAD_ENCRYPTION_UNKNOWN_KEY_ID"),
            ([("x-enc", B64.encode (BS.replicate 32 0))], ts, "PAYLOAD_ENCRYPTION_INVALID_CLIENT_KEY")
          ]
          $ \(overrides, requestTs, code) -> do
            wasRead <- newIORef False
            client <- mkClient serverPub methodPost "/ui/driver/ride/accept" requestTs rideBody Nothing wasRead
            resp <- run middleware echo (withHeaders overrides client.request)
            assertBool (show code) (isError code resp)
            readIORef wasRead >>= assertBool "body must not be read" . not,
      testCase "an old timestamp is accepted (no clock-skew check)" $ do
        (serverPub, middleware, _, echo) <- getEnv
        ts <- now
        client <- newIORef False >>= mkClient serverPub methodPost "/ui/driver/ride/accept" (ts - 86400) rideBody Nothing
        resp <- run middleware echo client.request
        openResponse client resp @?= Just ("{\"echo\":" <> rideBody <> "}"),
      testCase "tampered body is rejected" $ do
        (serverPub, middleware, _, echo) <- getEnv
        ts <- now
        client <- newIORef False >>= mkClient serverPub methodPost "/ui/driver/ride/accept" ts rideBody Nothing
        bodyReader <- newIORef False >>= \r -> oneShotBody r (BS.cons (BS.head client.sealedBody + 1) (BS.tail client.sealedBody))
        resp <- run middleware echo (client.request {requestBody = bodyReader})
        assertBool "PAYLOAD_DECRYPTION_FAILED" (isError "PAYLOAD_DECRYPTION_FAILED" resp),
      testCase "bodiless responses (HEAD, 204) are not sealed" $ do
        (serverPub, middleware, _, echo) <- getEnv
        ts <- now
        headClient <- newIORef False >>= mkClient serverPub methodHead "/ui/driver/profile" ts "" Nothing
        (_, headHeaders, _) <- run middleware echo headClient.request
        lookup "x-enc-resp-nonce" headHeaders @?= Nothing
        noContentClient <- newIORef False >>= mkClient serverPub methodPost "/ui/driver/ride/accept" ts rideBody Nothing
        (status, headers, _) <- run middleware (\_ respond -> respond (responseLBS status204 [] "")) noContentClient.request
        statusCode status @?= 204
        lookup "x-enc-resp-nonce" headers @?= Nothing,
      testCase "GET carries no body but its response is sealed" $ do
        (serverPub, middleware, _, echo) <- getEnv
        ts <- now
        client <- newIORef False >>= mkClient serverPub methodGet "/ui/driver/profile" ts "" (Just "token-abc")
        resp <- run middleware echo client.request
        openResponse client resp @?= Just "{\"echo\":}",
      testCase "old app (no x-enc-version) passes through untouched" $ do
        (_, middleware, _, echo) <- getEnv
        wasRead <- newIORef False
        bodyReader <- oneShotBody wasRead rideBody
        (_, headers, body) <- run middleware echo defaultRequest {requestBody = bodyReader, requestMethod = methodPost, requestHeaders = [("token", "token-abc")]}
        lookup "x-enc-resp-nonce" headers @?= Nothing
        body @?= "{\"echo\":" <> rideBody <> "}",
      testCase "multipart upload: handler gets the exact bytes, its boundary and the token" $ do
        (serverPub, middleware, seen, echo) <- getEnv
        ts <- now
        client <- newIORef False >>= mkClientWith multipartType serverPub methodPost "/ui/issue/upload" ts multipartBody (Just "token-abc")
        resp <- run middleware echo client.request
        (headers, body) <- readIORef seen
        body @?= multipartBody
        lookup hContentType headers @?= Just multipartType
        lookup "token" headers @?= Just "token-abc"
        assertBool "response is sealed" (isJust (openResponse client resp)),
      testCase "multipart content type is bound: a swapped boundary is rejected" $ do
        (serverPub, middleware, _, echo) <- getEnv
        ts <- now
        client <- newIORef False >>= mkClientWith multipartType serverPub methodPost "/ui/issue/upload" ts multipartBody Nothing
        resp <- run middleware echo (withHeaders [("x-enc-content-type", "multipart/form-data; boundary=other")] client.request)
        assertBool "rejected" (isError "PAYLOAD_DECRYPTION_FAILED" resp),
      testCase "unsupported x-enc-version is rejected" $ do
        (serverPub, middleware, _, echo) <- getEnv
        ts <- now
        client <- newIORef False >>= mkClient serverPub methodPost "/ui/driver/ride/accept" ts rideBody Nothing
        resp <- run middleware echo (withHeaders [("x-enc-version", "v9")] client.request)
        assertBool "PAYLOAD_ENCRYPTION_UNSUPPORTED_VERSION" (isError "PAYLOAD_ENCRYPTION_UNSUPPORTED_VERSION" resp)
    ]
  where
    rideBody = "{\"rideId\":\"r-555\"}"
    now = floor <$> getPOSIXTime :: IO Integer
    setup = do
      serverPriv <- X25519.generateSecretKey
      seen <- newIORef ([], BS.empty)
      let middleware = payloadEncryptionMiddleware (PayloadEncryptionConfig (Map.fromList [("v1", serverPriv)]))
          echo req respond = do
            body <- BSL.toStrict <$> strictRequestBody req
            writeIORef seen (requestHeaders req, body)
            respond (responseLBS status200 [(hContentType, "application/json")] (BSL.fromStrict ("{\"echo\":" <> body <> "}")))
      pure (X25519.toPublic serverPriv, middleware, seen, echo)
