 {-
  Copyright 2022-23, Juspay India Pvt Ltd
  
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
  
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is 
  
  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
  
  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero 
  
  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE PackageImports #-}

module SignatureAuth
  ( signatureAuthTests,
  )
where

import qualified Data.ByteString as BS
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime)
import EulerHS.Prelude
import Kernel.Types.Base64
import qualified Kernel.Utils.SignatureAuth as HttpSig
import Network.HTTP.Types (methodPost)
import Test.Tasty
import Test.Tasty.HUnit

secretKey :: Base64
secretKey = "faGTaRPYJx8OQ7zbQOlrevtWTmqs+hAJr2tn08Dgx1U="

publicKey :: Base64
publicKey = "oUvUHbL/9ZU4JT0RjcnAvgSXpXtyNTDBWRWDpnlP4N0="

examplePath :: ByteString
examplePath = "/foo?param=value&pet=dog"

exampleMethod :: ByteString
exampleMethod = methodPost

exampleHeaders :: [(Text, ByteString)]
exampleHeaders =
  [ ("(request-target)", exampleMethod <> " " <> examplePath),
    ("(created)", ""),
    ("(expires)", ""),
    ("host", "example.com"),
    ("date", "Sun, 05 Jan 2014 21:31:40 GMT"),
    ("content-type", "application/json"),
    ("digest", "BLAKE-512=20cb8f1175aaa3f23f020b3962300c483ba33dda3f1ae32734605db4d834419f874f19963636ff0c79d45a054af895b20fdac745f354c865d938ef6e801b8e33"),
    ("content-length", "18")
  ]

exampleBodyHash :: HttpSig.Hash
exampleBodyHash = HttpSig.becknSignatureHash "{\"hello\": \"world\"}"

exampleKeyId :: ByteString
exampleKeyId = "example-bg.com|bg432|ed25519"

exampleCreated :: Integer
exampleCreated = 1402170695

exampleExpires :: Integer
exampleExpires = 1402170699

exampleSignature :: ByteString
exampleSignature = "WJoZi2DBgQQ1ChHus4F6L0S4pOVeSvhmViZK3W0x+kCpmrVC6cgcmj2x7P2g5fHjft1OB5LKJLmIid2qd1XZCA=="

exampleSignatureMessage :: ByteString
exampleSignatureMessage =
  encodeUtf8 $
    unlines
      [ "(request-target): " <> decodeUtf8 methodPost <> " " <> decodeUtf8 examplePath,
        "(created): " <> show exampleCreated,
        "(expires): " <> show exampleExpires,
        "host: example.com",
        "date: Sun, 05 Jan 2014 21:31:40 GMT",
        "content-type: application/json",
        "digest: BLAKE-512=IMuPEXWqo/I/Ags5YjAMSDujPdo/GuMnNGBdtNg0QZ+HTxmWNjb/DHnUWgVK+JWyD9rHRfNUyGXZOO9ugBuOMw==",
        "content-length: 18"
      ]

-- | algorithm is modified to hs2019
exampleSignatureHeader :: ByteString
exampleSignatureHeader =
  "Signature keyId=\"" <> exampleKeyId <> "\",algorithm=\"ed25519\",\n"
    <> "created="
    <> show exampleCreated
    <> ",expires="
    <> show exampleExpires
    <> ",\n"
    <> "headers=\""
    <> (encodeUtf8 . T.intercalate " " $ fst <$> exampleHeaders)
    <> "\",\n"
    <> "signature=\""
    <> exampleSignature
    <> "\""

toTime :: Integer -> Maybe NominalDiffTime
toTime = Just . fromInteger

simpleDecode :: TestTree
simpleDecode =
  testCase "Simple header decode" $
    case HttpSig.decode exampleSignatureHeader of
      Left err -> assertFailure $ "signature decode failed: " <> err
      Right HttpSig.SignaturePayload {..} -> do
        Right signature @?= Base64.decode exampleSignature
        HttpSig.headers params @?= fst <$> exampleHeaders
        HttpSig.algorithm params @?= HttpSig.Ed25519
        HttpSig.created params @?= toTime exampleCreated
        HttpSig.expires params @?= toTime exampleExpires

exampleParams :: HttpSig.SignatureParams
exampleParams =
  let Right keyId = HttpSig.decodeKeyId $ decodeUtf8 exampleKeyId
   in HttpSig.SignatureParams
        keyId
        HttpSig.Ed25519
        (fst <$> exampleHeaders)
        (toTime exampleCreated)
        (toTime exampleExpires)

dropNewline :: ByteString -> ByteString
dropNewline = BS.filter (/= 10)

simpleEncode :: TestTree
simpleEncode =
  testCase "Simple header encode" $ do
    let Right sig = Base64.decode exampleSignature
    -- filtering '\n'
    HttpSig.encode (HttpSig.SignaturePayload sig exampleParams) @?= dropNewline exampleSignatureHeader

checkSignatureMessage :: TestTree
checkSignatureMessage =
  testCase "Check Signature Message" $ do
    let message = HttpSig.makeSignatureString exampleParams exampleBodyHash $ first (CI.mk . encodeUtf8) <$> exampleHeaders
    -- filtering '\n'
    dropNewline message @?= dropNewline exampleSignatureMessage

signRequest :: TestTree
signRequest =
  testCase "Sign a request" $ do
    let mSig =
          HttpSig.sign secretKey exampleParams exampleBodyHash (first (CI.mk . encodeUtf8) <$> exampleHeaders)
    case mSig of
      Nothing -> assertFailure "Could not sign request"
      Just sig ->
        -- filtering '\n'
        HttpSig.encode (HttpSig.SignaturePayload sig exampleParams) @?= dropNewline exampleSignatureHeader

verifyRequest :: TestTree
verifyRequest =
  testCase "Verify a signed request" $ do
    let eSig = Base64.decode exampleSignature
    case eSig of
      Left err -> assertFailure $ "Could not decode request: " <> show err
      Right sig -> do
        case HttpSig.verify publicKey exampleParams exampleBodyHash (first (CI.mk . encodeUtf8) <$> exampleHeaders) sig of
          Left err -> assertFailure $ "Could not verify signature: " <> show err
          Right isVerified -> assertBool "Signature is valid" isVerified

signAndVerifyWithGeneratedKeyPair :: TestTree
signAndVerifyWithGeneratedKeyPair =
  testCase "Sign and verify a request with generated key pair" $ do
    (privateKey, publicKey') <- HttpSig.generateKeyPair
    let mSig =
          HttpSig.sign privateKey exampleParams exampleBodyHash (first (CI.mk . encodeUtf8) <$> exampleHeaders)
    case mSig of
      Nothing -> assertFailure "Could not sign request"
      Just sig -> do
        case HttpSig.verify publicKey' exampleParams exampleBodyHash (first (CI.mk . encodeUtf8) <$> exampleHeaders) sig of
          Left err -> assertFailure $ "Could not verify signature: " <> show err
          Right isVerified -> assertBool "Signature is valid" isVerified

signatureAuthTests :: TestTree
signatureAuthTests =
  testGroup
    "Signature auth tests"
    [ simpleDecode,
      simpleEncode,
      checkSignatureMessage,
      signRequest,
      verifyRequest,
      signAndVerifyWithGeneratedKeyPair
    ]
