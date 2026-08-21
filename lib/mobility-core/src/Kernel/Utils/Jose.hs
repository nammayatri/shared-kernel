{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | JOSE: JWS signing and JWE encryption, in the nested arrangement banks commonly require --
-- sign the plaintext, then encrypt the signature envelope.
--
-- Deliberately not placed under any one partner: this is standard JOSE and more than one
-- integration uses it.
--
-- Supports exactly one algorithm per layer, because supporting more means negotiating, and
-- algorithm negotiation is how @alg: none@ gets accepted:
--
--   * JWS: @RS256@ (RSASSA-PKCS1-v1_5 with SHA-256)
--   * JWE: @RSA-OAEP-256@ key wrapping with @A256GCM@ content encryption
module Kernel.Utils.Jose
  ( JoseError (..),
    signJWS,
    verifyJWS,
    encryptJWE,
    decryptJWE,
    kidOf,
    parseRsaPublicKeyPem,
    parseRsaPrivateKeyPem,
    publicOf,
    RSA.PublicKey,
    RSA.PrivateKey,
  )
where

import qualified Crypto.Cipher.AES as Cipher
import qualified Crypto.Cipher.Types as Cipher
import qualified Crypto.Error as CE
import Crypto.Hash (Digest, hashWith)
import Crypto.Hash.Algorithms (SHA256 (..))
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.OAEP as OAEP
import qualified Crypto.PubKey.RSA.PKCS15 as PKCS15
import Crypto.Random.Types (MonadRandom (..))
import qualified Data.ASN1.BinaryEncoding as ASN1
import qualified Data.ASN1.Encoding as ASN1
import qualified Data.ASN1.Types as ASN1
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertFromBase, convertToBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.PEM as PEM
import qualified Data.Text as T
import qualified Data.X509 as X509
import Kernel.Prelude

data JoseError
  = MalformedCompact Text
  | UnsupportedAlgorithm Text
  | SignatureVerificationFailed
  | DecryptionFailed Text
  | KeyParseFailed Text
  | Base64DecodeFailed Text
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- base64url, no padding
--------------------------------------------------------------------------------

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

b64u :: BS.ByteString -> BS.ByteString
b64u = convertToBase Base64URLUnpadded

unb64u :: Text -> BS.ByteString -> Either JoseError BS.ByteString
unb64u what bs = case convertFromBase Base64URLUnpadded bs of
  Left (_ :: String) -> Left $ Base64DecodeFailed what
  Right out -> Right out

--------------------------------------------------------------------------------
-- key id
--------------------------------------------------------------------------------

-- | @BASE64URL_NOPAD(SHA256(SubjectPublicKeyInfo DER))@.
--
-- The DER here is the full X.509 SubjectPublicKeyInfo structure -- what Java's
-- @PublicKey.getEncoded()@ returns. Hashing the modulus, or the certificate, produces a
-- plausible-looking key id that the counterparty will reject.
kidOf :: RSA.PublicKey -> Text
kidOf pub =
  decodeUtf8 . b64u . BA.convert $ (hashWith SHA256 (spkiDer pub) :: Digest SHA256)

spkiDer :: RSA.PublicKey -> BS.ByteString
spkiDer pub =
  ASN1.encodeASN1' ASN1.DER $ ASN1.toASN1 (X509.PubKeyRSA pub) []

-- | The public half of a private key. Signing needs the private key; deriving the @kid@
-- needs the public one, and they must be the same pair or the counterparty cannot verify.
publicOf :: RSA.PrivateKey -> RSA.PublicKey
publicOf = RSA.private_pub

--------------------------------------------------------------------------------
-- PEM
--------------------------------------------------------------------------------

parseRsaPublicKeyPem :: Text -> Either JoseError RSA.PublicKey
parseRsaPublicKeyPem txt = do
  der <- pemContent txt
  asn1 <- mapLeft (const $ KeyParseFailed "public key ASN.1") $ ASN1.decodeASN1' ASN1.DER der
  case ASN1.fromASN1 asn1 of
    Right (X509.PubKeyRSA pub, _) -> Right pub
    Right (_, _) -> Left $ KeyParseFailed "public key is not RSA"
    Left _ -> Left $ KeyParseFailed "public key structure"

parseRsaPrivateKeyPem :: Text -> Either JoseError RSA.PrivateKey
parseRsaPrivateKeyPem txt = do
  der <- pemContent txt
  asn1 <- mapLeft (const $ KeyParseFailed "private key ASN.1") $ ASN1.decodeASN1' ASN1.DER der
  case ASN1.fromASN1 asn1 of
    Right (X509.PrivKeyRSA priv, _) -> Right priv
    Right (_, _) -> Left $ KeyParseFailed "private key is not RSA"
    Left _ -> Left $ KeyParseFailed "private key structure"

pemContent :: Text -> Either JoseError BS.ByteString
pemContent txt =
  case PEM.pemParseBS (encodeUtf8 txt) of
    Right (p : _) -> Right (PEM.pemContent p)
    Right [] -> Left $ KeyParseFailed "empty PEM"
    Left err -> Left . KeyParseFailed $ T.pack err

--------------------------------------------------------------------------------
-- JWS (RS256)
--------------------------------------------------------------------------------

-- | Sign a payload, producing a compact JWS: @header.payload.signature@.
signJWS :: RSA.PrivateKey -> Text -> BS.ByteString -> Either JoseError Text
signJWS priv kid payload = do
  let header = b64u . BL.toStrict . A.encode $ jwsHeader kid
      body = b64u payload
      signingInput = header <> "." <> body
  sig <- mapLeft (const $ KeyParseFailed "signing") $ PKCS15.sign Nothing (Just SHA256) priv signingInput
  pure . decodeUtf8 $ signingInput <> "." <> b64u sig

-- | Verify a compact JWS and return its payload.
verifyJWS :: RSA.PublicKey -> Text -> Either JoseError BS.ByteString
verifyJWS pub compact =
  case C8.split '.' (encodeUtf8 compact) of
    [header, body, sig] -> do
      hdr <- unb64u "jws header" header
      assertAlg "RS256" hdr
      sigRaw <- unb64u "jws signature" sig
      let signingInput = header <> "." <> body
      if PKCS15.verify (Just SHA256) pub signingInput sigRaw
        then unb64u "jws payload" body
        else Left SignatureVerificationFailed
    _ -> Left $ MalformedCompact "expected three dot-separated JWS parts"

jwsHeader :: Text -> A.Value
jwsHeader kid = A.object ["typ" A..= ("JWT" :: Text), "alg" A..= ("RS256" :: Text), "kid" A..= kid]

--------------------------------------------------------------------------------
-- JWE (RSA-OAEP-256 + A256GCM)
--------------------------------------------------------------------------------

-- | Encrypt a payload, producing a compact JWE of five parts:
-- @header.encryptedKey.iv.ciphertext.tag@.
--
-- The AAD is the ASCII of the base64url protected header, per RFC 7516 -- not the decoded
-- header, which is the usual way this is got wrong.
encryptJWE :: (MonadRandom m) => RSA.PublicKey -> Text -> BS.ByteString -> m (Either JoseError Text)
encryptJWE pub kid plaintext = do
  cek <- getRandomBytes 32 :: (MonadRandom m) => m BS.ByteString
  iv <- getRandomBytes 12 :: (MonadRandom m) => m BS.ByteString
  eWrapped <- OAEP.encrypt oaepParams pub cek
  pure $ do
    wrapped <- mapLeft (const $ DecryptionFailed "key wrap") eWrapped
    let header = b64u . BL.toStrict . A.encode $ jweHeader kid
    (ct, tag) <- gcmEncrypt cek iv header plaintext
    pure . decodeUtf8 $
      header <> "." <> b64u wrapped <> "." <> b64u iv <> "." <> b64u ct <> "." <> b64u tag

-- | Decrypt a compact JWE.
decryptJWE :: RSA.PrivateKey -> Text -> Either JoseError BS.ByteString
decryptJWE priv compact =
  case C8.split '.' (encodeUtf8 compact) of
    [header, encKey, iv, ct, tag] -> do
      hdrRaw <- unb64u "jwe header" header
      assertAlg "RSA-OAEP-256" hdrRaw
      assertEnc "A256GCM" hdrRaw
      wrapped <- unb64u "jwe key" encKey
      cek <- mapLeft (const $ DecryptionFailed "key unwrap") $ OAEP.decrypt Nothing oaepParams priv wrapped
      ivRaw <- unb64u "jwe iv" iv
      ctRaw <- unb64u "jwe ciphertext" ct
      tagRaw <- unb64u "jwe tag" tag
      gcmDecrypt cek ivRaw header ctRaw tagRaw
    _ -> Left $ MalformedCompact "expected five dot-separated JWE parts"

jweHeader :: Text -> A.Value
jweHeader kid = A.object ["alg" A..= ("RSA-OAEP-256" :: Text), "enc" A..= ("A256GCM" :: Text), "kid" A..= kid]

oaepParams :: OAEP.OAEPParams SHA256 BS.ByteString BS.ByteString
oaepParams = OAEP.defaultOAEPParams SHA256

gcmEncrypt :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Either JoseError (BS.ByteString, BS.ByteString)
gcmEncrypt cek iv aad plaintext = do
  aead <- initGcm cek iv
  let (ct, aeadFinal) = Cipher.aeadEncrypt (Cipher.aeadAppendHeader aead aad) plaintext
      tag = Cipher.aeadFinalize aeadFinal 16
  pure (ct, BA.convert tag)

gcmDecrypt :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Either JoseError BS.ByteString
gcmDecrypt cek iv aad ct expectedTag = do
  aead <- initGcm cek iv
  let (pt, aeadFinal) = Cipher.aeadDecrypt (Cipher.aeadAppendHeader aead aad) ct
      tag = BA.convert (Cipher.aeadFinalize aeadFinal 16) :: BS.ByteString
  if BA.constEq tag expectedTag
    then Right pt
    else Left $ DecryptionFailed "authentication tag mismatch"

initGcm :: BS.ByteString -> BS.ByteString -> Either JoseError (Cipher.AEAD Cipher.AES256)
initGcm cek iv = do
  cipher <- toJose "cipher init" (Cipher.cipherInit cek :: CE.CryptoFailable Cipher.AES256)
  toJose "gcm init" (Cipher.aeadInit Cipher.AEAD_GCM cipher iv)

toJose :: Text -> CE.CryptoFailable a -> Either JoseError a
toJose what = CE.onCryptoFailure (const . Left $ DecryptionFailed what) Right

--------------------------------------------------------------------------------
-- header assertions
--------------------------------------------------------------------------------

-- | Refuse anything we did not ask for. In particular this rejects @alg: none@, which is
-- the reason the check exists at all rather than trusting the header.
assertAlg :: Text -> BS.ByteString -> Either JoseError ()
assertAlg expected = assertHeaderField "alg" expected

assertEnc :: Text -> BS.ByteString -> Either JoseError ()
assertEnc expected = assertHeaderField "enc" expected

assertHeaderField :: Text -> Text -> BS.ByteString -> Either JoseError ()
assertHeaderField field expected raw =
  case A.decodeStrict raw :: Maybe A.Object of
    Nothing -> Left $ MalformedCompact "header is not a JSON object"
    Just obj -> case AKM.lookup (AK.fromText field) obj of
      Just (A.String actual)
        | actual == expected -> Right ()
        | otherwise -> Left $ UnsupportedAlgorithm (field <> "=" <> actual)
      _ -> Left $ UnsupportedAlgorithm (field <> " absent or not a string")
