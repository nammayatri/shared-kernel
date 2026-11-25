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
-- Supports a small, closed set of algorithms, and never negotiates: the caller names the
-- algorithm it expects and anything else in a header is refused, because algorithm
-- negotiation is how @alg: none@ gets accepted:
--
--   * JWS: @RS256@ (RSASSA-PKCS1-v1_5 with SHA-256)
--   * JWE key wrapping: @RSA-OAEP-256@ (the default) or @RSA-OAEP@ (SHA-1)
--   * JWE content encryption: @A256GCM@ (the default), @A128CBC-HS256@ or @A256CBC-HS512@
--
-- 'encryptJWE' / 'decryptJWE' use @RSA-OAEP-256@ + @A256GCM@. Partners that mandate other
-- algorithms encrypt with 'encryptJWEWith'; decryption supports only @RSA-OAEP-256@ + @A256GCM@.
module Kernel.Utils.Jose
  ( JoseError (..),
    JweKeyAlg (..),
    JweContentEnc (..),
    jweKeyAlgName,
    jweContentEncName,
    signJWS,
    verifyJWS,
    encryptJWE,
    decryptJWE,
    encryptJWEWith,
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
import qualified Crypto.Data.Padding as Padding
import qualified Crypto.Error as CE
import Crypto.Hash (Digest, hashWith)
import Crypto.Hash.Algorithms (SHA1 (..), SHA256 (..), SHA512 (..))
import qualified Crypto.MAC.HMAC as HMAC
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
import qualified Data.ByteString.Builder as BB
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
  | EncryptionFailed Text
  | DecryptionFailed Text
  | KeyParseFailed Text
  | Base64DecodeFailed Text
  deriving stock (Show, Eq, Generic)

-- | JWE key management algorithm: how the content key is wrapped to the recipient's RSA key.
data JweKeyAlg
  = -- | RSAES-OAEP with SHA-1 and MGF1-SHA-1 (@RSA-OAEP@).
    RSA_OAEP
  | -- | RSAES-OAEP with SHA-256 and MGF1-SHA-256 (@RSA-OAEP-256@).
    RSA_OAEP_256
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (A.FromJSON, A.ToJSON) -- JSON is the constructor name, e.g. "RSA_OAEP_256"

-- | JWE content encryption algorithm.
data JweContentEnc
  = -- | AES-256 in GCM mode, 96-bit IV, 128-bit tag.
    A256GCM
  | -- | AES-128-CBC with HMAC-SHA-256 truncated to 128 bits (RFC 7518 §5.2.3).
    A128CBC_HS256
  | -- | AES-256-CBC with HMAC-SHA-512 truncated to 256 bits (RFC 7518 §5.2.5).
    A256CBC_HS512
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (A.FromJSON, A.ToJSON) -- JSON is the constructor name, e.g. "A128CBC_HS256"

jweKeyAlgName :: JweKeyAlg -> Text
jweKeyAlgName = \case
  RSA_OAEP -> "RSA-OAEP"
  RSA_OAEP_256 -> "RSA-OAEP-256"

jweContentEncName :: JweContentEnc -> Text
jweContentEncName = \case
  A256GCM -> "A256GCM"
  A128CBC_HS256 -> "A128CBC-HS256"
  A256CBC_HS512 -> "A256CBC-HS512"

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

-- | Sign a payload, producing a compact JWS: @header.payload.signature@. Signed with RSA blinding
--   (a fresh random blinder per signature), so its timing says nothing about the private key.
signJWS :: (MonadRandom m) => RSA.PrivateKey -> Text -> BS.ByteString -> m (Either JoseError Text)
signJWS priv kid payload = do
  let header = b64u . BL.toStrict . A.encode $ jwsHeader kid
      body = b64u payload
      signingInput = header <> "." <> body
  eSig <- PKCS15.signSafer (Just SHA256) priv signingInput
  pure $ do
    sig <- mapLeft (const $ KeyParseFailed "signing") eSig
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
-- JWE
--------------------------------------------------------------------------------

-- | Encrypt a payload with the default algorithms (@RSA-OAEP-256@ + @A256GCM@).
encryptJWE :: (MonadRandom m) => RSA.PublicKey -> Text -> BS.ByteString -> m (Either JoseError Text)
encryptJWE = encryptJWEWith RSA_OAEP_256 A256GCM

-- | Decrypt a compact JWE. The key is unwrapped with RSA blinding, like signing in 'signJWS'.
decryptJWE :: (MonadRandom m) => RSA.PrivateKey -> Text -> m (Either JoseError BS.ByteString)
decryptJWE priv compact =
  case C8.split '.' (encodeUtf8 compact) of
    [header, encKey, iv, ct, tag] -> do
      let parts = do
            hdrRaw <- unb64u "jwe header" header
            assertAlg "RSA-OAEP-256" hdrRaw
            assertEnc "A256GCM" hdrRaw
            (,,,) <$> unb64u "jwe key" encKey <*> unb64u "jwe iv" iv <*> unb64u "jwe ciphertext" ct <*> unb64u "jwe tag" tag
      case parts of
        Left err -> pure (Left err)
        Right (wrapped, ivRaw, ctRaw, tagRaw) -> do
          eCek <- OAEP.decryptSafer oaepParams priv wrapped
          pure $ do
            cek <- mapLeft (const $ DecryptionFailed "key unwrap") eCek
            gcmDecrypt cek ivRaw header ctRaw tagRaw
    _ -> pure . Left $ MalformedCompact "expected five dot-separated JWE parts"

-- | Encrypt a payload, producing a compact JWE of five parts:
-- @header.encryptedKey.iv.ciphertext.tag@.
--
-- The AAD is the ASCII of the base64url protected header, per RFC 7516 -- not the decoded
-- header, which is the usual way this is got wrong.
encryptJWEWith :: (MonadRandom m) => JweKeyAlg -> JweContentEnc -> RSA.PublicKey -> Text -> BS.ByteString -> m (Either JoseError Text)
encryptJWEWith alg enc pub kid plaintext = do
  cek <- getRandomBytes (cekLength enc) :: (MonadRandom m) => m BS.ByteString
  iv <- getRandomBytes (ivLength enc) :: (MonadRandom m) => m BS.ByteString
  eWrapped <- wrapKey alg pub cek
  pure $ do
    wrapped <- eWrapped
    let header = b64u . BL.toStrict . A.encode $ jweHeader alg enc kid
    (ct, tag) <- contentEncrypt enc cek iv header plaintext
    pure . decodeUtf8 $
      header <> "." <> b64u wrapped <> "." <> b64u iv <> "." <> b64u ct <> "." <> b64u tag

jweHeader :: JweKeyAlg -> JweContentEnc -> Text -> A.Value
jweHeader alg enc kid = A.object ["alg" A..= jweKeyAlgName alg, "enc" A..= jweContentEncName enc, "kid" A..= kid]

--------------------------------------------------------------------------------
-- key wrapping
--------------------------------------------------------------------------------

wrapKey :: (MonadRandom m) => JweKeyAlg -> RSA.PublicKey -> BS.ByteString -> m (Either JoseError BS.ByteString)
wrapKey alg pub cek =
  mapLeft (const $ EncryptionFailed "key wrap") <$> case alg of
    RSA_OAEP -> OAEP.encrypt (OAEP.defaultOAEPParams SHA1 :: OAEP.OAEPParams SHA1 BS.ByteString BS.ByteString) pub cek
    RSA_OAEP_256 -> OAEP.encrypt oaepParams pub cek

oaepParams :: OAEP.OAEPParams SHA256 BS.ByteString BS.ByteString
oaepParams = OAEP.defaultOAEPParams SHA256

--------------------------------------------------------------------------------
-- content encryption
--------------------------------------------------------------------------------

-- | Content key length in bytes. For the CBC-HMAC algorithms this is the MAC key and the AES
-- key concatenated, of equal length.
cekLength :: JweContentEnc -> Int
cekLength = \case
  A256GCM -> 32
  A128CBC_HS256 -> 32
  A256CBC_HS512 -> 64

ivLength :: JweContentEnc -> Int
ivLength = \case
  A256GCM -> 12
  A128CBC_HS256 -> 16
  A256CBC_HS512 -> 16

contentEncrypt :: JweContentEnc -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Either JoseError (BS.ByteString, BS.ByteString)
contentEncrypt = \case
  A256GCM -> gcmEncrypt
  A128CBC_HS256 -> cbcHsEncrypt CbcHs128
  A256CBC_HS512 -> cbcHsEncrypt CbcHs256

gcmEncrypt :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Either JoseError (BS.ByteString, BS.ByteString)
gcmEncrypt cek iv aad plaintext = do
  -- initGcm labels its failures for the decrypt side; on this side they are encryption failures.
  aead <- mapLeft asEncryptionError (initGcm cek iv)
  let (ct, aeadFinal) = Cipher.aeadEncrypt (Cipher.aeadAppendHeader aead aad) plaintext
      tag = Cipher.aeadFinalize aeadFinal 16
  pure (ct, BA.convert tag)
  where
    asEncryptionError = \case
      DecryptionFailed what -> EncryptionFailed what
      other -> other

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

-- | The AES-CBC + HMAC-SHA-2 algorithms of RFC 7518 §5.2: @A128CBC-HS256@ ('CbcHs128') and
-- @A256CBC-HS512@ ('CbcHs256').
data CbcHs = CbcHs128 | CbcHs256

-- | Length in bytes of each CEK half (MAC key, then AES key), which is also the tag length.
cbcHsHalf :: CbcHs -> Int
cbcHsHalf = \case
  CbcHs128 -> 16
  CbcHs256 -> 32

-- | RFC 7518 §5.2.2.1: the first half of the CEK is the HMAC key, the second the AES key; the
-- tag is the first half of @HMAC(AAD || IV || ciphertext || AL)@, where @AL@ is the AAD length
-- in /bits/ as a 64-bit big-endian integer.
cbcHsEncrypt :: CbcHs -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Either JoseError (BS.ByteString, BS.ByteString)
cbcHsEncrypt v cek iv aad plaintext = do
  let (macKey, encKey) = BS.splitAt (cbcHsHalf v) cek
  ct <- mapLeft asEncryptionError (aesCbcEncrypt v encKey iv (Padding.pad (Padding.PKCS7 16) plaintext))
  pure (ct, cbcHsTag v macKey aad iv ct)
  where
    asEncryptionError = \case
      DecryptionFailed what -> EncryptionFailed what
      other -> other

cbcHsTag :: CbcHs -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
cbcHsTag v macKey aad iv ct = BS.take (cbcHsHalf v) $ case v of
  CbcHs128 -> BA.convert (HMAC.hmac macKey macInput :: HMAC.HMAC SHA256)
  CbcHs256 -> BA.convert (HMAC.hmac macKey macInput :: HMAC.HMAC SHA512)
  where
    macInput = aad <> iv <> ct <> al
    al = BL.toStrict . BB.toLazyByteString . BB.word64BE . fromIntegral $ BS.length aad * 8

-- | AES-CBC encryption of already padded input: AES-128 for 'CbcHs128', AES-256 for 'CbcHs256'.
aesCbcEncrypt :: CbcHs -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Either JoseError BS.ByteString
aesCbcEncrypt v encKey iv input = case v of
  CbcHs128 -> runCbcEncrypt (Cipher.cipherInit encKey :: CE.CryptoFailable Cipher.AES128) iv input
  CbcHs256 -> runCbcEncrypt (Cipher.cipherInit encKey :: CE.CryptoFailable Cipher.AES256) iv input

runCbcEncrypt :: (Cipher.BlockCipher c) => CE.CryptoFailable c -> BS.ByteString -> BS.ByteString -> Either JoseError BS.ByteString
runCbcEncrypt initialised iv input = do
  cipher <- toJose "cipher init" initialised
  ivT <- maybe (Left $ DecryptionFailed "iv length") Right (Cipher.makeIV iv)
  pure $ Cipher.cbcEncrypt cipher ivT input

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
