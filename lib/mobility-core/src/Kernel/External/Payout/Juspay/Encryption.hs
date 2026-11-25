{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Request encryption for Juspay's encrypted payout APIs, following
-- https://juspay.io/in/docs/payout/docs/resources/encryption-guide:
--
--   1. stringify the order JSON;
--   2. encrypt it to Juspay's public key as a compact JWE (@RSA-OAEP-256@ + @A128CBC-HS256@ by
--      default; the header carries @kid@ = Juspay's key id);
--   3. sign the JWE with our private key as a compact JWS (@RS256@, header @kid@ = our key id),
--      unless 'signRequest' is @false@;
--   4. send @{"payload": <jws or jwe>, "kid": <juspay key id>, "sid": <our key id>, "format": "compact"}@.
--
-- Juspay answers with the plain order JSON; encrypted responses are not supported.
module Kernel.External.Payout.Juspay.Encryption
  ( encryptPayoutRequest,
    keyWrapAlgorithmOf,
    contentEncryptionOf,
    signsRequest,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Kernel.External.Encryption
import Kernel.External.Payout.Juspay.Config
import Kernel.External.Payout.Juspay.Types.Payout
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Error.Throwing (fromEitherM, fromMaybeM)
import qualified Kernel.Utils.Jose as Jose

keyWrapAlgorithmOf :: JuspayPayoutEncryptionConfig -> Jose.JweKeyAlg
keyWrapAlgorithmOf cfg = fromMaybe Jose.RSA_OAEP_256 cfg.keyWrapAlgorithm

contentEncryptionOf :: JuspayPayoutEncryptionConfig -> Jose.JweContentEnc
contentEncryptionOf cfg = fromMaybe Jose.A128CBC_HS256 cfg.contentEncryption

-- | Step 3 of the guide is on unless the config turns it off.
signsRequest :: JuspayPayoutEncryptionConfig -> Bool
signsRequest cfg = fromMaybe True cfg.signRequest

-- | Encrypt (and, if configured, sign) a payout order into Juspay's envelope.
encryptPayoutRequest ::
  (EncFlow m r) =>
  JuspayPayoutEncryptionConfig ->
  CreatePayoutOrderReq ->
  m JuspayEncryptedPayload
encryptPayoutRequest cfg req = do
  juspayPub <- loadJuspayPublicKey cfg
  let plaintext = BL.toStrict $ A.encode req
  jwe <-
    liftIO (Jose.encryptJWEWith (keyWrapAlgorithmOf cfg) (contentEncryptionOf cfg) juspayPub cfg.juspayKeyId plaintext)
      >>= fromEitherM (\e -> InternalError $ "Juspay payout encrypt: " <> show e)
  envelopePayload <-
    if signsRequest cfg
      then do
        ourPriv <- loadMerchantPrivateKey cfg
        liftIO (Jose.signJWS ourPriv cfg.merchantKeyId (encodeUtf8 jwe))
          >>= fromEitherM (\e -> InternalError $ "Juspay payout sign: " <> show e)
      else pure jwe
  pure
    JuspayEncryptedPayload
      { payload = envelopePayload,
        kid = cfg.juspayKeyId,
        sid = cfg.merchantKeyId,
        format = "compact"
      }

loadJuspayPublicKey :: (EncFlow m r) => JuspayPayoutEncryptionConfig -> m Jose.PublicKey
loadJuspayPublicKey cfg = do
  pem <- decrypt cfg.juspayPublicKey
  Jose.parseRsaPublicKeyPem pem & fromEitherM (\e -> InternalError $ "Juspay payout public key: " <> show e)

loadMerchantPrivateKey :: (EncFlow m r) => JuspayPayoutEncryptionConfig -> m Jose.PrivateKey
loadMerchantPrivateKey cfg = do
  encrypted <- cfg.merchantPrivateKey & fromMaybeM (InternalError "Juspay payout: merchantPrivateKey is not configured (needed to sign requests unless signRequest is false)")
  pem <- decrypt encrypted
  Jose.parseRsaPrivateKeyPem pem & fromEitherM (\e -> InternalError $ "Juspay payout private key: " <> show e)
