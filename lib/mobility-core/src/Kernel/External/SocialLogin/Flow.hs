{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PackageImports #-}

module Kernel.External.SocialLogin.Flow
  ( SocialLoginFlow,
    verifyGoogleIdToken,
    verifyAppleIdToken,
    exchangeAppleAuthorizationCode,
    revokeAppleRefreshToken,
  )
where

import Crypto.Hash (SHA256 (..), hashWith)
import Crypto.Number.Basic (numBytes)
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Crypto.PubKey.RSA as RSA
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified "base64-bytestring" Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.X509 as X509
import qualified Data.X509.Memory as X509
import qualified EulerHS.Types as ET
import Kernel.External.Encryption
import Kernel.External.SocialLogin.API
import Kernel.External.SocialLogin.Types
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant.Client (Scheme (..))
import qualified Web.JWT as JWT

type SocialLoginFlow m r =
  ( CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m,
    Redis.HedisFlow m r,
    EncFlow m r
  )

data IdTokenIssuer = GoogleIssuer | AppleIssuer
  deriving (Show, Eq)

verifyGoogleIdToken :: SocialLoginFlow m r => SocialLoginConfig -> Maybe Text -> Text -> m VerifiedSocialIdentity
verifyGoogleIdToken cfg mbNonce idToken = do
  googleCfg <- cfg.google & fromMaybeM (SocialLoginProviderNotConfigured "GOOGLE")
  verifyIdToken GoogleIssuer cfg googleCfg.clientIds mbNonce idToken

verifyAppleIdToken :: SocialLoginFlow m r => SocialLoginConfig -> Maybe Text -> Text -> m VerifiedSocialIdentity
verifyAppleIdToken cfg mbNonce idToken = do
  let allowedAudiences = concatMap (.clientIds) cfg.apple
  when (null allowedAudiences) $ throwError (SocialLoginProviderNotConfigured "APPLE")
  verifyIdToken AppleIssuer cfg allowedAudiences mbNonce idToken

exchangeAppleAuthorizationCode :: SocialLoginFlow m r => SocialLoginConfig -> Text -> Text -> Text -> m (Maybe Text)
exchangeAppleAuthorizationCode cfg clientId expectedSubject authorizationCode = do
  appleCfg <- getAppleConfig cfg clientId
  clientSecret <- mkAppleClientSecret appleCfg clientId
  let req = AppleTokenReq {clientId, clientSecret, code = authorizationCode}
  resp <-
    callAPI appleBaseUrl (ET.client appleTokenAPI req) "appleAuthToken" appleTokenAPI
      >>= fromEitherM (ExternalAPICallError (Just "APPLE_AUTH_TOKEN_FAILED") appleBaseUrl)
  idToken <- resp.id_token & fromMaybeM (SocialLoginInvalidToken "Apple token response has no id_token")
  tokenSubject <-
    (JWT.decode idToken >>= fmap JWT.stringOrURIToText . JWT.sub . JWT.claims)
      & fromMaybeM (SocialLoginInvalidToken "malformed id_token in Apple token response")
  unless (tokenSubject == expectedSubject) $ throwError (SocialLoginInvalidToken "authorization code belongs to a different Apple user")
  pure resp.refresh_token

revokeAppleRefreshToken :: SocialLoginFlow m r => SocialLoginConfig -> Text -> Text -> m ()
revokeAppleRefreshToken cfg clientId refreshToken = do
  appleCfg <- getAppleConfig cfg clientId
  clientSecret <- mkAppleClientSecret appleCfg clientId
  let req = AppleRevokeReq {clientId, clientSecret, token = refreshToken}
  void $
    callAPI appleBaseUrl (ET.client appleRevokeAPI req) "appleAuthRevoke" appleRevokeAPI
      >>= fromEitherM (ExternalAPICallError (Just "APPLE_AUTH_REVOKE_FAILED") appleBaseUrl)

getAppleConfig :: (MonadThrow m, Log m) => SocialLoginConfig -> Text -> m AppleSignInConfig
getAppleConfig cfg clientId =
  List.find (\appleCfg -> clientId `elem` appleCfg.clientIds) cfg.apple
    & fromMaybeM (SocialLoginProviderNotConfigured $ "APPLE client " <> clientId)

verifyIdToken :: SocialLoginFlow m r => IdTokenIssuer -> SocialLoginConfig -> [Text] -> Maybe Text -> Text -> m VerifiedSocialIdentity
verifyIdToken expectedIssuer cfg allowedAudiences mbNonce idToken = do
  unverified <- JWT.decode idToken & fromMaybeM (SocialLoginInvalidToken "malformed token")
  let jwtHeader = JWT.header unverified
  unless (JWT.alg jwtHeader == Just JWT.RS256) $ throwError (SocialLoginInvalidToken "unsupported algorithm")
  keyId <- JWT.kid jwtHeader & fromMaybeM (SocialLoginInvalidToken "missing key id")
  publicKey <- findSigningKey expectedIssuer cfg.jwksCacheExpiry keyId
  verified <- JWT.verify (JWT.VerifyRSAPublicKey publicKey) unverified & fromMaybeM (SocialLoginInvalidToken "signature mismatch")
  now <- utcTimeToPOSIXSeconds <$> getCurrentTime
  let claimsSet = JWT.claims verified
      skew = fromIntegral cfg.clockSkew.getSeconds
      extraClaims = JWT.unClaimsMap (JWT.unregisteredClaims claimsSet)
      tokenAudiences = maybe [] (either pure identity) (JWT.aud claimsSet) <&> JWT.stringOrURIToText
  issuer <- (JWT.stringOrURIToText <$> JWT.iss claimsSet) & fromMaybeM (SocialLoginInvalidToken "missing issuer")
  unless (issuer `elem` issuerNames expectedIssuer) $ throwError (SocialLoginInvalidToken "issuer mismatch")
  audience <- List.find (`elem` allowedAudiences) tokenAudiences & fromMaybeM (SocialLoginInvalidToken "audience mismatch")
  expiry <- JWT.exp claimsSet & fromMaybeM (SocialLoginInvalidToken "missing expiry")
  when (JWT.secondsSinceEpoch expiry + skew < now) $ throwError (SocialLoginInvalidToken "token expired")
  whenJust (JWT.iat claimsSet) $ \issuedAt ->
    when (JWT.secondsSinceEpoch issuedAt - skew > now) $ throwError (SocialLoginInvalidToken "token issued in the future")
  subject <- (JWT.stringOrURIToText <$> JWT.sub claimsSet) & fromMaybeM (SocialLoginInvalidToken "missing subject")
  when (subject == "") $ throwError (SocialLoginInvalidToken "missing subject")
  whenJust mbNonce $ \nonce ->
    unless (maybe False (nonceMatches nonce) (textClaim "nonce" extraClaims)) $ throwError (SocialLoginInvalidToken "nonce mismatch")
  pure
    VerifiedSocialIdentity
      { issuer,
        subject,
        audience,
        email = textClaim "email" extraClaims,
        emailVerified = boolClaim "email_verified" extraClaims,
        isPrivateEmail = boolClaim "is_private_email" extraClaims,
        name = textClaim "name" extraClaims
      }

nonceMatches :: Text -> Text -> Bool
nonceMatches nonce tokenNonce = tokenNonce == nonce || tokenNonce == show (hashWith SHA256 (encodeUtf8 nonce :: ByteString))

textClaim :: Text -> Map.Map Text A.Value -> Maybe Text
textClaim key extraClaims = case Map.lookup key extraClaims of
  Just (A.String value) -> Just value
  _ -> Nothing

boolClaim :: Text -> Map.Map Text A.Value -> Bool
boolClaim key extraClaims = case Map.lookup key extraClaims of
  Just (A.Bool value) -> value
  Just (A.String "true") -> True
  _ -> False

issuerNames :: IdTokenIssuer -> [Text]
issuerNames GoogleIssuer = ["https://accounts.google.com", "accounts.google.com"]
issuerNames AppleIssuer = ["https://appleid.apple.com"]

jwksUrl :: IdTokenIssuer -> BaseUrl
jwksUrl GoogleIssuer = BaseUrl Https "www.googleapis.com" 443 "/oauth2/v3/certs"
jwksUrl AppleIssuer = BaseUrl Https "appleid.apple.com" 443 "/auth/keys"

appleBaseUrl :: BaseUrl
appleBaseUrl = BaseUrl Https "appleid.apple.com" 443 ""

jwksCacheKey :: IdTokenIssuer -> Text
jwksCacheKey issuer = "SocialLogin:Jwks:" <> show issuer

jwksRefreshLockKey :: IdTokenIssuer -> Text
jwksRefreshLockKey issuer = "SocialLogin:JwksRefreshLock:" <> show issuer

findSigningKey :: SocialLoginFlow m r => IdTokenIssuer -> Seconds -> Text -> m RSA.PublicKey
findSigningKey issuer cacheExpiry keyId = do
  mbCached <- Redis.safeGet (jwksCacheKey issuer)
  mbJwk <- case mbCached >>= lookupJwk keyId of
    Just jwk -> pure (Just jwk)
    Nothing -> do
      canRefresh <-
        if isNothing mbCached
          then pure True
          else Redis.setNxExpire (jwksRefreshLockKey issuer) 60 ()
      if canRefresh
        then lookupJwk keyId <$> refreshJwks issuer cacheExpiry
        else waitForRefreshedJwk issuer keyId 5
  jwk <- mbJwk & fromMaybeM (SocialLoginInvalidToken "unknown signing key")
  toRsaPublicKey jwk & fromMaybeM (SocialLoginInvalidToken "unusable signing key")

waitForRefreshedJwk :: SocialLoginFlow m r => IdTokenIssuer -> Text -> Int -> m (Maybe Jwk)
waitForRefreshedJwk issuer keyId attemptsLeft = do
  threadDelayMilliSec (Milliseconds 200)
  mbJwk <- (>>= lookupJwk keyId) <$> Redis.safeGet (jwksCacheKey issuer)
  if isJust mbJwk || attemptsLeft <= 1
    then pure mbJwk
    else waitForRefreshedJwk issuer keyId (attemptsLeft - 1)

refreshJwks :: SocialLoginFlow m r => IdTokenIssuer -> Seconds -> m JwkSet
refreshJwks issuer cacheExpiry = do
  let url = jwksUrl issuer
  jwkSet <-
    callAPI url (ET.client jwksAPI) "socialLoginJwks" jwksAPI
      >>= fromEitherM (ExternalAPICallError (Just "SOCIAL_LOGIN_JWKS_FETCH_FAILED") url)
  Redis.setExp (jwksCacheKey issuer) jwkSet cacheExpiry.getSeconds
  pure jwkSet

lookupJwk :: Text -> JwkSet -> Maybe Jwk
lookupJwk keyId jwkSet = List.find (\jwk -> jwk.kid == keyId) jwkSet.keys

toRsaPublicKey :: Jwk -> Maybe RSA.PublicKey
toRsaPublicKey jwk = do
  guard (jwk.kty == "RSA")
  modulus <- os2ip <$> (jwk.n >>= decodeBase64Url)
  exponent' <- os2ip <$> (jwk.e >>= decodeBase64Url)
  pure RSA.PublicKey {RSA.public_size = numBytes modulus, RSA.public_n = modulus, RSA.public_e = exponent'}

decodeBase64Url :: Text -> Maybe ByteString
decodeBase64Url value =
  let raw = encodeUtf8 value
      toMaybe = either (const Nothing) Just
   in maybe (toMaybe $ B64U.decodePadded raw) Just (toMaybe $ B64U.decodeUnpadded raw)

mkAppleClientSecret :: (MonadFlow m, EncFlow m r) => AppleSignInConfig -> Text -> m Text
mkAppleClientSecret appleCfg clientId = do
  pem <- decrypt (Encrypted appleCfg.encryptedPrivateKey :: Encrypted Text)
  privateKey <- case X509.readKeyFileFromMemory (encodeUtf8 pem) of
    [X509.PrivKeyEC (X509.PrivKeyEC_Named ECC.SEC_p256r1 privateNumber)] -> pure $ ECDSA.PrivateKey (ECC.getCurveByName ECC.SEC_p256r1) privateNumber
    _ -> throwError (SocialLoginAppleClientSecretError "expected a single P-256 (ES256) EC private key")
  issuedAt <- floor . utcTimeToPOSIXSeconds <$> getCurrentTime
  let jwtHeader = A.object ["alg" A..= ("ES256" :: Text), "kid" A..= appleCfg.keyId]
      jwtPayload =
        A.object
          [ "iss" A..= appleCfg.teamId,
            "iat" A..= (issuedAt :: Integer),
            "exp" A..= (issuedAt + 300),
            "aud" A..= ("https://appleid.apple.com" :: Text),
            "sub" A..= clientId
          ]
      signingInput = encodeSegment jwtHeader <> "." <> encodeSegment jwtPayload
  signature <- liftIO $ ECDSA.sign privateKey SHA256 signingInput
  let rawSignature = i2ospOf_ 32 (ECDSA.sign_r signature) <> i2ospOf_ 32 (ECDSA.sign_s signature) :: ByteString
  pure $ decodeUtf8 (signingInput <> "." <> B64U.encodeUnpadded rawSignature)
  where
    encodeSegment :: A.Value -> ByteString
    encodeSegment = B64U.encodeUnpadded . BSL.toStrict . A.encode
