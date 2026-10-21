module Kernel.External.SocialSecurity.EShram.Flow where

import EulerHS.Types as Euler
import Kernel.External.Encryption
import Kernel.External.SocialSecurity.EShram.Encryption (encryptPayload)
import Kernel.External.SocialSecurity.EShram.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

type GenerateAuthTokenAPI =
  "generateAuthTokenPA"
    :> Header "Ocp-Apim-Subscription-Key" Text
    :> ReqBody '[JSON] AuthTokenReq
    :> Post '[JSON] EShramResp

type GetUnverifiedPWDataAPI =
  "getUnVerifiedPWData"
    :> Header "Ocp-Apim-Subscription-Key" Text
    :> Header "X-USER-INFO" Text
    :> Header "Authorization" Text
    :> ReqBody '[JSON] GetUnverifiedPWDataReq
    :> Post '[JSON] EShramResp

type PushVerifiedPWDataAPI =
  "pushVerifiedPWData"
    :> Header "Ocp-Apim-Subscription-Key" Text
    :> Header "X-USER-INFO" Text
    :> Header "Authorization" Text
    :> ReqBody '[JSON] PushVerifiedPWDataReq
    :> Post '[JSON] EShramResp

type InsertPWMonthlyUtilizationAPI =
  "insertPWMonthlyUtilization"
    :> Header "Ocp-Apim-Subscription-Key" Text
    :> Header "X-USER-INFO" Text
    :> Header "Authorization" Text
    :> ReqBody '[JSON] InsertPWMonthlyUtilizationReq
    :> Post '[JSON] EShramResp

generateAuthToken ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  EShramConfig ->
  m EShramResp
generateAuthToken config = do
  subscriptionKey <- decrypt config.subscriptionKey
  pwd <- decrypt config.password
  tokenSecretCode <- decrypt config.tokenSecretCode
  tokenSaltValue <- decrypt config.tokenSaltValue
  let plainPayload = encodeToText AuthTokenPayload {email = config.userEmail, pwd}
  encRequest <- fromEitherM (\err -> InternalError $ "Failed to encrypt eShram auth payload: " <> err) $ encryptPayload tokenSecretCode tokenSaltValue plainPayload
  let proxy = Proxy @GenerateAuthTokenAPI
      eulerClient = Euler.client proxy (Just subscriptionKey) (AuthTokenReq {encRequest})
  callEShramAPI config.url eulerClient "eshram-generate-auth-token" proxy

getUnverifiedPWData ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  EShramConfig ->
  Text ->
  GetUnverifiedPWDataReq ->
  m EShramResp
getUnverifiedPWData config authToken req = do
  subscriptionKey <- decrypt config.subscriptionKey
  let proxy = Proxy @GetUnverifiedPWDataAPI
      eulerClient = Euler.client proxy (Just subscriptionKey) (Just config.userEmail) (Just $ "Bearer " <> authToken) req
  callEShramAPI config.url eulerClient "eshram-get-unverified-pw-data" proxy

pushVerifiedPWData ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  EShramConfig ->
  Text ->
  [VerifiedPWData] ->
  m EShramResp
pushVerifiedPWData config authToken records = do
  subscriptionKey <- decrypt config.subscriptionKey
  secretCode <- decrypt config.secretCode
  saltValue <- decrypt config.saltValue
  encVerifiedJsonRequest <- fromEitherM (\err -> InternalError $ "Failed to encrypt eShram pushVerifiedPWData payload: " <> err) $ encryptPayload secretCode saltValue (encodeToText records)
  let proxy = Proxy @PushVerifiedPWDataAPI
      eulerClient = Euler.client proxy (Just subscriptionKey) (Just config.userEmail) (Just $ "Bearer " <> authToken) (PushVerifiedPWDataReq {encVerifiedJsonRequest})
  callEShramAPI config.url eulerClient "eshram-push-verified-pw-data" proxy

insertPWMonthlyUtilization ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  EShramConfig ->
  Text ->
  [MonthlyUtilization] ->
  m EShramResp
insertPWMonthlyUtilization config authToken records = do
  subscriptionKey <- decrypt config.subscriptionKey
  secretCode <- decrypt config.secretCode
  saltValue <- decrypt config.saltValue
  encMonthlyUtilizationJsonRequest <- fromEitherM (\err -> InternalError $ "Failed to encrypt eShram insertPWMonthlyUtilization payload: " <> err) $ encryptPayload secretCode saltValue (encodeToText records)
  let proxy = Proxy @InsertPWMonthlyUtilizationAPI
      eulerClient = Euler.client proxy (Just subscriptionKey) (Just config.userEmail) (Just $ "Bearer " <> authToken) (InsertPWMonthlyUtilizationReq {encMonthlyUtilizationJsonRequest})
  callEShramAPI config.url eulerClient "eshram-insert-pw-monthly-utilization" proxy

callEShramAPI :: (MonadFlow m, HasRequestId r, MonadReader r m) => CallAPI' m r api res res
callEShramAPI url eulerClient description proxy = do
  callAPI url eulerClient description proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call eShram " <> description <> " API: " <> show err)
