module Kernel.External.SocialSecurity.Interface.EShram where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.External.Encryption (EncFlow, decrypt)
import Kernel.External.SocialSecurity.EShram.Encryption (decryptPayload)
import qualified Kernel.External.SocialSecurity.EShram.Flow as Flow
import qualified Kernel.External.SocialSecurity.EShram.Types as EShram
import Kernel.External.SocialSecurity.Interface.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error (GenericError (..))
import Kernel.Utils.Common (MonadFlow, fromEitherM, throwError)
import Kernel.Utils.Servant.Client (HasRequestId)

generateAuthToken ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  EShram.EShramConfig ->
  m AuthTokenResp
generateAuthToken config = do
  resp <- Flow.generateAuthToken config
  case (resp.code, resp.jwtToken) of
    -- eShram returns the token as "Bearer <token>"; keep only the token
    (1, Just token) -> pure AuthTokenResp {authToken = stripBearer token}
    _ -> throwApiFailure "generateAuthTokenPA" resp
  where
    stripBearer token = let t = T.strip token in fromMaybe t (T.strip <$> T.stripPrefix "Bearer " t)

getUnverifiedWorkers ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  EShram.EShramConfig ->
  Text ->
  UnverifiedWorkersReq ->
  m UnverifiedWorkersResp
getUnverifiedWorkers config authToken req = do
  resp <- Flow.getUnverifiedPWData config authToken $ EShram.GetUnverifiedPWDataReq (show req.fromDate) (show req.toDate)
  case (resp.code, resp.responseData) of
    -- the response `data` is AES-encrypted with secretCode / saltValue
    (1, Just (A.String encData)) -> do
      secretCode <- decrypt config.secretCode
      saltValue <- decrypt config.saltValue
      plainData <- fromEitherM (\err -> InternalError $ "Failed to decrypt eShram getUnVerifiedPWData data: " <> err) $ decryptPayload secretCode saltValue encData
      pwData :: EShram.UnverifiedPWData <-
        fromEitherM (\err -> InternalError $ "Failed to parse eShram getUnVerifiedPWData data: " <> T.pack err) $ A.eitherDecodeStrict (TE.encodeUtf8 plainData)
      pure
        UnverifiedWorkersResp
          { totalRecord = pwData.totalRecord,
            recordShared = pwData.recordShared,
            remainingRecord = pwData.remainingRecord,
            workers = map mkWorker pwData.pwData
          }
    _ -> throwApiFailure "getUnVerifiedPWData" resp
  where
    mkWorker :: EShram.PWData -> UnverifiedWorker
    mkWorker w = UnverifiedWorker {uanNo = w.uanNo, panNo = w.panNo, yob = w.yob, gender = w.gender}

pushWorkerVerifications ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  EShram.EShramConfig ->
  Text ->
  [WorkerVerification] ->
  m PushVerificationResp
pushWorkerVerifications config authToken verifications = do
  resp <- Flow.pushVerifiedPWData config authToken (map mkVerifiedPWData verifications)
  result :: EShram.PushVerifiedPWDataResult <- parseResponseData "pushVerifiedPWData" resp
  pure
    PushVerificationResp
      { recordsUpdated = mkRecordResult result.recordsUpdated,
        recordsNotUpdated = mkRecordResult result.recordsNotUpdated,
        recordsFailed = mkRecordResult result.recordsFailed,
        validationError = mkRecordResult result.validationError
      }
  where
    mkVerifiedPWData :: WorkerVerification -> EShram.VerifiedPWData
    mkVerifiedPWData v =
      EShram.VerifiedPWData v.panNo v.uanNo (verificationStatusCode v.verificationStatus) v.rejectionReason (show <$> v.dateOfOnboarding)

    verificationStatusCode :: WorkerVerificationStatus -> Int
    verificationStatusCode = \case
      NotProcessed -> 0
      Matched -> 1
      NotMatched -> 2

insertMonthlyUtilization ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  EShram.EShramConfig ->
  Text ->
  [WorkerMonthlyUtilization] ->
  m MonthlyUtilizationResp
insertMonthlyUtilization config authToken utilizations = do
  resp <- Flow.insertPWMonthlyUtilization config authToken (map mkMonthlyUtilization utilizations)
  result :: EShram.InsertPWMonthlyUtilizationResult <- parseResponseData "insertPWMonthlyUtilization" resp
  pure
    MonthlyUtilizationResp
      { recordsUpdated = mkRecordResult result.recordsUpdated,
        recordsInserted = mkRecordResult result.recordsInserted,
        recordsNotFound = mkRecordResult result.recordsNotFound,
        validationError = mkRecordResult result.validationError
      }
  where
    mkMonthlyUtilization :: WorkerMonthlyUtilization -> EShram.MonthlyUtilization
    mkMonthlyUtilization u = EShram.MonthlyUtilization u.month u.noOfDays u.uanNo u.year

mkRecordResult :: EShram.RecordBucket -> RecordResult
mkRecordResult b = RecordResult {recordCount = b.recordCount, dataList = b.dataList}

parseResponseData :: (MonadFlow m, FromJSON a) => Text -> EShram.EShramResp -> m a
parseResponseData apiName resp = case (resp.code, resp.responseData) of
  (1, Just dataValue) -> case A.fromJSON dataValue of
    A.Success result -> pure result
    A.Error err -> throwError . InternalError $ "Failed to parse eShram " <> apiName <> " data: " <> T.pack err
  _ -> throwApiFailure apiName resp

throwApiFailure :: MonadFlow m => Text -> EShram.EShramResp -> m a
throwApiFailure apiName resp =
  throwError . InternalError $
    "eShram " <> apiName <> " failed: code=" <> show resp.code <> " message=" <> fromMaybe "<none>" resp.message
