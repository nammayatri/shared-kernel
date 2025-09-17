{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Idfy.WebhookHandler where

import qualified Data.Aeson as A
import Data.Aeson.Types as DAT
import qualified Data.Aeson.Types as A
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import EulerHS.Prelude
import Kernel.External.Verification.Idfy.Auth
import Kernel.External.Verification.Idfy.Config
import Kernel.External.Verification.Idfy.Types.Response
import Kernel.External.Verification.Interface.Types
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.IOLogging

data WebhookHandler m verificationReq = WebhookHandler
  { findVerificationRequestByRequestId :: Text -> m verificationReq,
    getVerificationImageType :: verificationReq -> Maybe ImageType,
    verifyDLHandler :: verificationReq -> DLVerificationResponse -> Text -> m AckResponse,
    verifyPanHandler :: verificationReq -> PanVerificationResponse -> Text -> m AckResponse,
    verifyGstHandler :: verificationReq -> GstVerificationResponse -> Text -> m AckResponse,
    verifyRCHandler :: verificationReq -> RCVerificationResponse -> Text -> m AckResponse
  }

webhookHandler ::
  forall verificationReq m r.
  ( EncFlow m r,
    HasField "isShuttingDown" r (TMVar ()),
    HasField "coreMetrics" r CoreMetricsContainer,
    HasField "loggerEnv" r LoggerEnv
  ) =>
  WebhookHandler m verificationReq ->
  IdfyCfg ->
  Maybe Text ->
  Value ->
  m AckResponse
webhookHandler h cfg secret val = do
  withLogTag "webhookIdfy" $ do
    requestId :: Text <- case A.parse parseRequestId val of
      A.Success reqId -> pure reqId
      A.Error err -> throwError (InternalError $ T.pack err)
    verificationReq <- h.findVerificationRequestByRequestId requestId
    case h.getVerificationImageType verificationReq of
      Just DriverLicense -> webhookHandlerGeneric @DLVerificationResponse cfg verificationReq h.verifyDLHandler secret val
      Just VehicleRegistrationCertificate -> webhookHandlerGeneric @RCVerificationResponse cfg verificationReq h.verifyRCHandler secret val
      Just PanCard -> webhookHandlerGeneric @PanVerificationResponse cfg verificationReq h.verifyPanHandler secret val
      Just GSTCertificate -> webhookHandlerGeneric @GstVerificationResponse cfg verificationReq h.verifyGstHandler secret val
      _ -> throwError $ InternalError "Image type not supported"

parseRequestId :: Value -> A.Parser Text
parseRequestId = \case
  A.Object o -> o .: "request_id"
  A.Array arr -> case V.toList arr of
    A.Object o1 : _ -> o1 .: "request_id"
    _ -> fail "Failed to parse request_id"
  _ -> fail "Failed to parse request_id"

webhookHandlerGeneric ::
  forall verificationResponse verificationReq m r.
  ( EncFlow m r,
    HasField "isShuttingDown" r (TMVar ()),
    HasField "coreMetrics" r CoreMetricsContainer,
    HasField "loggerEnv" r LoggerEnv,
    FromJSON verificationResponse
  ) =>
  IdfyCfg ->
  verificationReq ->
  (verificationReq -> verificationResponse -> Text -> m AckResponse) ->
  Maybe Text ->
  Value ->
  m AckResponse
webhookHandlerGeneric cfg verificationReq verifyHandler secret val = do
  let respDump = encodeToText val
  let mResp = fromJSON val
  case mResp of
    DAT.Success (resp :: verificationResponse) -> do
      void $ verifyAuth cfg secret
      void $ verifyHandler verificationReq resp respDump
      pure Ack
    DAT.Error err1 -> do
      logInfo $ "Error 1: " <> show err1
      let mRespList = fromJSON val
      case mRespList of
        DAT.Success (respList :: [verificationResponse]) -> do
          let mResp_ = listToMaybe respList
          case mResp_ of
            Just resp_ -> do
              void $ verifyAuth cfg secret
              void $ verifyHandler verificationReq resp_ respDump
              pure Ack
            Nothing -> pure Ack
        DAT.Error err -> do
          logInfo $ "Error 2: " <> show err
          pure Ack
