{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Tokenize.Interface.JourneyMonitoring where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Kernel.External.Encryption (EncFlow)
import Kernel.External.Tokenize.Interface.Error
import qualified Kernel.External.Tokenize.Interface.Types as InterfaceTypes
import qualified Kernel.External.Tokenize.JourneyMonitoring.Flow as JourneyMonitoringFlow
import qualified Kernel.External.Tokenize.JourneyMonitoring.Types as JourneyMonitoringTypes
import Kernel.Prelude hiding (error)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Error.Throwing (fromMaybeM)
import Kernel.Utils.Servant.Client

tokenize ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  JourneyMonitoringTypes.JourneyMonitoringTokenizeConfig ->
  InterfaceTypes.TokenizationReq ->
  m InterfaceTypes.TokenizationResp
tokenize config _ = do
  resp <- JourneyMonitoringFlow.tokenize config
  makeJourneyMonitoringTokenizeResp resp
  where
    makeJourneyMonitoringTokenizeResp JourneyMonitoringTypes.JourneyMonitoringTokenizeResponse {..} = do
      token <- fromMaybeM (TokenNotFound "JourneyMonitoring") $ ((.accessToken) =<< tokenData)
      let expiresAt = parseTime =<< (.expiresAt) =<< tokenData
      return $ InterfaceTypes.TokenizationResp {..}

parseTime :: Text -> Maybe UTCTime
parseTime time = A.decode $ A.encode modifyText
  where
    modifyText = T.replace " " "T" time <> "Z"
