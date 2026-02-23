{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Plasma.LMS.Client where

import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.External.Plasma.LMS.Config
import Kernel.External.Plasma.LMS.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant
import Servant.Client.Core (ClientError)

-- | LMS API interface for getting modules
type GetModulesAPI =
  "api"
    :> "ui"
    :> "lms"
    :> "modules"
    :> Header "x-api-key" Text
    :> Header "x-person-id" Text
    :> Header "Content-Type" Text
    :> Get '[JSON] LMSModulesResp

lmsModulesAPI :: Proxy GetModulesAPI
lmsModulesAPI = Proxy

-- | Call LMS API to get modules
getModules ::
  ( CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  LMSCfg ->
  Text -> -- Decrypted API key
  Text -> -- Driver ID (person-id)
  m LMSModulesResp
getModules LMSCfg {..} decryptedApiKey driverId = do
  withLogTag "LMS" $ do
    let eulerClient = ET.client lmsModulesAPI (Just decryptedApiKey) (Just driverId) (Just "application/json")
    res <- callAPI url eulerClient "getModules" lmsModulesAPI
    checkLMSError url res

checkLMSError :: (MonadThrow m, Log m) => BaseUrl -> Either ClientError LMSModulesResp -> m LMSModulesResp
checkLMSError url res =
  fromEitherM (lmsError url) res

lmsError :: BaseUrl -> ClientError -> ExternalAPICallError
lmsError = ExternalAPICallError (Just "LMS_NOT_AVAILABLE")
