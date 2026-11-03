{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

 distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

 FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

 General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Grid.Flow
  ( GridDecisionAPI,
    decide,
  )
where

import qualified EulerHS.Types as ET
import Kernel.External.Encryption
import qualified Kernel.External.Grid.Error as Grid
import Kernel.External.Grid.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Servant hiding (throwError)

type GridDecisionAPI =
  "v1"
    :> "decisions"
    :> Header "Authorization" Text
    :> Header "x-litellm-api-key" Text
    :> ReqBody '[JSON] GridDecisionReq
    :> Post '[JSON] GridDecisionResp

-- | Calls @POST /v1/decisions@. Uses the 'gridHttpManagerKey' manager, so consumers must register
-- 'prepareGridHttpManager'. Non-2xx responses throw 'Grid.GridError' (or 'ExternalAPICallError').
-- No automatic retries: the caller decides, see 'Grid.isGridRetryable'.
decide ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  GridCfg ->
  GridDecisionReq ->
  m GridDecisionResp
decide cfg req = do
  apiKey <- decrypt cfg.apiKey
  let (bearer, litellmKey) = case fromMaybe AuthorizationBearer cfg.authHeader of
        AuthorizationBearer -> (Just $ "Bearer " <> apiKey, Nothing)
        XLiteLLMApiKey -> (Nothing, Just apiKey)
      eulerClient = ET.client (Proxy @GridDecisionAPI) bearer litellmKey req
  logDebug $ "Grid decision request: " <> show req
  resp <-
    callApiUnwrappingApiError
      (identity @Grid.GridError)
      (Just $ ET.ManagerSelector gridHttpManagerKey)
      (Just "GRID_API_ERROR")
      Nothing
      cfg.url
      eulerClient
      "GRID-DECISION-API"
      (Proxy @GridDecisionAPI)
  logInfo $
    "Grid decision: model=" <> show resp.model
      <> ", answers="
      <> show (length resp.answers)
      <> "/"
      <> show (length $ questionKeys req.questions)
      <> ", usage="
      <> show resp.usage
  pure resp
