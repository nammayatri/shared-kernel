{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Call.Ozonetel.Client where

import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.External.Call.Ozonetel.Config
import Kernel.External.Call.Ozonetel.Types
import Kernel.External.Encryption (decrypt)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

-- | Ozonetel API interface
type AddCampaignDataAPI =
  "ca_apis" :> "AddCampaignData"
    :> Header "apiKey" Text
    :> ReqBody '[JSON] OzonetelAddCampaignDataReq
    :> Post '[JSON] OzonetelAddCampaignDataResp

addCampaignData ::
  ( CoreMetrics m,
    EncFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  OzonetelCfg ->
  OzonetelAddCampaignDataReq ->
  m OzonetelAddCampaignDataResp
addCampaignData OzonetelCfg {..} ozonetelRequest = do
  apiKey_ <- decrypt apiKey
  withLogTag "Ozonetel" $ do
    callOzonetelAPI
      url
      (callOzonetel apiKey_)
      "addCampaignData"
      (Proxy :: Proxy AddCampaignDataAPI)
  where
    callOzonetel apiKey_ = ET.client (Proxy :: Proxy AddCampaignDataAPI) (Just apiKey_) ozonetelRequest

callOzonetelAPI :: CallAPI m r api a
callOzonetelAPI =
  callApiUnwrappingApiError
    (identity @OzonetelError)
    Nothing
    (Just "OZONETEL_NOT_AVAILABLE")
    Nothing
