{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Tokenize.Interface.HyperVerge where

import Kernel.External.Encryption (EncFlow, decrypt)
import qualified Kernel.External.Tokenize.HyperVerge.Flow as HyperVergeFlow
import qualified Kernel.External.Tokenize.HyperVerge.Types as HyperVergeTypes
import Kernel.External.Tokenize.Interface.Error
import qualified Kernel.External.Tokenize.Interface.Types as InterfaceTypes
import Kernel.Prelude hiding (error)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Error.Throwing (fromMaybeM)

tokenize ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  HyperVergeTypes.HyperVergeTokenizeConfig ->
  InterfaceTypes.TokenizationReq ->
  m InterfaceTypes.TokenizationResp
tokenize config req = do
  hvReq <- makeHyperVergeTokenizeRequest config req
  resp <- HyperVergeFlow.tokenize config.url hvReq
  makeHyperVergeTokenizeResp resp
  where
    makeHyperVergeTokenizeRequest :: EncFlow m r => HyperVergeTypes.HyperVergeTokenizeConfig -> InterfaceTypes.TokenizationReq -> m HyperVergeTypes.HyperVergeTokenizeRequest
    makeHyperVergeTokenizeRequest HyperVergeTypes.HyperVergeTokenizeConfig {..} InterfaceTypes.TokenizationReq {..} = do
      appkey' <- decrypt appKey
      return $
        HyperVergeTypes.HyperVergeTokenizeRequest
          { appKey = appkey',
            expiry = fromMaybe 0 expiry,
            ..
          }
    makeHyperVergeTokenizeResp HyperVergeTypes.HyperVergeTokenizeResponse {..} = do
      token <- fromMaybeM (TokenNotFound "HyperVerge") $ result <&> (.token)
      let expiresAt = Nothing
          scope = Nothing
      return $ InterfaceTypes.TokenizationResp {..}
