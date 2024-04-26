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
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)

tokenize ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  HyperVergeTypes.HyperVergeConfig ->
  Int ->
  m HyperVergeTypes.HyperVergeTokenizeResponse
tokenize config expSec = do
  req <- makeHyperVergeTokenizeRequest config expSec
  HyperVergeFlow.tokenize config.url req
  where
    makeHyperVergeTokenizeRequest :: EncFlow m r => HyperVergeTypes.HyperVergeConfig -> Int -> m HyperVergeTypes.HyperVergeTokenizeRequest
    makeHyperVergeTokenizeRequest HyperVergeTypes.HyperVergeConfig {..} expiry = do
      appkey' <- decrypt appKey
      return $
        HyperVergeTypes.HyperVergeTokenizeRequest
          { appKey = appkey',
            ..
          }
