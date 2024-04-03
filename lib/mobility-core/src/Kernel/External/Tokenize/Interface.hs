{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.External.Tokenize.Interface where

import Kernel.External.Encryption as Common (EncFlow)
import qualified Kernel.External.Tokenize.HyperVerge.Types as HyperVergeTypes
import qualified Kernel.External.Tokenize.Interface.HyperVerge as HyperVergeInt
import qualified Kernel.External.Tokenize.Interface.Types as InterfaceTypes
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)

tokenize ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  InterfaceTypes.TokenizationServiceConfig ->
  Int ->
  m HyperVergeTypes.HyperVergeTokenizeResponse
tokenize serviceConfig expSec =
  case serviceConfig of
    InterfaceTypes.HyperVergeTokenizationServiceConfig config -> HyperVergeInt.tokenize config expSec
