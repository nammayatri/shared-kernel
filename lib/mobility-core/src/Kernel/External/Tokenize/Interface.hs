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
import qualified Kernel.External.Tokenize.Interface.Gullak as GullakInt
import qualified Kernel.External.Tokenize.Interface.HyperVerge as HyperVergeInt
import qualified Kernel.External.Tokenize.Interface.Types as InterfaceTypes
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Utils.Error.Throwing (throwError)

tokenize ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  InterfaceTypes.TokenizationServiceConfig ->
  InterfaceTypes.TokenizationReq ->
  m InterfaceTypes.TokenizationResp
tokenize serviceConfig expSec =
  case serviceConfig of
    InterfaceTypes.HyperVergeTokenizationServiceConfig config -> HyperVergeInt.tokenize config expSec
    _ -> throwError (InternalError "Unsupported Tokenization Service")

onboard ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  InterfaceTypes.TokenizationServiceConfig ->
  InterfaceTypes.OnboardingReq ->
  m InterfaceTypes.OnboardingAndLoginRes
onboard serviceConfig req = do
  case serviceConfig of
    InterfaceTypes.GullakTokenizationServiceConfig config -> GullakInt.gullakOnboarding config req
    _ -> throwError (InternalError "Unsupported Tokenization Service")

login ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  InterfaceTypes.TokenizationServiceConfig ->
  InterfaceTypes.LoginReq ->
  m InterfaceTypes.OnboardingAndLoginRes
login serviceConfig req = do
  case serviceConfig of
    InterfaceTypes.GullakTokenizationServiceConfig config -> GullakInt.gullakLogin config req
    _ -> throwError (InternalError "Unsupported Tokenization Service")
