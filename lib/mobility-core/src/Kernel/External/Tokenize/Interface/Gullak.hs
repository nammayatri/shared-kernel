{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Tokenize.Interface.Gullak where

import Kernel.External.Encryption (EncFlow, decrypt)
import qualified Kernel.External.Tokenize.Gullak.Flow as GullakFlow
import qualified Kernel.External.Tokenize.Gullak.Types as GullakTypes
import qualified Kernel.External.Tokenize.Interface.Types as InterfaceTypes
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Servant.Client

gullakLogin ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  GullakTypes.GullakConfig ->
  InterfaceTypes.LoginReq ->
  m InterfaceTypes.OnboardingAndLoginRes
gullakLogin config req = do
  apikey' <- decrypt config.apiKey
  mkReqOnboardingAndLoginRes <$> GullakFlow.gullakLogin config.url apikey' config.merchantId (mkReqLoginReq req)
  where
    mkReqLoginReq :: InterfaceTypes.LoginReq -> GullakTypes.LoginReq
    mkReqLoginReq InterfaceTypes.LoginReq {..} = GullakTypes.LoginReq {..}

gullakOnboarding ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  GullakTypes.GullakConfig ->
  InterfaceTypes.OnboardingReq ->
  m InterfaceTypes.OnboardingAndLoginRes
gullakOnboarding config req = do
  apikey' <- decrypt config.apiKey
  mkReqOnboardingAndLoginRes <$> GullakFlow.gullakOnboarding config.url apikey' config.merchantId (mkReqOnboardingReq req)
  where
    mkReqOnboardingReq :: InterfaceTypes.OnboardingReq -> GullakTypes.OnboardingReq
    mkReqOnboardingReq InterfaceTypes.OnboardingReq {..} = GullakTypes.OnboardingReq {languageDetails = mkReqLanguageDetails <$> languageDetails, address = mkReqAddress <$> address, locationDetails = mkReqlocationDetails <$> locationDetails, ..}

    mkReqLanguageDetails :: InterfaceTypes.LanguageDetails -> GullakTypes.LanguageDetails
    mkReqLanguageDetails InterfaceTypes.LanguageDetails {..} = GullakTypes.LanguageDetails {..}

    mkReqlocationDetails :: InterfaceTypes.LocationDetails -> GullakTypes.LocationDetails
    mkReqlocationDetails InterfaceTypes.LocationDetails {..} = GullakTypes.LocationDetails {..}

    mkReqAddress :: InterfaceTypes.Address -> GullakTypes.Address
    mkReqAddress InterfaceTypes.Address {..} = GullakTypes.Address {..}

mkReqOnboardingAndLoginRes :: GullakTypes.OnboardingAndLoginRes -> InterfaceTypes.OnboardingAndLoginRes
mkReqOnboardingAndLoginRes GullakTypes.OnboardingAndLoginRes {..} = InterfaceTypes.OnboardingAndLoginRes {loginToken = mkReqLoginToken loginToken, ..}

mkReqLoginToken :: GullakTypes.LoginToken -> InterfaceTypes.LoginToken
mkReqLoginToken GullakTypes.LoginToken {..} = InterfaceTypes.LoginToken {..}
