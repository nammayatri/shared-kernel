{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Tokenize.Gullak.Flow where

import qualified Data.Aeson as A
import qualified Data.Text.Encoding as DT
import EulerHS.Types (client)
import qualified Kernel.External.Tokenize.Gullak.Types as Gullak
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common
import Servant hiding (throwError)

type GullakOnboardingAPI =
  "customer"
    :> "onboard"
    :> BasicAuth "username-password" BasicAuthData
    :> ReqBody '[JSON] Gullak.OnboardingReq
    :> Post '[JSON] Gullak.OnboardingAndLoginRes

type GullakLoginAPI =
  "customer"
    :> "authenticate"
    :> BasicAuth "username-password" BasicAuthData
    :> ReqBody '[JSON] Gullak.LoginReq
    :> Post '[JSON] Gullak.OnboardingAndLoginRes

gullakOnboarding ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Gullak.OnboardingReq ->
  m Gullak.OnboardingAndLoginRes
gullakOnboarding url apiKey merchantId req = do
  let eulerClient = client (Proxy @GullakOnboardingAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 merchantId,
            basicAuthPassword = DT.encodeUtf8 apiKey
          }
  void $ logDebug $ "Gullak Onboarding req" <> (show $ A.encode req)
  callAPI url (eulerClient basicAuthData req) "Gullak onbaording" (Proxy @GullakOnboardingAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call Gullak onbaording API: " <> show err)

gullakLogin ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Gullak.LoginReq ->
  m Gullak.OnboardingAndLoginRes
gullakLogin url apiKey merchantId req = do
  let eulerClient = client (Proxy @GullakLoginAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 merchantId,
            basicAuthPassword = DT.encodeUtf8 apiKey
          }
  void $ logDebug $ "Gullak login req" <> (show $ A.encode req)
  callAPI url (eulerClient basicAuthData req) "Gullak login" (Proxy @GullakLoginAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call Gullak login API: " <> show err)
