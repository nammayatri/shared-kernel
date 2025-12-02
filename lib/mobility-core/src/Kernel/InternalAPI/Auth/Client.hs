{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Kernel.InternalAPI.Auth.Client where

import qualified EulerHS.Types as E
import Kernel.InternalAPI.Auth.API
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Error.BaseError.HTTPError.APIError
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Servant.Client

authAPI :: Text -> E.EulerClient PersonId
authAPI = E.client (Proxy @API)

auth ::
  ( HasField "authServiceUrl" r BaseUrl,
    CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasRequestId r
  ) =>
  Text ->
  m PersonId
auth token = do
  url <- asks (.authServiceUrl)
  callOwnAPI Nothing (Just "AUTH_FAILED") Nothing url (authAPI token) "auth" (Proxy @API)
    `catchOwnAPI` throwError . \case
      "INVALID_TOKEN" -> InvalidToken token
      "TOKEN_IS_NOT_VERIFIED" -> TokenIsNotVerified
      "TOKEN_EXPIRED" -> TokenExpired
