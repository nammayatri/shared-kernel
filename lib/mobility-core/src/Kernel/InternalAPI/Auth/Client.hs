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

authAPI :: Text -> E.EulerClient PersonId
authAPI = E.client (Proxy @API)

auth ::
  ( HasField "authServiceUrl" r BaseUrl,
    CoreMetrics m,
    MonadFlow m,
    MonadReader r m
  ) =>
  Text ->
  m PersonId
auth token = do
  url <- asks (.authServiceUrl)
  callOwnAPI Nothing (Just "AUTH_FAILED") url (authAPI token) "auth"
    `catchOwnAPI` throwError . \case
      "INVALID_TOKEN" -> InvalidToken token
      "TOKEN_IS_NOT_VERIFIED" -> TokenIsNotVerified
      "TOKEN_EXPIRED" -> TokenExpired
