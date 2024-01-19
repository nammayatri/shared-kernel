{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.GoogleTranslate.Client where

import Control.Lens ((^.))
import Data.Generics.Product (HasField')
import EulerHS.Prelude hiding ((^.))
import qualified Kernel.External.GoogleTranslate.API as API
import qualified Kernel.External.GoogleTranslate.Types as GoogleTranslate
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant.Client.Core (ClientError)

translate ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Text ->
  m GoogleTranslate.TranslateResp
translate url apiKey source target query = do
  callAPI url (API.translate apiKey source target query) "translate" API.googleTranslateAPI
    >>= checkGoogleTranslateError url

checkGoogleTranslateError :: (MonadThrow m, Log m, HasField' "_error" a (Maybe GoogleTranslate.TranslateError)) => BaseUrl -> Either ClientError a -> m a
checkGoogleTranslateError url res =
  fromEitherM (googleTranslateError url) res >>= validateResponseStatus

googleTranslateError :: BaseUrl -> ClientError -> ExternalAPICallError
googleTranslateError = ExternalAPICallError (Just "GOOGLE_TRANSLATE_API_ERROR")

validateResponseStatus :: (MonadThrow m, Log m, HasField' "_error" a (Maybe GoogleTranslate.TranslateError)) => a -> m a
validateResponseStatus response =
  case response ^. #_error of
    Nothing -> pure response
    _ -> throwError GoogleTranslateInvalidRequest
