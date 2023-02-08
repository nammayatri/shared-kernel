module Kernel.External.GoogleTranslate.Client where

import qualified Kernel.External.GoogleTranslate.API as API
import qualified Kernel.External.GoogleTranslate.Types as GoogleTranslate
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import EulerHS.Prelude
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
  callAPI url (API.translate apiKey source target query) "translate"
    >>= checkGoogleTranslateError url

checkGoogleTranslateError :: (MonadThrow m, Log m, HasField "_error" a (Maybe GoogleTranslate.TranslateError)) => BaseUrl -> Either ClientError a -> m a
checkGoogleTranslateError url res =
  fromEitherM (googleTranslateError url) res >>= validateResponseStatus

googleTranslateError :: BaseUrl -> ClientError -> ExternalAPICallError
googleTranslateError = ExternalAPICallError (Just "GOOGLE_TRANSLATE_API_ERROR")

validateResponseStatus :: (MonadThrow m, Log m, HasField "_error" a (Maybe GoogleTranslate.TranslateError)) => a -> m a
validateResponseStatus response =
  case response._error of
    Nothing -> pure response
    _ -> throwError GoogleTranslateInvalidRequest
