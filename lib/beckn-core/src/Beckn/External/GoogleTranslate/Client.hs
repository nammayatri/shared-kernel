module Beckn.External.GoogleTranslate.Client where

import qualified Beckn.External.GoogleTranslate.API as API
import qualified Beckn.External.GoogleTranslate.Types as GoogleTranslate
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
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
