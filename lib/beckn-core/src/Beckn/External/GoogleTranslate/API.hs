module Beckn.External.GoogleTranslate.API where

import qualified Beckn.External.GoogleTranslate.Types as GoogleTranslate
import Beckn.Utils.Common
import EulerHS.Prelude
import EulerHS.Types (EulerClient, client)
import Servant

type GoogleTranslateAPI =
  "language" :> "translate" :> "v2"
    :> MandatoryQueryParam "key" Text
    :> MandatoryQueryParam "source" Text
    :> MandatoryQueryParam "target" Text
    :> MandatoryQueryParam "q" Text
    :> Get '[JSON] GoogleTranslate.TranslateResp

translate :: Text -> Text -> Text -> Text -> EulerClient GoogleTranslate.TranslateResp
translate = client googleTranslateAPI

googleTranslateAPI :: Proxy GoogleTranslateAPI
googleTranslateAPI = Proxy
