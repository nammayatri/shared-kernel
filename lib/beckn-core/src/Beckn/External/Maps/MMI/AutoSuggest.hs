{-# LANGUAGE TypeApplications #-}

module Beckn.External.Maps.MMI.AutoSuggest where

import qualified Beckn.External.Maps.MMI.MapsClient.Types as MMI
import Beckn.External.Types (Language)
import Beckn.Storage.Hedis as Redis
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import Data.Maybe
import EulerHS.Prelude
import EulerHS.Types as ET
import Servant

type MMIAutoSuggestAPI =
  "api" :> "places" :> "search" :> "json"
    :> Header "Authorization" MMI.MMIAuthToken
    :> MandatoryQueryParam "query" Text
    :> MandatoryQueryParam "location" Text
    :> MandatoryQueryParam "region" Text
    :> MandatoryQueryParam "responseLang" Language
    :> Get '[JSON] MMI.AutoSuggestResp

mmiAutoSuggestAPI :: Proxy MMIAutoSuggestAPI
mmiAutoSuggestAPI = Proxy

mmiAutoSuggestClient :: Maybe MMI.MMIAuthToken -> Text -> Text -> Text -> Language -> ET.EulerClient MMI.AutoSuggestResp
mmiAutoSuggestClient = ET.client mmiAutoSuggestAPI

mmiAutoSuggest ::
  ( CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  BaseUrl ->
  Maybe MMI.MMIAuthToken ->
  Text ->
  Text ->
  Text ->
  Language ->
  m MMI.AutoSuggestResp
mmiAutoSuggest url authToken query location region lang = do
  callMMIAutoSuggestAPI
    url
    (mmiAutoSuggestClient authToken query location region lang)
    "mmi-auto-suggest"

callMMIAutoSuggestAPI :: CallAPI env a
callMMIAutoSuggestAPI =
  callApiUnwrappingApiError
    (identity @MMIError)
    Nothing
    (Just "MMI_AUTO_SUGGEST_ERROR")
