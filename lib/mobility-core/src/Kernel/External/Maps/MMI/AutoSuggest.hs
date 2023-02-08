{-# LANGUAGE TypeApplications #-}

module Kernel.External.Maps.MMI.AutoSuggest where

import qualified Kernel.External.Maps.MMI.MapsClient.Types as MMI
import Kernel.External.Types (Language)
import Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
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
