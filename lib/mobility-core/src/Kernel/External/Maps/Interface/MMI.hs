module Kernel.External.Maps.Interface.MMI
  ( autoSuggest,
  )
where

import Kernel.External.Maps.Interface.Types as IT
import Kernel.External.Maps.MMI.AutoSuggest as MMI
import Kernel.External.Maps.MMI.Config
import Kernel.External.Maps.MMI.MMIAuthToken as MMIAuthToken
import qualified Kernel.External.Maps.MMI.MapsClient.Types as MMI
import Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Data.Maybe
import EulerHS.Prelude

autoSuggest ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  MMICfg ->
  IT.AutoCompleteReq ->
  m IT.AutoCompleteResp
autoSuggest mmiCfg AutoCompleteReq {..} = do
  let query = input
      loc = location
      region = "ind"
      lang = language
      mapsUrl = mmiCfg.mmiNonKeyUrl
  token <- MMIAuthToken.getTokenText mmiCfg
  res <- MMI.mmiAutoSuggest mapsUrl (Just $ MMI.MMIAuthToken token) query loc region lang
  let predictions = map (\MMI.SuggestedLocations {..} -> Prediction {placeId = Just eLoc, description = placeName <> " " <> placeAddress}) res.suggestedLocations
  return $ AutoCompleteResp predictions
