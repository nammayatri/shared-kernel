module Kernel.External.Weather.Interface where

import qualified Kernel.External.Weather.Interface.OpenWeatherMap as OWM
import Kernel.External.Weather.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common

getWeather ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  WeatherServiceConfig ->
  WeatherReq ->
  m WeatherResponse
getWeather serviceConfig req = case serviceConfig of
  OpenWeatherMapConfig cfg -> OWM.getWeather cfg req
