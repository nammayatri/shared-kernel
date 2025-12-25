module Kernel.External.Weather.Interface.OpenWeatherMap (getWeather) where

import Kernel.External.Encryption
import qualified Kernel.External.Weather.OpenWeatherMap.API as OWM
import Kernel.External.Weather.OpenWeatherMap.Config
import Kernel.External.Weather.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)

getWeather :: (CoreMetrics m, EncFlow m r) => OpenWeatherCfg -> WeatherReq -> m WeatherResponse
getWeather config req = do
  let url = config.url
      lat = req.latitude
      lon = req.longitude
  apiKey <- decrypt config.apiKey
  OWM.getWeather url apiKey lat lon
