module Kernel.External.Weather.OpenWeatherMap.API where

import EulerHS.Prelude
import EulerHS.Types as Euler
import qualified Kernel.External.Weather.Types as OW
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant

type OpenWeatherAPI =
  "data" :> "2.5" :> "weather"
    :> MandatoryQueryParam "lat" Text
    :> MandatoryQueryParam "lon" Text
    :> MandatoryQueryParam "appid" Text
    :> Get '[JSON] OW.WeatherResponse

getWeather :: (Metrics.CoreMetrics m, MonadFlow m) => BaseUrl -> Text -> Text -> Text -> m OW.WeatherResponse
getWeather url apiKey lat lon = do
  let eulerClient = Euler.client openWeatherAPI lat lon apiKey
  callOpenWeatherAPI url eulerClient "getWeather" openWeatherAPI

openWeatherAPI :: Proxy OpenWeatherAPI
openWeatherAPI = Proxy

callOpenWeatherAPI :: CallAPI' m api res res
callOpenWeatherAPI url eulerClient description proxy = do
  callAPI url eulerClient description proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call " <> description <> " API: " <> show err)
