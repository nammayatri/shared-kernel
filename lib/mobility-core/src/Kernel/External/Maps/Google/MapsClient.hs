{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.Google.MapsClient
  ( module GoogleMaps,
    GoogleMapsAPI,
    AutocompleteAPI,
    AutoCompleteV2API,
    PlaceDetailsAPI,
    PlaceNameAPI,
    DistanceMatrixAPI,
    DirectionsAPI,
    AdvancedDirectionsAPI,
    TransitDirectionsAPI,
    advancedDirectionsAPI,
    transitDirectionsAPI,
    autoComplete,
    getPlaceDetails,
    getPlaceName,
    distanceMatrix,
    directions,
    autoCompleteV2,
  )
where

import Data.Text as T
import EulerHS.Types (EulerClient, client)
import Kernel.External.Maps.Google.MapsClient.Types as GoogleMaps
import qualified Kernel.External.Maps.Interface.Types as MapsInterfaceTypes
import Kernel.External.Maps.Types
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Kernel.Utils.ExternalAPICallLogging as ApiCallLogger
import Servant hiding (throwError)
import Servant.Client.Core (ClientError)

type GoogleMapsAPI =
  AutocompleteAPI
    :<|> AutoCompleteV2API
    :<|> PlaceDetailsAPI
    :<|> PlaceNameAPI
    :<|> DistanceMatrixAPI
    :<|> DirectionsAPI
    :<|> AdvancedDirectionsAPI
    :<|> TransitDirectionsAPI

type AutocompleteAPI =
  "place" :> "autocomplete" :> "json"
    :> Header "sessiontoken" Text
    :> MandatoryQueryParam "key" Text
    :> MandatoryQueryParam "input" Text
    :> MandatoryQueryParam "location" Text
    :> MandatoryQueryParam "radius" Integer
    :> MandatoryQueryParam "components" Text
    :> MandatoryQueryParam "language" Language
    :> QueryParam "strictbounds" Bool
    :> QueryParam "origin" LatLong
    :> QueryParam "types" Text
    :> Get '[JSON] GoogleMaps.AutoCompleteResp

type AutoCompleteV2API =
  "places" :> ":autocomplete"
    :> MandatoryHeader "X-Goog-Api-Key" Text
    :> MandatoryQueryParam "languageCode" Language
    :> ReqBody '[JSON] (GoogleMaps.AutoCompleteReqV2)
    :> Post '[JSON] GoogleMaps.AutoCompleteRespV2

type PlaceDetailsAPI =
  "place" :> "details" :> "json"
    :> Header "sessiontoken" Text
    :> MandatoryQueryParam "key" Text
    :> MandatoryQueryParam "place_id" Text
    :> MandatoryQueryParam "fields" Text
    :> Get '[JSON] GoogleMaps.GetPlaceDetailsResp

type PlaceNameAPI =
  "geocode" :> "json"
    :> Header "sessiontoken" Text
    :> MandatoryQueryParam "key" Text
    :> QueryParam "latlng" LatLong -- Parameters order is important.
    :> QueryParam "place_id" Text
    :> QueryParam "language" Language
    :> Get '[JSON] GoogleMaps.GetPlaceNameResp

type DistanceMatrixAPI =
  "distancematrix" :> "json"
    :> MandatoryQueryParam "origins" [GoogleMaps.Place]
    :> MandatoryQueryParam "destinations" [GoogleMaps.Place]
    :> MandatoryQueryParam "key" Text
    :> QueryParam "mode" GoogleMaps.Mode
    :> QueryParam "avoid" Text
    :> Post '[JSON] GoogleMaps.DistanceMatrixResp

type DirectionsAPI =
  "directions" :> "json"
    :> MandatoryQueryParam "origin" GoogleMaps.Place
    :> MandatoryQueryParam "destination" GoogleMaps.Place
    :> MandatoryQueryParam "key" Text
    :> QueryParam "alternatives" Bool
    :> QueryParam "mode" GoogleMaps.Mode
    :> QueryParam "waypoints" [GoogleMaps.Place]
    :> QueryParam "avoid" Text
    :> Get '[JSON] GoogleMaps.DirectionsResp

type AdvancedDirectionsAPI =
  "directions" :> "v2" :> ":computeRoutes"
    :> MandatoryHeader "X-Goog-Api-Key" Text
    :> MandatoryHeader "X-Goog-FieldMask" Text
    :> ReqBody '[JSON] (GoogleMaps.AdvancedDirectionsReq)
    :> Post '[JSON] GoogleMaps.AdvancedDirectionsResp

type TransitDirectionsAPI =
  "directions" :> "v2" :> ":computeRoutes"
    :> MandatoryHeader "X-Goog-Api-Key" Text
    :> MandatoryHeader "X-Goog-FieldMask" Text
    :> ReqBody '[JSON] (GoogleMaps.TransitDirectionsReq)
    :> Post '[JSON] GoogleMaps.AdvancedDirectionsResp

autoCompleteClient :: Maybe Text -> Text -> Text -> Text -> Integer -> Text -> Language -> Maybe Bool -> Maybe LatLong -> Maybe Text -> EulerClient GoogleMaps.AutoCompleteResp
autoCompleteV2Client :: Text -> Language -> GoogleMaps.AutoCompleteReqV2 -> EulerClient GoogleMaps.AutoCompleteRespV2
getPlaceDetailsClient :: Maybe Text -> Text -> Text -> Text -> EulerClient GoogleMaps.GetPlaceDetailsResp
getPlaceNameClient :: Maybe Text -> Text -> Maybe LatLong -> Maybe Text -> Maybe Language -> EulerClient GoogleMaps.GetPlaceNameResp
distanceMatrixClient ::
  [GoogleMaps.Place] ->
  [GoogleMaps.Place] ->
  Text ->
  Maybe GoogleMaps.Mode ->
  Maybe Text ->
  EulerClient GoogleMaps.DistanceMatrixResp
directionsClient ::
  GoogleMaps.Place ->
  GoogleMaps.Place ->
  Text ->
  Maybe Bool ->
  Maybe GoogleMaps.Mode ->
  Maybe [GoogleMaps.Place] ->
  Maybe Text ->
  EulerClient GoogleMaps.DirectionsResp
advancedDirectionsClient ::
  Text ->
  Text ->
  GoogleMaps.AdvancedDirectionsReq ->
  EulerClient GoogleMaps.AdvancedDirectionsResp
transitDirectionsClient ::
  Text ->
  Text ->
  GoogleMaps.TransitDirectionsReq ->
  EulerClient GoogleMaps.AdvancedDirectionsResp
autoCompleteClient :<|> autoCompleteV2Client :<|> getPlaceDetailsClient :<|> getPlaceNameClient :<|> distanceMatrixClient :<|> directionsClient :<|> advancedDirectionsClient :<|> transitDirectionsClient = client (Proxy :: Proxy GoogleMapsAPI)

autoComplete ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MapsInterfaceTypes.AutoCompleteReq ->
  BaseUrl ->
  Text ->
  Text ->
  Maybe Text ->
  Text ->
  Integer ->
  Text ->
  Language ->
  Maybe Bool ->
  Maybe LatLong ->
  Maybe Text ->
  m GoogleMaps.AutoCompleteResp
autoComplete entityId req url apiKey input sessiontoken location radius components lang strictBounds origin types = do
  rsp <- callAPI url (autoCompleteClient sessiontoken apiKey input location radius components lang strictBounds origin types) "autoComplete" (Proxy :: Proxy GoogleMapsAPI)
  fork ("Logging external API Call of autoComplete Google ") $
    ApiCallLogger.pushExternalApiCallDataToKafka "autoComplete" "Google" entityId (Just req) rsp
  checkGoogleMapsError url rsp

autoCompleteV2 ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  BaseUrl ->
  Text ->
  Language ->
  GoogleMaps.AutoCompleteReqV2 ->
  m GoogleMaps.AutoCompleteRespV2
autoCompleteV2 entityId url apiKey language req = do
  rsp <- callAPI url (autoCompleteV2Client apiKey language req) "autoCompleteV2" (Proxy :: Proxy GoogleMapsAPI)
  fork ("Logging external API Call of autoCompleteV2 Google ") $
    ApiCallLogger.pushExternalApiCallDataToKafka "autoCompleteV2" "Google" entityId (Just req) rsp
  checkGooglePlaceError url rsp

getPlaceDetails ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MapsInterfaceTypes.GetPlaceDetailsReq ->
  BaseUrl ->
  Text ->
  Maybe Text ->
  Text ->
  Text ->
  m GoogleMaps.GetPlaceDetailsResp
getPlaceDetails entityId req url apiKey sessiontoken placeId fields = do
  rsp <- callAPI url (getPlaceDetailsClient sessiontoken apiKey placeId fields) "getPlaceDetails" (Proxy :: Proxy GoogleMapsAPI)
  fork ("Logging external API Call of getPlaceDetails Google ") $
    ApiCallLogger.pushExternalApiCallDataToKafka "getPlaceDetails" "Google" entityId (Just req) rsp
  checkGoogleMapsError url rsp

getPlaceName ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MapsInterfaceTypes.GetPlaceNameReq ->
  BaseUrl ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe LatLong ->
  Maybe Language ->
  m GoogleMaps.GetPlaceNameResp
getPlaceName entityId req url apiKey sessiontoken mbByPlaceId mbByLatLong language = do
  rsp <- callAPI url (getPlaceNameClient sessiontoken apiKey mbByLatLong mbByPlaceId language) "getPlaceName" (Proxy :: Proxy GoogleMapsAPI)
  fork ("Logging external API Call of getPlaceName Google ") $
    ApiCallLogger.pushExternalApiCallDataToKafka "getPlaceName" "Google" entityId (Just req) rsp
  checkGoogleMapsError url rsp

distanceMatrix ::
  ( CoreMetrics m,
    MonadFlow m,
    ToJSON a,
    ToJSON b,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MapsInterfaceTypes.GetDistancesReq a b ->
  BaseUrl ->
  Text ->
  [GoogleMaps.Place] ->
  [GoogleMaps.Place] ->
  Maybe GoogleMaps.Mode ->
  Bool ->
  m GoogleMaps.DistanceMatrixResp
distanceMatrix entityId req url key origins destinations mode isAvoidTolls = do
  let avoidToll = if isAvoidTolls then Just "tolls" else Nothing
  let avoid = T.intercalate "|" $ catMaybes [avoidToll, Just "ferries"]
  rsp <- callAPI url (distanceMatrixClient origins destinations key mode (Just avoid)) "distanceMatrix" (Proxy :: Proxy GoogleMapsAPI)
  fork ("Logging external API Call of distanceMatrix Google ") $
    ApiCallLogger.pushExternalApiCallDataToKafka "distanceMatrix" "Google" entityId (Just req) rsp
  checkGoogleMapsError url rsp
    >>= \resp -> do
      mapM_ (mapM validateResponseStatus . (.elements)) resp.rows
      return resp

directions ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MapsInterfaceTypes.GetRoutesReq ->
  BaseUrl ->
  Text ->
  GoogleMaps.Place ->
  GoogleMaps.Place ->
  Maybe GoogleMaps.Mode ->
  Maybe [GoogleMaps.Place] ->
  Bool ->
  m GoogleMaps.DirectionsResp
directions entityId req url key origin destination mode waypoints isAvoidTolls = do
  let avoidToll = if isAvoidTolls then Just "tolls" else Nothing
  let avoid = T.intercalate "|" $ catMaybes [avoidToll, Just "ferries"]
  rsp <- callAPI url (directionsClient origin destination key (Just True) mode waypoints (Just avoid)) "directionsAPI" (Proxy :: Proxy GoogleMapsAPI)
  fork ("Logging external API Call of directions Google ") $
    ApiCallLogger.pushExternalApiCallDataToKafka "directions" "Google" entityId (Just req) rsp
  checkGoogleMapsError url rsp

transitDirectionsAPI ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  BaseUrl ->
  Text ->
  GoogleMaps.WayPointV2 ->
  GoogleMaps.WayPointV2 ->
  Maybe GoogleMaps.ModeV2 ->
  Bool ->
  GoogleMaps.RoutingPreference ->
  Maybe GoogleMaps.TransitPreferencesV2 ->
  Maybe String ->
  Maybe String ->
  m GoogleMaps.AdvancedDirectionsResp
transitDirectionsAPI entityId url key origin destination mode computeAlternativeRoutes routingPreference transitPreferences arrivalTime departureTime = do
  let travelMode = mode
      routeModifiers = Nothing
      req = GoogleMaps.TransitDirectionsReq {..}
  rsp <- callAPI url (transitDirectionsClient key "routes.*" req) "transitDirectionsAPI" (Proxy :: Proxy GoogleMapsAPI)
  fork ("Logging external API Call of transitDirectionsAPI Google ") $
    ApiCallLogger.pushExternalApiCallDataToKafka "transitDirectionsAPI" "Google" entityId (Just req) rsp
  checkGoogleMapsError' url rsp

advancedDirectionsAPI ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  BaseUrl ->
  Text ->
  GoogleMaps.WayPointV2 ->
  GoogleMaps.WayPointV2 ->
  Maybe GoogleMaps.ModeV2 ->
  Maybe [GoogleMaps.WayPointV2] ->
  Bool ->
  Bool ->
  GoogleMaps.RoutingPreference ->
  m GoogleMaps.AdvancedDirectionsResp
advancedDirectionsAPI entityId url key origin destination mode intermediates isAvoidTolls computeAlternativeRoutes routingPreference = do
  let routeModifiers = GoogleMaps.RouteModifiers {avoidTolls = if isAvoidTolls then Just True else Nothing, avoidFerries = True}
      travelMode = mode
      req = GoogleMaps.AdvancedDirectionsReq {..}
  rsp <- callAPI url (advancedDirectionsClient key "routes.legs.*,routes.distanceMeters,routes.duration,routes.staticDuration.*,routes.viewport.*,routes.polyline.*,routes.routeLabels.*" req) "advancedDirectionsAPI" (Proxy :: Proxy GoogleMapsAPI)
  fork ("Logging external API Call of advancedDirectionsAPI Google ") $
    ApiCallLogger.pushExternalApiCallDataToKafka "advancedDirectionsAPI" "Google" entityId (Just req) rsp
  checkGoogleMapsError' url rsp

checkGoogleMapsError :: (MonadThrow m, Log m, HasField "status" a Text) => BaseUrl -> Either ClientError a -> m a
checkGoogleMapsError url res =
  fromEitherM (googleMapsError url) res >>= validateResponseStatus

checkGooglePlaceError :: (MonadThrow m, Log m, HasField "suggestions" a [GoogleMaps.Suggestion]) => BaseUrl -> Either ClientError a -> m a
checkGooglePlaceError url res =
  fromEitherM (googleMapsError url) res

checkGoogleMapsError' :: (MonadThrow m, Log m, HasField "routes" a [GoogleMaps.RouteV2]) => BaseUrl -> Either ClientError a -> m a
checkGoogleMapsError' url res =
  fromEitherM (googleMapsError url) res

googleMapsError :: BaseUrl -> ClientError -> ExternalAPICallError
googleMapsError = ExternalAPICallError (Just "GOOGLE_MAPS_API_ERROR")

validateResponseStatus :: (MonadThrow m, Log m, HasField "status" a Text) => a -> m a
validateResponseStatus response =
  case response.status of
    "OK" -> pure response
    "ZERO_RESULTS" -> pure response
    "INVALID_REQUEST" -> throwError GoogleMapsInvalidRequest
    _ -> throwError $ GoogleMapsCallError response.status
