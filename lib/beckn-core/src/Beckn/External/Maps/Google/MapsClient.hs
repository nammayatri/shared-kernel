module Beckn.External.Maps.Google.MapsClient
  ( module GoogleMaps,
    autoComplete,
    placeDetails,
    getPlaceName,
    distanceMatrix,
    directions,
  )
where

import Beckn.External.Maps.Google.MapsClient.Types as GoogleMaps
import Beckn.External.Maps.Types (LatLong)
import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import EulerHS.Types (EulerClient, client)
import Servant hiding (throwError)
import Servant.Client.Core (ClientError)

type GoogleMapsAPI =
  AutocompleteAPI
    :<|> PlaceDetailsAPI
    :<|> PlaceNameAPI
    :<|> DistanceMatrixAPI
    :<|> DirectionsAPI

type AutocompleteAPI =
  "place" :> "autocomplete" :> "json"
    :> Header "sessiontoken" Text
    :> MandatoryQueryParam "key" Text
    :> MandatoryQueryParam "input" Text
    :> MandatoryQueryParam "location" Text
    :> MandatoryQueryParam "radius" Integer
    :> MandatoryQueryParam "components" Text
    :> MandatoryQueryParam "language" GoogleMaps.Language
    :> Get '[JSON] GoogleMaps.SearchLocationResp

type PlaceDetailsAPI =
  "place" :> "details" :> "json"
    :> Header "sessiontoken" Text
    :> MandatoryQueryParam "key" Text
    :> MandatoryQueryParam "place_id" Text
    :> MandatoryQueryParam "fields" Text
    :> Get '[JSON] GoogleMaps.PlaceDetailsResp

type PlaceNameAPI =
  "geocode" :> "json"
    :> Header "sessiontoken" Text
    :> MandatoryQueryParam "key" Text
    :> QueryParam "latlng" LatLong -- Parameters order is important.
    :> QueryParam "place_id" Text
    :> QueryParam "language" GoogleMaps.Language
    :> Get '[JSON] GoogleMaps.GetPlaceNameResp

type DistanceMatrixAPI =
  "distancematrix" :> "json"
    :> MandatoryQueryParam "origins" [GoogleMaps.Place]
    :> MandatoryQueryParam "destinations" [GoogleMaps.Place]
    :> MandatoryQueryParam "key" Text
    :> QueryParam "mode" GoogleMaps.Mode
    :> Post '[JSON] GoogleMaps.DistanceMatrixResp

type DirectionsAPI =
  "directions" :> "json"
    :> MandatoryQueryParam "origin" GoogleMaps.Place
    :> MandatoryQueryParam "destination" GoogleMaps.Place
    :> MandatoryQueryParam "key" Text
    :> QueryParam "alternatives" Bool
    :> QueryParam "mode" GoogleMaps.Mode
    :> QueryParam "waypoints" [GoogleMaps.Place]
    :> Get '[JSON] GoogleMaps.DirectionsResp

autoCompleteClient :: Maybe Text -> Text -> Text -> Text -> Integer -> Text -> GoogleMaps.Language -> EulerClient GoogleMaps.SearchLocationResp
placeDetailsClient :: Maybe Text -> Text -> Text -> Text -> EulerClient GoogleMaps.PlaceDetailsResp
getPlaceNameClient :: Maybe Text -> Text -> Maybe LatLong -> Maybe Text -> Maybe GoogleMaps.Language -> EulerClient GoogleMaps.GetPlaceNameResp
distanceMatrixClient ::
  [GoogleMaps.Place] ->
  [GoogleMaps.Place] ->
  Text ->
  Maybe GoogleMaps.Mode ->
  EulerClient GoogleMaps.DistanceMatrixResp
directionsClient ::
  GoogleMaps.Place ->
  GoogleMaps.Place ->
  Text ->
  Maybe Bool ->
  Maybe GoogleMaps.Mode ->
  Maybe [GoogleMaps.Place] ->
  EulerClient GoogleMaps.DirectionsResp
autoCompleteClient :<|> placeDetailsClient :<|> getPlaceNameClient :<|> distanceMatrixClient :<|> directionsClient = client (Proxy :: Proxy GoogleMapsAPI)

autoComplete ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Maybe Text ->
  Text ->
  Integer ->
  Text ->
  GoogleMaps.Language ->
  m GoogleMaps.SearchLocationResp
autoComplete url apiKey input sessiontoken location radius components lang = do
  callAPI url (autoCompleteClient sessiontoken apiKey input location radius components lang) "autoComplete"
    >>= checkGoogleMapsError url

placeDetails ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Maybe Text ->
  Text ->
  Text ->
  m GoogleMaps.PlaceDetailsResp
placeDetails url apiKey sessiontoken placeId fields = do
  callAPI url (placeDetailsClient sessiontoken apiKey placeId fields) "placeDetails"
    >>= checkGoogleMapsError url

getPlaceName ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Maybe Text ->
  GetPlaceNameBy ->
  Maybe GoogleMaps.Language ->
  m GoogleMaps.GetPlaceNameResp
getPlaceName url apiKey sessiontoken by language = do
  callAPI url clientAPI "getPlaceName"
    >>= checkGoogleMapsError url
  where
    clientAPI = case by of
      ByPlaceId id -> getPlaceNameClient sessiontoken apiKey Nothing (Just id) language
      ByLatLong latLong -> do
        getPlaceNameClient sessiontoken apiKey (Just latLong) Nothing language

distanceMatrix ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  [GoogleMaps.Place] ->
  [GoogleMaps.Place] ->
  Maybe GoogleMaps.Mode ->
  m GoogleMaps.DistanceMatrixResp
distanceMatrix url key origins destinations mode = do
  callAPI url (distanceMatrixClient origins destinations key mode) "distanceMatrix"
    >>= checkGoogleMapsError url
    >>= \resp -> do
      mapM_ (mapM validateResponseStatus . (.elements)) resp.rows
      return resp

directions ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  GoogleMaps.Place ->
  GoogleMaps.Place ->
  Maybe GoogleMaps.Mode ->
  Maybe [GoogleMaps.Place] ->
  m GoogleMaps.DirectionsResp
directions url key origin destination mode waypoints = do
  callAPI url (directionsClient origin destination key (Just True) mode waypoints) "directionsAPI"
    >>= checkGoogleMapsError url

checkGoogleMapsError :: (MonadThrow m, Log m, HasField "status" a Text) => BaseUrl -> Either ClientError a -> m a
checkGoogleMapsError url res =
  fromEitherM (googleMapsError url) res >>= validateResponseStatus

googleMapsError :: BaseUrl -> ClientError -> ExternalAPICallError
googleMapsError = ExternalAPICallError (Just "GOOGLE_MAPS_API_ERROR")

validateResponseStatus :: (MonadThrow m, Log m, HasField "status" a Text) => a -> m a
validateResponseStatus response =
  case response.status of
    "OK" -> pure response
    "ZERO_RESULTS" -> pure response
    "INVALID_REQUEST" -> throwError GoogleMapsInvalidRequest
    _ -> throwError $ GoogleMapsCallError response.status
