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
    PlaceDetailsAPI,
    PlaceNameAPI,
    DistanceMatrixAPI,
    DirectionsAPI,
    autoComplete,
    getPlaceDetails,
    getPlaceName,
    distanceMatrix,
    directions,
  )
where

import qualified Data.Aeson as J
import Data.Either.Combinators (whenLeft)
import EulerHS.Types (EulerClient, client)
import Kernel.External.Maps.Google.MapsClient.Types as GoogleMaps
import Kernel.External.Maps.Types
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Network.HTTP.Types (Status (statusCode, statusMessage))
import Servant hiding (throwError)
import Servant.Client.Core ( ClientError, ClientError(FailureResponse), ResponseF(Response) )

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
    :> MandatoryQueryParam "language" Language
    :> Get '[JSON] GoogleMaps.AutoCompleteResp

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

autoCompleteClient :: Maybe Text -> Text -> Text -> Text -> Integer -> Text -> Language -> EulerClient GoogleMaps.AutoCompleteResp
getPlaceDetailsClient :: Maybe Text -> Text -> Text -> Text -> EulerClient GoogleMaps.GetPlaceDetailsResp
getPlaceNameClient :: Maybe Text -> Text -> Maybe LatLong -> Maybe Text -> Maybe Language -> EulerClient GoogleMaps.GetPlaceNameResp
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
autoCompleteClient :<|> getPlaceDetailsClient :<|> getPlaceNameClient :<|> distanceMatrixClient :<|> directionsClient = client (Proxy :: Proxy GoogleMapsAPI)

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
  Language ->
  m GoogleMaps.AutoCompleteResp
autoComplete url apiKey input sessiontoken location radius components lang = do
  callAPI url (autoCompleteClient sessiontoken apiKey input location radius components lang) "autoComplete"
    >>= checkGoogleMapsError url

getPlaceDetails ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Maybe Text ->
  Text ->
  Text ->
  m GoogleMaps.GetPlaceDetailsResp
getPlaceDetails url apiKey sessiontoken placeId fields = do
  callAPI url (getPlaceDetailsClient sessiontoken apiKey placeId fields) "getPlaceDetails"
    >>= checkGoogleMapsError url

getPlaceName ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe LatLong ->
  Maybe Language ->
  m GoogleMaps.GetPlaceNameResp
getPlaceName url apiKey sessiontoken mbByPlaceId mbByLatLong language = do
  res <- callAPI url (getPlaceNameClient sessiontoken apiKey mbByLatLong mbByPlaceId language) "getPlaceName"
  whenLeft res $
    \(FailureResponse _ (Response code _ _ resBody)) ->
      case statusCode code of
        400 -> case J.eitherDecode resBody of
          Right (apiEr :: GoogleMaps.ResBody) ->
            throwError $ GoogleMapsInvalidRequest $ apiEr.error_message
          Left _ ->
            throwError $ GoogleMapsInvalidRequest $ Just $ show (statusMessage code)
        500 ->
          throwError $ InternalError $ show $ statusMessage code
        _ ->
          throwError $ GoogleMapsCallError $ show $ statusMessage code
  checkGoogleMapsError url res

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
    "INVALID_REQUEST" -> throwError $ GoogleMapsInvalidRequest Nothing
    _ -> throwError $ GoogleMapsCallError response.status
