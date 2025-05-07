{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Maps.Google.RoadsClient where

import qualified Data.Text as T
import EulerHS.Types as Euler
import qualified Kernel.External.Maps.Interface.Types as MapsInterfaceTypes
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.App (MandatoryQueryParam, MonadFlow)
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (callAPI, fork, fromEitherM, throwError)
import qualified Kernel.Utils.ExternalAPICallLogging as ApiCallLogger
import Servant hiding (throwError)

type SnapToRoadResponse = SnapToRoadResponse' LatLong

data SnapToRoadResponse' a = SnapToRoadResponse
  { snappedPoints :: [SnappedPoint' a],
    warningMessage :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

type SnappedPoint = SnappedPoint' LatLong

data SnappedPoint' a = SnappedPoint
  { location :: a,
    originalIndex :: Maybe Int,
    placeId :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

data SPLocation = SPLocation
  { latitude :: Double,
    longitude :: Double
  }
  deriving (Generic, FromJSON, ToJSON)

type SnapToRoadAPI =
  "v1" :> "snapToRoads"
    :> MandatoryQueryParam "key" Text
    :> MandatoryQueryParam "interpolate" Bool
    :> MandatoryQueryParam "path" Text
    :> Get '[JSON] (SnapToRoadResponse' SPLocation)

snapToRoad ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MapsInterfaceTypes.SnapToRoadReq ->
  BaseUrl ->
  Text ->
  [LatLong] ->
  m SnapToRoadResponse
snapToRoad entityId req roadsUrl apiKey pointsList = do
  let eulerClient = Euler.client (Proxy @SnapToRoadAPI)
      interpolate = True
  eitherRes <- callAPI roadsUrl (eulerClient apiKey interpolate $ convertPointsList pointsList) "snap-to-road" (Proxy @SnapToRoadAPI)
  fork ("Logging external API Call of snapToRoad Google ") $
    ApiCallLogger.pushExternalApiCallDataToKafka "snapToRoad" "Google" entityId (Just req) eitherRes
  res <- fromEitherM (\err -> InternalError $ "Failed to call snap-to-road API: " <> show err) eitherRes
  maybe
    (pure ())
    (\warning -> throwError $ InternalError ("Snap-to-road API throwing warning" <> warning))
    res.warningMessage
  return $ SnapToRoadResponse (map (\sp -> sp {location = spLocToLatLong sp.location}) res.snappedPoints) res.warningMessage
  where
    convertPoint :: LatLong -> Text
    convertPoint pt = mconcat [show pt.lat, ",", show pt.lon]

    convertPointsList :: [LatLong] -> Text
    convertPointsList = T.intercalate "|" . map convertPoint

    spLocToLatLong :: SPLocation -> LatLong
    spLocToLatLong s =
      LatLong
        { lat = s.latitude,
          lon = s.longitude
        }
