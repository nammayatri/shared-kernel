{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.FleetEngine.Types where

import qualified Data.Aeson as A
import Kernel.Prelude

-- | Lifecycle of a Fleet Engine trip. JSON values must match the Fleet Engine
-- REST enum spelling exactly (the constructor names are used verbatim).
data TripStatus
  = UNKNOWN_TRIP_STATUS
  | NEW
  | ENROUTE_TO_PICKUP
  | ARRIVED_AT_PICKUP
  | ENROUTE_TO_INTERMEDIATE_DESTINATION
  | ARRIVED_AT_INTERMEDIATE_DESTINATION
  | ENROUTE_TO_DROPOFF
  | COMPLETE
  | CANCELED
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TripType
  = UNKNOWN_TRIP_TYPE
  | SHARED
  | EXCLUSIVE
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data LatLng = LatLng
  { latitude :: Double,
    longitude :: Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A Fleet Engine TerminalLocation (only the @point@ is required for our use).
newtype TerminalLocation = TerminalLocation
  { point :: LatLng
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | The subset of the Fleet Engine @Trip@ resource we read/write. All fields are
-- optional so the same record serves as both the CreateTrip body and the
-- (masked) UpdateTrip body. @Nothing@ fields are omitted from the JSON so they
-- never clobber server state on a PATCH.
data Trip = Trip
  { tripType :: Maybe TripType,
    tripStatus :: Maybe TripStatus,
    vehicleId :: Maybe Text,
    numberOfPassengers :: Maybe Int,
    pickupPoint :: Maybe TerminalLocation,
    dropoffPoint :: Maybe TerminalLocation
  }
  deriving (Show, Eq, Generic)

tripJSONOptions :: A.Options
tripJSONOptions = A.defaultOptions {A.omitNothingFields = True}

instance ToJSON Trip where
  toJSON = A.genericToJSON tripJSONOptions

instance FromJSON Trip where
  parseJSON = A.genericParseJSON tripJSONOptions

emptyTrip :: Trip
emptyTrip =
  Trip
    { tripType = Nothing,
      tripStatus = Nothing,
      vehicleId = Nothing,
      numberOfPassengers = Nothing,
      pickupPoint = Nothing,
      dropoffPoint = Nothing
    }

-- | Build the CreateTrip body. Fleet Engine requires @tripType@; pickup/dropoff
-- are optional but improve ETA quality.
mkCreateTripBody :: TripType -> Maybe LatLng -> Maybe LatLng -> Maybe Int -> Trip
mkCreateTripBody tType mbPickup mbDropoff mbPassengers =
  emptyTrip
    { tripType = Just tType,
      tripStatus = Just NEW,
      pickupPoint = TerminalLocation <$> mbPickup,
      dropoffPoint = TerminalLocation <$> mbDropoff,
      numberOfPassengers = mbPassengers
    }
