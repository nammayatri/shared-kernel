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
    dropoffPoint :: Maybe TerminalLocation,
    intermediateDestinations :: Maybe [TerminalLocation],
    -- Opaque RFC3339 timestamp; must be echoed back on every
    -- intermediateDestinations mutation (server-side optimistic lock).
    intermediateDestinationsVersion :: Maybe Text,
    intermediateDestinationIndex :: Maybe Int
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
      dropoffPoint = Nothing,
      intermediateDestinations = Nothing,
      intermediateDestinationsVersion = Nothing,
      intermediateDestinationIndex = Nothing
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

data VehicleState
  = UNKNOWN_VEHICLE_STATE
  | OFFLINE
  | ONLINE
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Google spells the zero value as bare @UNKNOWN@; we namespace the constructor
-- and remap via 'vehicleCategoryOptions' to avoid clashes.
data VehicleCategory
  = UNKNOWN_VEHICLE_CATEGORY
  | AUTO
  | TAXI
  | TRUCK
  | TWO_WHEELER
  | BICYCLE
  | PEDESTRIAN
  deriving (Show, Eq, Generic)

vehicleCategoryOptions :: A.Options
vehicleCategoryOptions = A.defaultOptions {A.constructorTagModifier = tagFn}
  where
    tagFn "UNKNOWN_VEHICLE_CATEGORY" = "UNKNOWN"
    tagFn other = other

instance ToJSON VehicleCategory where
  toJSON = A.genericToJSON vehicleCategoryOptions

instance FromJSON VehicleCategory where
  parseJSON = A.genericParseJSON vehicleCategoryOptions

newtype VehicleType = VehicleType
  { category :: VehicleCategory
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Vehicle = Vehicle
  { vehicleState :: VehicleState,
    supportedTripTypes :: [TripType],
    maximumCapacity :: Int,
    vehicleType :: VehicleType
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Custom vehicle attribute (server-side filterable in SearchVehicles). Keys are
-- unique per vehicle; Fleet Engine caps at 100 attributes per vehicle.
data VehicleAttribute = VehicleAttribute
  { key :: Text,
    value :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Attribute group used by 'requiredOneOfAttributes' and 'requiredOneOfAttributeSets'
-- in SearchVehicles for OR/AND combinations across attribute lists.
newtype VehicleAttributeList = VehicleAttributeList
  { attributes :: [VehicleAttribute]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data VehicleMatchOrder
  = UNKNOWN_VEHICLE_MATCH_ORDER
  | PICKUP_POINT_ETA
  | PICKUP_POINT_DISTANCE
  | DROPOFF_POINT_ETA
  | PICKUP_POINT_STRAIGHT_DISTANCE
  | COST
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Google spells one value 'EXCLUSIVE' which clashes with TripType.EXCLUSIVE;
-- 'EXCLUSIVE_MATCH' is remapped to the wire name via 'vehicleMatchTypeOptions'.
data VehicleMatchType
  = UNKNOWN_VEHICLE_MATCH_TYPE
  | EXCLUSIVE_MATCH
  | BACK_TO_BACK
  | CARPOOL
  | CARPOOL_BACK_TO_BACK
  deriving (Show, Eq, Generic)

vehicleMatchTypeOptions :: A.Options
vehicleMatchTypeOptions = A.defaultOptions {A.constructorTagModifier = tagFn}
  where
    tagFn "UNKNOWN_VEHICLE_MATCH_TYPE" = "UNKNOWN"
    tagFn "EXCLUSIVE_MATCH" = "EXCLUSIVE"
    tagFn other = other

instance ToJSON VehicleMatchType where
  toJSON = A.genericToJSON vehicleMatchTypeOptions

instance FromJSON VehicleMatchType where
  parseJSON = A.genericParseJSON vehicleMatchTypeOptions

-- SHARED-only knob; mutually exclusive with 'includeBackToBack' on the request.
data CurrentTripsPresent
  = CURRENT_TRIPS_PRESENT_UNSPECIFIED
  | CURRENT_TRIPS_PRESENT_NONE
  | CURRENT_TRIPS_PRESENT_ANY
  deriving (Show, Eq, Generic)

currentTripsPresentOptions :: A.Options
currentTripsPresentOptions = A.defaultOptions {A.constructorTagModifier = tagFn}
  where
    tagFn "CURRENT_TRIPS_PRESENT_NONE" = "NONE"
    tagFn "CURRENT_TRIPS_PRESENT_ANY" = "ANY"
    tagFn other = other

instance ToJSON CurrentTripsPresent where
  toJSON = A.genericToJSON currentTripsPresentOptions

instance FromJSON CurrentTripsPresent where
  parseJSON = A.genericParseJSON currentTripsPresentOptions

-- Waypoint in a matched vehicle's remaining route (VehicleMatch.vehicleTripsWaypoints).
data Waypoint = Waypoint
  { latLng :: LatLng,
    eta :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Waypoint where
  toJSON = A.genericToJSON tripJSONOptions

instance FromJSON Waypoint where
  parseJSON = A.genericParseJSON tripJSONOptions

-- | SearchVehicles request body. Required by Fleet Engine: pickupPoint,
-- pickupRadiusMeters (400–10,000), count (1–50), minimumCapacity (>=1),
-- tripTypes, vehicleTypes, orderBy. Optional fields are omitted from JSON
-- when 'Nothing'.
data SearchVehiclesReq = SearchVehiclesReq
  { pickupPoint :: TerminalLocation,
    dropoffPoint :: Maybe TerminalLocation,
    pickupRadiusMeters :: Int,
    count :: Int,
    minimumCapacity :: Int,
    tripTypes :: [TripType],
    maximumStaleness :: Maybe Text,
    vehicleTypes :: [VehicleType],
    requiredAttributes :: Maybe [VehicleAttribute],
    requiredOneOfAttributes :: Maybe [VehicleAttributeList],
    requiredOneOfAttributeSets :: Maybe [VehicleAttributeList],
    orderBy :: VehicleMatchOrder,
    includeBackToBack :: Maybe Bool,
    tripId :: Maybe Text,
    currentTripsPresent :: Maybe CurrentTripsPresent,
    -- Field name shadows Prelude.filter; JSON key stays "filter" via
    -- 'searchVehiclesReqJSONOptions'.
    filterExpr :: Maybe Text
  }
  deriving (Show, Eq, Generic)

searchVehiclesReqJSONOptions :: A.Options
searchVehiclesReqJSONOptions =
  A.defaultOptions
    { A.omitNothingFields = True,
      A.fieldLabelModifier = \case
        "filterExpr" -> "filter"
        other -> other
    }

instance ToJSON SearchVehiclesReq where
  toJSON = A.genericToJSON searchVehiclesReqJSONOptions

instance FromJSON SearchVehiclesReq where
  parseJSON = A.genericParseJSON searchVehiclesReqJSONOptions

-- Extra fields on the wire (name, lastLocation, etc.) are ignored by the
-- generic parser; missing required fields on 'vehicle' will fail parsing —
-- Fleet Engine returns a full Vehicle for every match, so this is expected.
data VehicleMatch = VehicleMatch
  { vehicle :: Vehicle,
    vehiclePickupEta :: Maybe Text,
    vehiclePickupDistanceMeters :: Maybe Int,
    vehiclePickupStraightLineDistanceMeters :: Maybe Int,
    vehicleDropoffEta :: Maybe Text,
    vehiclePickupToDropoffDistanceMeters :: Maybe Int,
    tripType :: Maybe TripType,
    vehicleTripsWaypoints :: Maybe [Waypoint],
    vehicleMatchType :: Maybe VehicleMatchType,
    requestedOrderedBy :: Maybe VehicleMatchOrder,
    orderedBy :: Maybe VehicleMatchOrder
  }
  deriving (Show, Eq, Generic)

instance ToJSON VehicleMatch where
  toJSON = A.genericToJSON tripJSONOptions

instance FromJSON VehicleMatch where
  parseJSON = A.genericParseJSON tripJSONOptions

newtype SearchVehiclesResp = SearchVehiclesResp
  { matches :: Maybe [VehicleMatch]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SearchVehiclesResp where
  toJSON = A.genericToJSON tripJSONOptions

instance FromJSON SearchVehiclesResp where
  parseJSON = A.genericParseJSON tripJSONOptions
