{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Kernel.External.MultiModal.OpenTripPlanner.Types where

import Data.Morpheus.Client.CodeGen.Internal
import qualified Data.Text as T
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id, product)
import Kernel.Prelude (ToSchema)
import Prelude (Show (..))

data AbsoluteDirection
  = AbsoluteDirectionNORTH
  | AbsoluteDirectionNORTHEAST
  | AbsoluteDirectionEAST
  | AbsoluteDirectionSOUTHEAST
  | AbsoluteDirectionSOUTH
  | AbsoluteDirectionSOUTHWEST
  | AbsoluteDirectionWEST
  | AbsoluteDirectionNORTHWEST
  deriving (Generic, Show, Eq, ToSchema, Ord)

instance FromJSON AbsoluteDirection where
  parseJSON = \case
    "NORTH" -> pure AbsoluteDirectionNORTH
    "NORTHEAST" -> pure AbsoluteDirectionNORTHEAST
    "EAST" -> pure AbsoluteDirectionEAST
    "SOUTHEAST" -> pure AbsoluteDirectionSOUTHEAST
    "SOUTH" -> pure AbsoluteDirectionSOUTH
    "SOUTHWEST" -> pure AbsoluteDirectionSOUTHWEST
    "WEST" -> pure AbsoluteDirectionWEST
    "NORTHWEST" -> pure AbsoluteDirectionNORTHWEST
    v -> invalidConstructorError v

instance ToJSON AbsoluteDirection where
  toJSON = \case
    AbsoluteDirectionNORTH -> "NORTH"
    AbsoluteDirectionNORTHEAST -> "NORTHEAST"
    AbsoluteDirectionEAST -> "EAST"
    AbsoluteDirectionSOUTHEAST -> "SOUTHEAST"
    AbsoluteDirectionSOUTH -> "SOUTH"
    AbsoluteDirectionSOUTHWEST -> "SOUTHWEST"
    AbsoluteDirectionWEST -> "WEST"
    AbsoluteDirectionNORTHWEST -> "NORTHWEST"

data InputCoordinates = InputCoordinates
  { lat :: Double,
    lon :: Double
  }
  deriving (Generic, Show, Eq)

instance ToJSON InputCoordinates where
  toJSON (InputCoordinates inputCoordinatesLat inputCoordinatesLon) =
    omitNulls
      [ "lat" .= inputCoordinatesLat,
        "lon" .= inputCoordinatesLon
      ]

data Mode
  = ModeAIRPLANE
  | ModeBICYCLE
  | ModeBUS
  | ModeCABLE_CAR
  | ModeCAR
  | ModeCOACH
  | ModeFERRY
  | ModeFLEX
  | ModeFUNICULAR
  | ModeGONDOLA
  | ModeRAIL
  | ModeSCOOTER
  | ModeSUBWAY
  | ModeTRAM
  | ModeCARPOOL
  | ModeTAXI
  | ModeTRANSIT
  | ModeWALK
  | ModeTROLLEYBUS
  | ModeMONORAIL
  deriving (Generic, Eq)

instance Show Mode where
  show = \case
    ModeAIRPLANE -> "AIRPLANE"
    ModeBICYCLE -> "BICYCLE"
    ModeBUS -> "BUS"
    ModeCABLE_CAR -> "CABLE_CAR"
    ModeCAR -> "CAR"
    ModeCOACH -> "COACH"
    ModeFERRY -> "FERRY"
    ModeFLEX -> "FLEX"
    ModeFUNICULAR -> "FUNICULAR"
    ModeGONDOLA -> "GONDOLA"
    ModeRAIL -> "RAIL"
    ModeSCOOTER -> "SCOOTER"
    ModeSUBWAY -> "SUBWAY"
    ModeTRAM -> "TRAM"
    ModeCARPOOL -> "CARPOOL"
    ModeTAXI -> "TAXI"
    ModeTRANSIT -> "TRANSIT"
    ModeWALK -> "WALK"
    ModeTROLLEYBUS -> "TROLLEYBUS"
    ModeMONORAIL -> "MONORAIL"

instance FromJSON Mode where
  parseJSON = \case
    "AIRPLANE" -> pure ModeAIRPLANE
    "BICYCLE" -> pure ModeBICYCLE
    "BUS" -> pure ModeBUS
    "CABLE_CAR" -> pure ModeCABLE_CAR
    "CAR" -> pure ModeCAR
    "COACH" -> pure ModeCOACH
    "FERRY" -> pure ModeFERRY
    "FLEX" -> pure ModeFLEX
    "FUNICULAR" -> pure ModeFUNICULAR
    "GONDOLA" -> pure ModeGONDOLA
    "RAIL" -> pure ModeRAIL
    "SCOOTER" -> pure ModeSCOOTER
    "SUBWAY" -> pure ModeSUBWAY
    "TRAM" -> pure ModeTRAM
    "CARPOOL" -> pure ModeCARPOOL
    "TAXI" -> pure ModeTAXI
    "TRANSIT" -> pure ModeTRANSIT
    "WALK" -> pure ModeWALK
    "TROLLEYBUS" -> pure ModeTROLLEYBUS
    "MONORAIL" -> pure ModeMONORAIL
    v -> invalidConstructorError v

instance ToJSON Mode where
  toJSON = \case
    ModeAIRPLANE -> "AIRPLANE"
    ModeBICYCLE -> "BICYCLE"
    ModeBUS -> "BUS"
    ModeCABLE_CAR -> "CABLE_CAR"
    ModeCAR -> "CAR"
    ModeCOACH -> "COACH"
    ModeFERRY -> "FERRY"
    ModeFLEX -> "FLEX"
    ModeFUNICULAR -> "FUNICULAR"
    ModeGONDOLA -> "GONDOLA"
    ModeRAIL -> "RAIL"
    ModeSCOOTER -> "SCOOTER"
    ModeSUBWAY -> "SUBWAY"
    ModeTRAM -> "TRAM"
    ModeCARPOOL -> "CARPOOL"
    ModeTAXI -> "TAXI"
    ModeTRANSIT -> "TRANSIT"
    ModeWALK -> "WALK"
    ModeTROLLEYBUS -> "TROLLEYBUS"
    ModeMONORAIL -> "MONORAIL"

data RelativeDirection
  = RelativeDirectionDEPART
  | RelativeDirectionHARD_LEFT
  | RelativeDirectionLEFT
  | RelativeDirectionSLIGHTLY_LEFT
  | RelativeDirectionCONTINUE
  | RelativeDirectionSLIGHTLY_RIGHT
  | RelativeDirectionRIGHT
  | RelativeDirectionHARD_RIGHT
  | RelativeDirectionCIRCLE_CLOCKWISE
  | RelativeDirectionCIRCLE_COUNTERCLOCKWISE
  | RelativeDirectionELEVATOR
  | RelativeDirectionUTURN_LEFT
  | RelativeDirectionUTURN_RIGHT
  | RelativeDirectionENTER_STATION
  | RelativeDirectionEXIT_STATION
  | RelativeDirectionFOLLOW_SIGNS
  deriving (Generic, Show, Eq)

instance FromJSON RelativeDirection where
  parseJSON = \case
    "DEPART" -> pure RelativeDirectionDEPART
    "HARD_LEFT" -> pure RelativeDirectionHARD_LEFT
    "LEFT" -> pure RelativeDirectionLEFT
    "SLIGHTLY_LEFT" -> pure RelativeDirectionSLIGHTLY_LEFT
    "CONTINUE" -> pure RelativeDirectionCONTINUE
    "SLIGHTLY_RIGHT" -> pure RelativeDirectionSLIGHTLY_RIGHT
    "RIGHT" -> pure RelativeDirectionRIGHT
    "HARD_RIGHT" -> pure RelativeDirectionHARD_RIGHT
    "CIRCLE_CLOCKWISE" -> pure RelativeDirectionCIRCLE_CLOCKWISE
    "CIRCLE_COUNTERCLOCKWISE" -> pure RelativeDirectionCIRCLE_COUNTERCLOCKWISE
    "ELEVATOR" -> pure RelativeDirectionELEVATOR
    "UTURN_LEFT" -> pure RelativeDirectionUTURN_LEFT
    "UTURN_RIGHT" -> pure RelativeDirectionUTURN_RIGHT
    "ENTER_STATION" -> pure RelativeDirectionENTER_STATION
    "EXIT_STATION" -> pure RelativeDirectionEXIT_STATION
    "FOLLOW_SIGNS" -> pure RelativeDirectionFOLLOW_SIGNS
    v -> invalidConstructorError v

instance ToJSON RelativeDirection where
  toJSON = \case
    RelativeDirectionDEPART -> "DEPART"
    RelativeDirectionHARD_LEFT -> "HARD_LEFT"
    RelativeDirectionLEFT -> "LEFT"
    RelativeDirectionSLIGHTLY_LEFT -> "SLIGHTLY_LEFT"
    RelativeDirectionCONTINUE -> "CONTINUE"
    RelativeDirectionSLIGHTLY_RIGHT -> "SLIGHTLY_RIGHT"
    RelativeDirectionRIGHT -> "RIGHT"
    RelativeDirectionHARD_RIGHT -> "HARD_RIGHT"
    RelativeDirectionCIRCLE_CLOCKWISE -> "CIRCLE_CLOCKWISE"
    RelativeDirectionCIRCLE_COUNTERCLOCKWISE -> "CIRCLE_COUNTERCLOCKWISE"
    RelativeDirectionELEVATOR -> "ELEVATOR"
    RelativeDirectionUTURN_LEFT -> "UTURN_LEFT"
    RelativeDirectionUTURN_RIGHT -> "UTURN_RIGHT"
    RelativeDirectionENTER_STATION -> "ENTER_STATION"
    RelativeDirectionEXIT_STATION -> "EXIT_STATION"
    RelativeDirectionFOLLOW_SIGNS -> "FOLLOW_SIGNS"

newtype TransportMode = TransportMode
  { mode :: String
  }
  deriving (Generic, Show, Eq, ToSchema, FromJSON)

instance ToJSON TransportMode where
  toJSON (TransportMode transportModeMode) =
    omitNulls
      [ "mode" .= transportModeMode
      ]

instance RequestType OTPPlan where
  type RequestArgs OTPPlan = OTPPlanArgs
  __name _ = "OTPPlan"
  __query _ = "query OTPPlan (\n    $from: InputCoordinates!,\n    $to: InputCoordinates!,\n    $date: String,\n    $time:String,\n    $transportModes: [TransportMode],\n    $numItineraries: Int\n){\n  plan(\n    from: $from,\n    to: $to,\n    date: $date,\n    time: $time,\n    transportModes: $transportModes,\n    numItineraries : $numItineraries\n  ) {\n    itineraries {\n      duration\n      startTime\n      endTime\n      legs {\n        pickupType\n        distance\n        mode\n        duration\n        startTime\n        endTime\n        from {\n          name\n          lat\n          lon\n          departureTime\n          arrivalTime\n          stop {\n            code\n            gtfsId\n            platformCode\n          }\n        }\n        to {\n          name\n          lat\n          lon\n          departureTime\n          arrivalTime\n          stop {\n            code\n            gtfsId\n            platformCode\n          }\n        }\n        route {\n          gtfsId\n          longName\n          trips {\n            gtfsId\n          }\n          shortName\n          color\n          agency {\n            gtfsId\n            name\n          }\n        }\n        legGeometry {\n          points\n        }\n        fareProducts {\n          id\n        }\n      }\n    }\n  }\n}\n"
  __type _ = OPERATION_QUERY

newtype OTPPlan = OTPPlan
  { plan :: OTPPlanPlan
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlan where
  parseJSON =
    withObject "OTPPlan" (\v -> OTPPlan <$> v .: "plan")

newtype OTPPlanPlan = OTPPlanPlan
  { itineraries :: [Maybe OTPPlanPlanItineraries]
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlan where
  parseJSON =
    withObject "OTPPlanPlan" (\v -> OTPPlanPlan <$> v .: "itineraries")

data OTPPlanPlanItineraries = OTPPlanPlanItineraries
  { duration :: Maybe Double,
    startTime :: Maybe Double,
    endTime :: Maybe Double,
    legs :: [Maybe OTPPlanPlanItinerariesLegs]
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItineraries where
  parseJSON =
    withObject "OTPPlanPlanItineraries" (\v -> OTPPlanPlanItineraries <$> v .:? "duration" <*> v .:? "startTime" <*> v .:? "endTime" <*> v .: "legs")

data OTPPlanPlanItinerariesLegs = OTPPlanPlanItinerariesLegs
  { pickupType :: Maybe String,
    distance :: Maybe Double,
    mode :: Maybe Mode,
    entrance :: Maybe OTPPlanPlanItinerariesLegsEntrance,
    exit :: Maybe OTPPlanPlanItinerariesLegsExit,
    duration :: Maybe Double,
    startTime :: Maybe Double,
    endTime :: Maybe Double,
    from :: OTPPlanPlanItinerariesLegsFrom,
    to :: OTPPlanPlanItinerariesLegsTo,
    route :: Maybe OTPPlanPlanItinerariesLegsRoute,
    legGeometry :: Maybe OTPPlanPlanItinerariesLegsLegGeometry,
    fareProducts :: Maybe [Maybe OTPPlanPlanItinerariesLegsFareProducts]
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItinerariesLegs where
  parseJSON =
    withObject "OTPPlanPlanItinerariesLegs" (\v -> OTPPlanPlanItinerariesLegs <$> v .:? "pickupType" <*> v .:? "distance" <*> v .:? "mode" <*> v .:? "entrance" <*> v .:? "exit" <*> v .:? "duration" <*> v .:? "startTime" <*> v .:? "endTime" <*> v .: "from" <*> v .: "to" <*> v .:? "route" <*> v .:? "legGeometry" <*> v .:? "fareProducts")

data OTPPlanPlanItinerariesLegsEntrance = OTPPlanPlanItinerariesLegsEntrance
  { distance :: Maybe Double,
    lon :: Maybe Double,
    lat :: Maybe Double,
    relativeDirection :: Maybe RelativeDirection,
    absoluteDirection :: Maybe AbsoluteDirection,
    streetName :: Maybe String,
    exit :: Maybe String,
    stayOn :: Maybe Bool,
    area :: Maybe Bool,
    bogusName :: Maybe Bool,
    walkingBike :: Maybe Bool
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItinerariesLegsEntrance where
  parseJSON =
    withObject "OTPPlanPlanItinerariesLegsEntrance" (\v -> OTPPlanPlanItinerariesLegsEntrance <$> v .:? "distance" <*> v .:? "lon" <*> v .:? "lat" <*> v .:? "relativeDirection" <*> v .:? "absoluteDirection" <*> v .:? "streetName" <*> v .:? "exit" <*> v .:? "stayOn" <*> v .:? "area" <*> v .:? "bogusName" <*> v .:? "walkingBike")

data OTPPlanPlanItinerariesLegsExit = OTPPlanPlanItinerariesLegsExit
  { distance :: Maybe Double,
    lon :: Maybe Double,
    lat :: Maybe Double,
    relativeDirection :: Maybe RelativeDirection,
    absoluteDirection :: Maybe AbsoluteDirection,
    streetName :: Maybe String,
    exit :: Maybe String,
    stayOn :: Maybe Bool,
    area :: Maybe Bool,
    bogusName :: Maybe Bool,
    walkingBike :: Maybe Bool
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItinerariesLegsExit where
  parseJSON =
    withObject "OTPPlanPlanItinerariesLegsExit" (\v -> OTPPlanPlanItinerariesLegsExit <$> v .:? "distance" <*> v .:? "lon" <*> v .:? "lat" <*> v .:? "relativeDirection" <*> v .:? "absoluteDirection" <*> v .:? "streetName" <*> v .:? "exit" <*> v .:? "stayOn" <*> v .:? "area" <*> v .:? "bogusName" <*> v .:? "walkingBike")

data OTPPlanPlanItinerariesLegsFrom = OTPPlanPlanItinerariesLegsFrom
  { name :: Maybe String,
    lat :: Double,
    lon :: Double,
    departureTime :: Double,
    arrivalTime :: Double,
    stop :: Maybe OTPPlanPlanItinerariesLegsFromStop
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItinerariesLegsFrom where
  parseJSON =
    withObject "OTPPlanPlanItinerariesLegsFrom" (\v -> OTPPlanPlanItinerariesLegsFrom <$> v .:? "name" <*> v .: "lat" <*> v .: "lon" <*> v .: "departureTime" <*> v .: "arrivalTime" <*> v .:? "stop")

data OTPPlanPlanItinerariesLegsFromStop = OTPPlanPlanItinerariesLegsFromStop
  { code :: Maybe String,
    gtfsId :: String,
    platformCode :: Maybe String
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItinerariesLegsFromStop where
  parseJSON =
    withObject "OTPPlanPlanItinerariesLegsFromStop" (\v -> OTPPlanPlanItinerariesLegsFromStop <$> v .:? "code" <*> v .: "gtfsId" <*> v .:? "platformCode")

data OTPPlanPlanItinerariesLegsTo = OTPPlanPlanItinerariesLegsTo
  { name :: Maybe String,
    lat :: Double,
    lon :: Double,
    departureTime :: Double,
    arrivalTime :: Double,
    stop :: Maybe OTPPlanPlanItinerariesLegsToStop
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItinerariesLegsTo where
  parseJSON =
    withObject "OTPPlanPlanItinerariesLegsTo" (\v -> OTPPlanPlanItinerariesLegsTo <$> v .:? "name" <*> v .: "lat" <*> v .: "lon" <*> v .: "departureTime" <*> v .: "arrivalTime" <*> v .:? "stop")

data OTPPlanPlanItinerariesLegsToStop = OTPPlanPlanItinerariesLegsToStop
  { code :: Maybe String,
    gtfsId :: String,
    platformCode :: Maybe String
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItinerariesLegsToStop where
  parseJSON =
    withObject "OTPPlanPlanItinerariesLegsToStop" (\v -> OTPPlanPlanItinerariesLegsToStop <$> v .:? "code" <*> v .: "gtfsId" <*> v .:? "platformCode")

data OTPPlanPlanItinerariesLegsRoute = OTPPlanPlanItinerariesLegsRoute
  { gtfsId :: String,
    longName :: Maybe String,
    trips :: Maybe [Maybe OTPPlanPlanItinerariesLegsRouteTrips],
    shortName :: Maybe String,
    color :: Maybe String,
    agency :: Maybe OTPPlanPlanItinerariesLegsRouteAgency
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItinerariesLegsRoute where
  parseJSON =
    withObject "OTPPlanPlanItinerariesLegsRoute" (\v -> OTPPlanPlanItinerariesLegsRoute <$> v .: "gtfsId" <*> v .:? "longName" <*> v .:? "trips" <*> v .:? "shortName" <*> v .:? "color" <*> v .:? "agency")

newtype OTPPlanPlanItinerariesLegsRouteTrips = OTPPlanPlanItinerariesLegsRouteTrips
  { gtfsId :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItinerariesLegsRouteTrips where
  parseJSON =
    withObject "OTPPlanPlanItinerariesLegsRouteTrips" (\v -> OTPPlanPlanItinerariesLegsRouteTrips <$> v .: "gtfsId")

data OTPPlanPlanItinerariesLegsRouteAgency = OTPPlanPlanItinerariesLegsRouteAgency
  { gtfsId :: String,
    name :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItinerariesLegsRouteAgency where
  parseJSON =
    withObject "OTPPlanPlanItinerariesLegsRouteAgency" (\v -> OTPPlanPlanItinerariesLegsRouteAgency <$> v .: "gtfsId" <*> v .: "name")

newtype OTPPlanPlanItinerariesLegsLegGeometry = OTPPlanPlanItinerariesLegsLegGeometry
  { points :: Maybe String
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItinerariesLegsLegGeometry where
  parseJSON =
    withObject "OTPPlanPlanItinerariesLegsLegGeometry" (\v -> OTPPlanPlanItinerariesLegsLegGeometry <$> v .:? "points")

newtype OTPPlanPlanItinerariesLegsFareProducts = OTPPlanPlanItinerariesLegsFareProducts
  { id :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItinerariesLegsFareProducts where
  parseJSON =
    withObject "OTPPlanPlanItinerariesLegsFareProducts" (\v -> OTPPlanPlanItinerariesLegsFareProducts <$> v .: "id")

data OTPPlanArgs = OTPPlanArgs
  { from :: InputCoordinates,
    to :: InputCoordinates,
    date :: Maybe String,
    time :: Maybe String,
    transportModes :: Maybe [Maybe TransportMode],
    numItineraries :: Maybe Int
  }
  deriving (Generic, Show, Eq)

instance ToJSON OTPPlanArgs where
  toJSON (OTPPlanArgs oTPPlanArgsFrom oTPPlanArgsTo oTPPlanArgsDate oTPPlanArgsTime oTPPlanArgsTransportModes oTPPlanArgsNumItineraries) =
    omitNulls
      [ "from" .= oTPPlanArgsFrom,
        "to" .= oTPPlanArgsTo,
        "date" .= oTPPlanArgsDate,
        "time" .= oTPPlanArgsTime,
        "transportModes" .= oTPPlanArgsTransportModes,
        "numItineraries" .= oTPPlanArgsNumItineraries
      ]

instance RequestType MultiModePlan where
  type RequestArgs MultiModePlan = MultiModePlanArgs
  __name _ = "MultiModePlan"
  __query _ = "query MultiModePlan(\n  $from: InputCoordinates!,\n  $to: InputCoordinates!,\n  $date: String,\n  $time: String,\n  $metroTransportModes: [TransportMode]!,\n  $metroItineraries: Int!\n  $subwayTransportModes: [TransportMode]!,\n  $subwayItineraries: Int!\n  $busTransportModes: [TransportMode]!,\n  $busItineraries: Int!\n  $bestTransportModes: [TransportMode]!,\n  $bestItineraries: Int!\n) {\n  metro: plan(\n    from: $from,\n    to:   $to,\n    date: $date,\n    time: $time,\n    transportModes: $metroTransportModes,\n    numItineraries: $metroItineraries\n  ) {\n    itineraries { ...ItineraryFields }\n  }\n  subway: plan(\n    from: $from,\n    to:   $to,\n    date: $date,\n    time: $time,\n    transportModes: $subwayTransportModes,\n    numItineraries: $subwayItineraries\n  ) {\n    itineraries { ...ItineraryFields }\n  }\n  bus: plan(\n    from: $from,\n    to:   $to,\n    date: $date,\n    time: $time,\n    transportModes: $busTransportModes,\n    numItineraries: $busItineraries\n  ) {\n    itineraries { ...ItineraryFields }\n  }\n  best: plan(\n    from: $from,\n    to:   $to,\n    date: $date,\n    time: $time,\n    transportModes: $bestTransportModes,\n    numItineraries: $bestItineraries\n  ) {\n    itineraries { ...ItineraryFields }\n  }\n}\n\nfragment ItineraryFields on Itinerary {\n  duration\n  startTime\n  endTime\n  legs {\n    pickupType\n    distance\n    mode\n    entrance {\n      distance\n      lon\n      lat\n      relativeDirection\n      absoluteDirection\n      streetName\n      exit\n      stayOn\n      area\n      bogusName\n      walkingBike\n    }\n    exit {\n      distance\n      lon\n      lat\n      relativeDirection\n      absoluteDirection\n      streetName\n      exit\n      stayOn\n      area\n      bogusName\n      walkingBike\n    }\n    duration\n    startTime\n    endTime\n    from {\n      name\n      lat\n      lon\n      departureTime\n      arrivalTime\n      stop {\n        code\n        gtfsId\n        platformCode\n      }\n    }\n    to {\n      name\n      lat\n      lon\n      departureTime\n      arrivalTime\n      stop {\n        code\n        gtfsId\n        platformCode\n      }\n    }\n    route {\n      gtfsId\n      longName\n      trips {\n        gtfsId\n      }\n      shortName\n      color\n      agency {\n        gtfsId\n        name\n      }\n    }\n    legGeometry {\n      points\n    }\n    fareProducts {\n      id\n    }\n  }\n}\n"
  __type _ = OPERATION_QUERY

data MultiModePlanArgs = MultiModePlanArgs
  { from :: InputCoordinates,
    to :: InputCoordinates,
    date :: Maybe String,
    time :: Maybe String,
    metroTransportModes :: [Maybe TransportMode],
    metroItineraries :: Int,
    subwayTransportModes :: [Maybe TransportMode],
    subwayItineraries :: Int,
    busTransportModes :: [Maybe TransportMode],
    busItineraries :: Int,
    bestTransportModes :: [Maybe TransportMode],
    bestItineraries :: Int
  }
  deriving (Generic, Show, Eq)

instance ToJSON MultiModePlanArgs where
  toJSON (MultiModePlanArgs multiModePlanArgsFrom multiModePlanArgsTo multiModePlanArgsDate multiModePlanArgsTime multiModePlanArgsMetroTransportModes multiModePlanArgsMetroItineraries multiModePlanArgsSubwayTransportModes multiModePlanArgsSubwayItineraries multiModePlanArgsBusTransportModes multiModePlanArgsBusItineraries multiModePlanArgsBestTransportModes multiModePlanArgsBestItineraries) =
    omitNulls
      [ "from" .= multiModePlanArgsFrom,
        "to" .= multiModePlanArgsTo,
        "date" .= multiModePlanArgsDate,
        "time" .= multiModePlanArgsTime,
        "metroTransportModes" .= multiModePlanArgsMetroTransportModes,
        "metroItineraries" .= multiModePlanArgsMetroItineraries,
        "subwayTransportModes" .= multiModePlanArgsSubwayTransportModes,
        "subwayItineraries" .= multiModePlanArgsSubwayItineraries,
        "busTransportModes" .= multiModePlanArgsBusTransportModes,
        "busItineraries" .= multiModePlanArgsBusItineraries,
        "bestTransportModes" .= multiModePlanArgsBestTransportModes,
        "bestItineraries" .= multiModePlanArgsBestItineraries
      ]

data MultiModePlan = MultiModePlan
  { metro :: MultiModePlanMetro,
    subway :: MultiModePlanSubway,
    bus :: MultiModePlanBus,
    best :: MultiModePlanBest
  }
  deriving (Generic, Show, Eq)

instance FromJSON MultiModePlan where
  parseJSON =
    withObject "MultiModePlan" (\v -> MultiModePlan <$> v .: "metro" <*> v .: "subway" <*> v .: "bus" <*> v .: "best")

newtype MultiModePlanMetro = MultiModePlanMetro
  { itineraries :: [Maybe OTPPlanPlanItineraries]
  }
  deriving (Generic, Show, Eq)

instance FromJSON MultiModePlanMetro where
  parseJSON =
    withObject "MultiModePlanMetro" (\v -> MultiModePlanMetro <$> v .: "itineraries")

newtype MultiModePlanSubway = MultiModePlanSubway
  { itineraries :: [Maybe OTPPlanPlanItineraries]
  }
  deriving (Generic, Show, Eq)

instance FromJSON MultiModePlanSubway where
  parseJSON =
    withObject "MultiModePlanSubway" (\v -> MultiModePlanSubway <$> v .: "itineraries")

newtype MultiModePlanBus = MultiModePlanBus
  { itineraries :: [Maybe OTPPlanPlanItineraries]
  }
  deriving (Generic, Show, Eq)

instance FromJSON MultiModePlanBus where
  parseJSON =
    withObject "MultiModePlanBus" (\v -> MultiModePlanBus <$> v .: "itineraries")

newtype MultiModePlanBest = MultiModePlanBest
  { itineraries :: [Maybe OTPPlanPlanItineraries]
  }
  deriving (Generic, Show, Eq)

instance FromJSON MultiModePlanBest where
  parseJSON =
    withObject "MultiModePlanBest" (\v -> MultiModePlanBest <$> v .: "itineraries")

directionToText :: AbsoluteDirection -> Text
directionToText = \case
  AbsoluteDirectionNORTH -> "NORTH"
  AbsoluteDirectionNORTHEAST -> "NORTHEAST"
  AbsoluteDirectionEAST -> "EAST"
  AbsoluteDirectionSOUTHEAST -> "SOUTHEAST"
  AbsoluteDirectionSOUTH -> "SOUTH"
  AbsoluteDirectionSOUTHWEST -> "SOUTHWEST"
  AbsoluteDirectionWEST -> "WEST"
  AbsoluteDirectionNORTHWEST -> "NORTHWEST"

textToDirection :: Text -> Maybe AbsoluteDirection
textToDirection = \case
  "NORTH" -> Just AbsoluteDirectionNORTH
  "NORTHEAST" -> Just AbsoluteDirectionNORTHEAST
  "EAST" -> Just AbsoluteDirectionEAST
  "SOUTHEAST" -> Just AbsoluteDirectionSOUTHEAST
  "SOUTH" -> Just AbsoluteDirectionSOUTH
  "SOUTHWEST" -> Just AbsoluteDirectionSOUTHWEST
  "WEST" -> Just AbsoluteDirectionWEST
  "NORTHWEST" -> Just AbsoluteDirectionNORTHWEST
  _ -> Nothing

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be AbsoluteDirection where
  sqlValueSyntax = sqlValueSyntax . directionToText

instance FromBackendRow Postgres AbsoluteDirection where
  fromBackendRow = do
    txt <- fromBackendRow
    case textToDirection txt of
      Just d -> pure d
      Nothing -> fail $ "Invalid AbsoluteDirection value in DB: " ++ T.unpack txt

instance BeamSqlBackend be => B.HasSqlEqualityCheck be AbsoluteDirection
