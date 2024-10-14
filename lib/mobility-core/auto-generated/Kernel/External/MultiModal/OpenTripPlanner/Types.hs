{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Kernel.External.MultiModal.OpenTripPlanner.Types where

import Data.Morpheus.Client.CodeGen.Internal
import EulerHS.Prelude hiding (id, product)
import Kernel.Prelude (ToSchema)

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
  deriving (Generic, Show, Eq)

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
  __query _ = "query OTPPlan (\n    $from: InputCoordinates!,\n    $to: InputCoordinates!,\n    $date: String,\n    $time:String,\n    $transportModes: [TransportMode],\n    $numItineraries: Int\n){\n  plan(\n    from: $from,\n    to: $to,\n    date: $date,\n    time: $time,\n    transportModes: $transportModes,\n    numItineraries : $numItineraries\n  ) {\n    itineraries {\n      duration\n      legs {\n        pickupType\n        distance\n        mode\n        duration\n        startTime\n        endTime\n        from {\n          name\n          lat\n          lon\n          departureTime\n          arrivalTime\n          stop {\n            code\n          }\n        }\n        to {\n          name\n          lat\n          lon\n          departureTime\n          arrivalTime\n          stop {\n            code\n          }\n        }\n        route {\n          gtfsId\n          longName\n          shortName\n          agency {\n            gtfsId\n            name\n          }\n        }\n        legGeometry {\n          points\n        }\n        fareProducts {\n          id\n        }\n      }\n    }\n  }\n}\n"
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
    legs :: [Maybe OTPPlanPlanItinerariesLegs]
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItineraries where
  parseJSON =
    withObject "OTPPlanPlanItineraries" (\v -> OTPPlanPlanItineraries <$> v .:? "duration" <*> v .: "legs")

data OTPPlanPlanItinerariesLegs = OTPPlanPlanItinerariesLegs
  { pickupType :: Maybe String,
    distance :: Maybe Double,
    mode :: Maybe Mode,
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
    withObject "OTPPlanPlanItinerariesLegs" (\v -> OTPPlanPlanItinerariesLegs <$> v .:? "pickupType" <*> v .:? "distance" <*> v .:? "mode" <*> v .:? "duration" <*> v .:? "startTime" <*> v .:? "endTime" <*> v .: "from" <*> v .: "to" <*> v .:? "route" <*> v .:? "legGeometry" <*> v .:? "fareProducts")

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

newtype OTPPlanPlanItinerariesLegsFromStop = OTPPlanPlanItinerariesLegsFromStop
  { code :: Maybe String
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItinerariesLegsFromStop where
  parseJSON =
    withObject "OTPPlanPlanItinerariesLegsFromStop" (\v -> OTPPlanPlanItinerariesLegsFromStop <$> v .:? "code")

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

newtype OTPPlanPlanItinerariesLegsToStop = OTPPlanPlanItinerariesLegsToStop
  { code :: Maybe String
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItinerariesLegsToStop where
  parseJSON =
    withObject "OTPPlanPlanItinerariesLegsToStop" (\v -> OTPPlanPlanItinerariesLegsToStop <$> v .:? "code")

data OTPPlanPlanItinerariesLegsRoute = OTPPlanPlanItinerariesLegsRoute
  { gtfsId :: String,
    longName :: Maybe String,
    shortName :: Maybe String,
    agency :: Maybe OTPPlanPlanItinerariesLegsRouteAgency
  }
  deriving (Generic, Show, Eq)

instance FromJSON OTPPlanPlanItinerariesLegsRoute where
  parseJSON =
    withObject "OTPPlanPlanItinerariesLegsRoute" (\v -> OTPPlanPlanItinerariesLegsRoute <$> v .: "gtfsId" <*> v .:? "longName" <*> v .:? "shortName" <*> v .:? "agency")

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
