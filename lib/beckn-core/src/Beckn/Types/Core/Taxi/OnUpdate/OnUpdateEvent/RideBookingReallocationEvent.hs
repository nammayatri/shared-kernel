module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideBookingReallocationEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideBookingReallocationEvent,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.Common.CancellationSource as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_BOOKING_REALLOCATION))
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, name)
import GHC.Exts (fromList)

data RideBookingReallocationEvent = RideBookingReallocationEvent
  { id :: Text,
    update_target :: Text
  }
  deriving (Generic, Show)

instance ToJSON RideBookingReallocationEvent where
  toJSON RideBookingReallocationEvent {..} =
    A.Object $
      "id" .= id
        <> "./komn/update_target" .= update_target
        <> "fulfillment" .= (("state" .= (("code" .= RIDE_BOOKING_REALLOCATION) :: A.Object)) :: A.Object)

instance FromJSON RideBookingReallocationEvent where
  parseJSON = withObject "RideBookingReallocationEvent" $ \obj -> do
    update_type <- (obj .: "fulfillment") >>= (.: "state") >>= (.: "code")
    unless (update_type == RIDE_BOOKING_REALLOCATION) $ fail "Wrong update_type."
    RideBookingReallocationEvent
      <$> obj .: "id"
      <*> obj .: "./komn/update_target"

instance ToSchema RideBookingReallocationEvent where
  declareNamedSchema _ = do
    txt <- declareSchemaRef (Proxy :: Proxy Text)
    update_type <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
    let st =
          mempty
            & type_ L.?~ OpenApiObject
            & properties
              L..~ fromList
                [("code", update_type)]
            & required L..~ ["code"]
        fulfillment =
          mempty
            & type_ L.?~ OpenApiObject
            & properties
              L..~ fromList
                [("state", Inline st)]
            & required L..~ ["state"]
    return $
      NamedSchema (Just "RideBookingReallocationEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", txt),
                ("./komn/update_target", txt),
                ("fulfillment", Inline fulfillment)
              ]
          & required L..~ ["id", "./komn/update_target", "fulfillment"]
