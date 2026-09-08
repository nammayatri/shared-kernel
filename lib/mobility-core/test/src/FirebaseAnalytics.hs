{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module FirebaseAnalytics
  ( firebaseAnalyticsTests,
    clientRedactionTests,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Kernel.External.EventTracking.FirebaseAnalytics.Config
import Kernel.External.EventTracking.FirebaseAnalytics.Flow (limitWarnings, selectApp, toMicros, toMpCollectReq)
import Kernel.External.EventTracking.Interface.Types (EventTrackingReq (..))
import Kernel.Prelude
import Kernel.Types.Version (DeviceType (..))
import Kernel.Utils.Servant.Client (redactClientError)
import Test.Tasty
import Test.Tasty.HUnit

baseReq :: EventTrackingReq
baseReq =
  EventTrackingReq
    { customerId = "rider-1",
      eventName = "ny_cab_ride_completed",
      attributes = A.object ["ride_count" A..= (3 :: Int), "fare" A..= (180 :: Int)],
      timestamp = Just (posixSecondsToUTCTime 1757312345),
      appInstanceId = Just "iid-1",
      platform = Just ANDROID
    }

sampleCfgJson :: Text
sampleCfgJson =
  "{\"baseUrl\":\"https://www.google-analytics.com\","
    <> "\"apps\":[{\"platform\":\"ANDROID\",\"firebaseAppId\":\"1:1:android:a\",\"apiSecret\":\"enc-android\"},"
    <> "{\"platform\":\"IOS\",\"firebaseAppId\":\"1:1:ios:b\",\"apiSecret\":\"enc-ios\"}],"
    <> "\"enabled\":true,\"debug\":false}"

decodeCfg :: IO FirebaseAnalyticsCfg
decodeCfg = case A.eitherDecodeStrict (TE.encodeUtf8 sampleCfgJson) of
  Left err -> assertFailure $ "sample config did not decode: " <> err
  Right cfg -> pure cfg

objectWithParams :: Int -> A.Value
objectWithParams n =
  fromMaybe A.Null . A.decodeStrict . TE.encodeUtf8 $
    "{" <> T.intercalate "," ["\"p" <> show i <> "\":1" | i <- [1 .. n]] <> "}"

mappingTests :: TestTree
mappingTests =
  testGroup
    "Measurement Protocol body"
    [ testCase "maps the neutral request onto the documented body" $
        A.toJSON (toMpCollectReq "iid-1" baseReq)
          @?= A.object
            [ "app_instance_id" A..= ("iid-1" :: Text),
              "user_id" A..= ("rider-1" :: Text),
              "timestamp_micros" A..= (1757312345000000 :: Integer),
              "events"
                A..= [ A.object
                         [ "name" A..= ("ny_cab_ride_completed" :: Text),
                           "params" A..= A.object ["ride_count" A..= (3 :: Int), "fare" A..= (180 :: Int)]
                         ]
                     ]
            ],
      testCase "omits timestamp_micros when the request carries no timestamp" $
        A.toJSON (toMpCollectReq "iid-1" baseReq {timestamp = Nothing})
          @?= A.object
            [ "app_instance_id" A..= ("iid-1" :: Text),
              "user_id" A..= ("rider-1" :: Text),
              "events"
                A..= [ A.object
                         [ "name" A..= ("ny_cab_ride_completed" :: Text),
                           "params" A..= A.object ["ride_count" A..= (3 :: Int), "fare" A..= (180 :: Int)]
                         ]
                     ]
            ],
      testCase "converts UTCTime to whole microseconds" $
        toMicros (posixSecondsToUTCTime 1757312345.5) @?= 1757312345500000
    ]

configTests :: TestTree
configTests =
  testGroup
    "Config and app selection"
    [ testCase "decodes the documented config_json" $ do
        cfg <- decodeCfg
        cfg.enabled @?= True
        cfg.debug @?= False
        length cfg.apps @?= 2,
      testCase "selects the app for the rider's platform" $ do
        cfg <- decodeCfg
        (.firebaseAppId) <$> selectApp IOS cfg.apps @?= Just "1:1:ios:b"
        (.firebaseAppId) <$> selectApp ANDROID cfg.apps @?= Just "1:1:android:a",
      testCase "yields Nothing for an unconfigured platform" $ do
        cfg <- decodeCfg
        let androidOnly = filter ((== ANDROID) . (.platform)) cfg.apps
        (.firebaseAppId) <$> selectApp IOS androidOnly @?= Nothing
    ]

limitTests :: TestTree
limitTests =
  testGroup
    "Documented limits (warn only)"
    [ testCase "a well-formed event produces no warnings" $
        limitWarnings baseReq @?= [],
      testCase "warns on an event name over 40 characters" $
        length (limitWarnings baseReq {eventName = T.replicate 41 "a"}) @?= 1,
      testCase "accepts an event name of exactly 40 characters" $
        limitWarnings baseReq {eventName = T.replicate 40 "a"} @?= [],
      testCase "warns on a reserved event-name prefix" $
        length (limitWarnings baseReq {eventName = "firebase_ride"}) @?= 1,
      testCase "warns on more than 25 params" $
        length (limitWarnings baseReq {attributes = objectWithParams 26}) @?= 1,
      testCase "accepts exactly 25 params" $
        limitWarnings baseReq {attributes = objectWithParams 25} @?= [],
      testCase "warns on a string param value over 100 characters" $
        length (limitWarnings baseReq {attributes = A.object ["city" A..= T.replicate 101 "x"]}) @?= 1,
      testCase "warns on a param name over 40 characters" $
        length (limitWarnings baseReq {attributes = A.object [AK.fromText (T.replicate 41 "k") A..= (1 :: Int)]}) @?= 1,
      testCase "accepts a param name of exactly 40 characters" $
        limitWarnings baseReq {attributes = A.object [AK.fromText (T.replicate 40 "k") A..= (1 :: Int)]} @?= []
    ]

firebaseAnalyticsTests :: TestTree
firebaseAnalyticsTests =
  testGroup
    "Firebase Analytics event tracking"
    [ mappingTests,
      configTests,
      limitTests
    ]

clientRedactionTests :: TestTree
clientRedactionTests =
  testGroup
    "redactClientError"
    [ testCase "masks api_secret in a query string" $ do
        let out = redactClientError "https://www.google-analytics.com/mp/collect?firebase_app_id=1:1:android:a&api_secret=TOPSECRET"
        assertBool "secret still present" (not ("TOPSECRET" `T.isInfixOf` out))
        assertBool "placeholder missing" ("api_secret=[REDACTED]" `T.isInfixOf` out)
        assertBool "app id should survive" ("firebase_app_id=1:1:android:a" `T.isInfixOf` out),
      testCase "masks api_secret in a shown servant query item" $ do
        let out = redactClientError "fromList [(\"firebase_app_id\",Just \"1:1\"),(\"api_secret\",Just \"TOPSECRET\")]"
        assertBool "secret still present" (not ("TOPSECRET" `T.isInfixOf` out)),
      testCase "still masks Google API keys" $ do
        let out = redactClientError "key=AIzaSyA-abc_123"
        assertBool "key still present" (not ("AIzaSyA" `T.isInfixOf` out))
    ]
