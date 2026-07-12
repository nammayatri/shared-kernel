{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Golden codec tests for Kernel.External.Meta. Round-trips every message type
-- against the Meta-doc-verified examples + 20 live-driver-validated connector
-- envelopes. Comparison is on Data.Aeson.Value (semantic; key order/whitespace
-- ignored), never on encoded bytes.
module MetaCodecs (metaCodecTests) where

import qualified Data.Aeson as Ae
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import EulerHS.Prelude
import Kernel.External.Meta
import Paths_mobility_core (getDataFileName)
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

loadFixture :: FilePath -> IO Ae.Value
loadFixture p = do
  path <- getDataFileName ("test/resources/meta/" <> p)
  bs <- BS.readFile path
  either (\e -> assertFailure ("fixture " <> p <> ": " <> e)) pure (Ae.eitherDecodeStrict bs)

decodeAs :: Ae.FromJSON a => Ae.Value -> IO a
decodeAs v = case Ae.fromJSON v of
  Ae.Error e -> assertFailure ("decode failed: " <> e)
  Ae.Success x -> pure x

decodeLit :: Ae.FromJSON a => LBS.ByteString -> IO a
decodeLit bs = case Ae.eitherDecode bs of
  Left e -> assertFailure ("decode literal failed: " <> e)
  Right x -> pure x

assertJsonEq :: String -> Ae.Value -> Ae.Value -> Assertion
assertJsonEq = assertEqual

-- Envelope drill-down (first entry / first change / first message-or-status).
firstValueOf :: MetaWebhookEnvelope -> IO MetaChangeValue
firstValueOf env = case env.entry of
  (e : _) -> case e.changes of
    (c : _) -> pure c.value
    [] -> assertFailure "envelope entry has no changes"
  [] -> assertFailure "envelope has no entry"

firstMessageOf :: MetaWebhookEnvelope -> IO MetaInboundMessage
firstMessageOf env = do
  v <- firstValueOf env
  case v.messages of
    Just (m : _) -> pure m
    _ -> assertFailure "envelope has no messages"

firstStatusOf :: MetaWebhookEnvelope -> IO MetaStatus
firstStatusOf env = do
  v <- firstValueOf env
  case v.statuses of
    Just (s : _) -> pure s
    _ -> assertFailure "envelope has no statuses"

--------------------------------------------------------------------------------
-- Scenario file shape (only the fields we assert on; extras are ignored).
--------------------------------------------------------------------------------

newtype ScenarioFile = ScenarioFile {steps :: [ScenarioStep]}
  deriving (Generic)

instance Ae.FromJSON ScenarioFile

newtype ScenarioStep = ScenarioStep {inbound :: Maybe Ae.Value}
  deriving (Generic)

instance Ae.FromJSON ScenarioStep

loadScenario :: FilePath -> IO ScenarioFile
loadScenario nm = do
  path <- getDataFileName ("test/resources/meta/scenarios/" <> nm <> ".json")
  bs <- BS.readFile path
  either (\e -> assertFailure ("scenario " <> nm <> ": " <> e)) pure (Ae.eitherDecodeStrict bs)

scenarioInbounds :: ScenarioFile -> [Ae.Value]
scenarioInbounds sf = [ib | s <- sf.steps, Just ib <- [s.inbound]]

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

metaCodecTests :: TestTree
metaCodecTests =
  testGroup
    "Meta WhatsApp Cloud API"
    [ outboundTests,
      inboundTests,
      scenarioTests,
      webhookTests
    ]

--------------------------------------------------------------------------------
-- Outbound: construct -> encode == golden, and decode -> re-encode == golden.
--------------------------------------------------------------------------------

outboundTests :: TestTree
outboundTests =
  testGroup
    "outbound golden codecs"
    [ roundTrip "text" "outbound-text.json" $
        mkTextMessage
          "+16505551234"
          "As requested, here's the link to our latest product: https://www.meta.com/quest/quest-3/"
          (Just True),
      roundTrip "interactive buttons" "outbound-interactive-buttons.json" $
        mkInteractiveButtonsMessage
          "+16505551234"
          (Just "Your appointment")
          "Hi Pablo! Your gardening workshop is scheduled for 9am tomorrow. Use the buttons if you need to reschedule. Thank you!"
          (Just "Lucky Shrub: Your gateway to succulents!")
          [("change-button", "Change"), ("cancel-button", "Cancel")],
      roundTrip "interactive list" "outbound-interactive-list.json" $
        mkInteractiveListMessage
          "919876543210"
          (Just "Available rides")
          "Pick a ride option to continue your booking."
          (Just "Fares are fixed for Tumkur")
          "View Rides"
          [ ( Just "View Rides",
              [ ("ride_auto_001", "Auto", Just "ETA 4 min · Rs 45 fixed fare"),
                ("ride_cab_nonac_002", "Cab (Non-AC)", Just "ETA 7 min · Rs 90 fixed fare")
              ]
            )
          ],
      roundTrip "location request" "outbound-location-request.json" $
        mkLocationRequestMessage
          "+16505551234"
          "Let's start with your pickup. You can either manually *enter an address* or *share your current location*.",
      roundTrip "video" "outbound-video.json" $
        mkVideoMessage
          "919876543210"
          (Left "https://www.example.com/media/ride-confirmation.mp4")
          (Just "Your ride is on the way!")
    ]
  where
    roundTrip name fixture built =
      testGroup
        name
        [ testCase "construct -> golden" $ do
            golden <- loadFixture fixture
            assertJsonEq (name <> " construct") golden (Ae.toJSON built),
          testCase "decode -> re-encode" $ do
            golden <- loadFixture fixture
            req <- decodeAs @MetaSendMessageReq golden
            assertJsonEq (name <> " roundtrip") golden (Ae.toJSON req)
        ]

--------------------------------------------------------------------------------
-- Inbound.
--------------------------------------------------------------------------------

inboundTests :: TestTree
inboundTests =
  testGroup
    "inbound golden codecs"
    [ testCase "text envelope: drill + roundtrip" $ do
        golden <- loadFixture "inbound-envelope-text.json"
        env <- decodeAs @MetaWebhookEnvelope golden
        v <- firstValueOf env
        v.metadata.phoneNumberId @?= "106540352242922"
        m <- firstMessageOf env
        case m.text of
          Just t -> t.body @?= "Does it come in another color?"
          Nothing -> assertFailure "expected inbound text"
        assertJsonEq "text envelope roundtrip" golden (Ae.toJSON env),
      testCase "text message object" $ do
        m <- loadFixture "inbound-message-text.json" >>= decodeAs @MetaInboundMessage
        m.type_ @?= "text"
        case m.text of
          Just t -> t.body @?= "Does it come in another color?"
          Nothing -> assertFailure "expected inbound text",
      testCase "list_reply message" $ do
        m <- loadFixture "inbound-message-list-reply.json" >>= decodeAs @MetaInboundMessage
        case m.interactive of
          Just (MetaListReply d) -> d.id @?= "priority_express"
          _ -> assertFailure "expected list_reply",
      testCase "button_reply interactive object" $ do
        r <- loadFixture "inbound-interactive-button-reply.json" >>= decodeAs @MetaInteractiveReply
        case r of
          MetaButtonReply d -> d.id @?= "change-button"
          _ -> assertFailure "expected button_reply",
      testCase "location (named place): decode + field asserts" $ do
        m <- loadFixture "inbound-message-location.json" >>= decodeAs @MetaInboundMessage
        case m.location of
          Just loc -> do
            loc.latitude @?= 37.44221496582
            loc.name @?= Just "Philz Coffee"
            assertBool "address present" (isJust loc.address)
            assertBool "url present" (isJust loc.url)
          Nothing -> assertFailure "expected location",
      testCase "status sent envelope: status + expiration" $ do
        env <- loadFixture "inbound-envelope-status-sent.json" >>= decodeAs @MetaWebhookEnvelope
        s <- firstStatusOf env
        s.status @?= MetaSent
        case s.conversation of
          Just conv -> conv.expirationTimestamp @?= Just "1750116480"
          Nothing -> assertFailure "expected conversation",
      testCase "status failed element: failed + error code + no conversation" $ do
        s <- loadFixture "inbound-status-failed.json" >>= decodeAs @MetaStatus
        s.status @?= MetaFailed
        s.conversation @?= Nothing
        case s.errors of
          Just (e : _) -> e.code @?= 131049
          _ -> assertFailure "expected errors",
      -- Edge cases (inline literals): forward-compat tolerance must never fail.
      testCase "list_reply without description" $ do
        r <- decodeLit @MetaInteractiveReply "{\"type\":\"list_reply\",\"list_reply\":{\"id\":\"x\",\"title\":\"y\"}}"
        case r of
          MetaListReply d -> d.description @?= Nothing
          _ -> assertFailure "expected list_reply",
      testCase "unknown interactive type -> MetaUnknownReply (decode succeeds)" $ do
        r <- decodeLit @MetaInteractiveReply "{\"type\":\"nfm_reply\",\"nfm_reply\":{}}"
        r @?= MetaUnknownReply "nfm_reply",
      testCase "unknown message type -> decodes, payloads Nothing" $ do
        m <- decodeLit @MetaInboundMessage "{\"from\":\"16505551234\",\"id\":\"wamid.X\",\"timestamp\":\"1\",\"type\":\"reaction\"}"
        m.type_ @?= "reaction"
        m.text @?= Nothing
        m.interactive @?= Nothing,
      testCase "pin-share location (lat/lng only) -> name Nothing" $ do
        m <- decodeLit @MetaInboundMessage "{\"from\":\"x\",\"id\":\"y\",\"timestamp\":\"1\",\"type\":\"location\",\"location\":{\"latitude\":13.34,\"longitude\":77.1}}"
        case m.location of
          Just loc -> do
            loc.name @?= Nothing
            loc.latitude @?= 13.34
          Nothing -> assertFailure "expected location",
      testCase "unknown status value -> MetaUnknownStatus" $ do
        s <- decodeLit @MetaStatusValue "\"paused\""
        s @?= MetaUnknownStatus "paused",
      testCase "classify messages envelope -> EventMessages" $ do
        env <- loadFixture "inbound-envelope-text.json" >>= decodeAs @MetaWebhookEnvelope
        v <- firstValueOf env
        case classifyChangeValue v of
          EventMessages _ _ msgs -> assertBool "non-empty messages" (not (null msgs))
          other -> assertFailure ("expected EventMessages, got " <> show other),
      testCase "classify statuses envelope -> EventStatuses" $ do
        env <- loadFixture "inbound-envelope-status-sent.json" >>= decodeAs @MetaWebhookEnvelope
        v <- firstValueOf env
        case classifyChangeValue v of
          EventStatuses sts -> assertBool "non-empty statuses" (not (null sts))
          other -> assertFailure ("expected EventStatuses, got " <> show other)
    ]

--------------------------------------------------------------------------------
-- Connector scenarios: every steps[].inbound envelope decodes + classifies as
-- EventMessages, plus a couple of spot-checks against known values.
--------------------------------------------------------------------------------

scenarioFiles :: [FilePath]
scenarioFiles =
  [ "cancel-mid-search",
    "driver-not-found",
    "flexi-happy-path",
    "out-of-area",
    "regular-happy-path",
    "token-expiry-reauth"
  ]

scenarioTests :: TestTree
scenarioTests =
  testGroup
    "connector scenario envelopes"
    (map decodesAllInbounds scenarioFiles <> [flexiSpotCheck])
  where
    decodesAllInbounds nm = testCase nm $ do
      sf <- loadScenario nm
      let envs = scenarioInbounds sf
      assertBool (nm <> ": has inbound envelopes") (not (null envs))
      forM_ (zip [0 :: Int ..] envs) $ \(i, ev) -> do
        env <- decodeAs @MetaWebhookEnvelope ev
        v <- firstValueOf env
        case classifyChangeValue v of
          EventMessages {} -> pure ()
          other -> assertFailure (nm <> " step " <> show i <> ": expected EventMessages, got " <> show other)
    flexiSpotCheck = testCase "flexi-happy-path spot-check" $ do
      sf <- loadScenario "flexi-happy-path"
      ev0 <- case scenarioInbounds sf of
        (e : _) -> pure e
        [] -> assertFailure "flexi scenario has no inbound"
      env <- decodeAs @MetaWebhookEnvelope ev0
      v <- firstValueOf env
      v.metadata.phoneNumberId @?= "pn_flexi"
      m <- firstMessageOf env
      case m.interactive of
        Just (MetaButtonReply d) -> d.id @?= "ride_type:flexi"
        _ -> assertFailure "expected button_reply ride_type:flexi"

--------------------------------------------------------------------------------
-- Webhook: HMAC verify + GET challenge (pure; inline literals only).
--------------------------------------------------------------------------------

webhookSecret :: Text
webhookSecret = "meta-test-app-secret"

webhookBody :: LBS.ByteString
webhookBody = "{\"object\":\"whatsapp_business_account\",\"entry\":[]}"

goodSig :: Text
goodSig = "sha256=07b38670fa0f17c96197dfa4b856c927ea9a6a8809b722e025edd80b507b142f"

webhookTests :: TestTree
webhookTests =
  testGroup
    "webhook verification"
    [ testCase "valid X-Hub-Signature-256" $
        verifyMetaSignaturePure webhookSecret (Just goodSig) webhookBody @?= True,
      testCase "tampered signature -> False" $
        verifyMetaSignaturePure webhookSecret (Just "sha256=07b38670fa0f17c96197dfa4b856c927ea9a6a8809b722e025edd80b507b142e") webhookBody @?= False,
      testCase "missing header -> False" $
        verifyMetaSignaturePure webhookSecret Nothing webhookBody @?= False,
      testCase "no sha256= prefix -> False" $
        verifyMetaSignaturePure webhookSecret (Just "07b38670fa0f17c96197dfa4b856c927ea9a6a8809b722e025edd80b507b142f") webhookBody @?= False,
      testCase "malformed/odd-length hex -> False (total, no exception)" $
        verifyMetaSignaturePure webhookSecret (Just "sha256=abc") webhookBody @?= False,
      testCase "challenge: subscribe + matching token -> echo challenge" $
        metaVerifyChallenge "tok" (Just "subscribe") (Just "tok") (Just "1158201444") @?= Just "1158201444",
      testCase "challenge: wrong token -> Nothing" $
        metaVerifyChallenge "tok" (Just "subscribe") (Just "nope") (Just "1158201444") @?= Nothing,
      testCase "challenge: wrong mode -> Nothing" $
        metaVerifyChallenge "tok" (Just "unsubscribe") (Just "tok") (Just "1158201444") @?= Nothing,
      testCase "challenge: absent mode -> Nothing" $
        metaVerifyChallenge "tok" Nothing (Just "tok") (Just "1158201444") @?= Nothing,
      testCase "challenge: absent challenge -> Nothing" $
        metaVerifyChallenge "tok" (Just "subscribe") (Just "tok") Nothing @?= Nothing
    ]
