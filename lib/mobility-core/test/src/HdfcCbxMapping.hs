{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}

-- | The HDFC CBX mapping layer, checked against their published samples.
--
-- Deliberately needs no network, no certificate and no encryption service: these are the
-- decisions that turn HDFC's vocabulary into ours, and they are the ones that quietly pay
-- the wrong driver if they are wrong.
--
-- Payloads are copied from Bulk_Payments_API_JOSE_Kit.
module HdfcCbxMapping (hdfcCbxMappingTests) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import EulerHS.Prelude
import Kernel.External.Payout.HdfcCbx.StatusMap
import qualified Kernel.External.Payout.HdfcCbx.Types.Payment as W
import Kernel.External.Payout.Interface.HdfcCbx
import Kernel.External.Payout.Interface.Types
import Test.Tasty
import Test.Tasty.HUnit

-- | Verbatim from Response_Payloads.txt, "Payment Request Acknowledgement".
ackSample :: BL.ByteString
ackSample =
  "{\"txtstatus\":\"ACCEPTED\",\"codstatus\":\"0\",\
  \\"batchnum\":\"BULKAPI030787202509261456090582\",\
  \\"clientcode\":\"0787\",\"groupid\":\"BULKAPI03\",\"iduser\":\"1MKR\",\
  \\"nooftran\":1,\"filerefno\":\"2\",\"trans\":[]}"

-- | Verbatim from Neg_Ack_PaymentRequest.txt. Note batchnum is an empty string, not null.
nackSample :: BL.ByteString
nackSample =
  "{\"txtstatus\":\"Sorry, this is a duplicate transaction request\",\
  \\"codstatus\":\"1\",\"batchnum\":\"\",\
  \\"clientcode\":\"0787\",\"groupid\":\"BULKAPI03\",\"iduser\":\"1MKR\",\
  \\"nooftran\":1,\"filerefno\":\"1\",\"trans\":[]}"

decodeAck :: BL.ByteString -> IO W.CbxPaymentResp
decodeAck raw = case A.eitherDecode raw of
  Left err -> assertFailure ("could not decode HDFC sample: " <> err) >> fail "unreachable"
  Right v -> pure v

acknowledgementIsAccepted :: TestTree
acknowledgementIsAccepted = testCase "ACK: codstatus 0 with a batchnum is an acceptance" $ do
  resp <- decodeAck ackSample
  case readAck resp of
    BulkAccepted ref -> ref @?= "BULKAPI030787202509261456090582"
    other -> assertFailure $ "expected BulkAccepted, got " <> show other

nooftranAcceptsANumber :: TestTree
nooftranAcceptsANumber = testCase "ACK: nooftran parses when sent as a JSON number" $ do
  resp <- decodeAck ackSample
  (W.getLenientInt <$> resp.nooftran) @?= Just 1

nooftranAcceptsAString :: TestTree
nooftranAcceptsAString = testCase "ACK: nooftran parses when sent as a JSON string" $ do
  -- HDFC's updated request sample uses "1"; their acknowledgement uses 1
  resp <- decodeAck "{\"codstatus\":\"0\",\"batchnum\":\"B1\",\"nooftran\":\"7\"}"
  (W.getLenientInt <$> resp.nooftran) @?= Just 7

duplicateIsNotAPlainRejection :: TestTree
duplicateIsNotAPlainRejection =
  testCase "NACK: a duplicate is distinguished from an ordinary rejection" $ do
    resp <- decodeAck nackSample
    case readAck resp of
      -- This matters: a duplicate means they already hold the batch, so releasing the
      -- reservations would pay every beneficiary in it a second time.
      BulkDuplicate _ -> pure ()
      other -> assertFailure $ "expected BulkDuplicate, got " <> show other

emptyBatchnumIsNotAReference :: TestTree
emptyBatchnumIsNotAReference =
  testCase "NACK: an empty-string batchnum is treated as absent" $ do
    resp <- decodeAck nackSample
    case readAck resp of
      BulkDuplicate mbRef -> mbRef @?= Nothing
      other -> assertFailure $ "expected BulkDuplicate with no reference, got " <> show other

-- | The four codes HDFC actually use for the same reason on one rail. An implementation
-- keyed on codstatus rather than txtreason passes on one and fails on the others.
invalidAccountUnderEveryCode :: TestTree
invalidAccountUnderEveryCode =
  testCase "StatusMap: 'Invalid Account No' is Rejected under 1, R, V and 11018" $
    for_ [("A2A", "1"), ("IMPS", "R"), ("NEFT", "V"), ("NEFT", "11018")] $ \(rail, code) ->
      case statusCategory rail code "Invalid Account No" of
        Just Rejected -> pure ()
        other -> assertFailure $ show (rail, code) <> " gave " <> show other

successCodesAreRailSpecific :: TestTree
successCodesAreRailSpecific =
  testCase "StatusMap: each rail has its own Executed code" $
    for_ [("A2A", "3"), ("IMPS", "S"), ("NEFT", "P"), ("RTGS", "COM")] $ \(rail, code) ->
      case statusCategory rail code "Executed" of
        Just Processed -> pure ()
        other -> assertFailure $ show (rail, code) <> " gave " <> show other

rtgsIsInTheSheet :: TestTree
rtgsIsInTheSheet =
  testCase "StatusMap: RTGS is present -- an earlier extraction wrongly reported it absent" $ do
    statusCategory "RTGS" "TXSETT" "Completed" @?= Just Processed
    statusCategory "RTGS" "TXDSETT" "Deemed Settle" @?= Just Processed

unknownIsNeverTerminal :: TestTree
unknownIsNeverTerminal =
  testCase "StatusMap: an unrecognised combination is unmapped, not guessed" $ do
    statusCategory "NEFT" "ZZZ" "Something new" @?= Nothing
    -- and Interim is the only non-terminal category, so an unmapped code keeps polling
    isTerminal Interim @?= False
    isTerminal Processed @?= True
    isTerminal Rejected @?= True

reasonsDriveDistinctActions :: TestTree
reasonsDriveDistinctActions =
  testCase "Reasons: the four rejection kinds map to four different behaviours" $ do
    failureReasonFor "Invalid Account No" @?= INVALID_ACCOUNT
    failureReasonFor "Invalid Beneficiary account number or Beneficiary account number not found" @?= INVALID_ACCOUNT
    failureReasonFor "The accounts blocked" @?= ACCOUNT_BLOCKED
    failureReasonFor "Invalid IFSC code" @?= REJECTED_AT_VALIDATION
    failureReasonFor "Insufficient funds to carry out this instruction" @?= REJECTED_AT_VALIDATION

reasonMatchingIsCaseInsensitive :: TestTree
reasonMatchingIsCaseInsensitive =
  testCase "Reasons: matching does not depend on HDFC's capitalisation" $ do
    failureReasonFor "INVALID ACCOUNT NO" @?= INVALID_ACCOUNT
    failureReasonFor "the accounts blocked" @?= ACCOUNT_BLOCKED

hdfcCbxMappingTests :: TestTree
hdfcCbxMappingTests =
  testGroup
    "HDFC CBX mapping"
    [ acknowledgementIsAccepted,
      nooftranAcceptsANumber,
      nooftranAcceptsAString,
      duplicateIsNotAPlainRejection,
      emptyBatchnumIsNotAReference,
      invalidAccountUnderEveryCode,
      successCodesAreRailSpecific,
      rtgsIsInTheSheet,
      unknownIsNeverTerminal,
      reasonsDriveDistinctActions,
      reasonMatchingIsCaseInsensitive
    ]
