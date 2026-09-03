{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The HDFC CBX adapter: wire types in, canonical types out.
--
-- This is the only module that knows both vocabularies. Everything above it sees
-- 'BulkPayoutResp' and 'BulkItemOutcome'; nothing above it should ever see @codstatus@,
-- @batchnum@ or @filerefno@.
module Kernel.External.Payout.Interface.HdfcCbx
  ( submitBulkPayout,
    inquireBulkPayout,
    recoverBatchRef,
    registerBeneficiary,

    -- * Exposed for testing

    --
    -- These are the pure mapping decisions -- the part most likely to be wrong, and the
    -- part that can be checked against HDFC's published samples without a network, a
    -- certificate or an encryption service.
    readAck,
    readNoteAck,
    classifyInquiryNote,
    isKnownInterimReason,
    failureReasonFor,
    settled,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Kernel.External.Encryption
import Kernel.External.Payout.HdfcCbx.Auth (fetchToken)
import Kernel.External.Payout.HdfcCbx.Config
import qualified Kernel.External.Payout.HdfcCbx.Flow as Flow
import Kernel.External.Payout.HdfcCbx.StatusMap (StatusCategory (..), statusCategory)
-- Imported unqualified so DisambiguateRecordFields can resolve field names from the
-- constructor. The wire types carry a Cbx prefix precisely so they do not collide with the
-- canonical ones they map to.
import Kernel.External.Payout.HdfcCbx.Types.BeneReg
import Kernel.External.Payout.HdfcCbx.Types.Inquiry
import Kernel.External.Payout.HdfcCbx.Types.Payment
import Kernel.External.Payout.Interface.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Error.Throwing (fromEitherM, fromMaybeM, throwError)
import qualified Kernel.Utils.Jose as Jose
import Kernel.Utils.Logging (logWarning)
import Kernel.Utils.Servant.Client (HasRequestId)
import Numeric (showFFloat)

type HdfcFlow m r = (Metrics.CoreMetrics m, EncFlow m r, HasRequestId r, MonadReader r m)

--------------------------------------------------------------------------------
-- envelope
--------------------------------------------------------------------------------

-- | Sign with our key, encrypt to theirs, call, then reverse both on the way back.
--
-- Every call in this module goes through here, including the ones that fail: HDFC wrap
-- their errors too, so a plaintext-only error path would be dead code until the day it
-- mattered.
withEnvelope ::
  (HdfcFlow m r, ToJSON req, FromJSON resp) =>
  HdfcCbxConfig ->
  (Text -> BaseUrl -> Text -> Text -> Text -> Text -> Text -> m (Either Flow.GatewayNote Text)) ->
  req ->
  m (Either Flow.GatewayNote resp)
withEnvelope cfg call payload = do
  consumerSecret <- decrypt cfg.consumerSecret
  apiKey <- decrypt cfg.apiKey
  privPem <- decrypt cfg.signingPrivateKey

  ourPriv <- Jose.parseRsaPrivateKeyPem privPem & fromEitherM (\e -> InternalError $ "HDFC CBX signing key: " <> show e)
  bankPub <- Jose.parseRsaPublicKeyPem cfg.bankPublicKey & fromEitherM (\e -> InternalError $ "HDFC CBX bank key: " <> show e)

  token <- (.access_token) <$> fetchToken (hdfcManagerKey cfg) cfg.tokenUrl cfg.consumerKey consumerSecret cfg.scope

  -- our kid identifies the signing key; theirs identifies the key we encrypted to
  let ourKid = Jose.kidOf (Jose.publicOf ourPriv)
      theirKid = Jose.kidOf bankPub
  signed <- Jose.signJWS ourPriv ourKid (BL.toStrict $ A.encode payload) & fromEitherM (\e -> InternalError $ "HDFC CBX sign: " <> show e)
  envelope <- liftIO (Jose.encryptJWE bankPub theirKid (encodeUtf8 signed)) >>= fromEitherM (\e -> InternalError $ "HDFC CBX encrypt: " <> show e)

  -- a fresh trace id per call; the gateway's Postman guide shows one on every request
  txnId <- T.filter (/= '-') <$> generateGUIDText
  rawE <- call (hdfcManagerKey cfg) cfg.url apiKey cfg.scope txnId token envelope

  case rawE of
    -- the gateway answered outside the tunnel; the caller decides what the note means
    Left note -> pure (Left note)
    Right raw -> do
      inner <- Jose.decryptJWE ourPriv raw & fromEitherM (\e -> InternalError $ "HDFC CBX decrypt: " <> show e)
      verified <- Jose.verifyJWS bankPub (decodeUtf8 inner) & fromEitherM (\e -> InternalError $ "HDFC CBX verify: " <> show e)
      Right <$> (A.eitherDecodeStrict verified & fromEitherM (\e -> InternalError $ "HDFC CBX response shape: " <> show e))

--------------------------------------------------------------------------------
-- submit
--------------------------------------------------------------------------------

submitBulkPayout :: (HdfcFlow m r) => HdfcCbxConfig -> BulkPayoutReq -> m BulkPayoutResp
submitBulkPayout cfg req = do
  let limit = show cfg.maxItemsPerBatch :: Text
  when (length req.items > cfg.maxItemsPerBatch) $
    throwError (InvalidRequest $ "HDFC CBX accepts at most " <> limit <> " items per call")
  respE :: Either Flow.GatewayNote CbxPaymentResp <- withEnvelope cfg Flow.bulkPayment (mkPaymentReq cfg req)
  pure $ either readNoteAck readAck respE
  where
    mkPaymentReq c r =
      CbxPaymentReq
        { clientcode = c.clientCode,
          groupid = c.groupId,
          iduser = c.userId,
          nooftran = LenientInt (length r.items),
          filerefno = r.clientRefNo,
          trans = map (mkTxn r) r.items
        }
    -- Unused tags are sent as "" rather than omitted: the bank's converter expects every
    -- field of the sheet to be present (see the note on 'CbxPaymentTxn').
    mkTxn r item =
      CbxPaymentTxn
        { cdflag = railToCdFlag r.rail,
          code = fromMaybe "" item.beneficiaryCode,
          accno = item.bankAccountNumber,
          amount = money item.amount,
          -- HDFC cap the beneficiary name at 40 on NEFT and RTGS. Truncating can itself
          -- provoke a name-mismatch rejection, so it is done here deliberately and logged
          -- by the caller rather than silently at the bank.
          name = T.take (nameLimit r.rail) item.beneficiaryName,
          adrline = "",
          prtlctn = "",
          adrline1 = "",
          adrline2 = "",
          adrline3 = "",
          adrline4 = "",
          adrline5 = "",
          instrefno = "",
          custrefno = item.itemRef,
          payaddinfo1 = "",
          payaddinfo2 = "",
          payaddinfo3 = "",
          payaddinfo4 = "",
          payaddinfo5 = "",
          payaddinfo6 = "",
          payaddinfo7 = "",
          chqnb = "",
          reqdexctndt = ddmmyyyy r.valueDate,
          micrno = "",
          ifsc = item.bankIfscCode,
          bankname = "",
          branch = "",
          email = fromMaybe "" item.beneficiaryEmail
        }

-- | @codstatus@ @"0"@ is an acknowledgement, @"1"@ a refusal. Duplicates are refusals with a
-- recognisable @txtstatus@, and are pulled out separately because a duplicate means they
-- already hold the batch -- releasing reservations on one would pay everybody twice.
readAck :: CbxPaymentResp -> BulkPayoutResp
readAck r =
  case (r.codstatus, nonEmptyText =<< r.batchnum) of
    (Just "0", Just batchnum) -> BulkAccepted batchnum
    _
      | isDuplicate -> BulkDuplicate (nonEmptyText =<< r.batchnum)
      | otherwise -> BulkRejected (fromMaybe "unknown" r.codstatus) (fromMaybe "no reason given" r.txtstatus)
  where
    isDuplicate = maybe False (T.isInfixOf "duplicate" . T.toLower) r.txtstatus

-- | A refusal the gateway sent as a bare problem document instead of an envelope.
-- The same duplicate rule as 'readAck' applies: a duplicate means they hold the batch.
readNoteAck :: Flow.GatewayNote -> BulkPayoutResp
readNoteAck note
  | T.isInfixOf "duplicate" (T.toLower note.noteReason) = BulkDuplicate Nothing
  | otherwise = BulkRejected note.noteCode note.noteReason

--------------------------------------------------------------------------------
-- inquire
--------------------------------------------------------------------------------

inquireBulkPayout :: (HdfcFlow m r) => HdfcCbxConfig -> BulkInquiryReq -> m BulkInquiryResp
inquireBulkPayout cfg req = do
  batchnum <- req.partnerBatchRef & fromMaybeM (InvalidRequest "batchnum is required; recover it first")
  respE :: Either Flow.GatewayNote CbxInquiryResp <-
    withEnvelope cfg Flow.bulkPaymentInquiry $
      CbxInquiryReq
        { gcif = cfg.groupId,
          iduser = cfg.userId,
          batchnum = batchnum,
          reqdexctndt = ddmmyyyy req.valueDate,
          filerefno = Just req.clientRefNo
        }
  case respE of
    Left note ->
      classifyInquiryNote note
        & fromMaybeM (InternalError $ "HDFC CBX inquiry refused: " <> show note)
    Right resp -> interpret resp
  where
    interpret :: (HdfcFlow m r) => CbxInquiryResp -> m BulkInquiryResp
    interpret r
      | isNoData r = pure InquiryNoData
      | otherwise = case r.trans of
        -- "We have accepted your request. Please enquire again after sometime"
        Nothing -> pure InquiryNotReady
        Just [] -> pure InquiryNotReady
        Just rows -> InquiryResolved . catMaybes <$> mapM (outcomeOf cfg) rows
    isNoData r = maybe False (T.isInfixOf "no data" . T.toLower) r.message || r.codstatus == Just "NDF"

-- | An inquiry on a batch the bank has not finished lands OUTSIDE the JOSE tunnel: 202
-- with a bare problem document ("We have accepted your request. Please enquire again
-- after sometime") -- observed against UAT, 2026-09-02. Interim and no-data notes map to
-- canonical outcomes; an unrecognised note is 'Nothing' and the caller treats it as an
-- error rather than guessing.
classifyInquiryNote :: Flow.GatewayNote -> Maybe BulkInquiryResp
classifyInquiryNote note
  | any (`T.isInfixOf` t) ["enquire again", "still under process", "still in process", "accepted your request"] = Just InquiryNotReady
  | T.isInfixOf "no data" t = Just InquiryNoData
  | otherwise = Nothing
  where
    t = T.toLower note.noteReason

-- | One transaction row to one canonical outcome.
outcomeOf :: (HdfcFlow m r) => HdfcCbxConfig -> CbxInquiryTxn -> m (Maybe (Text, BulkItemOutcome))
outcomeOf _cfg row = case row.custrefno of
  Nothing -> pure Nothing -- a row we cannot attribute is worse than no row
  Just ref -> do
    outcome <- classify
    pure $ Just (ref, outcome)
  where
    rail = maybe "NEFT" cdFlagToRailText row.cdflag
    codstatus = fromMaybe "" row.codstatus
    txtreason = fromMaybe "" row.txtreason

    classify
      -- A settlement rejection means the money left our account and came back. It is not a
      -- validation failure and must never be retried automatically.
      | row.rbistatus == Just "TXREJE" =
        pure $ ItemRejected RETURNED_AFTER_DEBIT (fromMaybe "returned by beneficiary bank" row.rbireason)
      -- Interim states observed live that the generated status sheet does not carry;
      -- mapped here so a normal maker-checker wait does not warn on every poll.
      | isKnownInterimReason txtreason = pure $ ItemInterim (nonEmptyText txtreason)
      | otherwise = case statusCategory rail codstatus txtreason of
        Just Processed -> pure $ settled row
        Just Rejected -> pure $ ItemRejected (failureReasonFor txtreason) txtreason
        Just Interim -> pure $ ItemInterim (nonEmptyText txtreason)
        Nothing -> do
          -- An unrecognised combination is treated as interim, never as success or
          -- failure: guessing either pays twice or strands a balance. Alert on it -- HDFC
          -- do reissue the sheet.
          logWarning $ "HDFC CBX unmapped status: rail=" <> rail <> " codstatus=" <> codstatus <> " txtreason=" <> txtreason
          pure $ ItemInterim (Just $ "unmapped: " <> codstatus <> "/" <> txtreason)

-- | Which reference we hold depends on the rail: a UTR on NEFT and RTGS, an FT number
-- intra-bank. Typed rather than guessed, because the caller shows it to a driver.
settled :: CbxInquiryTxn -> BulkItemOutcome
settled row =
  case (nonEmptyText =<< row.refno, nonEmptyText =<< row.bankrefno) of
    (Just utr, _) -> ItemProcessed utr UTR
    (_, Just ft) -> ItemProcessed ft FT_NUMBER
    _ -> ItemProcessed "" PARTNER_REF

-- | Interim rows the generated sheet does not list, seen live: UAT answers a not-yet
-- approved batch with @codstatus "P", txtreason "Pending Approval"@ on every rail
-- (observed 2026-09-02, batches NODALT12...4066/4067).
isKnownInterimReason :: Text -> Bool
isKnownInterimReason t = "pending approval" `T.isInfixOf` T.toLower t

-- | The judgement layer: 67 distinct rejection texts collapse to five behaviours.
--
-- Deliberately hand-written rather than generated, because each grouping is a decision
-- about what we then /do/ -- notify, defer, or escalate to a human.
failureReasonFor :: Text -> BulkFailureReason
failureReasonFor raw
  | any (`T.isInfixOf` t) ["invalid account", "account number not found", "invalid beneficiary account", "invalid account status"] = INVALID_ACCOUNT
  | any (`T.isInfixOf` t) ["account blocked", "accounts blocked", "frozen", "dormant"] = ACCOUNT_BLOCKED
  | any (`T.isInfixOf` t) ["returned", "reversed"] = RETURNED_AFTER_DEBIT
  | otherwise = REJECTED_AT_VALIDATION
  where
    t = T.toLower raw

--------------------------------------------------------------------------------
-- recover
--------------------------------------------------------------------------------

recoverBatchRef :: (HdfcFlow m r) => HdfcCbxConfig -> BatchRefRecoveryReq -> m BatchRefRecoveryResp
recoverBatchRef cfg req = do
  respE :: Either Flow.GatewayNote CbxBatchNumResp <-
    withEnvelope cfg Flow.batchNumInquiry $
      CbxBatchNumReq
        { gcif = cfg.groupId,
          iduser = cfg.userId,
          reqdexctndt = ddmmyyyy req.valueDate,
          filerefno = req.clientRefNo
        }
  pure $ case respE of
    Left note
      -- No record at their end means the submission never landed; safe to resubmit.
      | T.isInfixOf "no record" (T.toLower note.noteReason) -> BatchRefNotFound
      | otherwise -> BatchRefRefused note.noteCode note.noteReason
    Right resp -> case nonEmptyText =<< resp.batchnum of
      Just batchnum -> BatchRefFound batchnum
      Nothing
        -- They have no record of it, so the submission never landed and it is safe to resubmit.
        | maybe False (T.isInfixOf "no record" . T.toLower) resp.message -> BatchRefNotFound
        | otherwise -> BatchRefRefused (fromMaybe "unknown" resp.codstatus) (fromMaybe "no reason given" resp.message)

--------------------------------------------------------------------------------
-- beneficiary
--------------------------------------------------------------------------------

registerBeneficiary :: (HdfcFlow m r) => HdfcCbxConfig -> BeneRegReq -> m BeneRegResp
registerBeneficiary cfg req = do
  respE :: Either Flow.GatewayNote CbxBeneRegResp <-
    withEnvelope cfg Flow.beneReg $
      CbxBeneRegReq
        { clientcode = cfg.clientCode,
          groupid = cfg.groupId,
          iduser = cfg.userId,
          code = req.beneficiaryCode,
          accno = req.bankAccountNumber,
          ifsc = req.bankIfscCode,
          name = T.take 40 req.beneficiaryName,
          cdflag = railToCdFlag <$> req.rail,
          email = req.beneficiaryEmail,
          bankname = Nothing,
          branch = Nothing
        }
  pure $ case respE of
    Left note
      | T.isInfixOf "already" (T.toLower note.noteReason) -> BeneRegAlreadyRegistered req.beneficiaryCode
      | otherwise -> BeneRegRejected note.noteCode note.noteReason
    Right resp -> case resp.codstatus of
      Just "0" -> BeneRegAccepted req.beneficiaryCode
      _
        | maybe False (T.isInfixOf "already" . T.toLower) resp.message -> BeneRegAlreadyRegistered req.beneficiaryCode
        | otherwise -> BeneRegRejected (fromMaybe "unknown" resp.codstatus) (fromMaybe "no reason given" resp.message)

--------------------------------------------------------------------------------
-- shared
--------------------------------------------------------------------------------

railToCdFlag :: PayoutRail -> CdFlag
railToCdFlag = \case
  RailA2A -> A2A
  RailNEFT -> NEFT
  RailRTGS -> RTGS
  RailIMPS -> IMPS

-- | Matches the keys in the generated status map.
cdFlagToRailText :: CdFlag -> Text
cdFlagToRailText = \case
  A2A -> "A2A"
  NEFT -> "NEFT"
  RTGS -> "RTGS"
  IMPS -> "IMPS"

nameLimit :: PayoutRail -> Int
nameLimit RailNEFT = 40
nameLimit RailRTGS = 40
nameLimit _ = 200

-- | @"2410.00"@. HDFC accept @123@, @123.0@ and @123.00@ and reject anything else, so the
-- format is pinned to two decimals rather than left to Show, which would emit @2.41e3@ for
-- some values.
money :: HighPrecMoney -> Text
money m = T.pack $ showFFloat (Just 2) (realToFrac m :: Double) ""

ddmmyyyy :: Day -> Text
ddmmyyyy = T.pack . formatTime defaultTimeLocale "%d/%m/%Y"

nonEmptyText :: Text -> Maybe Text
nonEmptyText t = if T.null (T.strip t) then Nothing else Just t
