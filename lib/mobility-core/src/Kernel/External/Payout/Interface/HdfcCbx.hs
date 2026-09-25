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
    checkBulkPayoutStatus,
    recoverBatchRef,

    -- * Exposed for testing

    --
    -- These are the pure mapping decisions -- the part most likely to be wrong, and the
    -- part that can be checked against HDFC's published samples without a network, a
    -- certificate or an encryption service.
    readAck,
    readNoteAck,
    classifyInquiryNote,
    isKnownInterimReason,
    settled,
    classifyRow,
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
-- Imported unqualified so DisambiguateRecordFields can resolve field names from the
-- constructor. The wire types carry a Cbx prefix precisely so they do not collide with the
-- canonical ones they map to.
import Kernel.External.Payout.HdfcCbx.Types.Inquiry
import Kernel.External.Payout.HdfcCbx.Types.Payment
import Kernel.External.Payout.Interface.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Error.Throwing (fromEitherM, fromMaybeM, throwError)
import qualified Kernel.Utils.Jose as Jose
import Kernel.Utils.Logging (logDebug, logWarning)
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

  consumerKey <- decrypt cfg.consumerKey
  token <- (.access_token) <$> fetchToken (hdfcManagerKey cfg) cfg.tokenUrl consumerKey consumerSecret cfg.scope

  -- a fresh trace id per call; the gateway's Postman guide shows one on every request
  txnId <- T.filter (/= '-') <$> generateGUIDText

  -- our kid identifies the signing key; theirs identifies the key we encrypted to
  let ourKid = Jose.kidOf (Jose.publicOf ourPriv)
      theirKid = Jose.kidOf bankPub
      requestJson = BL.toStrict $ A.encode payload

  -- Every HDFC call is logged here in plaintext, request and response. The wire carries a JOSE
  -- envelope signed and encrypted end to end, so this is the only point at which either side is
  -- readable; without it a rejection reason is unrecoverable after the fact. Correlated by the
  -- transactionId the gateway echoes in its own logs and quotes on support tickets.
  logDebug $ "HDFC CBX request " <> txnId <> ": " <> decodeUtf8 requestJson

  signed <- Jose.signJWS ourPriv ourKid requestJson & fromEitherM (\e -> InternalError $ "HDFC CBX sign: " <> show e)
  envelope <- liftIO (Jose.encryptJWE bankPub theirKid (encodeUtf8 signed)) >>= fromEitherM (\e -> InternalError $ "HDFC CBX encrypt: " <> show e)

  rawE <- call (hdfcManagerKey cfg) cfg.url apiKey cfg.scope txnId token envelope

  case rawE of
    -- the gateway answered outside the tunnel; the caller decides what the note means
    Left note -> do
      -- Debug, not error: this branch carries the routine "enquire again" 202 as well as real
      -- refusals, and every caller already logs the ones that matter at their own level.
      logDebug $ "HDFC CBX gateway note " <> txnId <> ": " <> show note
      pure (Left note)
    Right raw -> do
      inner <- Jose.decryptJWE ourPriv raw & fromEitherM (\e -> InternalError $ "HDFC CBX decrypt: " <> show e)
      verified <- Jose.verifyJWS bankPub (decodeUtf8 inner) & fromEitherM (\e -> InternalError $ "HDFC CBX verify: " <> show e)
      logDebug $ "HDFC CBX response " <> txnId <> ": " <> decodeUtf8 verified
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
          reqdexctndt = ddmmyyyy r.executionDate,
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
    -- Acknowledged, but no reference quoted. Distinguished from a refusal because the submission
    -- did land: treating it as rejected would release money the partner is about to move.
    (Just "0", Nothing) -> BulkAcceptedNoRef txtstatus
    _
      | isDuplicate -> BulkDuplicate (nonEmptyText =<< r.batchnum) txtstatus
      | otherwise -> BulkRejected (fromMaybe "unknown" r.codstatus) txtstatus
  where
    txtstatus = fromMaybe "no reason given" (nonEmptyText =<< r.txtstatus)
    isDuplicate = maybe False (T.isInfixOf "duplicate" . T.toLower) r.txtstatus

-- | What the gateway sent as a bare problem document instead of an envelope. Every response other
-- than 200 uses that shape, on all three APIs.
--
-- A duplicate is pulled out because it means they hold a file under our reference, which is never a
-- reason to send the money again. Anything else is the gateway refusing the request, carrying the
-- most specific thing these responses have: their own code.
readNoteAck :: Flow.GatewayNote -> BulkPayoutResp
readNoteAck note
  | T.isInfixOf "duplicate" (T.toLower note.noteReason) = BulkDuplicate Nothing note.noteReason
  | otherwise = BulkGatewayFailed note.noteCode note.noteReason

--------------------------------------------------------------------------------
-- inquire
--------------------------------------------------------------------------------

checkBulkPayoutStatus :: (HdfcFlow m r) => HdfcCbxConfig -> BulkStatusCheckReq -> m BulkStatusCheckResp
checkBulkPayoutStatus cfg req = do
  batchnum <- req.partnerBatchRef & fromMaybeM (InvalidRequest "batchnum is required; recover it first")
  respE :: Either Flow.GatewayNote CbxInquiryResp <-
    withEnvelope cfg Flow.bulkPaymentInquiry $
      CbxInquiryReq
        { gcif = cfg.groupId,
          iduser = cfg.userId,
          batchnum = batchnum,
          reqdexctndt = ddmmyyyy req.executionDate,
          filerefno = Just req.clientRefNo
        }
  case respE of
    Left note ->
      classifyInquiryNote note
        & fromMaybeM (InternalError $ "HDFC CBX inquiry refused: " <> show note)
    Right resp -> interpret resp
  where
    interpret :: (HdfcFlow m r) => CbxInquiryResp -> m BulkStatusCheckResp
    interpret r
      | isNoData r = pure StatusCheckNoData
      | otherwise = case r.trans of
        -- "We have accepted your request. Please enquire again after sometime"
        Nothing -> pure StatusCheckNotReady
        Just [] -> pure StatusCheckNotReady
        Just rows -> StatusCheckResolved . catMaybes <$> mapM (outcomeOf cfg) rows
    isNoData r = maybe False (T.isInfixOf "no data" . T.toLower) r.message || r.codstatus == Just "NDF"

-- | An inquiry on a batch the bank has not finished lands OUTSIDE the JOSE tunnel: 202
-- with a bare problem document ("We have accepted your request. Please enquire again
-- after sometime") -- observed against UAT, 2026-09-02. Interim and no-data notes map to
-- canonical outcomes; an unrecognised note is 'Nothing' and the caller treats it as an
-- error rather than guessing.
classifyInquiryNote :: Flow.GatewayNote -> Maybe BulkStatusCheckResp
classifyInquiryNote note
  | any (`T.isInfixOf` t) ["enquire again", "still under process", "still in process", "accepted your request"] = Just StatusCheckNotReady
  | T.isInfixOf "no data" t = Just StatusCheckNoData
  | otherwise = Nothing
  where
    t = T.toLower note.noteReason

-- | One transaction row to one canonical outcome plus its settlement (@rbistatus@) status.
outcomeOf :: (HdfcFlow m r) => HdfcCbxConfig -> CbxInquiryTxn -> m (Maybe (Text, BulkItemOutcome, Maybe TransferStatus))
outcomeOf cfg row = case row.custrefno of
  Nothing -> pure Nothing -- a row we cannot attribute is worse than no row
  Just ref -> do
    let (outcome, settlement, mbUnmapped) = classifyRow cfg.ownBankIfscPrefix row
    whenJust mbUnmapped $ \what -> logWarning $ "HDFC CBX unmapped status: " <> what
    pure $ Just (ref, outcome, settlement)

-- | The settlement axis (stored as @payout_order.transferStatus@) mirrors @rbistatus@ exactly, and
-- is set by nothing else: absent when there is no RBI settlement statement -- intra-bank, which
-- never gets one, or a transaction before settlement begins -- in-progress for @TXSIP@, transferred
-- for @TXSETT@/@TXDSETT@, failed for @TXREJE@.
settlementStatusOf :: Maybe Text -> Maybe TransferStatus
settlementStatusOf raw = case parseCbxRbiStatus raw of
  Just CbxSettled -> Just TRANSFERRED
  Just CbxDeemedSettled -> Just TRANSFERRED
  Just CbxSettlementInProgress -> Just TRANSFER_INITIATED
  Just CbxSettlementRejected -> Just TRANSFER_FAILED
  _ -> Nothing -- null or unrecognised rbistatus: no settlement statement

-- | One inquiry row to one canonical outcome, its settlement status, and whatever could not be
-- recognised.
--
-- Pure and total, so the whole decision can be exercised without a network, a certificate or a
-- partner. Classification is structural: it reads the two coded axes and never the prose, because
-- the partner's reason text is free-form and is carried through to the caller verbatim instead.
--
-- Axes, in order:
--
--   1. Intra-bank first, because it has no settlement axis at all. The partner executes a transfer
--      inside its own books whenever the beneficiary banks with it -- converting the rail silently
--      and echoing back the @cdflag@ we SENT -- and such a row never carries @rbistatus@. So the
--      beneficiary IFSC, not the rail we asked for, is what decides, and @codstatus@ alone is
--      terminal: @E@ is itself the credit confirmation.
--   2. Otherwise settlement outranks @codstatus@: @TXREJE@ is a return after debit and
--      @TXSETT@/@TXDSETT@ a confirmed credit, whatever @codstatus@ still says. Both are terminal;
--      @TXREJE@ is taken first only because @rbireason@ plus a failed settlement status is the more
--      informative pair when a row carries both.
--   3. Then @codstatus@: @R@ terminal rejection; @P@ the checker-queue wait; @C@ in process; @E@
--      debited from our nodal account, where the beneficiary credit is confirmed only by
--      @rbistatus@, so @E@ without one stays in flight.
--   4. Anything unrecognised stays interim and is reported for alerting, never guessed: reading it
--      as success pays twice, reading it as failure strands a balance.
classifyRow :: Maybe Text -> CbxInquiryTxn -> (BulkItemOutcome, Maybe TransferStatus, Maybe Text)
classifyRow mbOwnBankIfscPrefix row
  | intraBank = case parseCbxTxnStatus row.codstatus of
    -- No RBI leg is coming, so the debit is the credit. Settlement is reported done here rather
    -- than left absent, because nothing else will ever report it for this row.
    CbxCompleted -> (settled intraBank row, Just TRANSFERRED, Nothing)
    CbxRejected -> (rejected row.codstatus txtreason, Nothing, Nothing)
    CbxPendingApproval -> (ItemPendingApproval (nonEmptyText txtreason), Nothing, Nothing)
    CbxInProcess -> (ItemInterim (nonEmptyText txtreason), Nothing, Nothing)
    CbxUnknownStatus other ->
      ( ItemInterim (Just $ "unmapped codstatus: " <> other <> "/" <> txtreason),
        Nothing,
        Just $ "intra-bank codstatus=" <> other <> " txtreason=" <> txtreason
      )
  | rbi == Just CbxSettlementRejected =
    (rejected row.rbistatus (fromMaybe "returned by beneficiary bank" row.rbireason), Just TRANSFER_FAILED, Nothing)
  | rbi == Just CbxSettled || rbi == Just CbxDeemedSettled =
    (settled intraBank row, Just TRANSFERRED, Nothing)
  | otherwise = case parseCbxTxnStatus row.codstatus of
    CbxRejected -> (rejected row.codstatus txtreason, settlementStatusOf row.rbistatus, Nothing)
    CbxPendingApproval -> (ItemPendingApproval (nonEmptyText txtreason), settlementStatusOf row.rbistatus, Nothing)
    CbxInProcess -> (ItemInterim (nonEmptyText txtreason), settlementStatusOf row.rbistatus, Nothing)
    CbxCompleted -> onCompleted
    CbxUnknownStatus other ->
      ( ItemInterim (Just $ "unmapped codstatus: " <> other <> "/" <> txtreason),
        settlementStatusOf row.rbistatus,
        Just $ "rail=" <> rail <> " codstatus=" <> other <> " txtreason=" <> txtreason
      )
  where
    rbi = parseCbxRbiStatus row.rbistatus
    rail = maybe "NEFT" cdFlagToRailText row.cdflag
    txtreason = fromMaybe "" row.txtreason
    -- Both halves are the partner's own words: the coded axis that refused the item, and its text.
    -- Which axis it was is also readable from the settlement status -- TRANSFER_FAILED for a
    -- post-debit return, absent for a validation rejection.
    rejected mbCode detail = ItemRejected (nonEmptyText =<< mbCode) detail
    -- Either arm is sufficient. The @cdflag@ arm catches an intra-bank transfer we asked for; the
    -- IFSC arm catches one the partner performed on its own, which the echoed @cdflag@ cannot show.
    intraBank =
      row.cdflag == Just A2A
        || maybe False (\prefix -> maybe False (T.isPrefixOf (T.toUpper prefix) . T.toUpper) (nonEmptyText =<< row.ifsc)) mbOwnBankIfscPrefix

    -- @E@ on a rail that does have a settlement layer: debited from our nodal account, but the
    -- beneficiary credit is confirmed only by @rbistatus@.
    onCompleted = case rbi of
      Just (CbxUnknownRbiStatus other) ->
        ( ItemInterim (Just $ "unmapped settlement status: " <> other),
          Nothing,
          Just $ "rail=" <> rail <> " rbistatus=" <> other
        )
      _ -> (ItemInterim (Just "debited, awaiting settlement confirmation"), settlementStatusOf row.rbistatus, Nothing)

-- | Which reference we hold depends on the rail: a UTR on NEFT and RTGS, an FT number intra-bank.
-- Typed rather than guessed, because the caller shows it to a driver.
settled :: Bool -> CbxInquiryTxn -> BulkItemOutcome
settled intraBank row =
  case (nonEmptyText =<< row.refno, nonEmptyText =<< row.bankrefno) of
    -- @bankrefno@ is documented only for payment type @I@ (intra-bank), where it is an FT number,
    -- and it is preferred there: on a converted transfer @refno@ may still be absent while the FT
    -- number is the real instrument.
    _ | intraBank, Just ft <- nonEmptyText =<< row.bankrefno -> ItemProcessed ft FT_NUMBER
    -- @refno@ is one field carrying different instruments per rail: the specification calls it
    -- \"UTR No for RTGS\", but the same tag returns an RRN on IMPS. Labelling it by the rail the
    -- row came back on keeps the type honest -- a driver shown \"UTR\" against an RRN cannot
    -- trace it, and recon joins on the instrument.
    (Just ref, _) -> ItemProcessed ref (if intraBank then FT_NUMBER else refTypeForRail row.cdflag)
    (_, Just ft) -> ItemProcessed ft FT_NUMBER
    _ -> ItemProcessed "" PARTNER_REF

-- | Which instrument @refno@ holds, given the rail the partner reported.
refTypeForRail :: Maybe CdFlag -> SettlementRefType
refTypeForRail = \case
  Just IMPS -> RRN
  Just A2A -> FT_NUMBER
  _ -> UTR -- NEFT, RTGS, and an unstated rail: the documented meaning of the field

-- | Interim rows the generated sheet does not list, seen live: UAT answers a not-yet
-- approved batch with @codstatus "P", txtreason "Pending Approval"@ on every rail
-- (observed 2026-09-02, batches NODALT12...4066/4067).
isKnownInterimReason :: Text -> Bool
isKnownInterimReason t = "pending approval" `T.isInfixOf` T.toLower t

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
          reqdexctndt = ddmmyyyy req.executionDate,
          filerefno = req.clientRefNo
        }
  -- Only two answers are usable: the reference, or "no record". Anything else -- a note we do not
  -- recognise, or an envelope quoting neither -- leaves us not knowing whether the partner received
  -- the batch, and a batch we cannot place must never be failed on that basis. Throwing hands it to
  -- the caller's own retry schedule, which parks it for a human when the plan runs out.
  case respE of
    Left note
      -- No record at their end means the submission never landed.
      | T.isInfixOf "no record" (T.toLower note.noteReason) -> pure BatchRefNotFound
      | otherwise -> throwError . InternalError $ "HDFC CBX batch-ref recovery gave no answer: " <> show note
    Right resp -> case nonEmptyText =<< resp.batchnum of
      Just batchnum -> pure (BatchRefFound batchnum)
      Nothing
        | maybe False (T.isInfixOf "no record" . T.toLower) resp.message -> pure BatchRefNotFound
        | otherwise ->
          throwError . InternalError $
            "HDFC CBX batch-ref recovery quoted no batchnum: "
              <> fromMaybe "unknown" resp.codstatus
              <> " "
              <> fromMaybe "no reason given" resp.message

--------------------------------------------------------------------------------
-- beneficiary
--------------------------------------------------------------------------------

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
