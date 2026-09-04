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
  (Text -> BaseUrl -> Text -> Text -> Text -> m Text) ->
  req ->
  m resp
withEnvelope cfg call payload = do
  consumerSecret <- decrypt cfg.consumerSecret
  apiKey <- decrypt cfg.apiKey
  privPem <- decrypt cfg.signingPrivateKey

  ourPriv <- Jose.parseRsaPrivateKeyPem privPem & fromEitherM (\e -> InternalError $ "HDFC CBX signing key: " <> show e)
  bankPub <- Jose.parseRsaPublicKeyPem cfg.bankPublicKey & fromEitherM (\e -> InternalError $ "HDFC CBX bank key: " <> show e)

  token <- (.access_token) <$> fetchToken cfg.tlsManagerKey cfg.tokenUrl cfg.consumerKey consumerSecret cfg.scope

  -- our kid identifies the signing key; theirs identifies the key we encrypted to
  let ourKid = Jose.kidOf (Jose.publicOf ourPriv)
      theirKid = Jose.kidOf bankPub
  signed <- Jose.signJWS ourPriv ourKid (BL.toStrict $ A.encode payload) & fromEitherM (\e -> InternalError $ "HDFC CBX sign: " <> show e)
  envelope <- liftIO (Jose.encryptJWE bankPub theirKid (encodeUtf8 signed)) >>= fromEitherM (\e -> InternalError $ "HDFC CBX encrypt: " <> show e)

  raw <- call cfg.tlsManagerKey cfg.url apiKey token envelope

  inner <- Jose.decryptJWE ourPriv raw & fromEitherM (\e -> InternalError $ "HDFC CBX decrypt: " <> show e)
  verified <- Jose.verifyJWS bankPub (decodeUtf8 inner) & fromEitherM (\e -> InternalError $ "HDFC CBX verify: " <> show e)
  A.eitherDecodeStrict verified & fromEitherM (\e -> InternalError $ "HDFC CBX response shape: " <> show e)

--------------------------------------------------------------------------------
-- submit
--------------------------------------------------------------------------------

submitBulkPayout :: (HdfcFlow m r) => HdfcCbxConfig -> BulkPayoutReq -> m BulkPayoutResp
submitBulkPayout cfg req = do
  let limit = show cfg.maxItemsPerBatch :: Text
  when (length req.items > cfg.maxItemsPerBatch) $
    throwError (InvalidRequest $ "HDFC CBX accepts at most " <> limit <> " items per call")
  resp :: CbxPaymentResp <- withEnvelope cfg Flow.bulkPayment (mkPaymentReq cfg req)
  pure $ readAck resp
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
    mkTxn r item =
      CbxPaymentTxn
        { cdflag = railToCdFlag r.rail,
          accno = item.bankAccountNumber,
          ifsc = item.bankIfscCode,
          amount = money item.amount,
          -- HDFC cap the beneficiary name at 40 on NEFT and RTGS. Truncating can itself
          -- provoke a name-mismatch rejection, so it is done here deliberately and logged
          -- by the caller rather than silently at the bank.
          name = T.take (nameLimit r.rail) item.beneficiaryName,
          custrefno = item.itemRef,
          reqdexctndt = ddmmyyyy r.valueDate,
          code = item.beneficiaryCode,
          bankname = Nothing,
          branch = Nothing,
          email = item.beneficiaryEmail,
          instrefno = Nothing
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

--------------------------------------------------------------------------------
-- inquire
--------------------------------------------------------------------------------

inquireBulkPayout :: (HdfcFlow m r) => HdfcCbxConfig -> BulkInquiryReq -> m BulkInquiryResp
inquireBulkPayout cfg req = do
  batchnum <- req.partnerBatchRef & fromMaybeM (InvalidRequest "batchnum is required; recover it first")
  resp :: CbxInquiryResp <-
    withEnvelope cfg Flow.bulkPaymentInquiry $
      CbxInquiryReq
        { gcif = cfg.groupId,
          iduser = cfg.userId,
          batchnum = batchnum,
          reqdexctndt = ddmmyyyy req.valueDate,
          filerefno = Just req.clientRefNo
        }
  interpret resp
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
  resp :: CbxBatchNumResp <-
    withEnvelope cfg Flow.batchNumInquiry $
      CbxBatchNumReq
        { gcif = cfg.groupId,
          iduser = cfg.userId,
          reqdexctndt = ddmmyyyy req.valueDate,
          filerefno = req.clientRefNo
        }
  pure $ case nonEmptyText =<< resp.batchnum of
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
  resp :: CbxBeneRegResp <-
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
  pure $ case resp.codstatus of
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
