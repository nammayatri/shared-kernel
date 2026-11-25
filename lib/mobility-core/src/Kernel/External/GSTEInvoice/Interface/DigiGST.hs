module Kernel.External.GSTEInvoice.Interface.DigiGST where

import Data.Aeson (Value (String))
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Clock (addUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.LocalTime (minutesToTimeZone, utcToLocalTime)
import Kernel.External.Encryption (EncFlow)
import qualified Kernel.External.GSTEInvoice.CharteredInfo.Types as CITypes
import qualified Kernel.External.GSTEInvoice.DigiGST.Flow as Flow
import Kernel.External.GSTEInvoice.DigiGST.Types
import Kernel.External.GSTEInvoice.Interface.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error (GenericError (..))
import Kernel.Utils.Common (MonadFlow, getCurrentTime, throwError)
import Kernel.Utils.Servant.Client (HasRequestId)

-- | Log in and return the id token. tokenExpiry uses the same absolute IST
-- "YYYY-MM-DD HH:MM:SS" form as Chartered Info, so callers cache both alike.
authenticate ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m,
    MonadFlow m
  ) =>
  DigiGSTConfig ->
  m EInvoiceAuthResp
authenticate config = do
  authResp <- Flow.authenticate config
  let mbData = authResp.resp
  case (mbData >>= nonBlank . (.idToken), mbData >>= nonBlank . (.authError)) of
    (Just token, Nothing) -> do
      now <- getCurrentTime
      let expiresAt = addUTCTime (fromIntegral (fromMaybe 3600 (mbData >>= (.expiresIn)))) now
          expiry = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (utcToLocalTime (minutesToTimeZone 330) expiresAt)
      pure EInvoiceAuthResp {authToken = token, tokenExpiry = expiry}
    (_, mbError) ->
      throwError . InternalError $
        "DigiGST login failed: "
          <> fromMaybe "no id_token in response" mbError
          <> maybe "" (" - " <>) (mbData >>= nonBlank . (.authErrorDescription))

generateInvoice ::
  ( Metrics.CoreMetrics m,
    HasRequestId r,
    MonadReader r m,
    MonadFlow m
  ) =>
  DigiGSTConfig ->
  Text ->
  CITypes.EInvoicePayload ->
  m EInvoiceGenerateResp
generateInvoice config token payload =
  fromDigiGSTInvoiceResp <$> Flow.generateInvoice config token (mkDigiGSTInvoiceReq config payload)

-- ---------------------------------------------------------------------------
-- Internal transformers
-- ---------------------------------------------------------------------------

-- | DigiGST takes a flat ERP-style invoice; build it from the standard e-invoice
-- payload, with the supplier block taken from the DigiGST config.
mkDigiGSTInvoiceReq :: DigiGSTConfig -> CITypes.EInvoicePayload -> DigiGSTInvoiceReq
mkDigiGSTInvoiceReq config payload =
  let buyer = payload.buyerDtls
      vals = payload.valDtls
   in DigiGSTInvoiceReq
        { req =
            DigiGSTInvoice
              { returnPeriod = returnPeriodOf payload.docDtls.dt,
                taxScheme = payload.tranDtls.taxSch,
                docCat = "REG",
                docType = payload.docDtls.typ,
                docNo = payload.docDtls.no,
                docDate = payload.docDtls.dt,
                reverseCharge = "N",
                suppGstin = config.gstin,
                supTradeName = config.sellerGstinTradeName,
                supLegalName = config.sellerGstinLegalName,
                supBuildingNo = config.sellerGstinAddr,
                supBuildingName = config.sellerGstinAddr2,
                supLocation = config.sellerGstinLocation,
                supPincode = config.sellerGstinPinCode,
                supStateCode = config.sellerGstinStcd,
                custGstin = buyer.gstin,
                custOrSupName = buyer.lglNm,
                custOrSupAddr1 = buyer.addr1,
                custOrSupAddr4 = buyer.loc,
                custPincode = buyer.pin,
                billToState = buyer.stcd,
                pos = fromMaybe buyer.stcd buyer.pos,
                sec7OfIgstFlag = "N",
                tcsFlag = "N",
                claimRefundFlag = "N",
                autoPopToRefundFlag = "N",
                invAssessableAmt = vals.assVal,
                invIgstAmt = vals.igstVal,
                invCgstAmt = vals.cgstVal,
                invSgstAmt = vals.sgstVal,
                docAmt = vals.totInvVal,
                lineItems = zipWith mkDigiGSTLineItem [1 ..] payload.itemList
              }
        }

-- | The standard payload carries one combined GST rate; DigiGST wants it per tax head.
mkDigiGSTLineItem :: Int -> CITypes.ItemEntry -> DigiGSTLineItem
mkDigiGSTLineItem n item =
  let isInterState = item.igstAmt > 0
   in DigiGSTLineItem
        { itemNo = n,
          supplyType = "TAX",
          hsnsacCode = item.hsnCd,
          isService = item.isServc,
          itemUqc = item.unit,
          itemQty = item.qty,
          unitPrice = item.unitPrice,
          itemAmt = item.totAmt,
          taxableVal = item.assAmt,
          igstRt = if isInterState then item.gstRt else 0,
          igstAmt = item.igstAmt,
          cgstRt = if isInterState then 0 else item.gstRt / 2,
          cgstAmt = item.cgstAmt,
          sgstRt = if isInterState then 0 else item.gstRt / 2,
          sgstAmt = item.sgstAmt,
          totalItemAmt = item.totItemVal
        }

-- | MMYYYY of a DD/MM/YYYY document date.
returnPeriodOf :: Text -> Maybe Text
returnPeriodOf dateText =
  T.pack . formatTime defaultTimeLocale "%m%Y" <$> (parseTimeM True defaultTimeLocale "%d/%m/%Y" (T.unpack dateText) :: Maybe Day)

-- | Report status the way Chartered Info does: "1" only when an IRN came back, "0" otherwise.
fromDigiGSTInvoiceResp :: DigiGSTInvoiceResp -> EInvoiceGenerateResp
fromDigiGSTInvoiceResp DigiGSTInvoiceResp {..} =
  EInvoiceGenerateResp
    { status = if hdr.status == "S" && isJust (resp >>= nonBlank . (.irn)) then "1" else "0",
      irn = resp >>= nonBlank . (.irn),
      ackNo = resp >>= (.ackNo),
      ackDt = resp >>= (.ackDt),
      signedInvoice = resp >>= (.signedInvoice),
      signedQRCode = resp >>= (.signedQRCode),
      errors = do
        r <- resp
        let mbCode = nonBlank r.errorCode
            mbMessage = nonBlank r.errorMessage
        guard (isJust mbCode || isJust mbMessage)
        pure [EInvoiceErrorDetail {errorCode = fromMaybe "" mbCode, errorMessage = fromMaybe "" mbMessage}],
      infoDtls = do
        r <- resp
        code <- nonBlank r.infoErrorCode
        pure [EInvoiceInfoDetail {infoCode = code, description = String <$> nonBlank r.infoErrorMessage}]
    }

nonBlank :: Maybe Text -> Maybe Text
nonBlank = mfilter (not . T.null . T.strip)
