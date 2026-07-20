{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Facade for the Meta WhatsApp Cloud API: the only module business code needs
-- to import. Re-exports the config/types/error/webhook surface and provides
-- smart constructors that hardcode every wire constant (messaging_product,
-- recipient_type, the interactive/header discriminators) and truncate every
-- text field to Meta's documented length caps. Builders are total (truncate,
-- don't error). STRUCTURAL invariants that Meta enforces but a total builder
-- cannot express without erroring — >=1 (and <=3) reply buttons, unique button
-- ids/titles after truncation, >=1 row per section, and section.title being
-- required only when there is more than one section — are the CALLER's
-- responsibility (validate before send); the builder truncates but never rejects.
module Kernel.External.Meta
  ( module Kernel.External.Meta.Config,
    module Kernel.External.Meta.Types,
    module Kernel.External.Meta.Error,
    module Kernel.External.Meta.Webhook,
    mkTextMessage,
    mkInteractiveButtonsMessage,
    mkInteractiveListMessage,
    mkLocationRequestMessage,
    mkVideoMessage,
    sendText,
    sendInteractiveButtons,
    sendInteractiveList,
    sendLocationRequest,
    sendVideo,
  )
where

import qualified Data.Text as T
import Kernel.External.Meta.Config
import Kernel.External.Meta.Error
import qualified Kernel.External.Meta.Flow as Flow
import Kernel.External.Meta.Types
import Kernel.External.Meta.Webhook
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common (HasRequestId)

--------------------------------------------------------------------------------
-- Smart constructors — wire constants live here, not in the types.
--------------------------------------------------------------------------------

-- | Common outbound envelope. context / biz_opaque_callback_data are omitted
-- (no golden uses them outbound) and can be layered later.
mkReq :: Text -> Text -> Maybe MetaTextBody -> Maybe MetaInteractive -> Maybe MetaVideo -> MetaSendMessageReq
mkReq toNum typ mText mInteractive mVideo =
  MetaSendMessageReq
    { messagingProduct = "whatsapp",
      recipientType = Just "individual",
      to = toNum,
      type_ = typ,
      text = mText,
      interactive = mInteractive,
      video = mVideo,
      context = Nothing,
      bizOpaqueCallbackData = Nothing
    }

mkTextObj :: Text -> MetaTextObject
mkTextObj t = MetaTextObject {text = t}

mkTextHeader :: Text -> MetaHeader
mkTextHeader t =
  MetaHeader {type_ = "text", text = Just (T.take 60 t), image = Nothing, video = Nothing, document = Nothing}

-- | Plain text message. previewUrl toggles link preview.
mkTextMessage :: Text -> Text -> Maybe Bool -> MetaSendMessageReq
mkTextMessage toNum bodyText previewUrl =
  mkReq toNum "text" (Just MetaTextBody {body = T.take 4096 bodyText, previewUrl = previewUrl}) Nothing Nothing

-- | Reply-buttons interactive. Caps: id<=256, title<=20, at most 3 buttons
-- (mirrors whatsapp.ts truncation).
mkInteractiveButtonsMessage :: Text -> Maybe Text -> Text -> Maybe Text -> [(Text, Text)] -> MetaSendMessageReq
mkInteractiveButtonsMessage toNum mHeader bodyText mFooter btns =
  mkReq toNum "interactive" Nothing (Just interactive) Nothing
  where
    interactive =
      MetaInteractive
        { type_ = "button",
          header = mkTextHeader <$> mHeader,
          body = mkTextObj (T.take 1024 bodyText),
          footer = mkTextObj . T.take 60 <$> mFooter,
          action =
            MetaInteractiveAction
              { buttons = Just (map mkBtn (take 3 btns)),
                button = Nothing,
                sections = Nothing,
                name = Nothing
              }
        }
    mkBtn (bid, btitle) =
      MetaReplyButton
        { type_ = "reply",
          reply = MetaButtonRef {id = T.take 256 bid, title = T.take 20 btitle}
        }

-- | List-message interactive. Caps: button label<=20, row id<=200, row
-- title<=24, row description<=72, at most 10 rows total across sections.
-- Each section is (sectionTitle, [(rowId, rowTitle, rowDescription)]).
mkInteractiveListMessage ::
  Text ->
  Maybe Text ->
  Text ->
  Maybe Text ->
  Text ->
  [(Maybe Text, [(Text, Text, Maybe Text)])] ->
  MetaSendMessageReq
mkInteractiveListMessage toNum mHeader bodyText mFooter buttonLabel rawSections =
  mkReq toNum "interactive" Nothing (Just interactive) Nothing
  where
    interactive =
      MetaInteractive
        { type_ = "list",
          header = mkTextHeader <$> mHeader,
          body = mkTextObj (T.take 4096 bodyText),
          footer = mkTextObj . T.take 60 <$> mFooter,
          action =
            MetaInteractiveAction
              { buttons = Nothing,
                button = Just (T.take 20 buttonLabel),
                sections = Just (buildSections rawSections),
                name = Nothing
              }
        }

buildSections :: [(Maybe Text, [(Text, Text, Maybe Text)])] -> [MetaListSection]
buildSections = go (10 :: Int)
  where
    go _ [] = []
    go budget ((stitle, rws) : rest)
      | budget <= 0 = []
      | otherwise =
        let taken = take budget rws
            sec = MetaListSection {title = T.take 24 <$> stitle, rows = map mkRow taken}
         in sec : go (budget - length taken) rest
    mkRow (rid, rtitle, rdesc) =
      MetaListRow
        { id = T.take 200 rid,
          title = T.take 24 rtitle,
          description = T.take 72 <$> rdesc
        }

-- | Location-request interactive. Meta rejects header/footer here, so they are
-- forced Nothing; action.name is the fixed "send_location".
mkLocationRequestMessage :: Text -> Text -> MetaSendMessageReq
mkLocationRequestMessage toNum bodyText =
  mkReq toNum "interactive" Nothing (Just interactive) Nothing
  where
    interactive =
      MetaInteractive
        { type_ = "location_request_message",
          header = Nothing,
          body = mkTextObj (T.take 1024 bodyText),
          footer = Nothing,
          action =
            MetaInteractiveAction
              { buttons = Nothing,
                button = Nothing,
                sections = Nothing,
                name = Just "send_location"
              }
        }

-- | Video message. XOR link/media-id enforced by the Either; caption<=1024.
mkVideoMessage :: Text -> Either Text Text -> Maybe Text -> MetaSendMessageReq
mkVideoMessage toNum linkOrId mCaption =
  mkReq toNum "video" Nothing Nothing (Just video)
  where
    video = case linkOrId of
      Left lnk -> MetaVideo {link = Just lnk, id = Nothing, caption = capped}
      Right mid -> MetaVideo {link = Nothing, id = Just mid, caption = capped}
    capped = T.take 1024 <$> mCaption

--------------------------------------------------------------------------------
-- Thin senders (construct + send).
--------------------------------------------------------------------------------

type SendMessageFlow m r =
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  )

sendText :: SendMessageFlow m r => MetaCfg -> Text -> Text -> Maybe Bool -> m MetaSendMessageResp
sendText cfg toNum bodyText previewUrl =
  Flow.sendMessage cfg (mkTextMessage toNum bodyText previewUrl)

sendInteractiveButtons ::
  SendMessageFlow m r =>
  MetaCfg ->
  Text ->
  Maybe Text ->
  Text ->
  Maybe Text ->
  [(Text, Text)] ->
  m MetaSendMessageResp
sendInteractiveButtons cfg toNum mHeader bodyText mFooter btns =
  Flow.sendMessage cfg (mkInteractiveButtonsMessage toNum mHeader bodyText mFooter btns)

sendInteractiveList ::
  SendMessageFlow m r =>
  MetaCfg ->
  Text ->
  Maybe Text ->
  Text ->
  Maybe Text ->
  Text ->
  [(Maybe Text, [(Text, Text, Maybe Text)])] ->
  m MetaSendMessageResp
sendInteractiveList cfg toNum mHeader bodyText mFooter buttonLabel secs =
  Flow.sendMessage cfg (mkInteractiveListMessage toNum mHeader bodyText mFooter buttonLabel secs)

sendLocationRequest :: SendMessageFlow m r => MetaCfg -> Text -> Text -> m MetaSendMessageResp
sendLocationRequest cfg toNum bodyText =
  Flow.sendMessage cfg (mkLocationRequestMessage toNum bodyText)

sendVideo :: SendMessageFlow m r => MetaCfg -> Text -> Either Text Text -> Maybe Text -> m MetaSendMessageResp
sendVideo cfg toNum linkOrId mCaption =
  Flow.sendMessage cfg (mkVideoMessage toNum linkOrId mCaption)
