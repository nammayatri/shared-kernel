{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Client for the Xyne Conversation API that runs a workflow as a chat bot
-- (@POST /sdlc-api/apps/workflows/conversations@, @POST .../:id/end@).
--
-- One call advances the conversation by one turn: start it (workflowId +
-- context), answer the current turn (choice or text), or just read the
-- current turn (conversationId only). The turn is passed through as raw JSON:
-- the app renders it and its shape is owned by Xyne, so typing it here would
-- only add a place for the two to drift apart.
module Kernel.External.Ticket.XyneSpaces.Conversation
  ( XyneConversationReq (..),
    XyneConversationTurn,
    XyneConversationError (..),
    advanceConversationAPI,
    endConversationAPI,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (throwError)
import Kernel.Utils.Servant.Client
import qualified Network.HTTP.Types as HttpTypes
import Servant hiding (throwError)
import Servant.Client.Core (ClientError (..), ResponseF (..))

-- | One request to @POST /conversations@. Exactly one of the three shapes is
-- meaningful: start ('workflowId' set), answer ('turnId' plus 'choice' or
-- 'text'), or read (only 'conversationId'). 'ToJSON' is hand-written so absent
-- fields are omitted rather than sent as @null@, which Xyne rejects.
data XyneConversationReq = XyneConversationReq
  { workflowId :: Maybe Text,
    -- | Object whose keys are the names declared under "Started with" in the
    -- workflow builder; values may be any JSON.
    context :: Maybe A.Value,
    conversationId :: Maybe Text,
    turnId :: Maybe Text,
    choice :: Maybe Text,
    text :: Maybe Text,
    -- | Answers for @reply.fields@ screens (not produced by builder-made flows).
    values :: Maybe A.Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToSchema)

instance ToJSON XyneConversationReq where
  toJSON XyneConversationReq {..} =
    A.object $
      catMaybes
        [ ("workflowId" A..=) <$> workflowId,
          ("context" A..=) <$> context,
          ("conversationId" A..=) <$> conversationId,
          ("turnId" A..=) <$> turnId,
          ("choice" A..=) <$> choice,
          ("text" A..=) <$> text,
          ("values" A..=) <$> values
        ]

-- | A turn as Xyne returns it: @conversationId@, @turnId@, @status@
-- (@awaiting_reply@ | @finished@ | @handed_off@ | ...), @messages@,
-- @reply.options@, @outcome@. Passed through untouched.
type XyneConversationTurn = A.Value

-- | Non-success answers the caller can act on. 'StaleTurn' is Xyne's 409: the
-- @turnId@ was already answered. Its body is @{ error, turn }@ with the current
-- turn, carried here when present so the caller can render it without another
-- read; older deployments answered with @{ error }@ only, hence the 'Maybe'.
data XyneConversationError
  = StaleTurn (Maybe XyneConversationTurn)
  | ConversationNotFound
  | OtherFailure ClientError
  deriving (Show)

type XyneAdvanceConversationAPI =
  "sdlc-api"
    :> "apps"
    :> "workflows"
    :> "conversations"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] XyneConversationReq
    :> Post '[JSON] XyneConversationTurn

type XyneEndConversationAPI =
  "sdlc-api"
    :> "apps"
    :> "workflows"
    :> "conversations"
    :> Capture "conversationId" Text
    :> "end"
    :> Header "Authorization" Text
    :> Post '[JSON] A.Value

advanceConversationAPI ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  XyneConversationReq ->
  m (Either XyneConversationError XyneConversationTurn)
advanceConversationAPI url token req = do
  let eulerClient = Euler.client (Proxy @XyneAdvanceConversationAPI)
  res <- callAPI url (eulerClient (Just $ "Bearer " <> token) req) "xyneAdvanceConversationAPI" (Proxy @XyneAdvanceConversationAPI)
  pure $ either (Left . classify) Right res
  where
    classify err = case err of
      FailureResponse _ Response {responseStatusCode = status, responseBody = body}
        | HttpTypes.statusCode status == 409 -> StaleTurn (conflictTurn body)
        | HttpTypes.statusCode status == 404 -> ConversationNotFound
      _ -> OtherFailure err
    conflictTurn body = case A.decode body of
      Just (A.Object o) -> KM.lookup "turn" o
      _ -> Nothing

endConversationAPI ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  m ()
endConversationAPI url token conversationId = do
  let eulerClient = Euler.client (Proxy @XyneEndConversationAPI)
  res <- callAPI url (eulerClient conversationId (Just $ "Bearer " <> token)) "xyneEndConversationAPI" (Proxy @XyneEndConversationAPI)
  case res of
    Right _ -> pure ()
    -- Ending an already-closed or unknown conversation is not worth failing the caller over.
    Left (FailureResponse _ Response {responseStatusCode = status}) | HttpTypes.statusCode status `elem` [404, 409] -> pure ()
    Left err -> throwError $ InternalError $ "Failed to call Xyne end conversation API: " <> show err
