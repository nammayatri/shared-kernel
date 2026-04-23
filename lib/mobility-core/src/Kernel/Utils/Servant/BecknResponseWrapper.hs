{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-error=deprecations #-}

module Kernel.Utils.Servant.BecknResponseWrapper
  ( wrapBecknResponse,
    extractBapIdFromBody,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import EulerHS.Prelude
import qualified Network.Wai as Wai

wrapBecknResponse :: (BL.ByteString -> IO Bool) -> Wai.Middleware
wrapBecknResponse shouldWrap app req respond = do
  bodyBs <- consumeRequestBody req
  reqWithReplayableBody <- rebindRequestBody req bodyBs
  wrap <- shouldWrap bodyBs
  if wrap
    then app reqWithReplayableBody $ \resp -> do
      let (status, headers, withBody) = Wai.responseToStream resp
      respBody <- consumeStreamingBody withBody
      let final = fromMaybe respBody (wrapIfAckEnvelope respBody)
      respond $ Wai.responseLBS status headers final
    else app reqWithReplayableBody respond
  where
    consumeRequestBody :: Wai.Request -> IO BL.ByteString
    -- WAI's consumeRequestBodyStrict returns IO BL.ByteString in this codebase (the
    -- name refers to strict stream consumption, not the ByteString flavour). Pass
    -- through directly.
    consumeRequestBody = Wai.consumeRequestBodyStrict

    rebindRequestBody :: Wai.Request -> BL.ByteString -> IO Wai.Request
    rebindRequestBody r body = do
      consumed <- newIORef False
      let replay = do
            already <- readIORef consumed
            if already
              then pure BS.empty
              else do
                writeIORef consumed True
                pure (BL.toStrict body)
      pure r {Wai.requestBody = replay}

    consumeStreamingBody ::
      ((Wai.StreamingBody -> IO ()) -> IO ()) ->
      IO BL.ByteString
    consumeStreamingBody withBody = do
      ref <- newIORef mempty
      withBody $ \streamingBody ->
        streamingBody
          (\builder -> modifyIORef' ref (<> builder))
          (pure ())
      BB.toLazyByteString <$> readIORef ref

    wrapIfAckEnvelope :: BL.ByteString -> Maybe BL.ByteString
    wrapIfAckEnvelope body = do
      value <- either (const Nothing) Just (A.eitherDecode body)
      guard (isAckEnvelope value)
      pure $ A.encode (A.object ["response" A..= value])

    isAckEnvelope :: A.Value -> Bool
    isAckEnvelope (A.Object o) = case AKM.lookup "message" o of
      Just (A.Object m) -> AKM.member "ack" m
      _ -> False
    isAckEnvelope _ = False

extractBapIdFromBody :: BL.ByteString -> Maybe Text
extractBapIdFromBody body = do
  value <- either (const Nothing) Just (A.eitherDecode body)
  obj <- case value of
    A.Object o -> Just o
    _ -> Nothing
  context <- case AKM.lookup "context" obj of
    Just (A.Object c) -> Just c
    _ -> Nothing
  bapIdValue <- AKM.lookup "bap_id" context
  case bapIdValue of
    A.String t -> Just t
    _ -> Nothing
