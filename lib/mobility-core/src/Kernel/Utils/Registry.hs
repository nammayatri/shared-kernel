{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.Registry
  ( registryFetch,
    Kernel.Utils.Registry.registryLookup,
    checkBlacklisted,
    withSubscriberCache,
    checkWhitelisted,
  )
where

import Data.Generics.Labels ()
import qualified EulerHS.Types as T
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Beckn.Domain
import Kernel.Types.Cache
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Registry
import qualified Kernel.Types.Registry.API as API
import qualified Kernel.Types.Registry.Routes as Registry
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

registryLookup ::
  ( CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  SimpleLookupRequest ->
  Text ->
  m (Maybe Subscriber)
registryLookup registryUrl request selfSubId =
  registryFetch registryUrl (toLookupReq request) selfSubId
    >>= \case
      [] -> pure Nothing
      -- These are temporary changes as there is bug in ONDC registry. They are sending multiple entries irrespective of ukId So we added traversal at our end.
      -- It should be removed once issues has been fixed at their end.
      subs -> do
        let subscribers = filter (\subscriber -> subscriber.unique_key_id == request.unique_key_id) subs
        if length subscribers > 1
          then throwError $ InternalError "Multiple subscribers returned for a unique key."
          else
            if length subscribers == 1
              then pure $ Just $ head subscribers
              else pure Nothing
  where
    toLookupReq SimpleLookupRequest {..} =
      API.emptyLookupRequest
        { API.unique_key_id = Just unique_key_id,
          API.subscriber_id = Just subscriber_id,
          API.domain = Just domain,
          API._type = Just subscriber_type
        }

registryFetch ::
  ( MonadFlow m,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  API.LookupRequest ->
  Text ->
  m [Subscriber]
registryFetch registryUrl request selfSubId = do
  callAPI' (Just (T.ManagerSelector (getHttpManagerKey selfSubId))) registryUrl (T.client Registry.lookupAPI request) "lookup" Registry.lookupAPI
    >>= fromEitherM (ExternalAPICallError (Just "REGISTRY_CALL_ERROR") registryUrl)

checkBlacklisted ::
  (MonadThrow m, Log m) =>
  (Text -> Domain -> m Bool) ->
  Maybe Subscriber ->
  m (Maybe Subscriber)
checkBlacklisted isBlackListed = maybe (pure Nothing) \sub -> do
  whenM (isBlackListed sub.subscriber_id sub.domain) . throwError . InvalidRequest $
    "It is a Blacklisted subscriber " <> sub.subscriber_id
  pure (Just sub)

checkWhitelisted ::
  (MonadThrow m, Log m) =>
  (Text -> Domain -> Text -> m Bool) ->
  Text ->
  Maybe Subscriber ->
  m (Maybe Subscriber)
checkWhitelisted isNotWhiteListed merchantId = maybe (pure Nothing) \sub -> do
  whenM (isNotWhiteListed sub.subscriber_id sub.domain merchantId) . throwError . InvalidRequest $
    "Not Whitelisted subscriber " <> sub.subscriber_id
  pure (Just sub)

withSubscriberCache ::
  ( MonadTime m,
    CacheEx Subscriber m
  ) =>
  (CacheKey Subscriber -> m (Maybe Subscriber)) ->
  CacheKey Subscriber ->
  m (Maybe Subscriber)
withSubscriberCache getData key = do
  now <- getCurrentTime
  caching (getTtl now) getData key
  where
    getTtl now Subscriber {..} =
      nominalDiffTimeToSeconds . fromMaybe (5 * 60) $ valid_until <&> (`diffUTCTime` now)
