module Kernel.Utils.Registry
  ( registryFetch,
    Kernel.Utils.Registry.registryLookup,
    whitelisting,
    withSubscriberCache,
  )
where

import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Cache
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Registry
import qualified Kernel.Types.Registry.API as API
import qualified Kernel.Types.Registry.Routes as Registry
import Kernel.Utils.Common
import Data.Generics.Labels ()
import qualified EulerHS.Types as T

registryLookup ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  SimpleLookupRequest ->
  m (Maybe Subscriber)
registryLookup registryUrl request =
  registryFetch registryUrl (toLookupReq request)
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
          API.subscriber_id = Just subscriber_id
        }

registryFetch ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  API.LookupRequest ->
  m [Subscriber]
registryFetch registryUrl request = do
  callAPI registryUrl (T.client Registry.lookupAPI request) "lookup"
    >>= fromEitherM (ExternalAPICallError (Just "REGISTRY_CALL_ERROR") registryUrl)

whitelisting ::
  (MonadThrow m, Log m) =>
  (Text -> m Bool) ->
  Maybe Subscriber ->
  m (Maybe Subscriber)
whitelisting p = maybe (pure Nothing) \sub -> do
  unlessM (p sub.subscriber_id) . throwError . InvalidRequest $
    "Not whitelisted subscriber " <> sub.subscriber_id
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
