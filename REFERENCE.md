# mobility-core Reference Document

Comprehensive reference for the `mobility-core` shared Haskell library powering the Namma Yatri mobility platform. This library provides database abstractions, external service integrations, core types, and utilities consumed by multiple backend services (driver, rider, dashboard).

**536 Haskell modules | 149 directories | AGPL license**

---

## Table of Contents

1. [Module Inventory](#1-module-inventory)
2. [Core Abstractions](#2-core-abstractions)
3. [Storage Abstractions](#3-storage-abstractions)
4. [External Service Abstractions](#4-external-service-abstractions)
5. [Authentication & Authorization](#5-authentication--authorization)
6. [Error Handling](#6-error-handling)
7. [Configuration](#7-configuration)
8. [Utility Functions](#8-utility-functions)
9. [Re-exports](#9-re-exports)
10. [Design Patterns](#10-design-patterns)

---

## 1. Module Inventory

All modules live under `lib/mobility-core/src/Kernel/`.

### Core

| Module | Purpose |
|--------|---------|
| `Prelude` | Custom prelude replacing `Prelude`. Re-exports `Universum`, `safe-exceptions`, common types. Hides `show`, `error`, `undefined`, `id`. |
| `Prelude.OrphanInstances` | Orphan instances for OpenAPI `SecurityDefinitions` |
| `Exit` | Application exit codes (`exitSuccess`, `exitDBConnPrepFailure`, `exitRedisConnPrepFailure`, etc.) |
| `Randomizer` | `getRandomInRange`, `randomizeList`, `getRandomElement` |
| `ServantMultipart` | Re-exports Servant multipart with OpenAPI instance |
| `Serviceability` | `rideServiceable` -- geofencing check for ride requests |

### Types (58 modules)

| Module | Purpose |
|--------|---------|
| `Types.App` | `EnvR`, `FlowHandlerR`, `FlowServerR`, `MonadFlow`, `HasFlowEnv`, `RegToken`, `AuthHeader` |
| `Types.Common` | Re-exports all core types. `IdObject`, `Tables`, `KafkaProperties`, `CentiDouble` |
| `Types.Flow` | `FlowR r a = ReaderT r L.Flow a` -- the primary monad. `runFlowR`, `HasFlowHandlerR` |
| `Types.Id` | `Id domain`, `ShortId domain` -- phantom-typed identifiers |
| `Types.Price` | `Money`, `HighPrecMoney`, `Currency` (INR/USD/EUR), `Price`, `PriceAPIEntity` |
| `Types.Distance` | `Distance`, `HighPrecDistance`, `DistanceUnit` (Meter/Mile/Yard/Kilometer), `Meters`, `HighPrecMeters`, `Kilometers` |
| `Types.Time` | `Seconds`, `Minutes`, `Hours`, `Days`, `Milliseconds`, `Microseconds`, `MonadTime`, `MonadClock` |
| `Types.Centesimal` | `Centesimal` -- fixed-precision decimal (Centi) |
| `Types.Error` | 50+ error types with HTTP codes -- see [Error Handling](#6-error-handling) |
| `Types.Logging` | `LogLevel` (DEBUG/INFO/WARNING/ERROR), `Log` class, `LoggerConfig` |
| `Types.Forkable` | `Forkable` class -- `fork`, `awaitableFork`, `forkMultiple` |
| `Types.MonadGuid` | `MonadGuid` class -- `generateGUIDText` |
| `Types.GuidLike` | `GuidLike m a` class -- `generateGUID` |
| `Types.TryException` | `TryException` class -- `withTryCatch` |
| `Types.Cache` | `Cache a m` / `CacheEx a m` classes with associated `CacheKey` type |
| `Types.CacheFlow` | `CacheFlow m r` constraint, `CacheConfig`, `CacConfig`, `InMemEnv`, `InMemCacheInfo` |
| `Types.Cac` | CAC (Config-as-Code) types: `CACData`, `CacKeyValue`, `initializeCACThroughConfig` |
| `Types.Predicate` | Validation predicates: `Predicate`, `ShowablePredicate`, `And`, `Or`, `Not`, `Regex`, `LengthInRange`, etc. |
| `Types.Validation` | `Validation`, `ValidationDescription`, `Validate` |
| `Types.Version` | `Version` (semver), `Device`, `DeviceType` (IOS/ANDROID), `CloudType` (AWS/GCP) |
| `Types.Credentials` | `Credential`, `PrivateKey`, `PublicKey` |
| `Types.Base64` | `Base64` newtype with serialization instances |
| `Types.HideSecrets` | `HideSecrets` class for redacting sensitive request data |
| `Types.Geofencing` | `GeoRestriction` (Unrestricted/Regions), `GeofencingConfig` |
| `Types.Confidence` | `Confidence` (Sure/Unsure/Neutral) |
| `Types.Documents` | `VerificationStatus` (PENDING/VALID/INVALID/MANUAL_VERIFICATION_REQUIRED/...) |
| `Types.Field` | `(:::)` type-level pairs, `HasFields` constraint family |
| `Types.FromField` | `fromFieldJSON`, `fromFieldEnum`, `fromFieldDefault` helpers |
| `Types.SharedRedisKeys` | `BatchConfig` for batched processing |
| `Types.TimeBound` | `TimeBound` (BoundedByWeekday/BoundedByDay/Unbounded), `findBoundedDomain` |
| `Types.TimeRFC339` | `UTCTimeRFC3339` with RFC 3339 serialization |
| `Types.SlidingWindowCounters` | `SlidingWindowOptions`, `PeriodType` (Minutes/Hours/Days/Months/Years) |
| `Types.SlidingWindowLimiter` | `APIRateLimitOptions` for rate limiting |
| `Types.SystemConfigs` | `SystemConfigs` for runtime configuration |
| `Types.Value` | `MandatoryValue a`, `OptionalValue a` wrappers |
| `Types.Servant` | `PlainText_ISO_8859_1` content type |
| `Types.MerchantOperatingCity` | `MerchantOperatingCity` record |
| `Types.APISuccess` | `APISuccess = Success` |
| `Types.BecknRequest` | `BecknRequest` for inbound Beckn protocol messages |

### Beckn Protocol Types

| Module | Purpose |
|--------|---------|
| `Types.Beckn.Ack` | `AckResponse` |
| `Types.Beckn.City` | `City` with STD code mapping. `cityToStdCode`, `stdCodeToCity`, `initCityMaps` |
| `Types.Beckn.Context` | `Context`, `Action` (SEARCH/SELECT/INIT/CONFIRM/...) |
| `Types.Beckn.Country` | `Country` (India/France/USA/...) |
| `Types.Beckn.Domain` | `Domain` (MOBILITY/METRO/PARKING/LOGISTICS/...) |
| `Types.Beckn.Error` | `Error`, `ErrorType` |
| `Types.Beckn.Gps` | `Gps` with lat/lon parsing |
| `Types.Beckn.IndianState` | 38+ Indian states/territories |
| `Types.Beckn.DecimalValue` | `DecimalValue` (arbitrary precision Rational) |
| `Types.Beckn.ReqTypes` | `BecknReq a`, `BecknCallbackReq a` |
| `Types.Registry.API` | `LookupRequest`, `LookupResponse` |
| `Types.Registry.Routes` | `LookupAPI` Servant type |
| `Types.Registry.Subscriber` | `Subscriber`, `SubscriberType` (BAP/BPP/BG/...), `SubscriberStatus` |

### Storage (30+ modules)

| Directory | Purpose |
|-----------|---------|
| `Beam/` | Beam ORM types, functions, TH utilities, connection management |
| `Storage/Beam/` | Beam-based storage queries |
| `Storage/Esqueleto/` | Esqueleto ORM (alternative SQL builder) |
| `Storage/Hedis/` | Redis/cluster abstractions |
| `Storage/Clickhouse/` | Clickhouse V1 analytics |
| `Storage/ClickhouseV2/` | Clickhouse V2 type-safe ORM |
| `Storage/InMem` | In-memory LRU cache |
| `Storage/Queries/` | Common query patterns |

### External Services (23 service types, 50+ providers)

| Directory | Service |
|-----------|---------|
| `External/SMS/` | GupShup, Exotel, Karix, MyValueFirst, Pinbix, Twilio, Vonage, DigoEngage |
| `External/Call/` | Exotel, TataClickToCall, Twilio, Ozonetel |
| `External/Whatsapp/` | GupShup, Karix, TataCommunications |
| `External/Notification/` | FCM, PayTM, GRPC |
| `External/Payment/` | Juspay, Stripe, PaytmEDC |
| `External/Payout/` | Juspay |
| `External/Settlement/` | HyperPG, BillDesk |
| `External/Wallet/` | Juspay |
| `External/Maps/` | Google, MMI, NextBillion, OSRM |
| `External/MultiModal/` | Google Transit, OpenTripPlanner |
| `External/Verification/` | Idfy, GovtData, HyperVerge, DigiLocker, Tten, SafetyPortal |
| `External/AadhaarVerification/` | Gridline |
| `External/BackgroundVerification/` | Checkr |
| `External/Insurance/` | Acko, IffcoTokio |
| `External/Tokenize/` | HyperVerge, JourneyMonitoring, Gullak, DigiLocker, Tten |
| `External/SOS/` | ERSS, GJ112 |
| `External/IncidentReport/` | ERSS |
| `External/Ticket/` | Kapture |
| `External/Plasma/` | LMS |
| `External/Infobip/` | SendSms, Webhook |
| `External/GoogleTranslate/` | Translation API |
| `External/Encryption` | Passetto encryption, `DbHash`, `EncryptedHashed` |
| `External/Slack/` | Slack messaging |

### Tools & Infrastructure

| Module | Purpose |
|--------|---------|
| `Tools.Logging` | Dynamic log level from DB config |
| `Tools.Slack` | `notifyOnSlack`, `notifyOnSlackIO` |
| `Tools.Slack.Internal` | `SlackEnv`, `createSlackConfig`, `sendSlackMessage` |
| `Tools.Slack.Middleware` | Wai middleware for Slack logging |
| `Tools.Metrics.CoreMetrics` | `CoreMetrics` class, Prometheus implementations |
| `Tools.Metrics.CoreMetrics.Types` | Metric type definitions, `CoreMetricsContainer` |
| `Tools.Metrics.Init` | Prometheus server, Servant instrumentation |
| `Tools.Metrics.AppMetrics` | Application-level latency tracking |
| `Tools.SystemEnv` | Runtime environment variable management from DB |
| `Tools.LoopGracefully` | `loopGracefully` -- signal-aware loop |
| `Tools.ARTUtils` | ART (Application Runtime Tooling) utilities |

### Utilities (40+ modules)

| Module | Purpose |
|--------|---------|
| `Utils.Common` | `generateShortId`, `generateOTPCode`, `generateAplhaNumbericCode` |
| `Utils.Logging` | `logDebug/Info/Warning/Error`, `logTagDebug/Info/Warning/Error`, `withPersonIdLogTag` |
| `Utils.Time` | Time conversions, `isExpired`, `showTimeIst`, `measureDuration`, etc. |
| `Utils.CalculateDistance` | Haversine: `distanceBetweenInMeters`, `getRouteLinearLength` |
| `Utils.ComputeIntersection` | `doRouteIntersectWithLine`, `checkIntersection`, `getBoundingBox` |
| `Utils.Geometry` | GeoJSON/KML: `convertTo2D`, `extractGeometry`, `getGeomFromKML` |
| `Utils.SignatureAuth` | HTTP signatures: `sign`, `verify`, `generateKeyPair`, `becknSignatureHash` |
| `Utils.Validation` | `runRequestValidation`, `validateField`, `validateObject`, `validateList` |
| `Utils.Error.Throwing` | `throwError`, `fromMaybeM`, `fromEitherM`, `liftEither` |
| `Utils.Error.FlowHandling` | `withFlowHandler[API]`, `apiHandler`, `becknApiHandler` |
| `Utils.Error.Hierarchy` | TH: `instanceExceptionWithParent` |
| `Utils.JSON` | Aeson options, `camelToSnake`, `uniteObjects`, `maskText` |
| `Utils.Text` | `decodeFromText`, `encodeToText`, `padNumber`, `validateAllDigitWithMinLength` |
| `Utils.Dhall` | `readDhallConfig`, `readDhallConfigDefault` |
| `Utils.JWT` | Google JWT: `createJWT`, `doRefreshToken`, `isValid` |
| `Utils.Version` | `readVersion`, `getDeviceFromText` |
| `Utils.UUID` | `generateStaticUUID` (deterministic UUIDv5) |
| `Utils.App` | Wai middleware: `hashBodyForSignature`, `logRequestAndResponse`, `getPodName` |
| `Utils.Shutdown` | `handleShutdown`, `waitForShutdown`, `mkShutdown`, `untilShutdown` |
| `Utils.SlidingWindowLimiter` | `checkSlidingWindowLimit`, `slidingWindowLimiter` |
| `Utils.SlidingWindowCounters` | `incrementWindowCount`, `getCurrentWindowCount`, `getLatestRatio` |
| `Utils.Forkable` | `mapConcurrently`, `mapConcurrentlyTagged` |
| `Utils.Predicates` | Regex patterns: `mobileNumber`, `email`, `name`, country-specific phone validation |
| `Utils.FlowLogging` | EulerHS logging integration |
| `Utils.NonEmpty` | `singleton :: a -> NonEmpty a` |
| `Utils.ExternalAPICallLogging` | `pushExternalApiCallDataToKafka` |
| `Utils.InternalAPICallLogging` | `pushInternalApiCallDataToKafka` |
| `Utils.Context` | `buildTaxiContext` for Beckn protocol |

### Streaming

| Module | Purpose |
|--------|---------|
| `Streaming.Kafka.Producer` | `produceMessage`, `produceMessageInPartition` |
| `Streaming.Kafka.Producer.Types` | `KafkaProducerCfg`, `KafkaProducerTools`, `buildKafkaProducerTools` |
| `Streaming.Kafka.Consumer` | `receiveMessage`, `listenForMessages` |
| `Streaming.Kafka.Consumer.Types` | `KafkaConsumerCfg`, `KafkaConsumerTools`, `buildKafkaConsumerTools` |
| `Streaming.Kafka.Topic.BusinessEvent` | `BusinessEvent`, `buildBusinessEvent` |
| `Streaming.MonadConsumer` | `MonadConsumer` class |
| `Streaming.MonadProducer` | `MonadProducer` class with `Args` type family |

### Internal API & Mock

| Module | Purpose |
|--------|---------|
| `InternalAPI.Auth.API` | Internal auth endpoint: `/internal/auth/{token}` |
| `InternalAPI.Auth.Client` | `auth :: Text -> m PersonId` |
| `Mock.App` | `MockM e a` monad, `run`, `healthCheckServer` |
| `Mock.Exceptions` | `OrderError` |
| `Mock.ExternalAPI` | `callBapAPI`, `prepareAuthManager` |
| `Mock.Utils` | `textToError`, `generateOrderId` |

### Product Validation

| Module | Purpose |
|--------|---------|
| `Product.Validation.Context` | `validateContext`, `validateMetroContext`, `validateDomain`, `validateAction` |

---

## 2. Core Abstractions

### Monad Stack

The primary monad is a ReaderT pattern over EulerHS flows:

```haskell
-- Kernel.Types.Flow
newtype FlowR r a = FlowR (ReaderT r L.Flow a)
  deriving (Functor, Applicative, Monad, MonadReader r,
            MonadThrow, MonadCatch, MonadMask, L.MonadFlow,
            MonadIO, Log, MonadTime, MonadClock, CoreMetrics,
            MonadGuid, Forkable, TryException)

runFlowR :: R.FlowRuntime -> r -> FlowR r a -> IO a
```

### Key Type Classes

```haskell
-- Kernel.Types.Logging
class Log m where
  logOutput :: LogLevel -> Text -> m ()
  withLogTag :: Text -> m a -> m a

-- Kernel.Types.Forkable
class Forkable m where
  fork        :: Text -> m () -> m ()
  awaitableFork :: Text -> m a -> m (Awaitable (Either Text a))
  forkMultiple  :: [(Text, m ())] -> m ()

-- Kernel.Types.MonadGuid
class Monad m => MonadGuid m where
  generateGUIDText :: m Text

-- Kernel.Types.Time
class Monad m => MonadTime m where
  getCurrentTime :: m UTCTime

class Monad m => MonadClock m where
  getClockTime :: m Clock.TimeSpec

-- Kernel.Types.TryException
class TryException m where
  withTryCatch :: Text -> m a -> m (Either SomeException a)

-- Kernel.Tools.Metrics.CoreMetrics.Types
class CoreMetrics m where
  addRequestLatency      :: Text -> Text -> Milliseconds -> Either ClientError a -> m ()
  addDatastoreLatency    :: Text -> Text -> Milliseconds -> m ()
  incrementErrorCounter  :: Text -> SomeException -> m ()
  addUrlCallRetries      :: BaseUrl -> Int -> m ()
  incrementSortedSetCounter    :: Text -> m ()
  incrementStreamCounter       :: Text -> m ()
  addGenericLatency            :: Text -> Milliseconds -> m ()
  incrementSchedulerFailureCounter :: Text -> m ()
  incrementGenericMetrics      :: Text -> m ()
  -- ... 17 methods total
```

### Key Constraint Aliases

```haskell
-- Kernel.Types.Flow
type MonadFlow m = (L.MonadFlow m, Log m, MonadThrow m, MonadCatch m)

-- Kernel.Types.App
type HasFlowEnv m r fields = (MonadFlow m, MonadReader r m, HasFields r fields)

-- Kernel.Storage.Esqueleto.Config
type EsqDBFlow m r = (HasEsqEnv m r, MonadFlow m)

-- Kernel.External.Encryption
type EncFlow m r = HasFlowEnv m r '["encTools" ::: EncTools, "passettoContext" ::: PassettoContext]

-- Kernel.Types.CacheFlow
type CacheFlow m r = (HasCacheConfig r, HedisFlow m r, HasCacConfig r, HasInMemEnv r)

-- Kernel.Storage.Hedis.Config
type HedisFlow m env = (MonadTime m, MonadClock m, CoreMetrics m, MonadCatch m,
                        MonadReader env m, HedisFlowEnv env, MonadIO m, Log m, TryException m)
```

### Phantom-Typed Identifiers

```haskell
-- Kernel.Types.Id
newtype Id domain = Id { getId :: Text }
  deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, ToHttpApiData, ...)

newtype ShortId domain = ShortId { getShortId :: Text }
  deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, ...)

cast :: Id a -> Id b  -- unsafe domain cast
```

---

## 3. Storage Abstractions

### Beam ORM + KV Redis

The primary storage layer uses Beam for PostgreSQL with an integrated KV Redis caching layer.

#### Type Conversion Classes (`Kernel.Beam.Functions`)

```haskell
class FromTType' t a | t -> a where
  fromTType' :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => t -> m (Maybe a)

class ToTType' t a | a -> t where
  toTType' :: a -> t

-- Scheduler variants (lighter constraints)
class FromTType'' t a | a -> t where
  fromTType'' :: (MonadThrow m, Log m, L.MonadFlow m) => t -> m (Maybe a)

class ToTType'' t a | a -> t where
  toTType'' :: a -> t
```

#### Table Constraint (`Kernel.Beam.Functions`)

```haskell
type BeamTable table =
  ( Model Postgres table, MeshMeta Postgres table,
    KVConnector (table Identity), FromJSON (table Identity),
    ToJSON (table Identity), TableMappings (table Identity),
    Serialize.Serialize (table Identity), Show (table Identity) )

type BeamTableFlow table m = (HasCallStack, BeamTable table, MonadFlow m)
```

#### Find Operations (`Kernel.Beam.Functions`)

```haskell
findOneWithKV     :: (BeamTableFlow table m, CacheFlow m r, EsqDBFlow m r, FromTType' (table Identity) a)
                  => Where Postgres table -> m (Maybe a)
findOneWithDb     :: (BeamTableFlow table m, CacheFlow m r, EsqDBFlow m r, FromTType' (table Identity) a)
                  => Where Postgres table -> m (Maybe a)
findAllWithKV     :: (BeamTableFlow table m, CacheFlow m r, EsqDBFlow m r, FromTType' (table Identity) a)
                  => Where Postgres table -> m [a]
findAllWithDb     :: (BeamTableFlow table m, CacheFlow m r, EsqDBFlow m r, FromTType' (table Identity) a)
                  => Where Postgres table -> m [a]
findAllWithOptionsKV :: (BeamTableFlow table m, CacheFlow m r, EsqDBFlow m r, FromTType' (table Identity) a)
                     => Where Postgres table -> OrderBy table -> Maybe Int -> Maybe Int -> m [a]
findAllWithKVAndConditionalDB :: (BeamTableFlow table m, CacheFlow m r, EsqDBFlow m r, FromTType' (table Identity) a)
                              => Where Postgres table -> Maybe (OrderBy table) -> m [a]
findAllFromKvRedis :: (BeamTableFlow table m, CacheFlow m r, EsqDBFlow m r, FromTType' (table Identity) a)
                   => Where Postgres table -> Maybe (OrderBy table) -> m [a]

-- Scheduler variants
findOneWithKVScheduler  :: ... => Where Postgres table -> m (Maybe a)
findAllWithKVScheduler  :: ... => Where Postgres table -> m [a]
findAllWithOptionsKVScheduler :: ... => Where Postgres table -> OrderBy table -> Maybe Int -> Maybe Int -> m [a]

-- Redis-only
findOneWithKVRedis :: ... => Where Postgres table -> m (Maybe a)
```

#### Create Operations (`Kernel.Beam.Functions`)

```haskell
createWithKV :: (BeamTableFlow table m, EsqDBFlow m r, ToTType' (table Identity) a)
             => a -> m ()
createWithKVWithOptions :: (BeamTableFlow table m, EsqDBFlow m r, ToTType' (table Identity) a)
                        => Maybe Integer -> Bool -> a -> m ()
createWithKVScheduler :: (BeamTableFlow table m, EsqDBFlow m r, ToTType'' (table Identity) a)
                      => a -> m ()
```

#### Update Operations (`Kernel.Beam.Functions`)

```haskell
updateWithKV     :: (BeamTableFlow table m, EsqDBFlow m r)
                 => [Set Postgres table] -> Where Postgres table -> m ()
updateOneWithKV  :: (BeamTableFlow table m, EsqDBFlow m r)
                 => [Set Postgres table] -> Where Postgres table -> m ()
updateWithKVWithOptions :: (BeamTableFlow table m, EsqDBFlow m r)
                        => Maybe Integer -> Bool -> [Set Postgres table] -> Where Postgres table -> m ()
-- Scheduler variants available for all update operations
```

#### Delete Operations (`Kernel.Beam.Functions`)

```haskell
deleteWithKV :: (BeamTableFlow table m, EsqDBFlow m r) => Where Postgres table -> m ()
deleteWithDb :: (BeamTableFlow table m, EsqDBFlow m r) => Where Postgres table -> m ()
```

#### Execution Context (`Kernel.Beam.Functions`)

```haskell
runInReplica      :: (L.MonadFlow m, Log m) => m a -> m a
runInMasterDb     :: (L.MonadFlow m, Log m) => m a -> m a
runInMasterRedis  :: (L.MonadFlow m, Log m) => m a -> m a
runInMasterDbAndRedis :: (L.MonadFlow m, Log m) => m a -> m a
runInMultiCloud   :: (L.MonadFlow m, Log m) => m a -> m a

getMasterBeamConfig   :: (HasCallStack, L.MonadFlow m) => m (SqlConn Pg)
getReplicaBeamConfig  :: (HasCallStack, L.MonadFlow m) => m (SqlConn Pg)
getLocationDbBeamConfig :: (HasCallStack, L.MonadFlow m) => m (SqlConn Pg)
```

#### Template Haskell (`Kernel.Beam.Lib.UtilsTH`)

```haskell
enableKVPG :: Name -> [Name] -> [[Name]] -> Q [Dec]
mkTableInstances :: Name -> String -> String -> Q [Dec]
mkTableInstancesWithTModifier :: Name -> String -> String -> [(String, String)] -> Q [Dec]
mkTableInstancesGenericSchema :: Name -> String -> Q [Dec]
mkBeamInstancesForEnum :: Name -> Q [Dec]
mkBeamInstancesForList :: Name -> Q [Dec]
mkBeamInstancesForJSON :: Name -> Q [Dec]
```

### Esqueleto ORM (`Kernel.Storage.Esqueleto/`)

Alternative SQL query builder using `persistent`/`esqueleto`.

```haskell
-- Config
data EsqDBConfig = EsqDBConfig
  { connectHost, connectUser, connectPassword, connectDatabase, connectSchemaName :: Text,
    connectPort :: Word16, connectionPoolCount :: Int }

-- Type conversion
class FromTType t a | a -> t where
  fromTType :: (MonadThrow m, Log m) => t -> m a

class ToTType t a | a -> t where
  toTType :: a -> t

-- Queries (Kernel.Storage.Esqueleto.Queries)
findOne   :: (...) => Esq.SqlQuery b -> m (Maybe a)
findAll   :: (...) => Esq.SqlQuery b -> m [a]
findById  :: (...) => DomainKey t -> m (Maybe a)
create    :: (PersistEntity t, ..., ToTType t a) => a -> SqlDB ()
createMany :: (PersistEntity t, ..., ToTType t a) => [a] -> SqlDB ()
update    :: (PersistEntity a, ...) => (SqlExpr (Entity a) -> SqlQuery ()) -> SqlDB ()
delete    :: Esq.SqlQuery () -> SqlDB ()
repsert   :: (..., ToTType t a, TEntityKey t) => DomainKey t -> a -> SqlDB ()
upsert    :: (..., ToTType t a) => a -> [SqlExpr (Entity t) -> SqlExpr Update] -> SqlDB ()

-- Transaction management
class Transactionable' m1 m where
  runTransaction :: m1 a -> m a
runInReplica :: (EsqDBReplicaFlow m r) => SelectSqlDB a -> m a

-- Geographic functions (Kernel.Storage.Esqueleto.Functions)
(<->.)           :: SqlExpr (Value Point) -> SqlExpr (Value Point) -> SqlExpr (Value Double)
getPoint         :: (SqlExpr (Value Double), SqlExpr (Value Double)) -> SqlExpr (Value Point)
buildRadiusWithin :: SqlExpr (Value Point) -> (Double, Double) -> SqlExpr (Value Int) -> SqlExpr (Value b)
containsPoint    :: (Double, Double) -> SqlExpr (Value b)
containsRegion   :: (Double, Double) -> (Double, Double) -> SqlExpr (Value b)
interval         :: [IntervalVal] -> SqlExpr (Value UTCTime)
rand             :: SqlExpr OrderBy
```

### Redis (`Kernel.Storage.Hedis/`)

```haskell
-- Config
data HedisCfg = HedisCfg
  { connectHost :: HostName, connectPort :: Word16, connectAuth :: Maybe Text,
    connectDatabase :: Integer, connectMaxConnections :: Int,
    connectMaxIdleTime :: NominalDiffTime, connectTimeout :: Maybe NominalDiffTime,
    connectReadOnly :: Bool }

data HedisEnv = HedisEnv { hedisConnection :: Connection, keyModifier :: KeyModifierFunc }

-- Connection
connectHedis        :: HedisCfg -> KeyModifierFunc -> IO HedisEnv
connectHedisCluster :: HedisCfg -> KeyModifierFunc -> IO HedisEnv
disconnectHedis     :: HedisEnv -> IO ()

-- Execution
runHedis            :: (HedisFlow m env) => Redis (Either Reply a) -> m a
runHedisTransaction :: (HedisFlow m env) => RedisTx (Queued a) -> m a

-- Queries (Kernel.Storage.Hedis.Queries)
get  :: (FromJSON a, HedisFlow m env) => Text -> m (Maybe a)
-- set, del, expire, etc. -- standard Redis operations

-- Environment modifiers
withMasterRedis         :: (HedisFlow m env) => m f -> m f
withCrossAppRedis       :: (HedisFlow m env) => m f -> m f
withNonCriticalRedis    :: (HedisFlow m env) => m f -> m f

-- Multi-cloud
runInMultiCloudRedisMaybeResult :: (HedisFlow m env) => m (Maybe a) -> m (Maybe a)
runInMultiCloudRedisWrite       :: (HedisFlow m env) => m a -> m a
runInMultiCloudRedisForList     :: (HedisFlow m env) => m [a] -> m [a]
runInMasterCloudRedisCell       :: (HedisFlow m env) => m f -> m f
```

### In-Memory Cache (`Kernel.Storage.InMem`)

```haskell
withInMemCache :: (Show b, MonadFlow m, MonadReader r m, HasInMemEnv r, Typeable b, CoreMetrics m)
               => [Text] -> Seconds -> m b -> m b
-- LRU cache with TTL. SHA256 keys for entries > 200 chars.

inMemCleanupThread :: Maybe HedisEnv -> InMemEnv -> IO ()
-- Background eviction: expired entries + LRU when > 75% capacity.
```

### Clickhouse V2 (`Kernel.Storage.ClickhouseV2/`)

Type-safe analytics queries with GADT columns.

```haskell
class ClickhouseTable (t :: (Type -> Type) -> Type) where
  tableModification :: FieldModifications t
  getSelectModifier :: Proxy t -> SelectModifier  -- SELECT_FINAL_MODIFIER for ReplacingMergeTree

-- Operators
(==.), (!=.), (>.), (<.), (>=.), (<=.) :: Column a table value -> value -> Clause table
(&&.), (||.) :: Clause table -> Clause table -> Clause table
in_  :: Column a table value -> [value] -> Clause table
like_ :: Column a table Text -> Text -> Clause table
isNull, isNotNull :: Column a table (Maybe value) -> Clause table

-- Column GADT constructors: Sum, Avg, Count, Max, Distinct, Add, ToDate, ToHour,
--   TimeDiff, ToStartOfWeek, ToStartOfMonth, If, Case, ArgMax, Concat, IfNull, ...

-- Queries
findAll      :: (HasClickhouseEnv db m, ...) => Select ... -> m [ColumnsType a cols]
findAllEither :: (HasClickhouseEnv db m, ...) => Select ... -> m (Either Text [ColumnsType a cols])
runRawQuery  :: (HasClickhouseEnv db m, FromJSON a) => Proxy db -> RawQuery -> m (Either String a)
```

### Connection Management (`Kernel.Beam.Connection/`)

```haskell
-- Per-service connection configs
data ConnectionConfigDriver  = ConnectionConfigDriver  { esqDBCfg, esqDBReplicaCfg :: EsqDBConfig, hedisClusterCfg, hedisSecondaryClusterCfg :: HedisCfg }
data ConnectionConfigRider   = ConnectionConfigRider   { ... }
data ConnectionConfigDashboard = ConnectionConfigDashboard { ... }

prepareConnectionDriver   :: L.MonadFlow m => ConnectionConfigDriver -> Int -> m ()
prepareConnectionRider    :: L.MonadFlow m => ConnectionConfigRider -> Int -> m ()
prepareConnectionDashboard :: L.MonadFlow m => ConnectionConfigDashboard -> Int -> m ()
```

---

## 4. External Service Abstractions

All external services follow the **Provider Pattern**: an `Interface/` with abstract types, dispatching to concrete provider implementations.

### SMS (`Kernel.External.SMS/`)

**Providers:** GupShup, Exotel, Karix, MyValueFirst, Pinbix, Twilio, Vonage, DigoEngage

```haskell
-- Handler pattern
data SmsHandler m = SmsHandler { getProvidersPriorityList :: m [SmsService], getProviderConfig :: SmsService -> m SmsServiceConfig }

data SmsServiceConfig = ExotelSmsConfig ExotelSmsCfg | MyValueFirstConfig MyValueFirstCfg | GupShupConfig GupShupCfg | ...

data SendSMSReq = SendSMSReq { smsBody, phoneNumber, sender, templateId :: Text, messageType :: MessageType }
data SendSMSRes = Success | Fail | Pending | UnknownError

-- Interface
sendSMS :: (...) => SmsHandler m -> SendSMSReq -> m SendSMSRes
-- Circular fallback across provider priority list
```

### Payment (`Kernel.External.Payment/`)

**Providers:** Juspay, Stripe, PaytmEDC

```haskell
createOrder        :: (...) => PaymentServiceConfig -> CreateOrderReq -> m CreateOrderResp
orderStatus        :: (...) => PaymentServiceConfig -> OrderStatusReq -> m OrderStatusResp
offerList          :: (...) => PaymentServiceConfig -> OfferListReq -> m OfferListResp
mandateRevoke      :: (...) => PaymentServiceConfig -> MandateRevokeReq -> m MandateRevokeResp
mandateExecution   :: (...) => PaymentServiceConfig -> MandateExecutionReq -> m MandateExecutionResp
createPaymentIntent :: (...) => PaymentServiceConfig -> CreatePaymentIntentReq -> m CreatePaymentIntentResp
deleteCard         :: (...) => PaymentServiceConfig -> DeleteCardReq -> m DeleteCardResp
```

### Maps (`Kernel.External.Maps/`)

**Providers:** Google, OSRM, MMI (MapMyIndia), NextBillion

```haskell
getDistance  :: (...) => MapsServiceConfig -> GetDistanceReq -> m GetDistanceResp
getDistances :: (...) => MapsServiceConfig -> GetDistancesReq -> m GetDistancesResp
getRoutes    :: (...) => MapsServiceConfig -> GetRoutesReq -> m GetRoutesResp
autoComplete :: (...) => MapsServiceConfig -> AutoCompleteReq -> m AutoCompleteResp
getPlaceDetails :: (...) => MapsServiceConfig -> GetPlaceDetailsReq -> m GetPlaceDetailsResp
getPlaceName    :: (...) => MapsServiceConfig -> GetPlaceNameReq -> m GetPlaceNameResp

-- Snap-to-road with fallback
snapToRoadWithFallback :: (...) => SnapToRoadHandler m -> SnapToRoadReq -> m SnapToRoadResp
-- Complex fallback with pre/post-checks and rectification for distant points
```

### Notification (`Kernel.External.Notification/`)

**Providers:** FCM, PayTM, GRPC

```haskell
data Category = TRIP_STARTED | PAYMENT_SUCCESS | SOS_TRIGGERED | ... -- 140+ categories

notifyPerson :: (...) => NotificationServiceConfig -> NotificationReq -> m ()
notifyPersonWithAllProviders :: (...) => [NotificationServiceConfig] -> NotificationReq -> m ()
-- Forks to all providers in parallel
```

### Verification (`Kernel.External.Verification/`)

**Providers:** Idfy, GovtData, HyperVerge, DigiLocker, Tten, SafetyPortal

```haskell
verifyDLAsync  :: (...) => VerificationServiceConfig -> VerifyDLAsyncReq -> m VerifyDLAsyncResp
verifyRC       :: (...) => VerificationServiceConfig -> VerifyRCReq -> m VerifyRCResp
extractRCImage :: (...) => VerificationServiceConfig -> ExtractRCImageReq -> m ExtractRCImageResp
extractDLImage :: (...) => VerificationServiceConfig -> ExtractDLImageReq -> m ExtractDLImageResp
validateFaceImage :: (...) => VerificationServiceConfig -> FaceValidationReq -> m FaceValidationRes
getTask        :: (...) => VerificationServiceConfig -> GetTaskReq -> m GetTaskResp
```

### Other Services (abbreviated)

| Service | Key Functions |
|---------|---------------|
| **Call** | `initiateCall`, `addCampaignData` |
| **Whatsapp** | `whatsAppOptApi`, `whatsAppOtpApi`, `whatsAppSendMessageWithTemplateIdAPI` |
| **Payout** | `createPayoutOrder`, `payoutOrderStatus` |
| **Wallet** | `createWallet`, `walletPosting`, `walletReversal`, `walletBalance` |
| **Settlement** | `parsePaymentSettlementCsv`, `parsePayoutSettlementCsv` |
| **Insurance** | `createInsurance`, `registerHomeDeclaration` |
| **SOS** | `sendInitialSOS`, `sendSOSTrace`, `updateSOSStatus`, `uploadMedia` |
| **IncidentReport** | `reportIncident`, `reportIncidentUpdate` |
| **Ticket** | `createTicket`, `updateTicket` |
| **BackgroundVerification** | `createCandidate`, `createInvitation`, `getReport` |
| **Tokenize** | `tokenize`, `onboard`, `login` |
| **MultiModal** | `getTransitRoutes` |
| **AadhaarVerification** | `generateAadhaarOtp`, `verifyAadhaarOtp` |

### Encryption (`Kernel.External.Encryption`)

```haskell
data EncTools = EncTools { hashSalt :: HashSalt, service :: (String, Word16) }

type EncKind = AsEncrypted | AsUnencrypted
type family EncryptedField (e :: EncKind) a  -- collapses to a or Encrypted a
type family EncryptedHashedField (e :: EncKind) a  -- collapses to a or EncryptedHashed a

class DbHashable a where
  evalDbHash :: (a, HashSalt) -> DbHash

encrypt   :: (EncFlow m r, EncryptedItem' e) => UnencryptedItem e -> m e
decrypt   :: (EncFlow m r, EncryptedItem' e) => e -> m (UnencryptedItem e)
getDbHash :: (EncFlow m r, DbHashable a) => a -> m DbHash
```

---

## 5. Authentication & Authorization

### HTTP Signature Auth (`Kernel.Utils.SignatureAuth`)

Implements the Beckn HTTP Signatures specification.

```haskell
data SignatureAlgorithm = Hs2019 | Ed25519
data KeyId = KeyId { subscriberId, uniqueKeyId :: Text, alg :: SignatureAlgorithm }
data SignatureParams = SignatureParams { keyId :: KeyId, algorithm :: SignatureAlgorithm,
                                        headers :: [Text], created, expires :: Maybe POSIXTime }
data SignaturePayload = SignaturePayload { signature :: Signature, params :: SignatureParams }

sign   :: PrivateKey -> SignatureParams -> Hash -> [Header] -> Maybe Signature
verify :: PublicKey -> SignatureParams -> Hash -> [Header] -> Signature -> Either CryptoError Bool

generateKeyPair    :: IO (PrivateKey, PublicKey)
becknSignatureHash :: ByteString -> Hash  -- Blake2b_512
encode :: SignaturePayload -> ByteString
decode :: ByteString -> Either String SignaturePayload
mkSignatureRealm :: Text -> Text -> Header
```

### Internal Auth (`Kernel.InternalAPI.Auth`)

```haskell
-- API definition
type API = "internal" :> "auth" :> Capture "token" Token :> Get '[JSON] PersonId

-- Client
auth :: (HasField "authServiceUrl" r BaseUrl, CoreMetrics m, MonadFlow m, MonadReader r m)
     => Text -> m PersonId
```

### Auth Error Types (`Kernel.Types.Error`)

```haskell
data AuthError
  = Unauthorized | InvalidAuthData | TokenExpired | BusinessEmailTokenExpired
  | TokenIsNotVerified | TokenNotFound Text | InvalidToken Text
  | AuthBlocked Text | IncorrectOTP | AccessDenied | HitsLimitError Int
```

### Mock Auth (`Kernel.Mock.ExternalAPI`)

```haskell
prepareAuthManager :: AuthenticatingEntity cfg
                   => cfg -> [Text] -> Text -> Text -> (LogLevel -> Text -> IO ()) -> Http.ManagerSettings
-- Creates TLS manager with automatic request signing for outbound API calls
```

---

## 6. Error Handling

### Exception Hierarchy

All errors use a three-tier hierarchy built with Template Haskell:

```haskell
-- Base type classes (Kernel.Types.Error.BaseError.HTTPError)
class IsBaseError e where
  toMessage :: e -> Maybe Text

class IsBaseError e => IsHTTPError e where
  toErrorCode :: e -> Text
  toHttpCode  :: e -> HttpCode

class IsHTTPError e => IsAPIError e

-- TH macro to register in hierarchy
instanceExceptionWithParent 'HTTPException ''MyError
```

### Error Types (50+ defined in `Kernel.Types.Error`)

| Error Type | Key Constructors | HTTP Code |
|------------|------------------|-----------|
| `GenericError` | `InternalError Text`, `InvalidRequest Text` | 500, 400 |
| `AuthError` | `Unauthorized`, `TokenExpired`, `InvalidToken Text`, `HitsLimitError Int` | 401, 429 |
| `PersonError` | `PersonNotFound Text`, `PersonDoesNotExist Text`, `PersonFieldNotPresent Text` | 500 |
| `MerchantError` | `MerchantNotFound Text`, `MerchantServiceConfigNotFound Text Text Text` | 500 |
| `BookingError` | `BookingNotFound Text`, `BookingInvalidStatus Text` | 500, 400 |
| `RideError` | `RideNotFound Text`, `RideInvalidStatus Text`, `DriverNotAtPickupLocation Text` | 500, 400 |
| `DatabaseError` | `SQLRequestError Text Text`, `SQLResultError Text`, `DBUnknownError Text` | 500 |
| `ExternalAPICallError` | `{ errCode, baseUrl, clientError }` | 500 |
| `RedisError` | Wraps `KVDBReply` | 500 |
| `KafkaError` | `KafkaUnableToProduce Text`, `KafkaUnableToConsume Text` | 500 |
| `SMSError` | `SMSError SubmitSmsRes`, `SMSInvalidNumber` | 500, 400 |
| `PaymentOrderError` | `PaymentOrderNotFound Text`, `PaymentOrderDoesNotExist Text` | 500 |
| `ServiceabilityError` | `RideNotServiceable`, `RideNotServiceableInState Text` | 400 |
| `SearchRequestError` | `SearchRequestNotFound Text`, `SearchRequestExpired` | 500, 400 |
| `QuoteError` | `QuoteNotFound Text`, `QuoteExpired Text` | 500, 400 |
| `VehicleError` | `VehicleNotFound Text`, `VehicleAlreadyLinked` | 500, 400 |
| `HeaderError` | `MissingHeader HeaderName`, `InvalidHeader HeaderName Text` | 401 |
| `SignatureError` | `SignatureVerificationFailure [Header]`, `CannotDecodeSignature String` | 401 |
| `HealthCheckError` | `ServiceUnavailable` | 503 |
| `HedisError` | `HedisReplyError String`, `HedisDecodeError Text`, `HedisTransactionAborted` | 500 |

Plus provider-specific errors: `GupShupError` (9), `KarixSmsError` (37), `ExotelError` (9), `OzonetelError` (9), `TwillioError` (8), `IdfyCallError` (9), `MMIError` (7), `OsrmError` (3), `ClickToCallError` (9), `DigoEngageError` (10), `VonageSmsError` (14), `TataCommunicationsWhatsappError` (12), `KarixWhatsappError` (12).

### Throwing Utilities (`Kernel.Utils.Error.Throwing`)

```haskell
throwError  :: (HasCallStack, MonadThrow m, Log m, IsBaseException e) => e -> m b
fromMaybeM  :: (HasCallStack, MonadThrow m, Log m, IsBaseException e) => e -> Maybe b -> m b
fromEitherM :: (HasCallStack, MonadThrow m, Log m, IsBaseException e) => (left -> e) -> Either left b -> m b
liftEither  :: (HasCallStack, MonadThrow m, Log m, IsBaseException e) => Either e b -> m b
```

### Flow Handlers (`Kernel.Utils.Error.FlowHandling`)

```haskell
withFlowHandlerAPI     :: (...) => FlowR r a -> FlowHandlerR r a  -- API errors as JSON
withFlowHandlerBecknAPI :: (...) => FlowR r AckResponse -> FlowHandlerR r AckResponse  -- Beckn errors
apiHandler      :: (...) => m a -> m a  -- catches exceptions, logs, increments error metrics
becknApiHandler :: (...) => m a -> m a  -- catches exceptions, returns BecknAPIError
```

---

## 7. Configuration

### Dhall-Based Config (`Kernel.Utils.Dhall`)

```haskell
readDhallConfig :: FromDhall b => FilePath -> IO b
readDhallConfigDefault :: FromDhall b => String -> IO b  -- uses DHALL_CONFIG_PATH env
```

### Key Config Types

```haskell
-- Database (Kernel.Storage.Esqueleto.Config)
data EsqDBConfig = EsqDBConfig
  { connectHost, connectUser, connectPassword, connectDatabase, connectSchemaName :: Text,
    connectPort :: Word16, connectionPoolCount :: Int }

-- Redis (Kernel.Storage.Hedis.Config)
data HedisCfg = HedisCfg
  { connectHost :: HostName, connectPort :: Word16, connectAuth :: Maybe Text,
    connectDatabase :: Integer, connectMaxConnections :: Int,
    connectMaxIdleTime :: NominalDiffTime, connectTimeout :: Maybe NominalDiffTime,
    connectReadOnly :: Bool }

-- Clickhouse (Kernel.Storage.Clickhouse.Config)
data ClickhouseCfg = ClickhouseCfg
  { username, host, password, database :: Text, port :: Word16,
    tls :: Bool, retryInterval :: Vector Int }

-- Logging (Kernel.Types.Logging)
data LoggerConfig = LoggerConfig
  { level :: LogLevel, logToFile :: Bool, logFilePath :: FilePath,
    logToConsole :: Bool, logRawSql :: Bool, prettyPrinting :: Bool }

-- Cache (Kernel.Types.CacheFlow)
data CacheConfig = CacheConfig { configsExpTime :: Seconds }
data CacConfig = CacConfig { host :: Text, interval :: Int, tenant :: Text,
                             retryConnection :: Bool, cacExpTime :: Seconds,
                             enablePolling :: Bool, enableCac :: Bool }
data InMemConfig = InMemConfig { enableInMem :: Bool, maxInMemSize :: Bytes }

-- KV Tables (Kernel.Types.Common)
data Tables = Tables
  { disableForKV :: [Text], kvTablesTtl :: [(Text, Integer)],
    useCAC :: [Text], useCACForFrontend :: [Text],
    readFromMasterDb :: [Text], defaultShardMod :: (Int, Int),
    tableShardModRange :: Maybe [(Text, (Int, Int))],
    tableRedisKeyPrefix :: Maybe [(Text, Text)],
    allTablesDisabled :: Maybe Bool,
    enableSecondaryCloudRead :: Maybe Bool,
    tablesForSecondaryCloudRead :: Maybe [Text] }

-- Encryption (Kernel.External.Encryption)
data EncTools = EncTools { hashSalt :: HashSalt, service :: (String, Word16) }

-- Slack (Kernel.External.Slack.Types)
data SlackConfig = SlackConfig { slackToken :: Text, channelName :: Text }

-- SMS Session (Kernel.Sms.Config)
data SmsSessionConfig = SmsSessionConfig { attempts, authExpiry, tokenExpiry :: Int }
data SmsConfig = SmsConfig { sessionConfig :: SmsSessionConfig, credConfig :: SmsCredConfig,
                             useFakeSms :: Maybe Word16, url :: BaseUrl, sender :: Text }

-- Kafka (Kernel.Streaming.Kafka.Producer.Types)
data KafkaProducerCfg = KafkaProducerCfg { brokers :: KafkaBrokersList, kafkaCompression :: KafkaCompression }

-- API Rate Limiting (Kernel.Types.SlidingWindowLimiter)
data APIRateLimitOptions = APIRateLimitOptions { limit :: Int, limitResetTimeInSec :: Int }
```

### Environment Variables (`Kernel.Beam.Connection.EnvVars`)

```haskell
-- Connection names
postgresConnectionName, postgresR1ConnectionName :: IO Text
postgresLocationDBConnectionName, postgresLocationDBReplicaConnectionName :: IO Text

-- Pool configuration
getPostgresPoolStripes :: IO Int
getPostgresPoolIdleTime :: IO Integer
getPostgresPoolMax :: IO Int

-- Feature flags
getPreparePosgreSqlConnection, getPreparePosgreSqlR1Connection :: IO Bool
getPrepareRedisClusterConnection, getPrepareSecondaryRedisClusterConnection :: IO Bool
getRunInMasterCloudRedisCell, getRunInMasterLTSRedisCell :: IO Bool
```

### Runtime Config (`Kernel.Tools.SystemEnv`)

```haskell
updateSystemEnv :: (CacheFlow m r, EsqDBFlow m r) => m ()
updateSystemEnvInLoopFork :: (CacheFlow m r, EsqDBFlow m r) => Integer -> m ()
-- Loads environment variables from database `system_configs` table at runtime
```

---

## 8. Utility Functions

### Logging (`Kernel.Utils.Logging`)

```haskell
logDebug, logInfo, logWarning, logError :: Log m => Text -> m ()
logTagDebug, logTagInfo, logTagWarning, logTagError :: Log m => Text -> Text -> m ()
withPersonIdLogTag :: Log m => Id b -> m a -> m a
withTransactionIdLogTag :: (...) => b -> m a -> m a
logPretty :: (PrettyShow a, Show a, HasPrettyLogger m env) => LogLevel -> Text -> a -> m ()
makeLogSomeException :: SomeException -> Text
```

### Time (`Kernel.Utils.Time`)

```haskell
isExpired :: MonadTime m => NominalDiffTime -> UTCTime -> m Bool
getLocalCurrentTime :: MonadTime m => Seconds -> m UTCTime
showTimeIst :: UTCTime -> Text
measureDuration :: MonadClock m => m a -> m (a, Milliseconds)
measuringDurationToLog :: Log m => LogLevel -> Text -> MeasuringDuration m a
secondsToNominalDiffTime :: Seconds -> NominalDiffTime
nominalDiffTimeToSeconds :: NominalDiffTime -> Seconds
secondsToMinutes :: Seconds -> Minutes
millisToSecondsDouble :: Milliseconds -> Double
threadDelaySec :: MonadIO m => Seconds -> m ()
threadDelayMilliSec :: MonadIO m => Milliseconds -> m ()
utcToMilliseconds :: UTCTime -> Double
millisecondsToUTC :: Integer -> UTCTime
utcToEpochSeconds :: UTCTime -> Seconds
isTimeWithinBounds :: TimeOfDay -> TimeOfDay -> TimeOfDay -> Bool
```

### Distance & Geometry (`Kernel.Utils.CalculateDistance`, `Kernel.Utils.ComputeIntersection`)

```haskell
distanceBetweenInMeters :: LatLong -> LatLong -> HighPrecMeters  -- Haversine
getRouteLinearLength :: [LatLong] -> Maybe LatLong -> HighPrecMeters
everySnippetIs :: (HighPrecMeters -> Bool) -> [LatLong] -> Bool

checkIntersection :: RoutePoints -> LineSegment -> Bool
doRouteIntersectWithLine :: RoutePoints -> LineSegment -> Bool
getBoundingBox :: RoutePoints -> BoundingBox
pointWithinBoundingBox :: LatLong -> BoundingBox -> Bool
```

### Metrics (`Kernel.Tools.Metrics/`)

```haskell
-- Registration
registerCoreMetricsContainer :: IO CoreMetricsContainer
serve :: Int -> IO ()  -- Start Prometheus HTTP server

-- Instrumentation
addServantInfo :: SanitizedUrl a => DeploymentVersion -> Proxy a -> Application -> Application
startGenericLatencyMetrics :: (...) => LatencyAction -> Text -> m ()
finishGenericLatencyMetrics :: (...) => LatencyAction -> Text -> m ()
```

### Validation (`Kernel.Utils.Validation`)

```haskell
runRequestValidation :: (MonadThrow m, Log m) => Validate obj -> obj -> m ()
validateField :: (Predicate a p, ShowablePredicate p) => Text -> a -> p -> Validation
validateObject :: Text -> a -> Validate a -> Validation
validateList :: Container a => Text -> a -> Validate (Element a) -> Validation
```

### Rate Limiting (`Kernel.Utils.SlidingWindowLimiter`)

```haskell
checkSlidingWindowLimit :: (HedisFlow m r, HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions])
                        => Text -> m ()
checkSlidingWindowLimitWithOptions :: (HedisFlow m r, MonadTime m) => Text -> APIRateLimitOptions -> m ()
slidingWindowLimiter :: (HedisFlow m r, MonadTime m) => Text -> Int -> Int -> m Bool
```

### Sliding Window Counters (`Kernel.Utils.SlidingWindowCounters`)

```haskell
incrementWindowCount :: (L.MonadFlow m, HedisFlow m r) => Text -> SlidingWindowOptions -> m ()
decrementWindowCount :: (L.MonadFlow m, HedisFlow m r) => Text -> SlidingWindowOptions -> m ()
getCurrentWindowCount :: (L.MonadFlow m, HedisFlow m r) => Text -> SlidingWindowOptions -> m Integer
getLatestRatio :: (L.MonadFlow m, HedisFlow m r) => Text -> (Text -> Text) -> (Text -> Text) -> SlidingWindowOptions -> m Double
incrementByValueInTimeBucket :: (L.MonadFlow m, HedisFlow m r) => UTCTime -> Integer -> Text -> SlidingWindowOptions -> m ()
```

### Shutdown (`Kernel.Utils.Shutdown`)

```haskell
type Shutdown = TMVar ()
mkShutdown :: IO Shutdown
handleShutdown :: Shutdown -> IO () -> IO () -> IO ()
waitForShutdown :: Shutdown -> IO ()
untilShutdown :: (MonadIO m, MonadReader r m, HasField "isShuttingDown" r Shutdown) => m () -> m ()
```

### JSON (`Kernel.Utils.JSON`)

```haskell
camelToSnake :: String -> String
replaceUnderscores :: Text -> Text
uniteObjects :: [Value] -> Value  -- merge multiple JSON objects
maskText :: Text -> Text
truncateText :: Text -> Text

-- Aeson Options presets
constructorsWithHyphens, constructorsToLowerOptions, constructorsWithSnakeCase :: Options
stripPrefixUnderscoreIfAny, removeNullFields, untaggedValue :: Options
```

### JWT (`Kernel.Utils.JWT`)

```haskell
createJWT :: ServiceAccount -> [(Text, Value)] -> IO (Either String (JWTClaimsSet, Text))
doRefreshToken :: ServiceAccount -> IO (Either String JWToken)
isValid :: JWToken -> IO JWTValidity  -- JWTValid seconds | JWTInvalid | JWTExpired seconds
```

### Kafka Logging

```haskell
-- Kernel.Utils.ExternalAPICallLogging
pushExternalApiCallDataToKafka :: (...) => Text -> Text -> Maybe Text -> Maybe req' -> Either err res' -> m ()

-- Kernel.Utils.InternalAPICallLogging
pushInternalApiCallDataToKafka :: (...) => Text -> Text -> Maybe Text -> Maybe req' -> res' -> m ()
```

### Wai Middleware (`Kernel.Utils.App`)

```haskell
hashBodyForSignature :: Application -> Application  -- adds body hash header
supportProxyAuthorization :: Application -> Application
logRequestAndResponse :: HasLog f => EnvR f -> Application -> Application
withModifiedEnv :: HasLog f => (EnvR f -> Application) -> EnvR f -> Application
getPodName :: IO (Maybe Text)
lookupDeploymentVersion :: IO DeploymentVersion
lookupCloudType :: IO CloudType
```

---

## 9. Re-exports

### `Kernel.Prelude` re-exports from:

| Package | What |
|---------|------|
| `Prelude` | Everything except `error`, `id`, `log`, `print`, `putStr`, `putStrLn`, `show`, `undefined` |
| `Universum` | `Debug`, `Print`, `String.Conversion` (except `readMaybe`) |
| `safe-exceptions` | All exception handling (via `Control.Exception.Safe`) |
| `aeson` | `FromJSON`, `ToJSON`, `genericParseJSON`, `genericToJSON` |
| `text` | `Text` |
| `time` | `LocalTime`, `TimeOfDay`, `NominalDiffTime`, `UTCTime` |
| `openapi3` | `ToParamSchema`, `ToSchema` |
| `servant-client` | `BaseUrl` |
| `safe` | All safe functions |
| `base` | `Data.Kind.Type`, `Data.Proxy.Proxy`, `Data.String.IsString`, `GHC.Generics.Generic`, `GHC.Int.Int64`, `GHC.Stack.HasCallStack` |

### Custom functions defined in Prelude:

```haskell
identity :: p -> p
everyPossibleVariant :: (Enum a, Bounded a) => [a]
whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenM   :: Monad m => m Bool -> m () -> m ()
unlessM :: Monad m => m Bool -> m () -> m ()
whileM  :: Monad m => m Bool -> m () -> m ()
showBaseUrl :: BaseUrl -> Text
parseBaseUrl :: MonadThrow m => Text -> m BaseUrl
rightToMaybe :: Either e a -> Maybe a
intToNominalDiffTime :: Int -> NominalDiffTime
roundToIntegral :: (RealFrac a, Integral b) => a -> b
hoistMaybe :: Applicative m => Maybe b -> MaybeT m b
safeInit :: [a] -> [a]
(|<|>|) :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
```

### `Kernel.Types.Common` re-exports:

`EncFlow`, `EsqDBFlow`, all types from `Types.App`, `Types.Centesimal`, `Types.Distance`, `Types.Forkable`, `Types.FromField`, `Types.GuidLike`, `Types.Logging`, `Types.MonadGuid`, `Types.Price`, `Types.SharedRedisKeys`, `Types.Time`, `Types.TryException`.

---

## 10. Design Patterns

### 1. ReaderT + Constraint Pattern

Services never take explicit config arguments. Instead, config is in the Reader environment and accessed via `HasField` constraints:

```haskell
myFunction :: (HasFlowEnv m r '["encTools" ::: EncTools], CacheFlow m r, EsqDBFlow m r) => Text -> m Result
```

### 2. Provider Pattern (External Services)

Every external service follows:
```
External/[ServiceType]/
  Interface.hs            -- Public functions, dispatching by config type
  Interface/Types.hs      -- Sum type config, request/response types
  [Provider]/Config.hs    -- Provider config (FromDhall)
  [Provider]/Types.hs     -- Provider-specific types
  [Provider]/Client.hs    -- HTTP client calls
```

Services use handlers with `getProvidersPriorityList` for fallback:
```haskell
data SmsHandler m = SmsHandler
  { getProvidersPriorityList :: m [SmsService]
  , getProviderConfig :: SmsService -> m SmsServiceConfig }
```

### 3. Beam + KV Redis Dual-Write

All database operations go through `createWithKV`/`findOneWithKV`/`updateWithKV` which write to both PostgreSQL and Redis. The `Tables` config controls which tables use KV caching. `runInReplica` routes reads to the replica DB.

### 4. Type-Level Field Lists

Environment requirements are expressed as type-level lists:
```haskell
type HasFlowEnv m r fields = (MonadFlow m, MonadReader r m, HasFields r fields)
-- Usage:
HasFlowEnv m r '["encTools" ::: EncTools, "coreVersion" ::: Text]
```

### 5. NoImplicitPrelude

Every module imports `Kernel.Prelude` (or `EulerHS.Prelude`) instead of the standard Prelude. This gives `Universum` + `safe-exceptions` + common re-exports.

### 6. From/ToTType Conversion

Domain types and database types are always separate. `FromTType'`/`ToTType'` convert between them:
```haskell
instance FromTType' BookingT Booking where
  fromTType' bookingT = do
    merchant <- findOneWithKV [Se.Is MerchantT.id (Se.Eq bookingT.merchantId)]
    pure $ Just Booking { id = Id bookingT.id, merchant = merchant, ... }
```

### 7. TH Code Generation

Tables are wired up via Template Haskell:
```haskell
$(enableKVPG ''BookingT ['bookingTMod] [['MeshEnabled]])
$(mkTableInstances ''BookingT "booking" "atlas_app")
$(mkBeamInstancesForEnum ''BookingStatus)
```

### 8. Error Hierarchy with TH

Errors register in an exception hierarchy via TH:
```haskell
data MyError = NotFound Text | InvalidState Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''MyError
instance IsBaseError MyError where toMessage = ...
instance IsHTTPError MyError where toErrorCode = ...; toHttpCode = ...
instance IsAPIError MyError
```

### 9. Multi-Cloud Redis

For disaster recovery, the library supports primary + secondary Redis clusters (e.g., AWS + GCP). `runInMultiCloudRedisMaybeResult` reads from primary, falls back to secondary. `runInMultiCloudRedisWrite` writes to both.

### 10. Circular Provider Fallback

SMS/WhatsApp services use circular fallback: `lastDigit(phoneNumber) % providerCount` determines the starting provider, then rotates through all providers on failure.
