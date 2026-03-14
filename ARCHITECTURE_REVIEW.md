# Architecture Review: mobility-core (shared-kernel)

**Date:** 2026-03-14
**Scope:** Full codebase analysis — architecture, technical debt, performance, observability

---

## 1. Architecture Overview

`mobility-core` is a shared Haskell library for the [Namma Yatri](https://github.com/nammayatri/nammayatri) mobility platform. It provides the foundational layer consumed by multiple backend services: database abstractions, 40+ external service integrations, core domain types, streaming infrastructure, and utilities.

### Monad Stack

```
FlowR r a = ReaderT r L.Flow a
```

All effectful code runs in `FlowR`, a `ReaderT` over EulerHS flows. Dependency injection is achieved through the Reader environment with typeclass constraints (`MonadFlow`, `HasFlowEnv`, `EsqDBFlow`, `HasCoreMetrics`).

### Key Design Decisions

| Decision | Rationale |
|----------|-----------|
| `NoImplicitPrelude` + `Kernel.Prelude` | Custom prelude based on Universum; hides unsafe functions (`error`, `undefined`) |
| `RecordDotPreprocessor` | Dot-syntax record access across all modules |
| `-Wall -Werror` | Zero-tolerance for warnings |
| 50+ default extensions | Heavy type-level programming (GADTs, TypeFamilies, DataKinds, RankNTypes) |
| Provider pattern for external services | Pluggable implementations behind abstract interfaces |
| EulerHS as effect system | Pre-existing organizational choice; provides Flow monad, SQL, HTTP, logging |

---

## 2. Component Diagram

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        Consumer Services                                │
│              (Driver, Rider, Dashboard, Gateway, ...)                   │
└─────────────────────────────┬───────────────────────────────────────────┘
                              │ depends on
┌─────────────────────────────▼───────────────────────────────────────────┐
│                        mobility-core                                    │
│                                                                         │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  ┌────────────┐  │
│  │   Prelude    │  │    Types     │  │    Utils     │  │   Tools    │  │
│  │              │  │              │  │              │  │            │  │
│  │ Universum    │  │ Error (52    │  │ Servant      │  │ Metrics    │  │
│  │ re-exports   │  │  error types)│  │ Logging      │  │ (Prometheus│  │
│  │ safe-except  │  │ Beckn        │  │ SlidingWindow│  │  19+ gauges│  │
│  │              │  │ Common       │  │ Validation   │  │  histograms│  │
│  │              │  │ Registry     │  │ GenericUtils  │  │  counters) │  │
│  │              │  │ Logging      │  │ App bootstrap │  │ Slack      │  │
│  └──────────────┘  └──────────────┘  └──────────────┘  └────────────┘  │
│                                                                         │
│  ┌──────────────────────────────────────────────────────────────────┐   │
│  │                     External Services (334 files)                │   │
│  │                                                                  │   │
│  │  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌──────────┐  │   │
│  │  │ Payment │ │   SMS   │ │  Maps   │ │  Call   │ │Notificat.│  │   │
│  │  │ Juspay  │ │ GupShup │ │ Google  │ │ Exotel  │ │  FCM     │  │   │
│  │  │ Stripe  │ │ Karix   │ │ MMI     │ │ Ozonetel│ │  GRPC    │  │   │
│  │  │ PaytmEDC│ │ Vonage  │ │ OSRM    │ │ Twillio │ │  PayTM   │  │   │
│  │  │         │ │ Twillio  │ │ NextBil.│ │ Tata    │ │          │  │   │
│  │  │         │ │ +4 more │ │         │ │         │ │          │  │   │
│  │  └────┬────┘ └────┬────┘ └────┬────┘ └────┬────┘ └────┬─────┘  │   │
│  │       └───────────┴──────┬────┴───────────┴───────────┘         │   │
│  │                    Interface Layer                                │   │
│  │              (ServiceConfig ADT + pattern match dispatch)        │   │
│  │                                                                  │   │
│  │  + Verification (7 providers) | Insurance (2) | Whatsapp (3)     │   │
│  │  + Tokenize (5)  | SOS (2)    | Settlement (3) | Payout (1)     │   │
│  │  + Wallet (1) | Ticket (1) | Plasma (1) | MultiModal (1)        │   │
│  │  + AadhaarVerification (1) | BackgroundVerification (1)          │   │
│  │  + IncidentReport (1) | GoogleTranslate | Infobip | Slack        │   │
│  └──────────────────────────────────────────────────────────────────┘   │
│                                                                         │
│  ┌──────────────────────────────────────────────────────────────────┐   │
│  │                     Storage Layer (47 files)                     │   │
│  │                                                                  │   │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌───────────────┐   │   │
│  │  │  Beam    │  │ Esqueleto│  │  Hedis   │  │  Clickhouse   │   │   │
│  │  │ (Postgres│  │ (Legacy  │  │ (Redis   │  │  (V1 + V2     │   │   │
│  │  │  ORM)    │  │  SQL)    │  │  + InMem)│  │   analytics)  │   │   │
│  │  │          │  │          │  │          │  │               │   │   │
│  │  │ Pool mgmt│  │ Compat   │  │ Queries  │  │ ClickhouseV2  │   │   │
│  │  │ Functions│  │ layer    │  │ Config   │  │ Internal/     │   │   │
│  │  │ TH utils │  │          │  │ AppPrefx │  │ Connection    │   │   │
│  │  └──────────┘  └──────────┘  └──────────┘  └───────────────┘   │   │
│  └──────────────────────────────────────────────────────────────────┘   │
│                                                                         │
│  ┌──────────────────────┐  ┌────────────────────────────────────────┐   │
│  │   Streaming (18)     │  │          InternalAPI (2)               │   │
│  │   Kafka Producer     │  │          Auth / Servant types          │   │
│  │   Kafka Consumer     │  │                                        │   │
│  │   Topic definitions  │  │                                        │   │
│  └──────────────────────┘  └────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────────┘
```

### File Distribution

| Subsystem | Files | % of Total |
|-----------|-------|-----------|
| External  | 334   | 57.5%     |
| Types     | 60    | 10.3%     |
| Utils     | 52    | 8.9%     |
| Storage   | 37    | 6.4%     |
| Streaming | 18    | 3.1%     |
| Tools     | 11    | 1.9%     |
| Beam      | 10    | 1.7%     |
| Other     | 9     | 1.5%     |
| **Total** | **~581** | **100%** |

---

## 3. Core Modules and Responsibilities

### `Kernel.Prelude`
Custom prelude re-exporting Universum + safe-exceptions. Hides `show`, `error`, `undefined`, `id`. 151 LOC, lean and focused — **well-designed**.

### `Kernel.External.*` (334 files, 25 service categories)
Provider-based abstraction for external services. Each service type defines:
- `Interface/Types.hs` — abstract request/response types
- `Interface/ProviderName.hs` — adapter bridging interface types to provider types
- `Interface.hs` — dispatch via pattern match on `ServiceConfig` ADT
- `ProviderName/{Config,Types,Flow,API|Client}.hs` — concrete implementation

### `Kernel.Storage.*` + `Kernel.Beam.*` (47 files)
Four storage backends: Beam (PostgreSQL ORM), Esqueleto (legacy SQL), Hedis (Redis with multi-cluster support), Clickhouse (V1 + V2 analytics). Beam.Functions provides generic CRUD with Kafka change-data-capture.

### `Kernel.Types.*` (60 files)
Core domain types including a 1,828-line error hierarchy (`Error.hs`) with 52 error data types, Beckn protocol types, registry types, and common domain primitives.

### `Kernel.Utils.*` (52 files)
Utilities for Servant integration, logging, sliding window rate limiting, distance calculation, validation, app bootstrapping, and HTTP client management.

### `Kernel.Tools.*` (11 files)
Infrastructure: Prometheus metrics (19+ metrics), Slack alerting, logging utilities.

### `Kernel.Streaming.*` (18 files)
Kafka producer/consumer wrappers and topic definitions for business events and public transport.

---

## 4. Technical Debt

### TD-1: Monolithic Error Hierarchy (Critical)
**File:** `Kernel/Types/Error.hs` — **1,828 lines, 52 error types, 56 exception instances**

This single file is the largest source of technical debt. Each error type requires ~50-100 lines of boilerplate:
```haskell
data XyzError = XyzNotFound Text | XyzDoesNotExist Text | XyzFieldNotPresent Text | ...
instanceExceptionWithParent 'HTTPException ''XyzError
instance IsBaseError XyzError where toMessage = \case ...
instance IsHTTPError XyzError where toErrorCode = \case ... ; toHttpCode = \case ...
instance IsAPIError XyzError
```

**Duplication within Error.hs:**
- 8+ resource types repeat the `NotFound / DoesNotExist / FieldNotPresent` triple
- SMS provider errors (Karix 33 constructors, GupShup 9, Twillio 9, Exotel 9, Ozonetel 9, Vonage 15) are 60-70% mechanically identical
- WhatsApp provider errors repeat the same structure

**Impact:** Adding a new provider requires ~100+ lines of boilerplate. Finding/modifying error codes requires searching an 1,828-line file.

### TD-2: Inconsistent Provider Structure
- **Naming:** `API.hs` vs `Client.hs` used inconsistently across providers
- **Missing files:** 19 providers lack `Config.hs` despite it being the documented pattern
- **Skeleton providers:** `Call/Twillio` has only `Config.hs`, `OSRM` has no `Flow.hs`
- **Non-conforming modules:** `GoogleTranslate`, `Infobip`, `Slack` skip the Interface pattern entirely
- **`SharedLogic/`** contains cross-cutting code for DigiLocker and HyperVerge that doesn't belong in the provider hierarchy

### TD-3: Dual SQL Abstractions
Both `Beam` and `Esqueleto` are maintained as storage backends. Esqueleto appears to be a legacy layer with a compatibility shim. Maintaining two ORMs increases cognitive load and testing surface.

### TD-4: Two Clickhouse Implementations
`ClickhouseV1` and `ClickhouseV2` coexist with separate connection management, query builders, and type systems. V1 should be migrated and removed.

### TD-5: `unsafePerformIO` Usage
`Kernel/Types/Beckn/City.hs` uses `unsafePerformIO` with `MVar` for a city mapping cache. While guarded by `NOINLINE`, this is fragile and should be replaced with proper initialization in the Reader environment.

### TD-6: No Cache Invalidation Strategy for Beam+KV
The Beam ORM integrates with Redis via `findOneWithKV` / `findAllWithKV` (try cache first, fall back to DB). However, there is no documented or enforced cache invalidation mechanism. If the database is updated by another service or direct SQL, cached values become stale with no expiry or invalidation signal.

### TD-7: Fragile String-Based Error Detection in Clickhouse
**File:** `Kernel/Storage/Clickhouse/Queries.hs`

Uses `T.isInfixOf "ConnectionFailure"` to detect connection errors. This is fragile and version-dependent — a library update changing the error message format would silently break retry logic.

### TD-8: PostgreSQL Pool Destruction on Fatal Errors
**File:** `Kernel/Storage/Esqueleto/Transactionable.hs`

`destroyAllConnAndRetryIfExpired` destroys the entire connection pool and recreates it on `PostgresFatalError`. This can cause connection storms under load — all in-flight requests lose their connections simultaneously, then all race to reconnect.

---

## 5. Code Duplication

### D-1: Provider Error Boilerplate (~600 LOC redundant)
Each SMS/WhatsApp/Call provider duplicates:
- Error type definition with near-identical constructors
- `IsBaseError` instance with same message patterns
- `IsHTTPError` instance mapping to same HTTP codes
- `FromResponse` parsing logic

**Estimate:** 600+ lines of avoidable duplication across provider error types.

### D-2: Interface Adapter Pattern
Every `Interface/ProviderName.hs` follows the identical pattern:
1. Import provider `Flow` and `Types`
2. Import interface `Types`
3. Define type converter functions
4. Delegate to provider `Flow`

This is structurally necessary but could be reduced with a typeclass or TH-based approach.

### D-3: HTTP Client Boilerplate
External service calls repeat: decrypt credentials → build request → call API → validate response → convert types. No shared combinator abstracts this pipeline.

### D-4: Redis Query Patterns
`Hedis/Queries.hs` repeats the same encode/decode/error-handling wrapper for every Redis data type (String, List, Set, Hash, SortedSet). Each operation re-implements JSON encoding, error wrapping, and TTL management.

### D-5: Storage Error Handling
Each storage backend (Esqueleto, Hedis, Clickhouse) implements its own error recovery strategy with no shared retry/backoff combinator. PostgreSQL retries immediately on fatal errors, Clickhouse uses time-based retry with MVar locking, and Hedis has no retry logic at all.

---

## 6. Performance Risks

### P-1: O(n²) List Accumulation in Cache Cleanup (High)
**File:** `Kernel/Storage/InMem.hs:91,120`
```haskell
accCacheList ++ [(k, v)]  -- O(n) per append, O(n²) total
```
Uses `++` for list concatenation inside `foldl'`. Should use cons + reverse or `DList`.

### P-2: Non-Pipelined Redis Multi-Key Operations (High)
**File:** `Kernel/Utils/SlidingWindowCounters.hs:401,415`
```haskell
mapM Redis.get keys  -- N sequential Redis roundtrips
```
Each key lookup is a separate Redis call. Should use `mget` or pipelining.

### P-3: Synchronous Kafka Producing (High)
**File:** `Kernel/Streaming/Kafka/Producer.hs:68`

Each message is produced synchronously with no batching. Under high throughput, this becomes a bottleneck. Should use async producing with batch accumulation.

### P-4: Missing TTLs on Redis Lists (Medium)
**File:** `Kernel/Storage/Hedis/Queries.hs:446,450`

`lPush` and `rPush` operations set no expiration. These keys grow unbounded.

### P-5: Conservative PostgreSQL Pool Defaults (Medium)
**File:** `Kernel/Beam/Connection/Postgres.hs:87-92`

Default pool: 1 stripe × 5 connections = 5 total. Under high concurrency, requests queue waiting for connections.

### P-6: Dual-Cluster Redis Always Makes Two Trips (Medium)
**File:** `Kernel/Storage/Hedis/Queries.hs:167-236`

Multi-cloud Redis routing runs queries on both primary and secondary clusters regardless of success on the first.

### P-7: In-Memory Cache Cleanup Delay (Medium)
**File:** `Kernel/Storage/InMem.hs:134`

Cache cleanup runs every 60 seconds. During traffic spikes, the cache can grow well beyond its configured limit before eviction runs.

### P-8: Manual Kafka Offset Commits Per Message (Low)
**File:** `Kernel/Streaming/Kafka/Consumer.hs:39`

Commits offsets after every message. Should batch commits for throughput.

### P-9: Retry Without Jitter (Low)
**File:** `Kernel/Utils/Servant/Client.hs:253-254`

Exponential backoff without jitter. Under correlated failures, all retries fire simultaneously (thundering herd).

### P-10: Lazy `foldl` in Kafka Properties (Low)
**File:** `Kernel/Streaming/Kafka/Producer/Types.hs:60`

Uses `foldl` instead of `foldl'`, building up unevaluated thunks.

---

## 7. Monitoring & Observability Assessment

### Current State

| Capability | Status | Implementation |
|-----------|--------|----------------|
| Metrics | Active | Prometheus, 19+ metrics (latency histograms, error counters, retry counters) |
| Structured Logging | Active | JSON format, EulerHS runtime, context tags via `withLogTag` |
| Health Checks | Active | PostgreSQL `SELECT 1` + Redis `PING` on `/health` |
| HTTP Instrumentation | Active | Path-sanitized latency + status code + version labels |
| Datastore Latency | Active | Per-operation histograms for Postgres and Redis |
| External API Monitoring | Active | Latency, retry counts, error codes per host/service |
| Rate Limiting | Active | Redis-backed sliding window with Prometheus integration |
| Alerting | Active | Slack notifications (color-coded by status) via WAI middleware |
| Distributed Tracing | **Manual** | Log tag propagation only (`[tag1][tag2]`), no OpenTelemetry |
| Circuit Breakers | **Missing** | Retry-based resilience only, no circuit breaker state machine |
| Kafka Consumer Lag | **Missing** | No consumer lag tracking or alerting |
| Cache Hit/Miss Rates | **Missing** | In-memory and Redis caches have no hit/miss metrics |
| Connection Pool Metrics | **Missing** | No visibility into pool utilization or wait times |

### Recommendations

#### O-1: Add OpenTelemetry Distributed Tracing (High Impact)
Replace manual `withLogTag` propagation with OpenTelemetry spans. This enables:
- End-to-end request tracing across services
- Automatic span context propagation through Kafka and HTTP
- Visual request flow in Jaeger/Tempo

#### O-2: Add Circuit Breakers for External Services (High Impact)
Wrap all external provider calls with a circuit breaker (e.g., `retry` library with circuit breaker state). Current retry-only strategy means a down provider causes sustained latency spikes across all requests until timeouts expire.

#### O-3: Add Connection Pool Metrics (Medium Impact)
Expose Beam/PostgreSQL pool utilization:
- Active connections, idle connections, total connections
- Wait time for connection acquisition
- Pool exhaustion events

#### O-4: Add Redis/Cache Hit-Miss Metrics (Medium Impact)
Track cache effectiveness:
- `cache_hit_total` / `cache_miss_total` for in-memory cache
- `redis_cache_hit_total` / `redis_cache_miss_total` for Redis lookups
- Cache eviction counts

#### O-5: Add Kafka Consumer Lag Monitoring (Medium Impact)
Track `consumer_lag` (latest offset - committed offset) per topic/partition. Alert when lag exceeds thresholds.

#### O-6: Add SLI/SLO Dashboards (Low Impact)
Define and track SLIs:
- External provider availability (per provider)
- P99 latency per API endpoint
- Error budget burn rate

---

## 8. Improvements Ranked by Impact

| # | Improvement | Category | Impact | Effort | Risk |
|---|------------|----------|--------|--------|------|
| 1 | **Split `Error.hs` into per-domain modules** | Tech Debt | High — reduces file from 1,828 LOC, makes errors discoverable | Medium | Low |
| 2 | **Fix O(n²) list accumulation in `InMem.hs`** | Performance | High — prevents CPU spikes during cache cleanup | Low | Low |
| 3 | **Pipeline Redis multi-key operations** | Performance | High — reduces SlidingWindow latency by N× | Low | Low |
| 4 | **Add circuit breakers for external services** | Reliability | High — prevents cascading failures from down providers | Medium | Low |
| 5 | **Add OpenTelemetry distributed tracing** | Observability | High — enables cross-service request debugging | High | Medium |
| 6 | **Create generic provider error base types** | Duplication | High — eliminates ~600 LOC of boilerplate, simplifies adding new providers | Medium | Low |
| 7 | **Add TTLs to all Redis list/set operations** | Performance | Medium — prevents unbounded memory growth | Low | Low |
| 8 | **Switch Kafka to async batched producing** | Performance | Medium — improves throughput under load | Medium | Medium |
| 9 | **Increase PostgreSQL pool defaults** | Performance | Medium — prevents connection starvation | Low | Low |
| 10 | **Add connection pool metrics** | Observability | Medium — visibility into database bottlenecks | Low | Low |
| 11 | **Add Redis/cache hit-miss metrics** | Observability | Medium — data-driven cache tuning | Low | Low |
| 12 | **Standardize provider file naming** (`API.hs` vs `Client.hs`) | Tech Debt | Medium — reduces cognitive load for contributors | Low | Low |
| 13 | **Retire Esqueleto / ClickhouseV1** | Tech Debt | Medium — reduces maintenance surface | High | Medium |
| 14 | **Add Kafka consumer lag monitoring** | Observability | Medium — early warning for processing delays | Low | Low |
| 15 | **Extract HTTP client combinator** (decrypt → call → validate → convert) | Duplication | Medium — reduces provider boilerplate | Medium | Low |
| 16 | **Add Beam+KV cache invalidation strategy** | Reliability | Medium — prevents stale reads across services | Medium | Medium |
| 17 | **Replace pool-destroy-on-fatal with graceful reconnect** | Reliability | Medium — prevents connection storms under load | Medium | Low |
| 18 | **Unify storage error recovery** (shared retry/backoff combinator) | Duplication | Low — consistent resilience across backends | Medium | Low |
| 19 | **Add jitter to exponential backoff** | Performance | Low — prevents thundering herd on correlated failures | Low | Low |
| 20 | **Replace `unsafePerformIO` city cache** | Tech Debt | Low — eliminates unsafe code | Low | Low |
| 21 | **Fix string-based Clickhouse error detection** | Tech Debt | Low — prevents silent retry breakage on library updates | Low | Low |
| 22 | **Batch Kafka offset commits** | Performance | Low — marginal throughput improvement | Low | Low |
| 23 | **Use strict `foldl'` in Kafka properties** | Performance | Low — prevents minor space leak | Low | Low |

---

## Appendix: Provider Coverage Matrix

| Service | Providers | Interface? | Consistent Structure? |
|---------|-----------|-----------|----------------------|
| SMS | 8 (GupShup, Karix, Vonage, Twillio, Exotel, MyValueFirst, PinbixSms, DigoEngage) | Yes | Yes |
| Payment | 3 (Juspay, Stripe, PaytmEDC) | Yes | Yes |
| Maps | 4 (Google, MMI, OSRM, NextBillion) | Yes | Yes |
| Call | 4 (Exotel, Ozonetel, TataClickToCall, Twillio) | Yes | Partial (Twillio skeleton) |
| Notification | 3 (FCM, GRPC, PayTM) | Yes | Missing Config.hs |
| Whatsapp | 3 (GupShup, Karix, TataCommunications) | Yes | Yes |
| Verification | 7 (Idfy, Digilocker, HyperVerge, GovtData, InternalScripts, SafetyPortal, Tten) | Yes | Missing Config.hs in most |
| Tokenize | 5 (Digilocker, Gullak, HyperVerge, JourneyMonitoring, Tten) | Yes | Missing Config.hs in all |
| Insurance | 2 (Acko, IffcoTokio) | Yes | Missing Config.hs |
| SOS | 2 (ERSS, GJ112) | Yes | Yes |
| Settlement | 3 (BillDesk, HyperPG, Sources) | Yes | Sources is non-standard |
| Payout | 1 (Juspay) | Yes | Yes |
| Wallet | 1 (Juspay) | Yes | Yes |
| Ticket | 1 (Kapture) | Yes | Yes |
| Plasma | 1 (LMS) | Yes | Yes |
| MultiModal | 1 (OpenTripPlanner) | Yes | Auto-generated |
| AadhaarVerification | 1 (Gridline) | Yes | Yes |
| BackgroundVerification | 1 (Checkr) | Yes | Yes |
| IncidentReport | 1 (ERSS) | Yes | Yes |
| GoogleTranslate | - (standalone) | **No** | N/A |
| Infobip | - (standalone) | **No** | N/A |
| Slack | - (standalone) | **No** | N/A |
