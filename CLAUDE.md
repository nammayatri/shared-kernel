# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is `mobility-core`, a shared Haskell library (not a standalone service) for the [Namma Yatri](https://github.com/nammayatri/nammayatri) mobility platform. It provides database abstractions, external service integrations, core types, and utilities consumed by multiple backend services.

Licensed under AGPL. Uses `NoImplicitPrelude` — all modules import `Kernel.Prelude` (or `EulerHS.Prelude`) instead of the standard Prelude.

## Build & Development Commands

```bash
nix develop                  # Enter Nix dev shell (required first time)
direnv allow                 # Alternative: auto-load via direnv
cabal build                  # Build the library
cabal test                   # Run all tests (Tasty + HUnit)
cabal repl all               # Load REPL
nix build                    # Full Nix build
```

Format and lint:
```bash
./dev/format-all-files.sh    # Format with ormolu
hlint .                      # Lint (config: .hlint.yaml)
```

## Architecture

### Monad Stack

The codebase uses a **ReaderT pattern** over EulerHS flows:
- `FlowR r a` = `ReaderT r L.Flow a` — the primary monad for effectful code
- Key constraints: `MonadFlow`, `HasFlowEnv`, `EsqDBFlow`
- Dependency injection via the Reader environment

### Module Structure (`lib/mobility-core/src/Kernel/`)

- **`Prelude.hs`** — Custom prelude re-exporting common types/functions. Uses `Universum` and `safe-exceptions`. Hides standard `show`, `error`, `undefined`, `id`.
- **`Beam/`** — Type-safe PostgreSQL ORM layer using Beam
- **`Storage/`** — Storage backends: Beam ORM, Esqueleto SQL, Hedis (Redis), Clickhouse (V1/V2)
- **`External/`** — 40+ external service provider integrations organized as `External/[ServiceType]/[Provider]/` (e.g., `External/Payment/Juspay/`, `External/SMS/GupShup/`). Each provider has `Types.hs`, `Config.hs`, and API client modules. Abstract interfaces live in `External/[ServiceType]/Interface/`.
- **`Types/`** — Core domain types including a large error hierarchy (`Types/Error.hs`)
- **`Utils/`** — Utility modules (40+)
- **`Tools/`** — Infrastructure concerns: logging, metrics (Prometheus), Slack notifications
- **`InternalAPI/`** — Shared API type definitions (Servant-based)
- **`Streaming/`** — Kafka producer/consumer wrappers

### Key Patterns

- **Provider pattern**: External services define an `Interface/` with abstract types; concrete implementations live in provider-specific subdirectories
- **`RecordDotPreprocessor`**: Enabled as a GHC plugin for dot-syntax record access
- **Heavy type-level programming**: `TypeApplications`, `TypeFamilies`, `GADTs`, `RankNTypes`, `DataKinds` are all default extensions
- **`DuplicateRecordFields`** is enabled — use type annotations or `TypeApplications` to disambiguate

## Package Configuration

- **`package.yaml`** (Hpack) is the source of truth — it generates `mobility-core.cabal`
- Edit `package.yaml`, not the `.cabal` file directly
- GHC flags include `-Wall -Werror` — all warnings are errors

## Testing

Tests are in `lib/mobility-core/test/` using Tasty + HUnit. Test modules: `ComputeIntersectionTests`, `Centesimal`, `DistanceCalculation`, `SignatureAuth`, `SlidingWindowLimiter`, `SnippetsCheck`, `Version`, `APIExceptions`.

## Code Placement

- Database types → `src/Kernel/Beam/Types/`
- External service integrations → `src/Kernel/External/[ServiceType]/[Provider]/`
- Utility functions → `src/Kernel/Utils/`
- Common types → `src/Kernel/Types/`
