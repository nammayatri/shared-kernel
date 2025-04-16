# Shared Kernel Library

This is the core shared library component of the [Namma Yatri](https://github.com/nammayatri/nammayatri) project. It provides essential functionality, types, and interfaces that are used across various services in the Namma Yatri ecosystem.

## Core Purpose

The shared kernel library serves as the foundation for the Namma Yatri platform, providing:

1. **Database Layer** (`src/Kernel/Beam/`)
   - PostgreSQL connection management
   - Redis integration
   - Beam ORM utilities and types
   - Common database functions and queries

2. **External Service Integrations** (`src/Kernel/External/`)
   - Payment processing
   - Communication services (SMS, WhatsApp, Call)
   - Maps and location services
   - Verification services (Aadhaar, Background checks)
   - Multi-modal transportation integration
   - Incident reporting
   - Notification systems

3. **Core Utilities** (`src/Kernel/Utils/`)
   - Common data types and interfaces
   - Error handling and logging
   - Configuration management
   - Encryption utilities
   - Serviceability checks

4. **Internal APIs** (`src/Kernel/InternalAPI/`)
   - Shared API types and interfaces
   - Common API utilities

5. **Storage Layer** (`src/Kernel/Storage/`)
   - Clickhouse integration (both V1 and V2)
   - Redis integration via Hedis
   - SQL query building with Esqueleto
   - Beam ORM integration
   - Common storage queries and utilities

## Project Structure

```
src/
└── Kernel/
    ├── Beam/           # Database operations and ORM
    ├── External/       # External service integrations
    │   ├── Payment/    # Payment gateway integrations
    │   ├── SMS/        # SMS service integrations
    │   ├── Maps/       # Maps and location services
    │   └── Verification/# Verification services
    ├── InternalAPI/    # Internal API definitions
    ├── Storage/        # Storage utilities
    ├── Types/          # Common type definitions
    ├── Utils/          # Utility functions
    └── Prelude.hs      # Custom prelude with common imports
```

## Key Features

### Database Operations
- Type-safe database queries using Beam ORM
- Connection pooling and management
- Redis caching utilities
- Common database functions and utilities

### External Services
- Payment gateway integrations
- Communication services (SMS, WhatsApp, Voice calls)
- Maps and location services
- Verification services (Aadhaar, Background checks)
- Multi-modal transportation integration
- Incident reporting system
- Notification delivery system

### Core Utilities
- Common data types and interfaces
- Error handling and logging
- Configuration management
- Encryption utilities
- Serviceability checks
- Randomization utilities

## Development Setup

### Prerequisites

- [Nix](https://nixos.org/download.html) (version 2.4 or later)
- [Haskell](https://www.haskell.org/downloads/) (managed via Nix)
- [direnv](https://direnv.net/) (optional)
- PostgreSQL (for local development)
- Redis (for local development)

### Getting Started

1. Clone the Namma Yatri repository:
   ```bash
   git clone https://github.com/nammayatri/nammayatri.git
   cd nammayatri/Backend/shared-kernel
   ```

2. Set up the development environment:
   ```bash
   nix develop
   ```

3. Configure environment variables:
   ```bash
   direnv allow
   ```

4. Build the library:
   ```bash
   cabal build
   ```

5. Run tests:
   ```bash
   cabal test
   ```

## Development Guidelines

### Code Organization

- Place new database types in `src/Kernel/Beam/Types/`
- Add external service integrations in `src/Kernel/External/`
- Put utility functions in `src/Kernel/Utils/`
- Define common types in `src/Kernel/Types/`

### Common Tasks

1. Adding new database tables:
   ```bash
   # Add table definition in src/Kernel/Beam/Types/
   # Run code generation
   cabal run mobility-core -- generate-db-types
   ```

2. Adding new external service integration:
   ```bash
   # Create new module in src/Kernel/External/
   # Add configuration in src/Kernel/External/Config/
   # Add types in src/Kernel/External/Types/
   ```

### Code Quality

The project uses:
- ormolu for code formatting
- hlint for linting
- Pre-commit hooks for automated checks

## Contributing

Please read the [Namma Yatri Contributing Guidelines](https://github.com/nammayatri/nammayatri/blob/main/CONTRIBUTING.md) before submitting changes.

Key points:
- Follow the existing code style
- Add tests for new functionality
- Update documentation
- Ensure all tests pass
- Update CHANGELOG.md for significant changes

## License

This project is licensed under the AGPL License - see the [LICENSE.md](LICENSE.md) file for details.

## Support

For support:
1. Check the [Namma Yatri documentation](https://github.com/nammayatri/nammayatri/tree/main/Backend#getting-started)
2. Search existing issues
3. Create a new issue if needed

## Related Components

- [Backend Services](https://github.com/nammayatri/nammayatri/tree/main/Backend)
