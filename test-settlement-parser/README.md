# test-settlement-parser

A local CLI tool for testing CSV settlement report parsers from the `mobility-core` Settlement module. It reads a CSV file, runs it through the appropriate parser, and writes a human-readable `.txt` output.

> **Note:** This tool is for local development/testing only and is not intended to be pushed to the repository.

## Prerequisites

- **GHC 9.4.x** (tested with 9.4.8)
- **cabal** (cabal-install 3.x)

## Project structure

```
test-settlement-parser/
├── README.md
├── cabal.project
├── test-settlement-parser.cabal
├── src/
│   └── Main.hs                  # CLI entry point
├── shims/                        # Lightweight stubs for mobility-core internals
│   └── Kernel/
│       ├── Prelude.hs
│       ├── Prelude/OrphanInstances.hs
│       └── Types/Common.hs
├── samples/                      # Sample CSVs organised by payment gateway
│   ├── hyperpg/
│   │   ├── metropol_de6c3402_YESAP60685556165_20260309.csv   (payment)
│   │   └── cumta_tab_performance_table_...csv                 (payout)
│   └── billdesk/
│       └── billdesk_settlement_report.csv                     (payment)
└── output/                       # Parsed output (auto-created per PG)
    ├── hyperpg/
    └── billdesk/
```

The tool compiles the **real** parser modules from `shared-kernel/lib/mobility-core/src/Kernel/External/Settlement/` and uses minimal shims to satisfy internal imports (`Kernel.Prelude`, `Kernel.Types.Common`).

## Build

```bash
cd shared-kernel/test-settlement-parser
cabal build
```

## Usage

```
cabal run test-settlement-parser -- <pg> <report-type> <csv-file>
```

| Argument       | Description                                                         |
|----------------|---------------------------------------------------------------------|
| `<pg>`         | Payment gateway name: `hyperpg` or `billdesk`                      |
| `<report-type>`| Report type: `payment` or `payout`                                  |
| `<csv-file>`   | CSV filename (looked up in `samples/<pg>/`) or a relative/absolute path |

### List available samples for a PG

```bash
cabal run test-settlement-parser -- hyperpg list
cabal run test-settlement-parser -- billdesk list
```

### Parse a payment settlement report

```bash
# HyperPG payment report
cabal run test-settlement-parser -- hyperpg payment metropol_de6c3402_YESAP60685556165_20260309.csv

# BillDesk payment report (multi-section: settled, refund, chargeback)
cabal run test-settlement-parser -- billdesk payment billdesk_settlement_report.csv
```

### Parse a payout settlement report

```bash
# HyperPG payout report
cabal run test-settlement-parser -- hyperpg payout cumta_tab_performance_table_cumta_hari_currentTime_2026-03-09T00_00_00.000Z_2026-03-09T12_55_43.000Z_1773041152540.csv
```

### Use a custom CSV path

If the CSV file is not inside `samples/<pg>/`, pass a relative or absolute path:

```bash
cabal run test-settlement-parser -- hyperpg payment /path/to/custom_report.csv
```

## Output

Parsed results are written to:

```
output/<pg>/<csv-filename>.parsed.txt
```

The tool also prints a preview of the first 80 lines to the terminal.

### Sample output

```
========================================
 Payment Settlement Parse Result
========================================
Total rows:    3
Parsed OK:     3
Failed rows:   0

No parsing errors.

--- Report #1 ---
  orderId:                  cumta-g5Icw4RG6A-1
  txnId:                    hypqqG8bxgsYTMqCH7X
  txnType:                  ORDER
  txnStatus:                SUCCESS
  ...
```

## Adding a new PG

1. Create the parser modules under `shared-kernel/lib/mobility-core/src/Kernel/External/Settlement/<NewPG>/`.
2. Add the PG to the `SettlementService` enum in `Types.hs` and wire it into `Interface.hs`.
3. Add the new modules to both `mobility-core.cabal` and `test-settlement-parser.cabal` (`other-modules`).
4. Create a `samples/<newpg>/` directory and drop in sample CSVs.
5. Add a case to `parsePG` and `pgDirName` in `src/Main.hs`.
6. Build and run:
   ```bash
   cabal run test-settlement-parser -- newpg payment sample_report.csv
   ```

## Supported payment gateways

| PG        | Payment report | Payout report |
|-----------|:--------------:|:-------------:|
| HyperPG   | Yes            | Yes           |
| BillDesk  | Yes            | --            |
