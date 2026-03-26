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
├── samples/                      # Downloaded/sample CSVs organised by PG
│   ├── hyperpg/                  #   (email command saves CSVs here)
│   │   ├── metropol_de6c3402_YESAP60685556165_20260309.csv   (payment)
│   │   └── cumta_tab_performance_table_...csv                 (payout)
│   └── billdesk/
│       └── billdesk_settlement_report_sample.csv              (payment)
└── output/                       # Parsed output & debug files per PG
    ├── hyperpg/
    │   └── *.csv.parsed.txt
    └── billdesk/
        ├── *.csv.parsed.txt
        └── debug-raw-mime.txt    # Raw MIME dump from email fetch
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

## Email IMAP fetch (end-to-end validation)

The tool can fetch a CSV attachment directly from an email inbox via IMAP, save it to `samples/<pg>/`, parse it, and write the result to `output/<pg>/` — all in one command. It bypasses the encryption layer so you can test with plain-text credentials.

### Prerequisites

- **curl** installed (ships with macOS)
- A Gmail account with **2-Step Verification** enabled
- A **Google App Password** (generate at https://myaccount.google.com/apppasswords)

### Usage

```bash
cabal run test-settlement-parser -- email <pg> <user> <app-password> [subject-filter]
```

| Argument           | Description                                              |
|--------------------|----------------------------------------------------------|
| `<pg>`             | Payment gateway name: `hyperpg` or `billdesk`            |
| `<user>`           | Gmail address                                            |
| `<app-password>`   | Google App Password (16 characters)                      |
| `[subject-filter]` | Optional email subject to match (e.g. `"Settlement Report"`) |

### Examples

```bash
# Fetch the latest "Settlement Report" email for BillDesk
cabal run test-settlement-parser -- email billdesk myuser@gmail.com 'abcdefghijklmnop' "Settlement Report"

# Fetch the latest email for HyperPG (no subject filter)
cabal run test-settlement-parser -- email hyperpg myuser@gmail.com 'abcdefghijklmnop'
```

### Output

The email command produces three files, organised by PG:

| File | Description |
|------|-------------|
| `samples/<pg>/<attachment-name>.csv` | Downloaded CSV attachment (uses the original filename from the email) |
| `output/<pg>/<attachment-name>.csv.parsed.txt` | Parsed settlement report |
| `output/<pg>/debug-raw-mime.txt` | Raw MIME dump (always generated, useful for debugging) |

Example for `billdesk`:

```
samples/billdesk/billdesk_settlement_report_sample.csv       # downloaded CSV
output/billdesk/billdesk_settlement_report_sample.csv.parsed.txt  # parsed output
output/billdesk/debug-raw-mime.txt                            # raw MIME
```

### Switching between ALL / UNSEEN emails

By default the tool searches **all emails** (including already-read) for easier testing.
To switch to production behaviour (unread only), edit `Email.hs` and swap the commented lines:

```haskell
-- In Kernel.External.Settlement.Sources.Email, inside fetchSettlementFileWithPlainPassword:

-- Testing (current): searches ALL emails including already-read
searchQuery = maybe "ALL" (\s -> "SUBJECT \"" <> T.unpack s <> "\"") config.subjectFilter

-- Production: uncomment this, comment the line above
-- searchQuery = maybe "UNSEEN" (\s -> "SUBJECT \"" <> T.unpack s <> "\" UNSEEN") config.subjectFilter
```

### Troubleshooting

| Error | Cause | Fix |
|-------|-------|-----|
| `IMAP SEARCH failed (exit 67)` | Login denied | Use a Google App Password, not your regular password |
| `No unread settlement emails found` | No matching emails (or all already read) | Switch to ALL mode (see above), or send a new test email |
| `CSV saved to ... (0 bytes)` | MIME parser couldn't extract attachment | Check `output/<pg>/debug-raw-mime.txt` for the raw email structure |

## Supported payment gateways

| PG        | Payment report | Payout report |
|-----------|:--------------:|:-------------:|
| HyperPG   | Yes            | Yes           |
| BillDesk  | Yes            | --            |
