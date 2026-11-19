# Replication package: Who Joins Which Club? Football Agents and Player Recruitment in European Men’s Football

Code, data, result tables and file manifests for the paper of the same name (submitted to the *International Review for the Sociology of
Sport*). The paper asks whether a player’s destination club is predicted by an existing representative–club tie, in two studies.
Study 1 uses pre-window rosters of 14 European men’s top flights for the summers of 2023–2025 (2,000 transfers) and asks whether the
mover’s representative already has another client at the destination club. Study 2 uses the English FA’s published intermediary
transaction reports for 2015–16 to 2025–26 and asks whether the intermediary on a signing had dealt with the club before.

The estimation, the tables and the figures are written in R (the extraction of the FA reports from PDF is in Python and optional). One command
re-estimates every model of both studies from the data in this repository and compares the results with the stored tables.

## Quick start

```sh
Rscript run_all.R                  # about 12 minutes on four cores (about 30 with N_JOBS=1); writes results_reproduced/
```

Requirements: R 4.x (reference environment: R 4.6.0) with the packages `data.table`, `survival`, `readr`, `stringi`, `jsonlite`,
`ggplot2`, `xml2` and `zip`. The Word and PDF files additionally need `pandoc` and LibreOffice, and the paper sources
(`manuscript.md`, `online_appendix.md`), which are not part of this repository: `Rscript run_all.R <output_dir> <paper_dir>`.
Without them `run_all.R` writes `paper_tables.md` with every table of the paper (Tables 1–4) and the Online Appendix (A1–A2, B1–B16).
Every number in these tables is read from a result file written by the analysis scripts; none is typed in. Package versions used for the
stored results are recorded in `results_reproduced/session_info.txt` after a run.

`run_all.R` ends by comparing each re-estimate (Study 1 and Study 2) with `stored_results/`. Events and alternatives are identical; odds ratios
agree to within 1e-5 (relative), and the script stops if any differs by more than 0.2%. The environment variable `N_JOBS` sets how many scripts
run at the same time.

## Layout

| Path | Contents |
|---|---|
| `run_all.R` | Runs everything in order and checks the results against `stored_results/` |
| `R/analysis/` | One script per block of analyses (table below) |
| `R/report/` | `make_tables.R` (table rows and the two figures) and `render_manuscript.R` (all tables as markdown; Word and PDF if the paper sources are present) |
| `study2_fa_reports/` | Study 2: the FA transaction table, club divisions, the two R estimation scripts, the optional Python extraction and audit scripts; see its `README.md` |
| `R/lib/` | Shared code: `setup.R`, `prepare.R` (event and candidate-club frames, prior flows), `enrich.R` (fees, returns, contract status), `models.R` (conditional logit with clustered errors), `docx_style.R` |
| `R/data_build/` | Rebuilds every model input from the archived raw files (`rebuild_data.R` and numbered builders) and creates the pseudonymised data |
| `data_pseudonymised/` | Model-ready inputs with random player and representative codes (not under version control, see below) |
| `data_sources/` | Source lists, checksums and the manifest of archived Transfermarkt releases; see `data_sources/README.md` |
| `stored_results/` | The result tables and figures the paper reports (see `stored_results/README.md`) |
| `CHECKSUMS.sha256` | SHA-256 of every file in the repository |

## Analyses

| Script | Content | Main outputs |
|---|---|---|
| `01_main_models.R` | Club and league conditional logits, specification variants (Tables 2 and B15, Figure A1) | `model_results.csv`, `sample_summary.json` |
| `02_timing_fees_contract.R` | Fee type, returns to a former club, 30 June and 1 July, contract status (Tables B6, B14) | `timing_fees_results.csv`, `contract_status_results.csv` |
| `03_matched_agencies.R` | Own representative versus matched agencies, recent versus long-standing co-clients, matching diagnostics (Table 3, Online Appendix A) | `matched_agencies_*.csv` |
| `04_choice_sets_specialisation.R` | Tier and affordability controls, restricted alternatives, Elo decomposition, client-tier control (Table B16) | `choice_set_results.csv`, `elo_decomposition_results.csv`, `specialisation_results.csv` |
| `05_league.R` | League model with nationality-by-league effects and agency concentration (Tables B15, B16) | `league_results.csv` |
| `06_sample_variants.R` | First-version, player-level, carry-forward and look-ahead samples; harmonised labels; observed versus uniform share of connected destinations | `sample_variants_results.csv`, `baseline_share.csv` |
| `07_season_comparison.R` | Summer versus January destination sorting (Online Appendix) | `season_comparison_results.csv` |
| `08_move_type.R` | Loan-like moves: no fee and a return to the origin club within 14 months (Online Appendix B9) | `move_type_results.csv` |
| `09_matched_agencies_value.R` | Matched agencies when the profile includes market value, and with the mover left out (Table 3, Online Appendix B8) | `matched_agencies_value_results.csv` |
| `10_label_audit.R` | Baseline-only labels, summers separately, label coverage (Online Appendix B9) | `label_audit_results.csv` |
| `11_club_resources.R` | Association by club squad value within league and summer (Online Appendix B9) | `club_resources_results.csv` |
| `12_co_client_background.R` | Association through co-clients who share or differ from the mover in nationality, earlier club, position, age (Online Appendix B10) | `co_client_background_results.csv`, `co_client_background_counts.csv` |
| `13_agency_concentration_leagues.R` | Leave-out-largest-agencies and by-league estimates (Online Appendix B11, B12) | `agency_concentration*.csv`, `league_heterogeneity.csv`, `league_results_by_league.csv` |
| `study2_fa_reports/fa_choice_model.R` | Study 2 choice-set models: prior dealing between the intermediary and the destination club, six runs (Table 4, Online Appendix B13) | `fa_choice_results.csv`, `fa_choice_informative_events.csv` |
| `study2_fa_reports/fa_audit_model.R` | Study 2 audit and extensions: link audit, choice-set variants, one observation per signing, club-by-period effects, renewal model with three exposures (Table 4, B13) | `fa_audit_results.csv`, `fa_audit_counts.csv` |

Each Study 1 script is run as `Rscript R/analysis/<script> <data_root> [output_dir]` and the Study 2 scripts as `Rscript study2_fa_reports/<script> [output_dir]`; each can be used on its own.

**Terminology.** The paper calls the comparison agencies *matched agencies*. In the code and result files they are *twins*
(`twin_any`, `twin1`, model names such as `twin agencies (placebo)`). The labels `R1`, `R2`, `R4`, … and `V …` in model names only
identify blocks of analyses and have no other meaning.

## Data

Player identifiers and representative labels in `data_pseudonymised/` are random codes, player names are removed, and the mapping was
not kept. Two outputs depend on the sort order of the labels only (the anonymised worked example in the appendix picks its focal
agency among equals), so they can differ in which agency is shown but not in any estimate.

`data_pseudonymised/` is generated by `R/data_build/make_pseudonymised_data.R` from intermediate files that are not distributed. It is
kept out of version control until the data providers’ terms have been confirmed. Reviewers can be given it on request.

The analysis uses the following sources.

**Study 2.** The English FA’s published intermediary transaction reports, one PDF per reporting period (public documents, 2015–16 to 2025–26).
The harmonised table extracted from them, `study2_fa_reports/fa_transactions_long.csv`, is part of this repository, and the addresses and
checksums of the PDFs are in `study2_fa_reports/urls.tsv` and `source_manifest.tsv`. The table names the clubs, players and intermediaries
as the FA published them. The division of each club in each period (`club_division.csv`) uses Transfermarkt-derived season tables for the
EFL and National League and public league tables for the Premier League.

**Study 1.** The following files are not redistributed here.

- Historical versions of `players.csv` from *Football Data from Transfermarkt* (Cariboo, Kaggle), compiled from Transfermarkt.
  `data_sources/raw/kaggle/download_manifest.csv` records the version, the archive file’s Last-Modified timestamp, the URL and the
  checksum. The timestamp dates the archived file, not individual profile updates or representation agreements.
- The transfer, games and appearances tables of the `transfermarkt-datasets` repository (Cariboo, GitHub).
- Club Elo ratings through the `Club-Football-Match-Data` compilation (xgabora, GitHub), from ClubElo.
- EA Sports player ratings for FIFA 23 and FC 24 from public GitHub extracts, and the sofifa data set on Kaggle.

The Cariboo compilation carries a CC0 licence, but its content derives from Transfermarkt, whose terms of use apply to the underlying
data (the same holds for the division tables used in Study 2). Check the providers’ terms before reusing any of these files. The FA reports are
public documents; the FA’s own terms apply to reuse of the PDFs.

## Re-running Study 2 from the PDFs (optional)

`run_all.R` starts from `fa_transactions_long.csv`. To rebuild that table from the reports, download the PDFs listed in
`study2_fa_reports/urls.tsv` to `study2_fa_reports/raw/<period>/fa.bin`, compare their SHA-256 with `source_manifest.tsv` and run
`python3 -I parse_fa.py` in that folder (needs `pdftotext`). `study2_fa_reports/README.md` describes every step and file.

## Rebuilding the inputs from the raw files

```sh
Rscript R/data_build/rebuild_data.R <root> [output_dir]
```

rebuilds every model input (event panel, performance and candidate tables, winter rosters, transfers and backcast, extended sample)
from the archived raw files, with `<root>` laid out like `data_sources/` plus the downloaded raw files (`data_sources/README.md`).
Compared with the shipped tables, the values are identical. The files differ only in number formatting (15 rather than 17 significant digits,
integers without a trailing `.0`, quoting of empty text fields), and the two fitted-model files differ by 1e-8 or less (optimiser tolerance).
A run takes under a minute.

## Licence and citation

The code and result tables are released under the MIT licence (`LICENSE`). Third-party data are not covered.

[Add the full citation and DOI of the paper and of the archived release after acceptance.]
