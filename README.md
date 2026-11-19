# Replication package: Who Joins Which Club? Football Agents and Player Recruitment in European Men’s Football

Code, result tables and file manifests for the paper of the same name (submitted to the *International Review for the Sociology of
Sport*). The paper asks whether a player’s destination club is predicted by an existing client of the player’s representative at that
club. It uses pre-window rosters of 14 European men’s top flights for the summers of 2023–2025 and the January windows in between.

Everything is written in R. One command re-estimates every model in the paper from the pseudonymised data and compares the results with
the stored tables.

## Quick start

```sh
Rscript run_all.R                  # about 25 minutes; writes results_reproduced/
```

Requirements: R 4.x (reference environment: R 4.6.0) with the packages `data.table`, `survival`, `readr`, `stringi`, `jsonlite`,
`ggplot2`, `xml2` and `zip`. The Word and PDF files additionally need `pandoc` and LibreOffice, and the paper sources
(`manuscript.md`, `online_appendix.md`), which are not part of this repository: `Rscript run_all.R <output_dir> <paper_dir>`.
Without them `run_all.R` writes `paper_tables.md` with every table of the paper and the appendix. Package versions used for the
stored results are recorded in `results_reproduced/session_info.txt` after a run.

`run_all.R` ends by comparing each re-estimate with `stored_results/`. Events and alternatives are identical; odds ratios agree to
within 1e-5 (relative), and the script stops if any differs by more than 0.2%.

## Layout

| Path | Contents |
|---|---|
| `run_all.R` | Runs everything in order and checks the results against `stored_results/` |
| `R/analysis/` | One script per block of analyses (table below) |
| `R/report/` | `make_tables.R` (table rows and the two figures) and `render_manuscript.R` (all tables as markdown; Word and PDF if the paper sources are present) |
| `R/lib/` | Shared code: `setup.R`, `prepare.R` (event and candidate-club frames, prior flows), `enrich.R` (fees, returns, contract status), `models.R` (conditional logit with clustered errors), `docx_style.R` |
| `R/data_build/` | Rebuilds every model input from the archived raw files (`rebuild_data.R` and numbered builders) and creates the pseudonymised data |
| `data_pseudonymised/` | Model-ready inputs with random player and representative codes (not under version control, see below) |
| `data_sources/` | Source lists, checksums and the manifest of archived Transfermarkt releases; see `data_sources/README.md` |
| `stored_results/` | The result tables and figures the paper reports (see `stored_results/README.md`) |
| `CHECKSUMS.sha256` | SHA-256 of every file in the repository |

## Analyses

| Script | Content | Main outputs |
|---|---|---|
| `01_main_models.R` | Club and league conditional logits, specification variants (Tables 2 and 4, Figure A1) | `model_results.csv`, `sample_summary.json` |
| `02_timing_fees_contract.R` | Fee type, returns to a former club, 30 June and 1 July, contract status (Table 5) | `timing_fees_results.csv`, `contract_status_results.csv` |
| `03_matched_agencies.R` | Own representative versus matched agencies, recent versus long-standing co-clients, matching diagnostics (Table 3, Online Appendix A) | `matched_agencies_*.csv` |
| `04_choice_sets_specialisation.R` | Tier and affordability controls, restricted alternatives, Elo decomposition, client-tier control (Table 6) | `choice_set_results.csv`, `elo_decomposition_results.csv`, `specialisation_results.csv` |
| `05_league.R` | League model with nationality-by-league effects and agency concentration (Tables 4 and 6) | `league_results.csv` |
| `06_sample_variants.R` | First-version, player-level, carry-forward and look-ahead samples; harmonised labels; observed versus uniform share of connected destinations | `sample_variants_results.csv`, `baseline_share.csv` |
| `07_season_comparison.R` | Summer versus January destination sorting (Online Appendix) | `season_comparison_results.csv` |
| `08_move_type.R` | Loan-like moves: no fee and a return to the origin club within 14 months (Online Appendix B9) | `move_type_results.csv` |
| `09_matched_agencies_value.R` | Matched agencies when the profile includes market value, and with the mover left out (Table 3, Online Appendix B8) | `matched_agencies_value_results.csv` |
| `10_label_audit.R` | Baseline-only labels, summers separately, label coverage (Online Appendix B9) | `label_audit_results.csv` |
| `11_club_resources.R` | Association by club squad value within league and summer (Online Appendix B9) | `club_resources_results.csv` |

Each script is run as `Rscript R/analysis/<script> <data_root> [output_dir]` and can be used on its own.

**Terminology.** The paper calls the comparison agencies *matched agencies*. In the code and result files they are *twins*
(`twin_any`, `twin1`, model names such as `twin agencies (placebo)`). The labels `R1`, `R2`, `R4`, … and `V …` in model names only
identify blocks of analyses and have no other meaning.

## Data

Player identifiers and representative labels in `data_pseudonymised/` are random codes, player names are removed, and the mapping was
not kept. Two outputs depend on the sort order of the labels only (the anonymised worked example in the appendix picks its focal
agency among equals), so they can differ in which agency is shown but not in any estimate.

`data_pseudonymised/` is generated by `R/data_build/make_pseudonymised_data.R` from intermediate files that are not distributed. It is
kept out of version control until the data providers’ terms have been confirmed. Reviewers can be given it on request.

The analysis uses the following sources. They are not redistributed here.

- Historical versions of `players.csv` from *Football Data from Transfermarkt* (Cariboo, Kaggle), compiled from Transfermarkt.
  `data_sources/raw/kaggle/download_manifest.csv` records the version, the archive file’s Last-Modified timestamp, the URL and the
  checksum. The timestamp dates the archived file, not individual profile updates or representation agreements.
- The transfer, games and appearances tables of the `transfermarkt-datasets` repository (Cariboo, GitHub).
- Club Elo ratings through the `Club-Football-Match-Data` compilation (xgabora, GitHub), from ClubElo.
- EA Sports player ratings for FIFA 23 and FC 24 from public GitHub extracts, and the sofifa data set on Kaggle.

The Cariboo compilation carries a CC0 licence, but its content derives from Transfermarkt, whose terms of use apply to the underlying
data. Check the providers’ terms before reusing any of these files.

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
