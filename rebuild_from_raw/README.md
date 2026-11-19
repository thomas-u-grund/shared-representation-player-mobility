# Rebuilding the inputs from the raw files (optional)

Nothing here is needed for `Rscript run_all.R`. This folder rebuilds the inputs of the analysis from the raw downloads and the FA PDFs, and holds the
source lists and checksums. Raw third-party files are not redistributed.

## Study 1: from the raw downloads to `data/study1/`

```sh
Rscript rebuild_from_raw/rebuild_data.R <root> [output_dir]
```

`<root>` is laid out like `rebuild_from_raw/sources/` plus the downloaded files: `raw/kaggle/` (the `players.csv` releases as `data_<version>`, and
`download_manifest.csv`, which is kept in `results/`), `raw/performance/` (appearances, ratings, Elo files), `coach_retry/` (`games.gz`, `transfers.gz`,
`expanded/summer_transfers.csv`, the published event order), `winter_recovery/` and `winter_proxy/`. The builders are numbered `01`–`07`
(`winter_common.R` is shared); `make_pseudonymised_data.R` then creates `data/study1/` from intermediate files that are not distributed.
Compared with the shipped tables the rebuilt values are identical apart from number formatting (15 rather than 17 significant digits, integers without a
trailing `.0`, quoting of empty fields); the two fitted-model files differ by 1e-8 or less. A run takes under a minute.

**Sources** (retrieval and checksums: `results/download_manifest.csv`, `sources/*/*checksums*.txt`, `sources/*/sources.csv`)

- Kaggle `davidcariboo/player-scores` (https://www.kaggle.com/datasets/davidcariboo/player-scores): `players.csv` releases 240, 435, 600 and 660 for the summer
  baselines (available 23 May 2023, 31 May 2024, 11 April 2025, 26 May 2026) and 325 and 547 for the observed January windows; versions 355, 566, 603 and
  606 for the January backcast. The timestamps are the archive files’ Last-Modified dates, not the dates of individual profile updates or agreements.
  The 2022 release (150) lacks the agent field; the 2026 outcome source is incomplete and is excluded from transfer counts and models.
- `transfers.csv.gz`, `games.csv.gz`, `appearances.csv.gz` from https://github.com/dcaribou/transfermarkt-datasets (kept as `transfers.gz`, `games.gz`, `appearances.gz`).
- Club Elo: https://raw.githubusercontent.com/xgabora/Club-Football-Match-Data/main/data/EloRatings.csv (as `elo.csv`; `sources/coach_retry/perf/elo_aliases.csv` is included).
- EA ratings: https://raw.githubusercontent.com/MithileshAnavkar/FOOTEM/d361b4fe145054d973b0b6f0dcedbab9cbdb9a5a/Data/players_fifa23.csv (as `2023.ratings.csv`),
  https://raw.githubusercontent.com/stephensoccer/EA-Sports-FC-24-Players-Data/fa16d9bd06b5afd9c56fc8a1f282ece855a9edde/FC%2024%20Players%20Data.csv (`2024.ratings.csv`),
  and https://www.kaggle.com/datasets/jmacd745/sofifa-data-set (`sofifa.zip`).

**What the inputs contain.** The summer frame covers the top flights of Belgium, Denmark, England, France, Germany, Greece, Italy, the Netherlands, Portugal,
Russia, Scotland, Spain, Türkiye and Ukraine: 4,706 transfer records between covered clubs from 1 June to 1 September. A likely loan return is a zero or unknown-fee
reverse move on 30 June or 1 July with the opposite move recorded since the previous June (new loans are eligible); these are reported records, not verified
transaction types. Snapshot labels are individual or corporate names as listed, with no agreement dates. Performance inputs join player and club IDs and use
league games up to 31 May; EA ratings are archived before each window. The January windows use December snapshots (2024, 2025) and, for 2026, a backcast from
February releases that reverses recorded transfers; the directly observed windows are the primary January analysis (`sources/winter_*`; `validation.csv` benchmarks the backcast).

## Study 2: from the FA PDFs to `data/study2/`

The FA’s page *The FA publishes Football Agent fees and transactions details*
(https://www.thefa.com/football-rules-governance/policies/player-status---agents/football-agent-transactions) links one transaction PDF per reporting period, 2015–16 to 2025–26 (retrieved 19 Nov 2026).

1. Download the PDFs listed in `study2/urls.tsv` to `study2/raw/<period>/fa.bin` and compare their SHA-256 with `study2/source_manifest.tsv` (the FA may move or replace files; the checksums show whether a newer file is identical).
2. `python3 -I rebuild_from_raw/study2/parse_fa.py` (needs `pdftotext`) writes `data/study2/fa_transactions_long.csv`: one row per transaction × intermediary, 25,802 rows. Five wrapped rows were repaired by hand (listed in the script); unparsed lines are in `study2/fa_unparsed_lines.csv`.
3. `python3 -I rebuild_from_raw/study2/build_divisions.py <folder>` writes `data/study2/club_division.csv`. It needs Transfermarkt-derived season membership files (`sptd_clubs.csv`, `sptd_lower_club_league_history.csv`, not distributed); `study2/club_aliases.json` holds club-name aliases.

Layouts differ by period (2015–19 wide with three sides; 2019–23 with registration type and, to 2022–23, registration numbers; 2023–26 a single agent field without numbers),
so intermediaries are matched by registration number where available and by normalised name otherwise.

`study2/audit/` holds optional checks of the extraction (`audit_and_first_number.py` needs pandas, `audit_name_matching.py` plain Python; their outputs are in the same folder)
and `PRESPEC.md`, which fixes the runs specified before the first rerun (the audit and extensions were added afterwards and are marked as such).

**Limits of the FA records.** They name the agent on the completed deal, not who held the player’s representation before it; early periods have no registration type, so
signings cannot be separated from renewals there; agent IDs exist only for 2019–20 to 2022–23, so other periods match by name; reporting windows are not calendar years.
A machine-readable extract with stable IDs, transaction dates, the party that retained each intermediary and dated representation agreements would need a data request to the FA.
