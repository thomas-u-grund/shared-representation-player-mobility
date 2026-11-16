# Revision analyses

The main analysis now uses the **extended sample** (2,000 club-model events). Everything from the first version is
preserved: the first-version results are stored in `agency_paper/model_results_archived.csv` and `season_comparison_results_archived.csv`; the first-version sample also enters `rev5_league.R`, `rev7_baseline.R` and `rev8b_fill_models.R`.
The first-version manuscript, renderer and PDF are in `archive_before_revision/`.

## What defines the main sample
1. The mover's label is read from his own row in the pre-window release, wherever he is listed (the first version
   required him on the origin club's roster, which loses loanees returning to the parent club on 30 June).
2. Players with no label in the baseline release take the latest label from an earlier pre-window release (no look-ahead).
3. Movers listed at clubs outside the 14 leagues are added.
4. Loan-to-buy conversions (destination = the club that held the player on loan) are excluded.
Variants: `perf_extended_base` (1+3+4, no carry-forward), `perf_extended_post` (adds labels from later releases; look-ahead,
sensitivity only). Build with `R/build/rev8_fill.R {pre|base|post}` (writes to `../coach_retry/perf_extended*`). The winter
analogue is `R/build/build_winter_extended.R` (writes `winter_recovery_extended/`).
`R/rebuild_data.R` rebuilds both the first-version and the extended data from the raw inputs (verified byte-identical).

## Scripts (all in `../R/`; `Rscript R/run_all.R` runs them in order and compares with the stored tables)

| Script | Purpose | Output |
|---|---|---|
| `rev1_timing.R` | Fee type, return to a former club, timing | `rev1_timing_results_R.csv` |
| `rev2_twin.R` | Twin-agency placebo (also within contract groups); recently placed vs long-standing co-clients; sample flow, cross-tab, closest twin, matching quality | `rev2*_R.csv` |
| `rev4_choiceset.R` | Tier / affordability controls, restricted alternatives, same-event Elo decomposition | `rev4_choiceset_results_R.csv`, `rev4b_elo_results_R.csv` |
| `rev5_league.R` | League model: nationality-by-league effects, agency concentration | `rev5_league_results_R.csv` |
| `rev6_contract.R` | Contract-expiry status (and ratio tests); no-fixed-effects check | `rev6_misc_results_R.csv` |
| `rev7_baseline.R` | Observed vs uniform-choice share of connected destinations | `rev7_baseline_R.csv` |
| `rev8b_fill_models.R` | First-version vs player-level vs carry-forward vs look-ahead samples; harmonised labels | `rev8_fill_results_R.csv` |
| `rev9_specialisation.R` | Client-tier match (specialisation) control | `rev9_specialisation_results_R.csv` |
| `make_tables.R` | Rows for the tables and the figures | `revision_results_R.csv`, `figure1_main_R.png`, `figureA1_specifications_R.png` |

The stored result tables in this folder (`*.csv`) are the ones the manuscript reports; `run_all.R` checks the re-estimates against them.
