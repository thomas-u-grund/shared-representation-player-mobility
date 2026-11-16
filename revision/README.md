# Revision analyses

The main analysis now uses the **extended sample** (2,000 club-model events). Everything from the first version is
preserved: `analyse.py` and `compare_windows.py` take a sample argument, and `archived` reproduces the first-version
results (`agency_paper/model_results_archived.csv`, `season_comparison_results_archived.csv`). `reproduce.py` checks both.
The first-version manuscript, renderer and PDF are in `archive_before_revision/`.

## What defines the main sample
1. The mover's label is read from his own row in the pre-window release, wherever he is listed (the first version
   required him on the origin club's roster, which loses loanees returning to the parent club on 30 June).
2. Players with no label in the baseline release take the latest label from an earlier pre-window release (no look-ahead).
3. Movers listed at clubs outside the 14 leagues are added.
4. Loan-to-buy conversions (destination = the club that held the player on loan) are excluded.
Variants: `perf_extended_base` (1+3+4, no carry-forward), `perf_extended_post` (adds labels from later releases; look-ahead,
sensitivity only). Build with `rev8_fill.py {pre|base|post}` (writes to `../coach_retry/perf_extended*`). The winter
analogue is `../winter_recovery/build_winter_extended.py` (writes `winter_recovery_extended/`).
`rebuild_data.py` rebuilds both the first-version and the extended data from the raw inputs (verified byte-identical).

## Scripts (run from this folder, after `rev8_fill.py`)
| Script | Question | Output |
|---|---|---|
| `rev1_timing.py` | Fee type, return to a former club, timing | `rev1_timing_results.csv` |
| `rev2_identification.py` | Twin-agency placebo (also within contract groups); recently placed vs long-standing co-clients | `rev2_identification_results.csv`, `rev2b_twin_by_contract_results.csv` |
| `rev4_choiceset.py` | Tier / affordability controls, restricted alternatives | `rev4_choiceset_results.csv` |
| `rev5_league.py` | League model: nationality-by-league effects, agency concentration | `rev5_league_results.csv` |
| `rev6_misc.py` | Contract-expiry status (and ratio tests); no-fixed-effects check | `rev6_misc_results.csv` |
| `rev7_baseline.py` | Observed vs uniform-choice share of connected destinations | `rev7_baseline.csv` |
| `rev8b_fill_models.py` | First-version vs player-level vs carry-forward vs look-ahead samples; harmonised labels | `rev8_fill_results.csv` |
| `make_tables.py` | Rows for Tables 4-5, Figure 1 | `../agency_paper/revision_results.csv`, `figure1_specifications.png` |

Then `cd ../agency_paper && python render_manuscript.py`. `common.py` executes the preparation and estimator of
`analyse.py` unchanged for the chosen sample. Python 3.12 with `../requirements.txt` is the reference environment; the
scripts were run on newer pandas/scipy and agree with the archived tables within `reproduce.py`'s tolerance.

## Screening of further sources (summary)
- Older Kaggle versions (before 240): anonymous download returns 404; not retrievable here.
- Later releases (up to 660): labelled 114 additional club-sample events; excluded from the main sample because the
  agent may have changed after the window (included as `perf_extended_post`; estimate unchanged, 1.37).
- Public sources of historical agent-client relationships: none found. FIFA's agent platform lists clients but offers no
  bulk download; per-player Transfermarkt/Wayback collection would be rate-limited and subject to site terms (not done).
- Remaining unlabelled screened events: 813 (55 players absent from the releases; the rest have no named representative
  in any pre-window release).
