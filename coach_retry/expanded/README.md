# Expanded historical agent and summer transfer panel

The summer transfer frame covers the 14 top flights consistently present
in the archived releases for 2023–25. It includes Belgium, Denmark,
England, France, Germany, Greece, Italy, the Netherlands, Portugal,
Russia, Scotland, Spain, Türkiye and Ukraine.

## Files

- player_snapshots.csv preserves agent labels and club/player IDs. Its
  2026 baseline also includes the additional leagues with competition
  codes in version 660, giving 32 leagues. Snapshot labels are individual
  or corporate names as listed, with no agreement dates.
- summer_transfers.csv contains 4,706 unique player/date/origin/destination
  records between covered clubs during 1 June–1 September inclusive.
  Origin tier membership is from the preceding season and destination
  league from the following season, using played league records.
  Moves involving a promoted or relegated club can therefore change
  league categories. Counts are events, including repeat movers.
- counts.csv shows the full count, linkage to the exact baseline player
  and origin club, the subset with an observed agent, and the subset
  remaining after flagging likely returns.
- league_coverage.csv gives snapshot coverage by league and year.
- destination_clubs.csv and model_results.csv support the destination
  comparisons.

A likely return is a zero or unknown-fee reverse move on 30 June or
1 July with the opposite move recorded since the previous June. New
loans are eligible. Some returns, conversions and same-club spells
may still affect the screen. These are reported summer records, not
verified transaction types. No single-move restriction was applied.

Conditional logits adjust for overall destination league popularity
(and a common origin-league indicator in the all-league model). The
within-league club model conditions on the realised league and adjusts
for candidate roster size with known agent labels. It uses only clubs
with observed baseline agent coverage. Inference clusters by player.
These are descriptive associations conditional on observed moves.
Nationality routes, positional fit and club-specific demand remain
unadjusted. Agency keys use orthographic normalisation only.

## Inputs and reproduction

Download Kaggle davidcariboo/player-scores players.csv releases 240,
435, 600 and 660 as VERSION.data. Their recorded availability dates
are 23 May 2023, 31 May 2024, 11 April 2025 and 26 May 2026. Download
transfers.csv.gz and games.csv.gz from dcaribou/transfermarkt-datasets
as transfers.gz and games.gz. Work in the directory of these inputs.
Run python count.py and python fit.py (NumPy and SciPy for fitting).

The 2022 release tested (150, 28 April) lacks the agent field.
The 2026 outcome source is incomplete, especially in August, and is
excluded from transfer counts and models.

- https://www.kaggle.com/datasets/davidcariboo/player-scores
- https://github.com/dcaribou/transfermarkt-datasets
