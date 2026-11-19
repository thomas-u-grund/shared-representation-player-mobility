# Pre-window club and player performance

This enrichment joins the 14-league panel for summer 2023–2025 by
Transfermarkt player and club IDs. No model has been fitted.

- player_performance.csv contains the 19,549 baseline player–summer rows,
  with profile age, nationality, position and value, plus domestic league
  appearances, minutes, goals and assists at the baseline club before
  1 June. Zero means no playing time observed at that club.
- club_performance.csv contains prior-season domestic match counts,
  points earned on the pitch, points per match, goal difference and
  a points-per-match percentile within the prior league. The percentile
  is computed from games, without official deductions or adjustments for
  splits and play-offs. Clubs new to the tracked top flight have no prior
  top-flight matches in this extract. That flag can include league
  entrants other than clubs promoted from tier two.
- transfer_performance.csv preserves the 4,706 transfer records and adds
  origin and destination performance and the difference in pre-window Elo.
- within_league_candidates.csv contains the chosen club and alternate clubs
  in the destination league for each agent-linked transfer. It includes
  co-clients, squad depth and minutes by broad position, and the gap from
  the median rating among rated players in the same position. The focal
  player is excluded. Candidate pools use next-season league membership.
- coverage.csv audits linkage. The Elo crosswalk uses exact normalised
  names within country and explicit aliases. Multiple possible club
  labels are left unresolved. The latest May snapshot is used.
  No Ukraine Elo records were available in this archive.
- EA ratings use FIFA 23 name and age, FC 24 name and full date of birth,
  and FC 25 via the verified FC 24 game ID. New FC 25 entrants without a
  verified earlier ID can lack ratings. Ratings are archived before the
  relevant window. Profile rosters are from the earlier pre-window snapshots,
  while performance includes league games up to 31 May.

## Reproduction

Pass the expanded panel input folder (including its underlying Kaggle
240.data, 435.data, 600.data snapshots and games.gz), the performance
source folder and an output folder to R/build/build_performance.R. Source
folder files and download locations:

- appearances.gz: appearances.csv.gz from dcaribou/transfermarkt-datasets
- elo.csv: https://raw.githubusercontent.com/xgabora/Club-Football-Match-Data/main/data/EloRatings.csv
- 2023.ratings.csv: https://raw.githubusercontent.com/MithileshAnavkar/FOOTEM/d361b4fe145054d973b0b6f0dcedbab9cbdb9a5a/Data/players_fifa23.csv
- 2024.ratings.csv: https://raw.githubusercontent.com/stephensoccer/EA-Sports-FC-24-Players-Data/fa16d9bd06b5afd9c56fc8a1f282ece855a9edde/FC%2024%20Players%20Data.csv
- sofifa.zip: https://www.kaggle.com/datasets/jmacd745/sofifa-data-set
- elo_aliases.csv: included
