# Pre-January football roster and representation snapshots

Kaggle versions 325 and 547 were recovered and checked against their
archived download bodies. The dates in sources.csv are the storage
objects' Last-Modified timestamps, used to verify that each snapshot
was available before 1 January. They do not date individual agreements.

The first observed successor versions 326 and 548 were dated 2 January
2024 and 2 January 2025 respectively. For January 2026, Kaggle versions
602 and 603 were dated 11 April 2025 and 14 February 2026. No December
2025 snapshot was recovered from this version sequence. The developer
repository's latest commit before that January was in June 2025.

## Data

- winter_player_snapshots.csv has active-season roster rows from the
  same 14 top flights as the summer panel. It preserves original
  representative labels and orthographically normalised keys.
- january_transfers.csv has unique reported January moves between
  clubs in the covered top flights, joined to the snapshot by player
  and origin club. The dates span January 1–31 as a common calendar
  screen. They do not implement association-specific registration dates.
- january_destination_candidates.csv has the realised club and
  alternative clubs in the realised league with at least one observed
  representation label. It excludes the origin club and the focal
  player from co-client counts. Returns are retained in this file.
- coverage.csv audits roster, origin, label and candidate linkage.
  A zero or unknown-fee move reversing a prior move since 1 January
  of the previous year is flagged as a possible loan return. This can
  also flag other moves. Counts using that screen are provisional.

Named labels can identify individuals or firms. The same normalisation
as in the summer study is used for substantive labels. The extract
gives roster and affiliation evidence only, plus transfer linkage.
It contains no fitted winter estimates.

## Reproduce

Place the two Kaggle players.csv download bodies in a directory as
data_325 and data_547. Use the original zip or CSV bytes, respectively.
The verified release links are in sources.csv and the source digests in
snapshot_checksums.txt. Place the public dcaribou transfer and games
files in a second directory as transfers.gz and games.gz.
Run R/build/build_winter.R with those two directories as arguments.
