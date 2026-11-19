# Data sources

Source lists, checksums and notes for the inputs of the analysis. The raw third-party files are not redistributed.

| Folder | Contents |
|---|---|
| `raw/kaggle/download_manifest.csv` | Version, archive timestamp, URL and SHA-256 of every historical `players.csv` used |
| `coach_retry/` | Notes and lists for the summer inputs: `expanded/` (event panel), `perf/` (pre-window club and player performance; `perf/README.md` lists the source addresses), `perf_extended/` (extended sample; `coverage.csv`) |
| `winter_recovery/` | Observed January windows built from December snapshots (`sources.csv`, `snapshot_checksums.txt`) |
| `winter_proxy/` | January 2026 backcast from February 2026 releases (`proxy_sources.csv`, `source_checksums.txt`, `validation.csv`, `winter_models.csv`) |

To rebuild the model inputs (`R/data_build/rebuild_data.R`), add the downloaded files to a copy of this folder: the archived
`players.csv` releases listed in the manifest as `raw/kaggle/data_<version>`, `raw/performance/` (appearances, ratings, Elo files),
`coach_retry/games.gz` and `coach_retry/transfers.gz`, and `coach_retry/expanded/summer_transfers.csv` (the published event order).
The folder names are historical and kept because the builders read them.
