# Replication package: Who Joins Which Club? Football Agents and Player Recruitment in European Men’s Football

Code, data and results for the paper (submitted to the *International Review for the Sociology of Sport*). Study 1 uses pre-window rosters of
14 European men’s top leagues for the summers of 2023–2025 (2,000 transfers); Study 2 uses the English FA’s intermediary transaction reports
(1,381 signings, 2019–20 to 2022–23).

## Run it

```sh
Rscript run_all.R          # about 12 minutes on four cores; writes results_reproduced/
```

Needs R 4.x with `data.table`, `survival`, `readr`, `stringi`, `jsonlite`, `ggplot2`, `xml2`, `zip`. The script re-estimates every model of both
studies, builds every table (Tables 1–4, Online Appendix A1–A2, B1–B16, written to `results_reproduced/paper_tables.md`) and the two figures,
and compares each estimate with `results/`: it stops if any odds ratio differs by more than 0.2%.
With the paper sources, `Rscript run_all.R <output_dir> <paper_dir>` also builds the Word and PDF files (needs `pandoc` and LibreOffice).

## Layout

| Folder | Contents |
|---|---|
| `run_all.R` | The one command above |
| `code/study1/`, `code/study2/` | Analysis scripts: `01`–`13` for Study 1 (file names say what they do; outputs are listed in `results/README.md`), two scripts for Study 2 |
| `code/lib/`, `code/report/` | Shared helpers; table, figure and document builders |
| `data/study1/` | Model-ready Study 1 inputs with random player and representative codes (not under version control, see below) |
| `data/study2/` | Study 2 inputs: the harmonised FA transaction table and the division of each club |
| `results/` | The stored results the paper reports, and `results/README.md` |
| `rebuild_from_raw/` | Optional: rebuilds the inputs from the raw downloads and the FA PDFs (`rebuild_from_raw/README.md`) |

## Data

- **Study 2** uses the FA’s published transaction reports (public PDFs, 2015–16 to 2025–26). `data/study2/fa_transactions_long.csv` is the table extracted from
  them, with clubs, players and intermediaries as the FA published them. Club divisions use Transfermarkt-derived season tables (EFL, National League)
  and public league tables (Premier League). The FA’s own terms apply to reuse of the PDFs.
- **Study 1** uses archived releases of *Football Data from Transfermarkt* (Cariboo, Kaggle and GitHub, compiled from Transfermarkt), club Elo ratings
  (ClubElo through xgabora) and EA Sports player ratings. These raw files are not redistributed; their versions and checksums are in
  `results/download_manifest.csv` and `rebuild_from_raw/README.md`. The Cariboo compilation carries a CC0 licence, but its content derives from
  Transfermarkt, whose terms of use apply. Check the providers’ terms before reusing any of these files.
- `data/study1/` has player and representative codes replaced by random codes, player names removed and the mapping not kept (two outputs depend on
  the sort order of the codes only: which agency the anonymised worked example shows, never an estimate). It is kept out of version control until the
  providers’ terms are confirmed; reviewers can be given it on request.

**Terminology.** The paper calls the comparison agencies *matched agencies*; in code and result files they are *twins* (`twin_any`, `twin1`).
The labels `R1`, `R2`, … and `V …` in model names only identify blocks of analyses.

## Licence and citation

MIT licence for code and result tables (`LICENSE`); third-party data are not covered. `CHECKSUMS.sha256` lists the SHA-256 of every file in the repository.

[Add the full citation and DOI of the paper and of the archived release after acceptance.]
