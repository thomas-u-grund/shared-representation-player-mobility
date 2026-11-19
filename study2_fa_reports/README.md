# Study 2: English FA intermediary transaction reports, 2015–16 to 2025–26

Data, code and results behind Study 2 of the paper (Table 4 and Online Appendix Table B13). The data are the FA's own published
transaction reports for Premier League, EFL and National League System clubs, one PDF per reporting period (public documents).
This folder is part of the replication package: `Rscript run_all.R` (repository root) re-estimates every Study 2 result from the
harmonised table `fa_transactions_long.csv` and compares it with `stored_results/`.

## Reproducing

| Step | Command (run in this folder unless stated) | Needs | Output |
|---|---|---|---|
| Estimation (part of `run_all.R`) | `Rscript fa_choice_model.R [output_dir]` | R: data.table, survival, stringi; about 40 s | `fa_choice_results.csv`, `fa_choice_informative_events.csv` |
| Estimation, audit, renewal model | `Rscript fa_audit_model.R [output_dir]` | same; about 2 min | `fa_audit_results.csv`, `fa_audit_counts.csv` |
| Name-matching audit (optional) | `python3 -I audit_name_matching.py` | Python 3 | `audit_name_matching_sample_noid.txt`, printed counts |
| Audit tables and first number (optional) | `python3 -I audit_and_first_number.py` | pandas | `audit_by_period.csv`, `audit_club_names.csv`, `first_number_by_period.csv`, `first_number_examples.csv` |
| Club divisions (optional) | `python3 -I build_divisions.py <folder>` | Transfermarkt-derived season membership files `sptd_clubs.csv` and `sptd_lower_club_league_history.csv` (not distributed) | `club_division.csv` (shipped) |
| PDF extraction (optional) | `python3 -I parse_fa.py` | the PDFs in `raw/<period>/fa.bin` (download them, see below), `pdftotext` | `fa_transactions_long.csv` (shipped) |

`fa_choice_model.R` holds the choice-set construction and the main runs; `fa_audit_model.R` reuses its data preparation and adds the audit
of the exposure, the choice-set variants, the one-observation-per-signing and club-by-period checks and the renewal model with three exposures
(renewal or other update, other club-side registrations, player side only; the 1.40 estimate of Table 4). `PRESPEC.md` fixes the runs
that were specified before the first rerun; the audit and extensions were added afterwards and are marked as such in it.

**Extraction from the PDFs.** `urls.tsv` lists the address of each report and `source_manifest.tsv` the byte size and SHA-256 of the file the
shipped table was extracted from. To redo the extraction, download each file to `raw/<period>/fa.bin`, check the checksum, and run `parse_fa.py`.
The FA may move or replace the files; the checksums show whether a newer file is identical. `fa_transactions_long.csv` is the extraction result and
the input of everything else. Five wrapped rows were repaired by hand; they are listed in `parse_fa.py`.

**Data terms.** The reports are public documents published by the FA. The table reproduces their content (club, player, intermediary and registration number) as published.
The club divisions use Transfermarkt-derived season membership (see the main README on the terms of use of Transfermarkt data) and the public league tables.

## Sources (retrieved 19 Nov 2026; URLs and checksums in `urls.tsv` and `source_manifest.tsv`)
The FA's page *The FA publishes Football Agent fees and transactions details* (thefa.com/football-rules-governance/policies/player-status---agents/football-agent-transactions; saved in `pages/`) links one transaction PDF per reporting period. All eleven were obtained:

| Period | Covers | Layout |
|---|---|---|
| 2015-16 | 1 Apr 2015 – 1 Feb 2016 | wide: receipt date, player, registering club, then intermediary and firm for each of three sides (player, registering club, former club) |
| 2016-17, 2017-18, 2018-19 | 2 Feb 2016 – 31 Jan 2019 | same wide layout |
| 2019-20 | 1 Feb 2019 – 31 Jan 2020 (a separate release, 24 Jun 2020) | date, club, player, former club, registration type, intermediary, registration number, who represented, subcontractor flag |
| 2020-21 | 1 Feb 2020 – 1 Feb 2021 | as above, no date |
| 2021-22, 2022-23 | PDFs carry no title; the periods are the FA's annual cycles | club, player, registration type, intermediary, registration number, who represented, subcontractor flag |
| 2023-24 | 1 Feb 2023 – 1 Feb 2024 | no registration numbers |
| 2024-25 | 2 Feb 2024 – 3 Feb 2025 | adds coaches |
| 2025-26 | 4 Feb 2025 – 2 Feb 2026 (published 1 Apr 2026) | adds women's leagues |

## Files
- `fa_transactions_long.csv`: one row per transaction × intermediary, 25,802 rows. Columns: period, date (2015-16 to 2019-20 only), registering club, former club, player, registration type (2019-20 onwards only), agent name, individual, firm, agent ID (2019-20 to 2022-23 only), `representing` as published, side flags (player / registering club / former club / coach), subcontractor flag (2019-20 onwards), section (league heading where parsed), source page, `flags` (parser notes), raw text.
- `club_division.csv`, `club_aliases.json`: division of each club in each period and name aliases used to build it.
- `fa_choice_results.csv`, `fa_audit_results.csv`, `fa_audit_counts.csv`, `fa_choice_informative_events.csv`: estimates and counts (the first two are also in `stored_results/`).
- `fa_unparsed_lines.csv`, `audit_by_period.csv`, `audit_club_names.csv`, `first_number_by_period.csv`, `first_number_examples.csv`, `audit_name_matching_sample_noid.txt`, `periods.json`, `source_manifest.tsv`, `urls.tsv`.

## Estimates
Event-stratified conditional logits (`survival::clogit`, Efron ties) on 1,451 signing–intermediary observations (1,381 signings) in 2019-20 to 2022-23: the club joined against the other clubs of the same division in the period's report, exposure from earlier reporting periods only, clustered by intermediary. With club fixed effects the odds ratio of an earlier dealing is 1.78 (1.49–2.13); without them 1.84 (1.54–2.20) once clubs without an incoming-transfer row (foreign counterparties listed in the reports) are removed from the alternatives. Earlier drafts of this analysis, which counted those clubs as alternatives, gave larger no-fixed-effects values (2.07); the paper reports the fixed-effects estimates.

## Audit (see `audit_by_period.csv`)
| Period | Rows | Transactions | Date | Type | ID | Firm named | Side given |
|---|---:|---:|---:|---:|---:|---:|---:|
| 2015-16 | 1,451 | 1,344 | 100% | – | – | 92% | 100% |
| 2016-17 | 1,755 | 1,596 | 100% | – | – | 88% | 100% |
| 2017-18 | 1,933 | 1,807 | 100% | – | – | 92% | 100% |
| 2018-19 | 1,947 | 1,793 | 100% | – | – | 89% | 100% |
| 2019-20 | 2,084 | 1,922 | 100% | 92% | 99.9% | 30% | 100% |
| 2020-21 | 2,040 | 1,845 | – | 100% | 100% | 31% | 100% |
| 2021-22 | 2,440 | 2,282 | – | 88% | 100% | 31% | 100% |
| 2022-23 | 2,552 | 2,405 | – | 88% | 100% | 35% | 100% |
| 2023-24 | 2,680 | 2,483 | – | 100% | – | – | 100% |
| 2024-25 | 3,273 | 3,054 | – | 100% | – | – | 100% |
| 2025-26 | 3,647 | 3,447 | – | 100% | – | – | 100% |

"Firm named" is a firm in its own field (early reports) or an agent ID starting IMSC (2019-20 to 2022-23); from 2023-24 the single agent field mixes individuals and firms with no ID. Parser checks: every flagged line was either parsed or listed in `fa_unparsed_lines.csv`; between 0 and 300 rows per period carry a parser note (blank cells placed by position, merged wrapped lines, compound "individual – firm" names split). The audit has not been checked against a hand count of rows, only against the page structure and the earlier extract (which found 2,401, 2,552, 2,679, 3,273 and 3,647 rows for the five most recent reports; this extract finds 2,440, 2,552, 2,680, 3,273 and 3,647; the 39 extra rows in 2021-22 are mostly pages that omit the registration-type column, which the earlier script did not accept; the extra row in 2023-24 was not examined).

## First number (`first_number_by_period.csv`)
Focal records: incoming signings with an intermediary recorded on the player's side. Two definitions: strict (Permanent transfer / Perm Transfer In) and broad (adds New Registration, which includes free agents and young players). The history comes only from earlier reporting periods (all periods back to 2015-16); an intermediary matches when its name (individual or firm, normalised) appears for the same club (normalised) in a record of a *different* player.

| Focal period | Strict signings | Agent on club side earlier | Club side only (not also player side) | Player side only | No earlier dealing |
|---|---:|---:|---:|---:|---:|
| 2019-20 | 348 | 69 (19.8%) | 23 | 5 | 274 |
| 2020-21 | 296 | 86 (29.1%) | 24 | 5 | 205 |
| 2021-22 | 348 | 89 (25.6%) | 32 | 8 | 251 |
| 2022-23 | 459 | 115 (25.1%) | 33 | 8 | 336 |
| 2023-24 | 477 | 106 (22.2%) | 31 | 12 | 359 |
| 2024-25 | 666 | 78 (11.7%) | 18 | 14 | 574 |
| 2025-26 | 691 | 115 (16.6%) | 23 | 25 | 551 |

Roughly one signing in five to four has a player-side agent who earlier appears on the club's side for another player; a few per cent have only a player-side earlier record. "Club side" in the FA tables mostly means the club paid or engaged the agent in a deal where the agent also acted for the player (68% of rows in 2019-20 list both sides, 47–53% from 2022-23), so the club-side-only column is the narrower reading. The drop in 2024-25 coincides with the move to the single agent field without IDs, so name matching is weaker there. Broad-definition counts and a within-period variant using receipt dates (2019-20 only) are in the CSV.

## Limits
- The reports record the agent named on the completed deal. They say nothing about who held the player's representation before the deal, so they cannot test whether a player's *existing* representative predicts the destination; they can only show repeat dealings between a club and an agent across clients and roles.
- Early periods (2015-16 to 2018-19) have no registration type, so signings cannot be separated from renewals there, and they enter only as history. Former-club names are given in under 7% of early rows.
- No stable agent IDs in 2015-19 and from 2023-24, so entities are matched by normalised name. Common names can collide; firm and individual names are mixed in the later single field; an agent who changed firm cannot be followed. Player names differ in form across years (middle names), so "different player" uses first and last name only.
- Club names differ across reports (e.g. "AFC Bournemouth", "Aston Villa FC", "Arsenal (to Napoli)"); normalisation removes FC/AFC and bracketed text. `audit_club_names.csv` lists clubs not seen in earlier periods (promotions and relegations explain most; 2019-20 has 53 because the 2019-20 report names clubs differently from the early reports and lower-league clubs enter).
- Reports are as published by the FA; they cover registered transactions only, with the reporting windows above (they are not calendar years and the 2021-22 and 2022-23 windows are not stated in the PDFs).
- The 2021-22 and 2022-23 PDFs have no header text; columns were taken from the cell structure of each page. Five wrapped rows were repaired by hand (listed in `parse_fa.py`).

## What would need a data request to the FA
Not verified as available to researchers; this is a list of what the analysis would need, not a claim about what the FA holds: a machine-readable extract (not PDF) with stable agent and agency IDs in every period, transaction dates, who retained whom (player, registering club, former club), the transaction type in all periods, and dated representation agreements, so that the player's representative before the move can be observed.

