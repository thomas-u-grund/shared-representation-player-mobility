# Results

The result tables and figures the paper reports (Study 1: `code/study1/01`–`13`; Study 2: `code/study2/`). `Rscript run_all.R` re-estimates them and compares each file with the version stored
here (same file name). Estimates are conditional logit odds ratios with confidence intervals clustered by the mover’s representative
label (`label_lo`, `label_hi`) and by player (`player_id_lo`, `player_id_hi`).

| File | Produced by |
|---|---|
| `model_results.csv`, `sample_summary.json` | `code/study1/01_main_models.R` |
| `timing_fees_results.csv`, `contract_status_results.csv` | `02_timing_fees_contract.R` |
| `matched_agencies_*.csv` | `03_matched_agencies.R` (`_results`, `_by_contract`, `_closest`: estimates; `_sample_flow`, `_crosstab`, `_choice_sets`, `_quality`, `_coverage`, `_worked_example`: Online Appendix A) |
| `choice_set_results.csv`, `elo_decomposition_results.csv`, `specialisation_results.csv` | `04_choice_sets_specialisation.R` |
| `league_results.csv` | `05_league.R` |
| `sample_variants_results.csv`, `baseline_share.csv` | `06_sample_variants.R` |
| `season_comparison_results.csv` | `07_season_comparison.R` |
| `move_type_results.csv` | `08_move_type.R` |
| `matched_agencies_value_results.csv` | `09_matched_agencies_value.R` |
| `label_audit_results.csv` | `10_label_audit.R` |
| `club_resources_results.csv` | `11_club_resources.R` |
| `co_client_background_results.csv`, `co_client_background_counts.csv` | `12_co_client_background.R` (Online Appendix B10) |
| `agency_concentration.csv`, `agency_concentration_results.csv`, `league_heterogeneity.csv`, `league_results_by_league.csv` | `13_agency_concentration_leagues.R` (Online Appendix B11, B12) |
| `fa_choice_results.csv` | `code/study2/fa_choice_model.R` (Study 2: Table 4 and B13; odds ratios with 95% CI clustered by intermediary, columns `lo`, `hi`) |
| `fa_audit_results.csv`, `fa_audit_counts.csv` | `code/study2/fa_audit_model.R` (Study 2: renewal model, one observation per signing, club-by-period effects, link audit) |
| `table_estimates.csv`, `figures/` | `code/report/make_tables.R` (rows of the paper’s tables; Figure 1 and Figure A1) |
| `fa_choice_informative_events.csv` | `code/study2/fa_choice_model.R` (events and alternatives per Study 2 run) |
| `download_manifest.csv`, `winter_models.csv` | Provenance and the January backcast models, read by the table builder (Online Appendix A1, B10); see `rebuild_from_raw/README.md` |

The scripts of Study 1 are run as `Rscript code/study1/<script> <data_root> [output_dir]` and those of Study 2 as `Rscript code/study2/<script> [output_dir]`.
