# Stored results

The result tables and figures the paper reports. `Rscript run_all.R` re-estimates them and compares each file with the version stored
here (same file name). Estimates are conditional logit odds ratios with confidence intervals clustered by the mover’s representative
label (`label_lo`, `label_hi`) and by player (`player_id_lo`, `player_id_hi`).

| File | Produced by |
|---|---|
| `model_results.csv`, `sample_summary.json` | `R/analysis/01_main_models.R` |
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
| `table_estimates.csv`, `figures/` | `R/report/make_tables.R` (rows of the paper’s tables; Figure 1 and Figure A1) |
