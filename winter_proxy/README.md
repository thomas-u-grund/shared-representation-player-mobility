# January representation backcast and validation

This sensitivity analysis uses February player profiles to reconstruct
1 January club rosters. It reverses recorded intervening transfers one
date at a time across all observed leagues. A player is omitted if a
unique path cannot be traced backwards from their February club, or if
they are absent from the relevant snapshot. The reconstruction ends
before moves recorded on the snapshot date. Version dates refer to
Kaggle download objects, rather than player-specific observation dates.

The reconstruction assigns the February agent label to the player's
1 January club. It cannot verify that the same representative was in
place on 1 January. Exact normalised labels can refer to individuals or
companies. Focal players are excluded from candidate co-client counts.

## Validation and models

The February procedure is benchmarked against directly observed
December rosters and labels for January 2024 and 2025. Concordance
statistics are conditional on overlapping players or choice rows, with
denominators in validation.csv. The matched estimate compares the same
screened events and candidate clubs in both representations. Events
are screened for zero or unknown-fee reversals of a transfer since the
previous January. The screen can remove some genuine moves.

Winter conditional logits condition on the realised destination league,
include labelled roster size, and in the flow-adjusted specification
also include destination club fixed effects and directional three-year
club-pair flow counts excluding the focal player. Alternatives never
chosen within the estimation sample are dropped for the fixed effects.
These are reduced winter models and do not reproduce all summer
performance and player–club fit controls. Inference is clustered
separately by representative label and player in winter_models.csv.
The January 2026 estimates use versions 603 and 606 as a sensitivity to
coverage even within the same snapshot date. Use the directly observed
2024–25 winter data as the primary analysis.

## Reproduce

Run R/build/evaluate_winter_proxy.R from this directory with the directories containing
the Kaggle downloads (data_355, data_566, data_603, data_606), the
dcaribou games.gz and transfers.gz files, and the extracted December
winter roster package. Repeat from version_606_sensitivity using its
proxy_sources.csv to reproduce the alternate results (pass the same
input directories and call R/build/evaluate_winter_proxy.R).

Kaggle archive root
https://www.kaggle.com/datasets/davidcariboo/player-scores/versions

Transfer and games tables
https://github.com/dcaribou/transfermarkt-datasets
