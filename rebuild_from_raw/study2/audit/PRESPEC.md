# Pre-specification of the Study 2 rerun (written before the run; the run is executed once and everything is reported)

Divisions: `club_division.csv` (built by `build_divisions.py`; derived divisions agree with the reports' own labels for all 313 club-seasons where both exist). Choice set = clubs of the same division that appear in the focal period's report, minus the former club. Sensitivity: tier group (Premier League; Championship; League One and Two together; National League System).

Runs, each with the four exposure specifications of the earlier run (A any prior dealing; B club-side vs player-side-only; C club-side-only vs other; D log1p count), with and without club fixed effects, standard errors clustered by intermediary:
1. primary: registration-number match (name where the history row has no ID), 2019-20 to 2022-23, same division
2. primary restricted to deals where the agent is not also on the club side of the focal deal
3. name-only match, 2019-20 to 2022-23
4. name-only match, 2019-20 to 2025-26
5. primary with tier-group choice sets
6. name-only match, 2019-20 to 2022-23, with the 31 names that map to more than one registration number removed
New specifications (run on 1, 3 and 4):
E. recency: `any_prev` (dealing in the immediately preceding period) and `any_old` (dealing two or more periods earlier) entered together
F. popular clubs: any prior dealing plus log(1 + number of distinct intermediaries that dealt with the club in earlier periods)
No other specification is run; nothing is tuned afterwards.

## Audit and extensions (written before the run; each item run once, everything reported)
Script `fa_audit_model.R` (reuses the data preparation of `fa_choice_model.R`). Exposure definitions are unchanged: an earlier completed reporting period in which the intermediary named on the deal acted for the club in a transaction for a different player. Specifications A (any prior dealing) and B (club-side vs player-side-only) unless stated; no fixed effects and club fixed effects; standard errors clustered by intermediary unless stated.
1a. Document how earlier history is linked (ID where the history row carries an ID, otherwise normalised name); report the share of exposed (event, club) pairs linked only by name, by cohort.
1b. Primary (2019-20 to 2022-23) for comparison; primary without the 2019-20 cohort; ID-at-both-ends (event ID matched to history rows with an ID, history from ID periods only, so 2019-20 has no history and drops out); and the same cohorts under the primary rule.
1c. Count signings by number of player-side intermediaries; the main code makes one event per signing-intermediary pair. Checks: one event per signing keeping the first-listed intermediary; one event per signing with exposure if any listed intermediary had dealt earlier; both clustered by intermediary and by the chosen club.
2. Choice sets: report-appearance clubs vs clubs that have an incoming (non-"out", non-cancellation) row vs all division members (Premier League, Championship, League One, League Two; National League System has no complete membership list and is excluded) on exactly the same signings; the membership comparison uses the periods with a clean membership list (2019-20, 2021-22, 2022-23), because the 2020-21 report's "Premier League" and "Championship" sections contain foreign clubs (outbound transfers) and its list is not a membership list. Club-by-period fixed effects on the primary sample.
3. Renewals: exposure = intermediary acted on the club's side in a renewal/updated-contract registration of another player in an earlier completed period (history only from periods with registration types, 2019-20 onwards), versus club-side dealings in other registration types, versus player-side only; ID-matched 2020-21 to 2022-23 and name-matched 2020-21 to 2025-26. Counts first.
4. Report the fixed-effects estimate and informative-set count for deals where the intermediary is not also on the club side.

## Extensions to balance Study 2 with Study 1 (written 23 Nov 2026, before any of these was run; each item run once, everything reported)
Script `fa_balance_model.R` (reuses the data preparation and `build2` of `fa_audit_model.R`). Same conditional logit as the primary model: club fixed effects, standard errors clustered by the deal's intermediary.
B1. Matched-intermediary comparison (analogue of Study 1's own-versus-matched agencies). Intermediary identity = registration number (ID at both ends), history from 2019-20, signings 2020-21 to 2022-23 (2019-20 has no ID history). For each focal intermediary with dealings with at least three distinct clubs in earlier periods, the three matched intermediaries are those with dealings with at least three distinct clubs, within a factor of two of the focal intermediary's number of distinct clubs, and the highest cosine similarity of the profile (share of dealings in each division, share of dealings on the club's side), using no information about the focal club. Focal intermediaries with fewer than three such candidates are dropped. The matched exposure is: any of the three (or the single closest) matched intermediaries dealt earlier with the club for a different player. The own and matched exposures enter the same model; report both odds ratios, the ratio of the two with a cluster-robust interval, and the own estimate on the same events. If the interval of the ratio includes 1, report that.
B2. Leave-out of the largest intermediaries: primary specification (2019-20 to 2022-23, ID where available) dropping each of the ten intermediaries with the most signings singly and all ten together.
B3. Heterogeneity: the primary any-prior-dealing term split by Premier League, EFL and National League System, with a Wald test of equality; and the average fitted probability of choosing an exposed club, in events with at least one exposed alternative, with the exposure coefficient as estimated and set to zero (primary model with club fixed effects).
No other specification is run; nothing is tuned afterwards.

Post-run note: the National League System split of B3 had no observations (the four primary periods contain only Premier League and EFL signings); B3 reports Premier League against EFL.
