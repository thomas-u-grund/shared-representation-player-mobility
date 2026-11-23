# Collect the estimates reported in the paper's tables into table_estimates.csv.
# Usage: Rscript code/report/make_tables.R <results_dir>   (the directory holding the result tables written by code/study1/ and code/study2/; the outputs go there too)
suppressPackageStartupMessages(library(data.table))
dir <- commandArgs(TRUE)[1]
rf <- function(f) fread(file.path(dir, f))
get <- function(f, mod, trm) { d <- rf(f); r <- d[d$model == mod & d$term == trm]; stopifnot(nrow(r) == 1); c(r$events, r$odds_ratio, r$label_lo, r$label_hi) }
rows <- list()
add <- function(block, label, v, src = "") rows[[length(rows) + 1]] <<- data.table(block = block, label = label, events = as.integer(v[1]), odds_ratio = v[2], lo = v[3], hi = v[4], source = src)
f <- "contract_status_results.csv"; M <- "R6 by contract status (main sample)"
add("mechanism", "Contract ends by 30 June of the window year", get(f, M, "contract expiring by 30 June"), f)
add("mechanism", "Contract runs one more year", get(f, M, "contract runs one more year"), f)
add("mechanism", "Contract runs longer", get(f, M, "contract runs longer"), f)
add("mechanism", "Ratio: expiring contract to longer contract", get(f, "R6 expiring vs longer contract (main sample)", "difference, expiring vs longer contract (ratio)"), f)
f <- "timing_fees_results.csv"; M <- "R1 four-way transaction type"
for (p in list(c("turn, free/loan", "30 June or 1 July, no fee"), c("turn, paid", "30 June or 1 July, fee paid"), c("off-turn, free/loan", "Other dates, no fee"), c("off-turn, paid", "Other dates, fee paid")))
  add("mechanism", p[2], get(f, M, p[1]), f)
add("mechanism", "Excluding moves to a former club", get(f, "R1 exclude former-club destinations", "same"), f)
f <- "matched_agencies_results.csv"; T <- "R2 twin sample: own + twin agencies"
add("mechanism", "Own agency (twin-agency sample)", get(f, T, "own agency"), f)
add("mechanism", "Twin agencies with similar portfolios (placebo)", get(f, T, "twin agencies (placebo)"), f)
add("mechanism", "Difference: own minus twin agencies", get(f, T, "difference (own - twin)"), f)
f <- "matched_agencies_closest.csv"
add("mechanism", "Closest single twin (placebo)", get(f, "R2c own + closest single twin", "closest twin (placebo)"), f)
add("mechanism", "Difference: own minus closest single twin", get(f, "R2c own + closest single twin", "difference (own - closest twin)"), f)
f <- "matched_agencies_by_contract.csv"
add("mechanism", "Own agency, contract expiring", get(f, "R2 twin placebo, expiring contract", "own agency"), f)
add("mechanism", "Own agency, contract runs longer", get(f, "R2 twin placebo, contract runs longer", "own agency"), f)
add("mechanism", "Twin agencies, contract runs longer (placebo)", get(f, "R2 twin placebo, contract runs longer", "twin agencies (placebo)"), f)
add("mechanism", "Difference, contract runs longer: own minus twin agencies", get(f, "R2 twin placebo, contract runs longer", "difference (own - twin)"), f)
add("mechanism", "Twin agencies, contract expiring (placebo)", get(f, "R2 twin placebo, expiring contract", "twin agencies (placebo)"), f)
add("mechanism", "Difference, contract expiring: own minus twin agencies", get(f, "R2 twin placebo, expiring contract", "difference (own - twin)"), f)
f <- "matched_agencies_results.csv"
add("mechanism", "Recently placed co-client", get(f, "R2 recent vs long-standing co-clients", "recently placed co-client"), f)
add("mechanism", "Long-standing co-client", get(f, "R2 recent vs long-standing co-clients", "long-standing co-client"), f)
f <- "sample_variants_results.csv"
add("sensitivity", "First-version linkage (1,514 events)", get(f, "V first-version linkage", "same"), f)
add("sensitivity", "First-version linkage, omit 30 June and 1 July", get(f, "V first-version linkage | omit 30 June and 1 July", "same"), f)
add("sensitivity", "Player-level linkage, baseline labels only", get(f, "V player-level linkage, baseline labels", "same"), f)
add("sensitivity", "Main sample, omit 30 June and 1 July", get(f, "V MAIN: + carry-forward labels | omit 30 June and 1 July", "same"), f)
add("sensitivity", "Main sample, no club fixed effects", get(f, "V MAIN | no club fixed effects", "same"), f)
add("sensitivity", "Main sample, harmonised representative labels", get(f, "V MAIN | harmonised labels", "same"), f)
add("sensitivity", "Adding look-ahead labels from later releases", get(f, "V + look-ahead labels (later releases)", "same"), f)
f <- "choice_set_results.csv"
add("sensitivity", "Tier and affordability controls", get(f, "R4 + tier/affordability fit controls", "same"), f)
add("sensitivity", "Alternatives within a 4.5-fold squad-value gap", get(f, "R4 alternatives within |value gap|<=1.5", "same"), f)
f <- "elo_decomposition_results.csv"
add("sensitivity", "Same 1,643 events, all clubs as alternatives", get(f, "R4b same events, all alternatives, no fit controls", "same"), f)
add("sensitivity", "Same 1,643 events, all clubs, + tier controls", get(f, "R4b same events, all alternatives, fit controls", "same"), f)
add("sensitivity", "Same 1,643 events, alternatives within 200 Elo", get(f, "R4b restricted alternatives, no fit controls", "same"), f)
add("sensitivity", "Same 1,643 events, within 200 Elo, + tier controls", get(f, "R4b restricted alternatives, fit controls", "same"), f)
d <- rf("league_results.csv")
for (p in list(c("first-version sample: baseline (agency log count, local nationality)", "League model, first-version sample (per doubling)"),
               c("main sample: + nationality x destination-league FE", "League model, main sample, nationality-by-league effects (per doubling)"))) {
  r <- d[model == p[1] & term == "agency_log"]
  rows[[length(rows) + 1]] <- data.table(block = "league", label = p[2], events = as.integer(r$events), odds_ratio = r$or_per_doubling, lo = r$lo_doubling, hi = r$hi_doubling, source = "league_results.csv")
}
r <- d[model == "main sample: agency share + nationality x league FE" & term == "agency_share"]
rows[[length(rows) + 1]] <- data.table(block = "league", label = "League model, main sample, agency concentration (per 10 pp of clients), nationality-by-league effects", events = as.integer(r$events), odds_ratio = r$odds_ratio, lo = r$label_lo, hi = r$label_hi, source = "league_results.csv")
R <- rbindlist(rows); fwrite(R, file.path(dir, "table_estimates.csv"))

cat("wrote table_estimates.csv to", dir, "\n")
