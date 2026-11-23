# Build the paper's tables from the result tables and, if the paper sources are available, the manuscript and online appendix (.docx and .pdf).
# Usage: Rscript code/report/render_manuscript.R <paper_dir> <results_dir> <data_root> <manifest.csv> [output_dir]
#   paper_dir   holds manuscript.md and online_appendix.md (the sources of the paper; not part of this repository)
#   results_dir holds the result tables written by code/study1/, code/study2/ and code/report/make_tables.R
# Always writes paper_tables.md (all tables of the paper and the appendix). The Word and PDF files need pandoc and LibreOffice.
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(TRUE); paper <- args[1]; res <- args[2]; root <- args[3]; manifest <- args[4]; outdir <- if (length(args) > 4) args[5] else "."
script_dir <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE))
source(file.path(script_dir, "docx_style.R")); dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
Sys.setlocale("LC_TIME", "C")
R <- function(f) fread(file.path(res, f), na.strings = c("", "NA"))
n0 <- function(x) format(as.integer(x), big.mark = ",", trim = TRUE)
f2 <- function(x) sprintf("%.2f", x)
grid <- function(x) paste0("| ", paste(x, collapse = " | "), " |")
table_md <- function(title, headers, rows, note) paste(c(sprintf("**%s**", title), "", grid(headers), grid(c("---", rep("---:", length(headers) - 1))), vapply(rows, grid, ""), "", paste0("*Note.* ", note)), collapse = "\n")
models <- R("model_results.csv")
mrow <- function(mod, trm, caption, change = 1) { r <- models[models$model == mod & models$term == trm]; stopifnot(nrow(r) == 1)
  c(caption, n0(r$events), f2(r$odds_ratio^change), sprintf("%s–%s", f2(r$label_lo^change), f2(r$label_hi^change))) }

# ---- Table 1: sample flow
e <- fread(file.path(root, "perf_extended", "transfer_performance.csv"), na.strings = c("", "NA"), colClasses = list(character = c("player_id", "origin_club", "destination_club", "snapshot_club")))
club <- fread(file.path(root, "perf_extended", "within_league_candidates.csv"), na.strings = c("", "NA"), colClasses = list(character = c("candidate_club")))
p <- fread(file.path(root, "perf_extended", "player_performance.csv"), na.strings = c("", "NA"), colClasses = list(character = c("club_id")))
tf <- function(x) x %in% c("True", "TRUE", "true", TRUE)
e[, known_agent := tf(known_agent)]; e[, likely_return := tf(likely_return)]; e[, baseline_match := tf(baseline_match)]
e[, conversion := known_agent & snapshot_club == destination_club]; e[, screened := known_agent & !likely_return & !conversion]
coverage <- unique(p[!is.na(agent_label), .(summer, club_id)])
chosen <- merge(merge(club[chosen == 1, .(event_id, summer, candidate_club)], coverage, by.x = c("summer", "candidate_club"), by.y = c("summer", "club_id")), e[, .(event_id, screened)], by = "event_id")
cs <- chosen[screened == TRUE, .N, by = summer][order(summer)]; stopifnot(sum(cs$N) == 2000)
sample <- lapply(sort(unique(e$summer)), function(y) { g <- e[summer == y]; c(y, n0(nrow(g)), n0(sum(g$baseline_match)), n0(sum(g$known_agent)), n0(sum(g$screened)), n0(cs[summer == y, N])) })
sample[[length(sample) + 1]] <- c("Total", n0(nrow(e)), n0(sum(e$baseline_match)), n0(sum(e$known_agent)), n0(sum(e$screened)), n0(sum(cs$N)))
tables <- list(SAMPLE_TABLE = table_md("Table 1. Transfer sample in Europe’s top leagues, summers 2023–25", c("Summer", "Recorded moves", "Mover in release", "Named representative", "Return screen", "Club sample"), sample,
  "Counts are transfer events. Named representative includes labels carried forward from earlier pre-window releases. The return screen drops flagged likely returns and loan-to-buy conversions and retains new loans."))
headers <- c("Specification", "Events", "Odds ratio", "95% CI")
cl <- list(c("Club baseline", "Roster coverage"), c("Club adjusted", "Coverage, nationality and football fit"), c("Club fixed effects", "Destination club fixed effects"),
           c("Club by summer fixed effects", "Club-by-summer fixed effects"), c("Club past flows", "Club fixed effects and three-year flows"), c("Club by summer past flows", "Club-by-summer fixed effects and flows"),
           c("Club past flows excluding turn of July", "Omit 30 June and 1 July, with prior flows"), c("Club player quality by Elo", "Player quality by destination Elo"),
           c("Club including unobserved rosters", "Include clubs with unobserved agency rosters"), c("Club broad sample", "Include flagged likely returns"), c("Club excluding turn of July", "Omit 30 June and 1 July, without prior flows"))
club_rows <- c(lapply(cl, function(x) mrow(x[1], "same", x[2])), list(mrow("Club dose", "co_client_count_log", "Double one plus the co-client count", log(2))))
tables$CLUB_TABLE <- table_md("Table 2. Association with choosing a club", headers, club_rows, "Intervals are clustered by the mover’s representative label. The reference is a club without an observed co-client, except in the count specification.")
route_rows <- list(mrow("League fixed routes", "agency_log", "League client count, all routes", log(2)), mrow("League fixed cross routes", "agency_log", "League client count, between-league moves", log(2)),
                   mrow("League fixed routes", "local_nationality", "Player nationality matches league"), mrow("Club route interaction", "same", "Shared label, within-league moves"),
                   mrow("Club route interaction", "between league total", "Shared label, between-league moves"), mrow("Club route interaction", "same_between", "Ratio of between- to within-league association"))
app_tab <- list()
app_tab$APP_ROUTE <- table_md("Table B10. Nationality and agency links by route", headers, route_rows, "League client count estimates are for doubling one plus the count. The three club estimates derive from the same interaction model, with destination club fixed effects. Intervals cluster by representative label.")
rev <- R("table_estimates.csv")
revrows <- function(b) lapply(seq_len(nrow(rev[block == b])), function(i) { r <- rev[block == b][i]; c(r$label, n0(r$events), f2(r$odds_ratio), sprintf("%s–%s", f2(r$lo), f2(r$hi))) })
one <- function(lbl, show = lbl) { r <- rev[label == lbl]; stopifnot(nrow(r) == 1); c(show, n0(r$events), f2(r$odds_ratio), sprintf("%s–%s", f2(r$lo), f2(r$hi))) }
dd <- models[model == "Club dose" & term == "co_client_count_log"]
dose_row <- c("Co-client count (per doubling of one plus the count)", n0(dd$events), f2(dd$odds_ratio^log(2)), sprintf("%s–%s", f2(dd$label_lo^log(2)), f2(dd$label_hi^log(2))))
grp <- function(x) c(paste0("*", x, "*"), "", "", "")
ci <- function(r) sprintf("%s (%s–%s)", f2(r$odds_ratio), f2(r$label_lo), f2(r$label_hi))
t2 <- R("matched_agencies_value_results.csv"); g2 <- function(m, t) { r <- t2[model == m & term == t]; stopifnot(nrow(r) == 1); ci(r) }
plac <- list()
for (v in list(c("Nationality, position, age", "orig"), c("Plus market-value quintiles (added after the first results)", "value"))) for (k in list(c("Any of three", "any"), c("Closest", "one"))) {
  m <- sprintf("Cfull %s %s", k[2], v[2]); plac[[length(plac) + 1]] <- c(if (k[2] == "any") v[1] else "", k[1], g2(m, "own"), g2(m, "matched"), g2(m, "own minus matched")) }
tables$PLACEBO_TABLE <- table_md("Table 3. Own representative and matched agencies", c("Match profile", "Benchmark", "Own agency", "Matched agency", "Ratio of odds ratios"), plac, "Odds ratios (95% CI) for a co-client at the destination club from conditional logits with destination club fixed effects and three-year flows, on the 1,674 events with matched agencies defined. In each row the own-agency and matched-agency indicators enter one model, so the ratio compares two estimates from the same model, with 1 as the null value. Intervals cluster by representative label. Any of three: any of the three agencies of similar size with the most similar client profile has a client at the club; Closest: the single most similar agency. The profile is the agency’s client mix by nationality, position and age, with market-value quintiles added in the second block (Online Appendix B).")
grad <- list(grp("Strength of the channel"), dose_row, one("Recently placed co-client"), one("Long-standing co-client"),
  grp("Contract status"), one("Contract ends by 30 June of the window year"), one("Contract runs one more year"), one("Contract runs longer"), one("Ratio: expiring contract to longer contract"), one("Excluding moves to a former club"))
contract_app <- list(grp("By date and fee"), one("30 June or 1 July, no fee"), one("30 June or 1 July, fee paid"), one("Other dates, no fee"), one("Other dates, fee paid"),
  grp("Own and matched agencies by contract status"), one("Own agency, contract expiring"), one("Twin agencies, contract expiring (placebo)", "Matched agencies, contract expiring"),
  one("Difference, contract expiring: own minus twin agencies", "Ratio of odds ratios, contract expiring"), one("Own agency, contract runs longer"),
  one("Twin agencies, contract runs longer (placebo)", "Matched agencies, contract runs longer"), one("Difference, contract runs longer: own minus twin agencies", "Ratio of odds ratios, contract runs longer"))
app_tab$APP_GRAD <- table_md("Table B9. When the association is larger: exploratory analyses", headers, grad, "Odds ratios for a co-client at the destination club from conditional logits with destination club fixed effects and three-year flows, main sample unless the event count differs. Ratios and differences have 1 as the null value. Intervals cluster by representative label. Contract status comes from the pre-window release. The contract rows describe overlapping moves and are not independent tests. A recently placed co-client arrived by transfer within three years.")
app_tab$APP_ROBUST <- table_md("Table B11. Sample, choice-set and league sensitivity", headers, c(revrows("sensitivity"), revrows("league")), "Club rows are odds ratios for a co-client at the destination club with destination club fixed effects and three-year flows unless otherwise indicated. First-version linkage requires the mover on the origin club’s roster; player-level linkage reads the label from the mover’s own row. Look-ahead labels come from releases dated after the window. League rows are for doubling one plus the client count, except the concentration row (per ten percentage points of the agency’s clients).")
fill <- function(text, tabs) { for (m in names(tabs)) text <- gsub(sprintf("[[%s]]", m), tabs[[m]], text, fixed = TRUE); text }
build <- function(text, name, title, widthf) {
  md <- file.path(outdir, paste0(name, ".md")); writeLines(text, md, useBytes = TRUE)
  docx <- file.path(outdir, paste0(name, ".docx")); stopifnot(system2("pandoc", c(shQuote(md), "-o", shQuote(docx), paste0("--resource-path=", shQuote(outdir)))) == 0)
  style_docx(docx, title, widthf); docx
}

# ---- online appendix
q <- R("matched_agencies_quality.csv"); cv <- R("matched_agencies_coverage.csv"); ex <- R("matched_agencies_worked_example.csv")
cap <- function(s) paste(vapply(strsplit(s, ",")[[1]], function(x) { x <- trimws(x); paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2))) }, ""), collapse = ", ")
at <- app_tab
at$APP_COVERAGE <- table_md("Table B2. Matched-agency selection by summer window", c("Summer", "Representatives with at least 3 clients", "With three twins", "Median matched-agency size relative to focal"),
  lapply(seq_len(nrow(cv)), function(i) c(cv$summer[i], cv[["labels with at least 3 clients"]][i], cv[["focal labels with twins"]][i], f2(cv[["median twin/focal size"]][i]))),
  "Twin size is the mean client count of the three twins divided by the focal agency’s client count.")
at$APP_BALANCE <- table_md("Table B1. Mean cosine similarity between a representative’s client profile and its comparison agencies", c("Profile block", "Three twins", "Other agencies of similar size"),
  lapply(seq_len(nrow(q)), function(i) c(paste0(toupper(substring(q$block[i], 1, 1)), substring(q$block[i], 2)), f2(q$twin[i]), f2(q$eligible_pool[i]))),
  "Averages over focal representatives and windows. Similarity is computed within each block of the profile (nationality, position, age band). The comparison column averages over all other agencies that meet the size rule.")
fl <- R("matched_agencies_sample_flow.csv"); ct <- R("matched_agencies_crosstab.csv"); cs_ <- R("matched_agencies_choice_sets.csv")
at$APP_FLOW <- table_md("Table B3. From the club model to the matched-agency model", c("Step", "Events"),
  lapply(which(startsWith(fl$step, "Events") | fl$events != 0), function(i) c(fl$step[i], if (startsWith(fl$step[i], "Events")) n0(fl$events[i]) else sub("-", "−", n0(fl$events[i])))),
  "Events are transfers in the main club model. Twins require a focal representative with at least three clients on the pre-window rosters of the covered clubs. No further events drop out.")
mf <- fread(manifest, colClasses = "character")
roles <- c(`240` = "Summer 2023 baseline; earlier-label source for summers 2024–25 and January 2024–25", `325` = "January 2024 baseline (December 2023 rosters); earlier-label source for summers 2024–25 and January 2025; look-ahead check, summer 2023",
  `355` = "Earlier-label source for summers 2024–25 and January 2025; look-ahead check, summer 2023", `435` = "Summer 2024 baseline; earlier-label source for summer 2025 and January 2025; look-ahead check, summer 2023",
  `547` = "January 2025 baseline (December 2024 rosters); earlier-label source for summer 2025; look-ahead check, summer 2024", `566` = "Earlier-label source for summer 2025; look-ahead check, summer 2024",
  `600` = "Summer 2025 baseline; look-ahead check, summer 2024", `603` = "February 2026 backcast (sensitivity); look-ahead check, summer 2025",
  `606` = "February 2026 backcast, second release (sensitivity); look-ahead check, summer 2025", `660` = "Look-ahead check, summer 2025")
fmtd <- function(s) { d <- as.Date(substr(s, 6, 16), "%d %b %Y"); paste(as.integer(format(d, "%d")), format(d, "%B %Y")) }
at$APP_VERS <- table_md("Table A1. Versions of players.csv used", c("Version", "Archived file timestamp", "SHA-256 (first 12)", "Analytical role"),
  lapply(seq_len(nrow(mf)), function(i) c(mf$version[i], fmtd(mf$last_modified[i]), substr(mf$sha256[i], 1, 12), roles[[mf$version[i]]])),
  "Earlier-label sources are releases dated before the window and other than the baseline, used to fill a missing label. Look-ahead releases are dated after the window and used only in a sensitivity analysis.")
at$APP_OTHER <- table_md("Table A2. Other inputs", c("Input", "Source", "Role"), list(
  c("transfers table (transfers.gz)", "Cariboo, transfermarkt-datasets (github.com/dcaribou/transfermarkt-datasets); copy SHA-256 90326983dc3e", "Transfer events, move dates, fees and earlier club-to-club flows"),
  c("games table (games.gz)", "Cariboo, transfermarkt-datasets (github.com/dcaribou/transfermarkt-datasets); copy SHA-256 142561989017", "Prior-season points, league membership of clubs"),
  c("appearances table", "Cariboo, transfermarkt-datasets (GitHub)", "Pre-window league appearances and minutes"),
  c("Club Elo ratings", "xgabora/Club-Football-Match-Data (GitHub, data/EloRatings.csv), from ClubElo", "Pre-window Elo of origin and candidate clubs"),
  c("EA player ratings", "FIFA 23 extract (github.com/MithileshAnavkar/FOOTEM, Data/players_fifa23.csv); FC 24 extract (github.com/stephensoccer/EA-Sports-FC-24-Players-Data); sofifa data set (kaggle.com/datasets/jmacd745/sofifa-data-set)", "Player rating gap to the candidate club’s squad")),
  "Retrieval dates of the transfer and games copies were not recorded; the checksums identify the files used. Source addresses are in rebuild_from_raw/README.md.")
# ---- appendix tables B8 and B9 (extra checks)
vrows <- list()
for (v in list(c("Nationality, position, age (main match)", "orig"), c("Plus market-value quintiles", "value"), c("Main match, mover left out of focal profile", "loo"), c("Plus market-value quintiles, mover left out", "loo_value"))) for (k in list(c("Any of three", "any"), c("Closest", "one"))) {
  m <- sprintf("Cfull %s %s", k[2], v[2]); vrows[[length(vrows) + 1]] <- c(if (k[2] == "any") v[1] else "", k[1], n0(t2[model == m & term == "own"]$events), g2(m, "own"), g2(m, "matched"), g2(m, "own minus matched")) }
at$APP_CONTRACT <- table_md("Table B8. Further analyses by date, fee and contract status", headers, contract_app,
  "Odds ratios for a co-client at the destination club from conditional logits with destination club fixed effects and three-year flows. The date and fee rows use the 2,000-event club sample. The contract-specific rows use events with a recorded contract end date in the two groups. Ratios of odds ratios compare two estimates from the same model, with 1 as the null value. Rows describe overlapping moves and are not independent tests, and the difference between the two contract-status ratios was not tested.")
wm <- fread(file.path(dirname(manifest), "winter_models.csv")); wm <- wm[cluster == "agent_key" & specification == "flow_adjusted"]
wrow <- function(lbl, key) { x <- wm[sample == key]; stopifnot(nrow(x) == 1); c(lbl, n0(x$events), f2(x$odds_ratio), sprintf("%s–%s", f2(x$lo), f2(x$hi))) }
at$APP_VALUE <- table_md("Table B4. Own and matched agencies when the match includes player value", c("Match profile", "Benchmark", "Events", "Own agency", "Matched agency", "Ratio of odds ratios"), vrows,
  "Odds ratios (95% CI) for a co-client at the destination club, from conditional logits with destination club fixed effects, prior flows and the controls of Table 2; intervals cluster by representative. The first block reproduces Table 3. Market-value quintiles are formed within summer from the pre-window release. Leaving the mover out removes the focal player from the focal agency’s profile before matching and requires at least four clients.")
t1 <- R("move_type_results.csv"); t3 <- R("label_audit_results.csv"); t4 <- R("club_resources_results.csv")
crow <- function(lab, t, m, trm = "same") { r <- t[model == m & term == trm]; stopifnot(nrow(r) == 1); c(lab, n0(r$events), f2(r$odds_ratio), sprintf("%s–%s", f2(r$label_lo), f2(r$label_hi))) }
checks <- list(grp("Move type (loan-like: no fee and back at the origin club within 14 months)"),
  crow("Moves with a fee (809 events)", t1, "L1 by type", "paid"), crow("Loan-like moves (548 events)", t1, "L1 by type", "loan-like"), crow("Other no-fee moves (643 events)", t1, "L1 by type", "unpaid, kept"), crow("Without loan-like moves", t1, "L2 excluding loan-like"),
  grp("Labels and coverage"), crow("Baseline labels only (no carry-forward)", t3, "B3 baseline-only labels, all"), crow("Summers 2023–24 only", t3, "B2 extended, summers 2023-24"), crow("Baseline labels only, summers 2023–24", t3, "B4 baseline-only labels, summers 2023-24"),
  crow("Summer 2023", t3, "B5 extended, summer 2023"), crow("Summer 2024", t3, "B5 extended, summer 2024"), crow("Summer 2025", t3, "B5 extended, summer 2025"),
  crow("Candidate clubs with at least 15 named players", t3, "B6 candidate clubs with >= 15 named players"),
  grp("Club resources (squad value within league and summer)"),
  crow("Clubs in the lower half", t4, "R1 same x lower half of league by squad value", "lower-valued clubs"), crow("Clubs in the upper half", t4, "R1 same x lower half of league by squad value", "higher-valued clubs"),
  crow("Ratio: lower half to upper half", t4, "R1 same x lower half of league by squad value", "lower minus higher"))

f2h <- function(x) sprintf("%.2f", x)
# ---- Study 2 (English FA records): Table 4 and Table B13, from the stored estimates of code/study2/
fc <- R("fa_choice_results.csv"); fau <- R("fa_audit_results.csv")
fa_c <- function(lab, rn, mod, trm, fe = TRUE) { k <- which(substr(fc$run, 1, 1) == rn & startsWith(fc$model, mod) & fc$term == trm & fc$fixed_effects == fe); stopifnot(length(k) == 1); r <- fc[k]
  c(lab, n0(r$events), f2h(r$odds_ratio), sprintf("%s–%s", f2h(r$lo), f2h(r$hi))) }
fa_a <- function(lab, rn, sp, trm, fe = "club") { k <- which(startsWith(fau$run, rn) & fau$spec == sp & fau$term == trm & fau$fe == fe & fau$cluster == "agent_key"); stopifnot(length(unique(fau$odds_ratio[k])) == 1); r <- fau[k[1]]
  c(lab, n0(r$events), f2h(r$odds_ratio), sprintf("%s–%s", f2h(r$lo), f2h(r$hi))) }
ci2 <- function(r) sprintf("%s (%s–%s)", f2h(r$odds_ratio), f2h(r$lo), f2h(r$hi))
fa_rows <- list(fa_c("Any prior dealing (primary)", "1", "A", "any_prior"), fa_c("Earlier dealings on the club’s side", "1", "B", "club_side"), fa_c("Earlier dealings on a player’s side only", "1", "B", "player_only"),
  fa_c("Dealing in the previous period", "1", "E", "any_prev"), fa_c("Dealing two or more periods earlier", "1", "E", "any_old"), fa_c("Focal deal: intermediary not also on the club’s side", "2", "A", "any_prior"),
  fa_a("Prior club-side work on a renewal or registration update (2020–21 to 2022–23)", "ID-matched 2020-21..2022-23", "R", "club_renew"),
  fa_a("History linked by registration number at both ends (see note)", "ID at both ends (history", "A", "any_prior"), fa_a("Without the 2019–20 signings", "primary without 2019-20", "A", "any_prior"),
  fa_a("One observation per signing", "one event per signing (first-listed", "A", "any_prior"), fa_a("Club-by-period fixed effects", "primary periods, club-by-period", "A", "any_prior", "clubperiod"),
  fa_c("Name matching, 2019–20 to 2025–26", "4", "A", "any_prior"))
nofe <- fau[which(startsWith(fau$run, "primary periods, alternatives = clubs with an incoming row") & fau$fe == "none")]; stopifnot(nrow(nofe) == 1)
nofe_old <- fc[which(substr(fc$run, 1, 1) == "1" & startsWith(fc$model, "A") & fc$fixed_effects == FALSE)]; stopifnot(nrow(nofe_old) == 1)
id_wo <- fau[which(startsWith(fau$run, "ID at both ends, without") & fau$spec == "A" & fau$fe == "club")]; pr_wo <- fau[which(startsWith(fau$run, "primary without 2019-20") & fau$spec == "A" & fau$fe == "club")]
tables$FA_TABLE <- table_md("Table 5. Prior dealings between the deal’s intermediary and the destination club, English FA records", c("Specification", "Observations", "Odds ratio", "95% CI"), fa_rows,
  sprintf("Conditional logits comparing the club joined with the other clubs of the same division in the period’s report, with destination club fixed effects (the last row also uses name matching for all periods); intervals cluster by intermediary. Exposure: an earlier reporting period in which the deal’s intermediary had dealt with the club, on the club’s or a player’s side, in a transaction for a different player. The primary estimate without club fixed effects is %s. Observations are signing–intermediary pairs (1,381 signings; 70 signings have two player-side intermediaries). The 2019–20 signings stay in the registration-number row but have no ID-linked history, so they add no variation to it; dropping them from that model gives %s, and dropping them from the primary model gives %s. The renewal row is a model with three exposures (renewal or other update, other club-side registrations, player side only), and the update category may include changes other than renewals.",
          ci2(nofe), ci2(id_wo), ci2(pr_wo)))
# ---- Study 2 sample by period (Table 4) and own versus matched intermediaries (Table 6)
ie <- R("fa_choice_informative_events.csv")[startsWith(run, "1 primary") & exposure == "any_prior"]
c1 <- R("fa_audit_counts.csv")[item == "1c"]
smp <- lapply(seq_len(nrow(ie)), function(k) c(ie$period[k], n0(c1[period == ie$period[k], signings]), n0(ie$events[k]), n0(ie$informative_events[k]), n0(ie$events_where_chosen_club_exposed[k]),
  sprintf("%.1f", 100 * ie$events_where_chosen_club_exposed[k] / ie$informative_events[k])))
smp[[length(smp) + 1]] <- c("All four periods", n0(sum(c1[period %in% ie$period, signings])), n0(sum(ie$events)), n0(sum(ie$informative_events)), n0(sum(ie$events_where_chosen_club_exposed)),
  sprintf("%.1f", 100 * sum(ie$events_where_chosen_club_exposed) / sum(ie$informative_events)))
tables$FA_SAMPLE_TABLE <- table_md("Table 4. English FA sample by reporting period", c("Period", "Signings", "Observations", "Informative choice sets", "Chosen club exposed", "Chosen club exposed (% of informative sets)"), smp,
  "Observations are signing–intermediary pairs (70 signings have two player-side intermediaries). A choice set is informative if it contains both a club with earlier dealings with the deal’s intermediary and a club without; chosen club exposed counts the informative sets in which the club joined had earlier dealings with the intermediary.")
fb <- R("fa_balance_results.csv")[item == "B1"]
fbg <- function(rn, tm) { x <- fb[which(fb$run == rn & fb$term == tm)]; stopifnot(nrow(x) == 1); c(sprintf("%.2f (%.2f–%.2f)", x$odds_ratio, x$lo, x$hi), n0(x$events)) }
fbrow <- function(lab, rn) c(lab, fbg(rn, "own")[2], fbg(rn, "own")[1], fbg(rn, "matched")[1], fbg(rn, "ratio own/matched")[1])
tables$FA_MATCH_TABLE <- table_md("Table 6. Own intermediary and matched intermediaries, English FA records", c("Benchmark", "Observations", "Own intermediary", "Matched intermediary", "Ratio of odds ratios"),
  list(fbrow("Any of three matched intermediaries", "own + any of three matched intermediaries"), fbrow("Closest matched intermediary", "own + closest matched intermediary")),
  "Odds ratios (95% CI) from conditional logits with destination club fixed effects and standard errors clustered by the deal’s intermediary. Own and matched exposures enter the same model. Registration numbers identify intermediaries in 2020–21 to 2022–23, with history from 2019–20; 191 intermediaries have a matched set. Matched intermediaries have dealt with at least three clubs, a similar number of clubs (within a factor of two) and the closest profile of divisions and club-side share (Online Appendix B9).")
fa_rows_b <- c(fa_rows, list(fa_c("Name matching, ambiguous names removed", "6", "A", "any_prior"), fa_c("Choice sets by tier group", "5", "A", "any_prior"), fa_c("Controlling for the club’s number of earlier intermediaries", "1", "F", "any_prior"),
  fa_a("Renewal-or-update model: other club-side registrations", "ID-matched 2020-21..2022-23", "R", "club_other"), fa_a("Renewal-or-update model: player side only", "ID-matched 2020-21..2022-23", "R", "player_only")))
at$APP_FA <- table_md("Table B12. Study 2: specifications, English FA records", c("Specification", "Observations", "Odds ratio", "95% CI"), fa_rows_b,
  sprintf("Odds ratios from conditional logits with destination club fixed effects; intervals cluster by intermediary. Registration numbers identify intermediaries in 2019–20 to 2022–23 and normalised names elsewhere. In the row “history linked by registration number at both ends” the 2019–20 signings stay in the sample but have no ID-linked history, so they add no variation; dropping them from that model gives %s, and dropping them from the primary model gives %s. “One observation per signing” keeps the first-listed player-side intermediary. The estimates without club fixed effects are not shown: report-appearance choice sets included foreign clubs listed in two reports, which inflated them (%s against %s for the primary specification once corrected).",
          ci2(id_wo), ci2(pr_wo), f2h(nofe_old$odds_ratio), f2h(nofe$odds_ratio)))

# ---- Tables B10 to B12 (co-client background, leave-out agencies, by league)
bgr <- R("co_client_background_results.csv"); bgc <- R("co_client_background_counts.csv")[cat == "same_only"]
gb <- function(m, t) { x <- bgr[model == m & term == t]; stopifnot(nrow(x) == 1); ci(x) }
bg <- list()
for (d in list(c("nationality", "Nationality"), c("shared earlier club", "Shared earlier club"), c("position", "Position"), c("age", "Age band"))) { m <- sprintf("X1 %s four categories", d[1])
  bg[[length(bg) + 1]] <- c(d[2], gb(m, "same-background co-clients only"), gb(m, "different-background co-clients only"), gb(m, "both"), gb(m, "different only minus same only (ratio)"), n0(bgc[dimension == d[1]]$events_with_same_only_and_diff_only)) }
for (d in list(c("nationality", "Nationality, one co-client"), c("shared earlier club", "Shared earlier club, one co-client"), c("position", "Position, one co-client"), c("age", "Age band, one co-client"))) { m <- sprintf("X2 %s exactly one co-client", d[1])
  bg[[length(bg) + 1]] <- c(d[2], gb(m, "one same-background co-client"), gb(m, "one different-background co-client"), "–", gb(m, "different minus same (ratio)"), "–") }
m3 <- "X3 nationality by prior transfers"
bg[[length(bg) + 1]] <- c("Nationality, no recorded transfers", gb(m3, "same-background only, no prior transfers"), gb(m3, "different-background only, no prior transfers"), gb(m3, "both, no prior transfers"), "–", "–")
bg[[length(bg) + 1]] <- c("Nationality, recorded transfers", gb(m3, "same-background only, prior transfers"), gb(m3, "different-background only, prior transfers"), gb(m3, "both, prior transfers"), "–", "–")
at$APP_BACKGROUND <- table_md("Table B5. Association by co-client background", c("Compared on", "Only similar co-clients", "Only different co-clients", "Both", "Ratio: different to similar", "Events with both single types"), bg,
  "Odds ratios (95% CI) for candidate clubs with the stated co-clients relative to clubs with none, from conditional logits with destination club fixed effects, prior flows and the controls of Table 2; intervals cluster by representative. The first block uses 2,000 events. “One co-client” rows keep only clubs with at most one co-client and use 1,723 events (see B12). A co-client with unrecorded nationality counts as different.")
cr <- R("agency_concentration_results.csv")[term == "same"]
cc <- function(lab, m) { x <- cr[model == m]; stopifnot(nrow(x) == 1); c(lab, n0(x$events), f2(x$odds_ratio), sprintf("%s–%s", f2(x$label_lo), f2(x$label_hi))) }
rk <- cr[model %in% sprintf("G1 leave out agency rank %d", 1:10)]; stopifnot(nrow(rk) == 10)
conc <- list(cc("Main sample", "G0 main sample"), c("Leave out one of the ten largest agencies (range of ten estimates)", sprintf("%s–%s", n0(min(rk$events)), n0(max(rk$events))), sprintf("%s–%s", f2(min(rk$odds_ratio)), f2(max(rk$odds_ratio))), "–"),
  cc("Leave out the 3 largest agencies", "G2 leave out the 3 largest agencies"), cc("Leave out the 5 largest agencies", "G2 leave out the 5 largest agencies"), cc("Leave out the 10 largest agencies", "G2 leave out the 10 largest agencies"),
  cc("Leave out the 20 largest agencies", "G2 leave out the 20 largest agencies"), cc("Only the 20 largest agencies", "G3 only the 20 largest agencies"))
at$APP_CONC <- table_md("Table B6. Leaving out the largest agencies", c("Sample", "Events", "Odds ratio", "95% CI"), conc, "Co-client at the destination club, conditional logit with destination club fixed effects, prior flows and the controls of Table 2; intervals cluster by representative. Agencies are ranked by labelled clients on the pre-window rosters.")
lh <- R("league_heterogeneity.csv")
at$APP_LEAGUE <- table_md("Table B7. Co-client association by destination league", c("League", "Events", "Informative events", "Connected clubs chosen", "Odds ratio", "95% CI"),
  lapply(seq_len(nrow(lh)), function(i) c(sub("Turkey", "Türkiye", lh$country[i]), n0(lh$events[i]), n0(lh$informative_events[i]), n0(lh$chosen_connected[i]), f2(lh$odds_ratio[i]), sprintf("%s–%s", f2(lh$lo[i]), f2(lh$hi[i])))),
  sprintf("Event-stratified conditional logits estimated separately by destination league with destination club fixed effects, prior flows and the controls of Table 2; intervals cluster by representative. Informative events have a connected and an unconnected alternative. Heterogeneity across leagues: Q = %.1f, %d degrees of freedom, p = %.2f.", lh$Q[1], lh$Q_df[1], lh$Q_p[1]))
app_w <- function(i, n) { if (n == 6 && i == 11) return(c(1.2, 1.05, 1.05, 1.15, 1.05, .9)); if (n == 6 && i == 13) return(c(1.3, .8, 1.1, 1.3, .9, 1.1)); if (n == 6) return(c(1.7, .9, .6, 1.1, 1.1, 1.1))
  if (n == 4 && i >= 7) return(c(3.6, .8, 1.0, 1.1)); if (n == 4 && i == 0) return(c(.8, 1.3, 1.1, 3.3)); if (n == 4) return(c(1.0, 2.0, 1.6, 1.9))
  if (n == 3) return(if (i == 1) c(1.8, 2.6, 2.1) else c(2.3, 1.6, 2.6)); if (n == 2) return(c(5.4, 1.0)); if (i == 5) c(1.5, 1.0, 1.4, 1.2, 1.4) else c(.9, .6, 1.5, 1.7, 1.8) }
# The code and result files say "twin"; the paper says "matched agency".
rename <- function(t) { for (p in list(c("Twin-agency matching", "Matched-agency selection"), c("twin-agency placebo", "matched-agency comparison"), c("The placebo result therefore", "The matched-agency result therefore"),
  c("What the placebo does and does not control for", "What the matched-agency comparison does and does not control for"), c("twin presence", "matched-agency presence"), c("Twin presence", "Matched-agency presence"),
  c("twin indicator", "matched-agency indicator"), c("twin model", "matched-agency model"), c("twin sample", "matched-agency sample"), c("twin comparison", "matched-agency comparison"),
  c("three-twin", "three-agency"), c("twins", "matched agencies"), c("Twins", "Matched agencies"), c("twin", "matched agency"), c("Twin", "Matched agency"), c("placebo", "comparison"))) t <- gsub(p[1], p[2], t, fixed = TRUE); t }
at <- lapply(at, rename)
have_paper <- file.exists(file.path(paper, "manuscript.md")) && file.exists(file.path(paper, "online_appendix.md"))
writeLines(c("# Tables of the paper and the Online Appendix, generated from the result tables", "", unlist(lapply(c(tables[c("SAMPLE_TABLE", "CLUB_TABLE", "PLACEBO_TABLE", "FA_SAMPLE_TABLE", "FA_TABLE", "FA_MATCH_TABLE")], at[order(vapply(at, function(x) { l <- sub("^\\*\\*Table ([A-Z])([0-9]+)\\..*", "\\1 \\2", strsplit(x, "\n")[[1]][1]); sprintf("%s%03d", substr(l, 1, 1), as.integer(substring(l, 3))) }, ""))]), function(x) c(x, "")))), file.path(outdir, "paper_tables.md"), useBytes = TRUE)
if (!have_paper) { cat("wrote", file.path(outdir, "paper_tables.md"), "(the paper sources were not found, so no Word files were built)\n"); quit(save = "no") }
main_w <- function(i, n) if (n == 6 && i == 0) c(.85, 1.0, 1.1, 1.2, 1.1, 1.25) else if (n == 6) c(.95, .8, 1.0, 1.1, 1.0, 1.6) else if (n == 5) c(1.6, .9, 1.2, 1.2, 1.2) else c(3.58, .72, .85, 1.35)
d1 <- build(fill(paste(readLines(file.path(paper, "manuscript.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n"), tables), "Shared_Representation_and_Player_Mobility", "Who Joins Which Club? Football Agents and Player Recruitment in European Men’s Football", main_w)

d2 <- build(fill(paste(readLines(file.path(paper, "online_appendix.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n"), at), "Online_Appendix_Twin_Matching", "Online Appendix", app_w)
for (d in c(d1, d2)) system2("soffice", c("--headless", "--convert-to", "pdf", "--outdir", shQuote(outdir), shQuote(d)), stdout = FALSE, stderr = FALSE)
cat("wrote", d1, "and", d2, "\n")
