# Build the paper's tables from the result tables and, if the paper sources are available, the manuscript and online appendix (.docx and .pdf).
# Usage: Rscript R/report/render_manuscript.R <paper_dir> <results_dir> <data_root> <manifest.csv> [output_dir]
#   paper_dir   holds manuscript.md and online_appendix.md (the sources of the paper; not part of this repository)
#   results_dir holds the result tables written by R/analysis/ and R/report/make_tables.R
# Always writes paper_tables.md (all tables of the paper and the appendix). The Word and PDF files need pandoc and LibreOffice.
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(TRUE); paper <- args[1]; res <- args[2]; root <- args[3]; manifest <- args[4]; outdir <- if (length(args) > 4) args[5] else "."
script_dir <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE))
source(file.path(script_dir, "..", "lib", "docx_style.R")); dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
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
tables <- list(SAMPLE_TABLE = table_md("Table 1. Transfer sample", c("Summer", "Recorded moves", "Mover in release", "Named representative", "Return screen", "Club sample"), sample,
  "Counts are transfer events. Named representative includes labels carried forward from earlier pre-window releases. The return screen drops flagged likely returns and loan-to-buy conversions and retains new loans."))
headers <- c("Specification", "Events", "Odds ratio", "95% CI")
cl <- list(c("Club baseline", "Roster coverage"), c("Club adjusted", "Coverage, nationality and football fit"), c("Club fixed effects", "Destination club fixed effects"),
           c("Club by summer fixed effects", "Club-by-summer fixed effects"), c("Club past flows", "Club fixed effects and three-year flows"), c("Club by summer past flows", "Club-by-summer fixed effects and flows"),
           c("Club past flows excluding turn of July", "Past flows and omit 30 June and 1 July"), c("Club player quality by Elo", "Player quality by destination Elo"),
           c("Club including unobserved rosters", "Include clubs with unobserved agency rosters"), c("Club broad sample", "Include flagged likely returns"), c("Club excluding turn of July", "Omit all moves on 30 June and 1 July"))
club_rows <- c(lapply(cl, function(x) mrow(x[1], "same", x[2])), list(mrow("Club dose", "co_client_count_log", "Double one plus the co-client count", log(2))))
tables$CLUB_TABLE <- table_md("Table 2. Association with choosing a club", headers, club_rows, "Intervals are clustered by the mover’s representative label. The reference is a club without an observed co-client, except in the count specification.")
route_rows <- list(mrow("League fixed routes", "agency_log", "League client count, all routes", log(2)), mrow("League fixed cross routes", "agency_log", "League client count, between-league moves", log(2)),
                   mrow("League fixed routes", "local_nationality", "Player nationality matches league"), mrow("Club route interaction", "same", "Shared label, within-league moves"),
                   mrow("Club route interaction", "between league total", "Shared label, between-league moves"), mrow("Club route interaction", "same_between", "Ratio of between- to within-league association"))
tables$ROUTE_TABLE <- table_md("Table 4. Nationality and agency links by route", headers, route_rows, "League client count estimates are for doubling one plus the count. The three club estimates derive from the same interaction model, with destination club fixed effects. Intervals cluster by representative label.")
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
tables$GRAD_TABLE <- table_md("Table 5. When the association is larger: exploratory analyses", headers, grad, "Odds ratios for a co-client at the destination club from conditional logits with destination club fixed effects and three-year flows, main sample unless the event count differs. Ratios and differences have 1 as the null value. Intervals cluster by representative label. Contract status comes from the pre-window release. The contract rows describe overlapping moves and are not independent tests. A recently placed co-client arrived by transfer within three years.")
tables$ROBUST_TABLE <- table_md("Table 6. Sample, choice-set and league sensitivity", headers, c(revrows("sensitivity"), revrows("league")), "Club rows are odds ratios for a co-client at the destination club with destination club fixed effects and three-year flows. First-version linkage requires the mover on the origin club’s roster; player-level linkage reads the label from the mover’s own row. Look-ahead labels come from releases dated after the window. League rows are for doubling one plus the client count, except the concentration row (per ten percentage points of the agency’s clients).")
if (normalizePath(res) != normalizePath(outdir)) for (f in c("figure1_main", "figureA1_specifications")) file.copy(file.path(res, paste0(f, ".png")), file.path(outdir, paste0(f, ".png")), overwrite = TRUE)
fill <- function(text, tabs) { for (m in names(tabs)) text <- gsub(sprintf("[[%s]]", m), tabs[[m]], text, fixed = TRUE); text }
build <- function(text, name, title, widthf) {
  md <- file.path(outdir, paste0(name, ".md")); writeLines(text, md, useBytes = TRUE)
  docx <- file.path(outdir, paste0(name, ".docx")); stopifnot(system2("pandoc", c(shQuote(md), "-o", shQuote(docx), paste0("--resource-path=", shQuote(outdir)))) == 0)
  style_docx(docx, title, widthf); docx
}

# ---- online appendix
q <- R("matched_agencies_quality.csv"); cv <- R("matched_agencies_coverage.csv"); ex <- R("matched_agencies_worked_example.csv")
cap <- function(s) paste(vapply(strsplit(s, ",")[[1]], function(x) { x <- trimws(x); paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2))) }, ""), collapse = ", ")
at <- list()
at$APP_COVERAGE <- table_md("Table B1. Matched-agency selection by summer window", c("Summer", "Representatives with at least 3 clients", "With three twins", "Median matched-agency size relative to focal"),
  lapply(seq_len(nrow(cv)), function(i) c(cv$summer[i], cv[["labels with at least 3 clients"]][i], cv[["focal labels with twins"]][i], f2(cv[["median twin/focal size"]][i]))),
  "Twin size is the mean client count of the three twins divided by the focal agency’s client count.")
at$APP_BALANCE <- table_md("Table B3. Mean cosine similarity between a representative’s client profile and its comparison agencies", c("Profile block", "Three twins", "Other agencies of similar size"),
  lapply(seq_len(nrow(q)), function(i) c(paste0(toupper(substring(q$block[i], 1, 1)), substring(q$block[i], 2)), f2(q$twin[i]), f2(q$eligible_pool[i]))),
  "Averages over focal representatives and windows. Similarity is computed within each block of the profile (nationality, position, age band). The comparison column averages over all other agencies that meet the size rule.")
at$APP_EXAMPLE <- table_md("Table B5. Worked example: one focal agency and its three matched agencies, summer 2024", c("Agency", "Clients", "Largest nationalities", "Position mix (attack / midfield / defender / goalkeeper)", "Age mix (21 or under / 22–25 / 26–29 / 30 or over)"),
  lapply(seq_len(nrow(ex)), function(i) c(ex$agency[i], ex$clients[i], cap(ex$top_nationalities[i]), sprintf("%s / %s / %s / %s", ex$forwards[i], ex$midfield[i], ex$defenders[i], ex$goalkeepers[i]), sprintf("%s / %s / %s / %s", ex$age_21_or_under[i], ex$age_22_25[i], ex$age_26_29[i], ex$age_30_plus[i]))),
  "The focal agency is the one nearest the median size among agencies with 18 to 26 clients in 2024. Agencies are anonymised.")
fl <- R("matched_agencies_sample_flow.csv"); ct <- R("matched_agencies_crosstab.csv"); cs_ <- R("matched_agencies_choice_sets.csv")
at$APP_FLOW <- table_md("Table B2. From the club model to the matched-agency model", c("Step", "Events"),
  lapply(which(startsWith(fl$step, "Events") | fl$events != 0), function(i) c(fl$step[i], if (startsWith(fl$step[i], "Events")) n0(fl$events[i]) else sub("-", "−", n0(fl$events[i])))),
  "Events are transfers in the main club model. Twins require a focal representative with at least three clients on the pre-window rosters of the covered clubs. No further events drop out.")
at$APP_CROSS <- table_md("Table B4. Candidate clubs by own-agency and matched-agency presence", c("Candidate club has", "Alternatives", "Share of alternatives", "Chosen", "Chosen as share of alternatives"),
  lapply(seq_len(nrow(ct)), function(i) c(ct$cat[i], n0(ct$alternatives[i]), sprintf("%.1f%%", 100 * ct$share_of_alternatives[i]), n0(ct$chosen[i]), sprintf("%.1f%%", 100 * ct$chosen_share[i]))),
  sprintf("Alternatives in the twin model (1,674 events). Of the 1,674 choice sets, %s contain at least one own-only club, %s at least one twin-only club and %s contain both. The last column is the raw share of clubs in each category that were chosen, before any adjustment.", n0(cs_$with_own_only), n0(cs_$with_twin_only), n0(cs_$with_both_types)))
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
  "Retrieval dates of the transfer and games copies were not recorded; the checksums identify the files used. Source addresses are in data_sources/coach_retry/perf/README.md.")
# ---- appendix tables B8 and B9 (extra checks)
vrows <- list()
for (v in list(c("Nationality, position, age (main match)", "orig"), c("Plus market-value quintiles", "value"), c("Main match, mover left out of focal profile", "loo"), c("Plus market-value quintiles, mover left out", "loo_value"))) for (k in list(c("Any of three", "any"), c("Closest", "one"))) {
  m <- sprintf("Cfull %s %s", k[2], v[2]); vrows[[length(vrows) + 1]] <- c(if (k[2] == "any") v[1] else "", k[1], n0(t2[model == m & term == "own"]$events), g2(m, "own"), g2(m, "matched"), g2(m, "own minus matched")) }
at$APP_CONTRACT <- table_md("Table B6. Further analyses by date, fee and contract status", headers, contract_app,
  "Odds ratios for a co-client at the destination club from conditional logits with destination club fixed effects and three-year flows. The date and fee rows use the 2,000-event club sample. The contract-specific rows use events with a recorded contract end date in the two groups. Ratios of odds ratios compare two estimates from the same model, with 1 as the null value. Rows describe overlapping moves and are not independent tests, and the difference between the two contract-status ratios was not tested.")
wm <- fread(file.path(dirname(dirname(dirname(manifest))), "winter_proxy", "winter_models.csv")); wm <- wm[cluster == "agent_key" & specification == "flow_adjusted"]
wrow <- function(lbl, key) { x <- wm[sample == key]; stopifnot(nrow(x) == 1); c(lbl, n0(x$events), f2(x$odds_ratio), sprintf("%s–%s", f2(x$lo), f2(x$hi))) }
at$APP_WINTER <- table_md("Table B7. January 2024–25: observed December rosters and the February reconstruction", headers,
  list(wrow("Observed December rosters, same 260 moves", "matched 2024–25 December"), wrow("February reconstruction, same 260 moves", "matched 2024–25 backcast"),
       wrow("Observed December rosters, all comparable moves", "2024–25 December"), wrow("Reconstructed January 2026 (version 603)", "2026 backcast")),
  "Flow-adjusted conditional logits within the realised destination league, with labelled roster size, destination club fixed effects and directional three-year club-pair flows. Intervals cluster by representative label. The first two rows use the same screened events and candidate clubs in both representations.")
at$APP_VALUE <- table_md("Table B8. Own and matched agencies when the match includes player value", c("Match profile", "Benchmark", "Events", "Own agency", "Matched agency", "Ratio of odds ratios"), vrows,
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
at$APP_CHECKS <- table_md("Table B9. Further checks: move type, labels and club resources", headers, checks, "Odds ratios for a co-client at the destination club from conditional logits with destination club fixed effects, prior flows and the controls of Table 2; intervals cluster by representative. The main sample has 2,000 events and an odds ratio of 1.37 (1.16–1.62). The three move-type rows come from one model fitted to all 2,000 events with the co-client indicator split by move type, so their Events column repeats the full sample; the counts in brackets are the events of each type. Return moves are read from the full transfer record, which has no loan flag, so the loan-like class is a proxy.")
app_w <- function(i, n) { if (n == 6) return(c(1.7, .9, .6, 1.1, 1.1, 1.1)); if (n == 4 && i >= 7) return(c(3.6, .8, 1.0, 1.1)); if (n == 4 && i == 0) return(c(.8, 1.3, 1.1, 3.3))
  switch(as.character(n), `4` = c(1.0, 2.0, 1.6, 1.9), `3` = if (i == 1) c(1.8, 2.6, 2.1) else c(2.3, 1.6, 2.6), `2` = c(5.4, 1.0), `5` = if (i == 5) c(1.5, 1.0, 1.4, 1.2, 1.4) else c(.9, .6, 1.5, 1.7, 1.8)) }
# The code and result files say "twin"; the paper says "matched agency".
rename <- function(t) { for (p in list(c("Twin-agency matching", "Matched-agency selection"), c("twin-agency placebo", "matched-agency comparison"), c("The placebo result therefore", "The matched-agency result therefore"),
  c("What the placebo does and does not control for", "What the matched-agency comparison does and does not control for"), c("twin presence", "matched-agency presence"), c("Twin presence", "Matched-agency presence"),
  c("twin indicator", "matched-agency indicator"), c("twin model", "matched-agency model"), c("twin sample", "matched-agency sample"), c("twin comparison", "matched-agency comparison"),
  c("three-twin", "three-agency"), c("twins", "matched agencies"), c("Twins", "Matched agencies"), c("twin", "matched agency"), c("Twin", "Matched agency"), c("placebo", "comparison"))) t <- gsub(p[1], p[2], t, fixed = TRUE); t }
at <- lapply(at, rename)
have_paper <- file.exists(file.path(paper, "manuscript.md")) && file.exists(file.path(paper, "online_appendix.md"))
writeLines(c("# Tables of the paper and the Online Appendix, generated from the result tables", "", unlist(lapply(c(tables, at), function(x) c(x, "")))), file.path(outdir, "paper_tables.md"), useBytes = TRUE)
if (!have_paper) { cat("wrote", file.path(outdir, "paper_tables.md"), "(the paper sources were not found, so no Word files were built)\n"); quit(save = "no") }
main_w <- function(i, n) if (i == 0) c(.85, 1.0, 1.1, 1.2, 1.1, 1.25) else if (n == 5) c(1.6, .9, 1.2, 1.2, 1.2) else c(3.58, .72, .85, 1.35)
d1 <- build(fill(paste(readLines(file.path(paper, "manuscript.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n"), tables), "Shared_Representation_and_Player_Mobility", "Who Joins Which Club? Football Agents and Player Recruitment in European Men’s Football", main_w)

d2 <- build(fill(paste(readLines(file.path(paper, "online_appendix.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n"), at), "Online_Appendix_Twin_Matching", "Online Appendix", app_w)
for (d in c(d1, d2)) system2("soffice", c("--headless", "--convert-to", "pdf", "--outdir", shQuote(outdir), shQuote(d)), stdout = FALSE, stderr = FALSE)
cat("wrote", d1, "and", d2, "\n")
