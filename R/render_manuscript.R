# Build the manuscript and online appendix (.docx, optionally .pdf) from the Markdown sources and the estimates.
# Usage: Rscript R/render_manuscript.R <paper_dir> <results_dir> <data_root> <manifest.csv> [output_dir]
#   paper_dir   holds manuscript.md and online_appendix.md (the sources of the paper)
#   results_dir holds the *_R.csv files written by the analysis scripts
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
models <- R("model_results_R.csv")
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
rev <- R("revision_results_R.csv")
revrows <- function(b) lapply(seq_len(nrow(rev[block == b])), function(i) { r <- rev[block == b][i]; c(r$label, n0(r$events), f2(r$odds_ratio), sprintf("%s–%s", f2(r$lo), f2(r$hi))) })
one <- function(lbl) { r <- rev[label == lbl]; stopifnot(nrow(r) == 1); c(lbl, n0(r$events), f2(r$odds_ratio), sprintf("%s–%s", f2(r$lo), f2(r$hi))) }
dd <- models[model == "Club dose" & term == "co_client_count_log"]
dose_row <- c("Co-client count (per doubling of one plus the count)", n0(dd$events), f2(dd$odds_ratio^log(2)), sprintf("%s–%s", f2(dd$label_lo^log(2)), f2(dd$label_hi^log(2))))
grp <- function(x) c(paste0("*", x, "*"), "", "", "")
plac <- list(one("Own agency (twin-agency sample)"), one("Twin agencies with similar portfolios (placebo)"), one("Difference: own minus twin agencies"), one("Closest single twin (placebo)"), one("Difference: own minus closest single twin"))
tables$PLACEBO_TABLE <- table_md("Table 3. Own representative and matched agencies", headers, plac, "Odds ratios for a co-client at the destination club from conditional logits with destination club fixed effects and three-year flows. The first three rows come from one model in which the own-agency and three-twin indicators enter together; the last two rows come from a second model with the closest single twin. Differences are ratios of two odds ratios from the same model, with 1 as the null value. Intervals cluster by representative label. Twins are the agencies of similar size with the most similar client nationality, position and age profile (Online Appendix A).")
grad <- list(grp("Strength of the channel"), dose_row, one("Recently placed co-client"), one("Long-standing co-client"), grp("Contract status and fee"), one("Contract ends by 30 June of the window year"), one("Contract runs one more year"),
  one("Contract runs longer"), one("Ratio: expiring contract to longer contract"), one("30 June or 1 July, no fee"), one("30 June or 1 July, fee paid"), one("Other dates, no fee"), one("Other dates, fee paid"), one("Excluding moves to a former club"),
  grp("Own and matched agencies by contract status"), one("Own agency, contract expiring"), one("Twin agencies, contract expiring (placebo)"), one("Difference, contract expiring: own minus twin agencies"),
  one("Own agency, contract runs longer"), one("Twin agencies, contract runs longer (placebo)"), one("Difference, contract runs longer: own minus twin agencies"))
tables$GRAD_TABLE <- table_md("Table 5. When the association is larger: exploratory analyses", headers, grad, "Odds ratios for a co-client at the destination club from conditional logits with destination club fixed effects and three-year flows, main sample unless the event count differs. Ratios and differences have 1 as the null value. Intervals cluster by representative label. Contract status comes from the pre-window release. The contract, fee and date rows describe overlapping moves and are not independent tests. A recently placed co-client arrived by transfer within three years.")
tables$ROBUST_TABLE <- table_md("Table 6. Sample, choice-set and league sensitivity", headers, c(revrows("sensitivity"), revrows("league")), "Club rows are odds ratios for a co-client at the destination club with destination club fixed effects and three-year flows. First-version linkage requires the mover on the origin club’s roster; player-level linkage reads the label from the mover’s own row. Look-ahead labels come from releases dated after the window. League rows are for doubling one plus the client count, except the concentration row (per ten percentage points of the agency’s clients).")
for (f in c("figure1_main", "figureA1_specifications")) file.copy(file.path(res, paste0(f, "_R.png")), file.path(outdir, paste0(f, ".png")), overwrite = TRUE)
fill <- function(text, tabs) { for (m in names(tabs)) text <- gsub(sprintf("[[%s]]", m), tabs[[m]], text, fixed = TRUE); text }
build <- function(text, name, title, widthf) {
  md <- file.path(outdir, paste0(name, ".md")); writeLines(text, md, useBytes = TRUE)
  docx <- file.path(outdir, paste0(name, ".docx")); stopifnot(system2("pandoc", c(shQuote(md), "-o", shQuote(docx))) == 0)
  style_docx(docx, title, widthf); docx
}
main_w <- function(i, n) if (i == 0) c(.6, 1.06, 1.15, 1.25, 1.17, 1.27) else c(3.58, .72, .85, 1.35)
d1 <- build(fill(paste(readLines(file.path(paper, "manuscript.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n"), tables), "Shared_Representation_and_Player_Mobility", "Shared Representation and Player Mobility in European Football", main_w)

# ---- online appendix
q <- R("rev2_matching_quality_R.csv"); cv <- R("rev2_matching_coverage_R.csv"); ex <- R("rev2_worked_example_R.csv")
cap <- function(s) paste(vapply(strsplit(s, ",")[[1]], function(x) { x <- trimws(x); paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2))) }, ""), collapse = ", ")
at <- list()
at$APP_COVERAGE <- table_md("Table A1. Twin selection by summer window", c("Summer", "Representatives with at least 3 clients", "With three twins", "Median twin size relative to focal"),
  lapply(seq_len(nrow(cv)), function(i) c(cv$summer[i], cv[["labels with at least 3 clients"]][i], cv[["focal labels with twins"]][i], f2(cv[["median twin/focal size"]][i]))),
  "Twin size is the mean client count of the three twins divided by the focal agency’s client count.")
at$APP_BALANCE <- table_md("Table A3. Mean cosine similarity between a representative’s client profile and its comparison agencies", c("Profile block", "Three twins", "Other agencies of similar size"),
  lapply(seq_len(nrow(q)), function(i) c(paste0(toupper(substring(q$block[i], 1, 1)), substring(q$block[i], 2)), f2(q$twin[i]), f2(q$eligible_pool[i]))),
  "Averages over focal representatives and windows. Similarity is computed within each block of the profile (nationality, position, age band). The comparison column averages over all other agencies that meet the size rule.")
at$APP_EXAMPLE <- table_md("Table A5. Worked example: one focal agency and its three twins, summer 2024", c("Agency", "Clients", "Largest nationalities", "Position mix (attack / midfield / defender / goalkeeper)", "Age mix (21 or under / 22–25 / 26–29 / 30 or over)"),
  lapply(seq_len(nrow(ex)), function(i) c(ex$agency[i], ex$clients[i], cap(ex$top_nationalities[i]), sprintf("%s / %s / %s / %s", ex$forwards[i], ex$midfield[i], ex$defenders[i], ex$goalkeepers[i]), sprintf("%s / %s / %s / %s", ex$age_21_or_under[i], ex$age_22_25[i], ex$age_26_29[i], ex$age_30_plus[i]))),
  "The focal agency is the one nearest the median size among agencies with 18 to 26 clients in 2024. Agencies are anonymised.")
fl <- R("rev2c_sample_flow_R.csv"); ct <- R("rev2c_crosstab_R.csv"); cs_ <- R("rev2c_choice_sets_R.csv")
at$APP_FLOW <- table_md("Table A2. From the club model to the twin model", c("Step", "Events"),
  lapply(which(startsWith(fl$step, "Events") | fl$events != 0), function(i) c(fl$step[i], if (startsWith(fl$step[i], "Events")) n0(fl$events[i]) else sub("-", "−", n0(fl$events[i])))),
  "Events are transfers in the main club model. Twins require a focal representative with at least three clients on the pre-window rosters of the covered clubs. No further events drop out.")
at$APP_CROSS <- table_md("Table A4. Candidate clubs by own-agency and twin presence", c("Candidate club has", "Alternatives", "Share of alternatives", "Chosen", "Chosen as share of alternatives"),
  lapply(seq_len(nrow(ct)), function(i) c(ct$cat[i], n0(ct$alternatives[i]), sprintf("%.1f%%", 100 * ct$share_of_alternatives[i]), n0(ct$chosen[i]), sprintf("%.1f%%", 100 * ct$chosen_share[i]))),
  sprintf("Alternatives in the twin model (1,674 events). Of the 1,674 choice sets, %s contain at least one own-only club, %s at least one twin-only club and %s contain both. The last column is the raw share of clubs in each category that were chosen, before any adjustment.", n0(cs_$with_own_only), n0(cs_$with_twin_only), n0(cs_$with_both_types)))
mf <- fread(manifest, colClasses = "character")
roles <- c(`240` = "Summer 2023 baseline; earlier-label source for summers 2024–25 and January 2024–25", `325` = "January 2024 baseline (December 2023 rosters); earlier-label source for summers 2024–25 and January 2025; look-ahead check, summer 2023",
  `355` = "Earlier-label source for summers 2024–25 and January 2025; look-ahead check, summer 2023", `435` = "Summer 2024 baseline; earlier-label source for summer 2025 and January 2025; look-ahead check, summer 2023",
  `547` = "January 2025 baseline (December 2024 rosters); earlier-label source for summer 2025; look-ahead check, summer 2024", `566` = "Earlier-label source for summer 2025; look-ahead check, summer 2024",
  `600` = "Summer 2025 baseline; look-ahead check, summer 2024", `603` = "February 2026 backcast (sensitivity); look-ahead check, summer 2025",
  `606` = "February 2026 backcast, second release (sensitivity); look-ahead check, summer 2025", `660` = "Look-ahead check, summer 2025")
fmtd <- function(s) { d <- as.Date(substr(s, 6, 16), "%d %b %Y"); paste(as.integer(format(d, "%d")), format(d, "%B %Y")) }
at$APP_VERS <- table_md("Table B1. Versions of players.csv used", c("Version", "Archived file timestamp", "SHA-256 (first 12)", "Analytical role"),
  lapply(seq_len(nrow(mf)), function(i) c(mf$version[i], fmtd(mf$last_modified[i]), substr(mf$sha256[i], 1, 12), roles[[mf$version[i]]])),
  "Earlier-label sources are releases dated before the window and other than the baseline, used to fill a missing label. Look-ahead releases are dated after the window and used only in a sensitivity analysis.")
at$APP_OTHER <- table_md("Table B2. Other inputs", c("Input", "Source", "Role"), list(
  c("transfers table (transfers.gz)", "Cariboo, transfermarkt-datasets (GitHub); copy SHA-256 90326983dc3e", "Transfer events, move dates, fees and earlier club-to-club flows"),
  c("games table (games.gz)", "Cariboo, transfermarkt-datasets (GitHub); copy SHA-256 142561989017", "Prior-season points, league membership of clubs"),
  c("appearances table", "Cariboo, transfermarkt-datasets (GitHub)", "Pre-window league appearances and minutes"),
  c("Club Elo ratings", "xgabora/Club-Football-Match-Data (GitHub), from ClubElo", "Pre-window Elo of origin and candidate clubs"),
  c("EA player ratings", "FIFA 23 and FC 24 extracts (GitHub); sofifa data set (Kaggle, jmacd745)", "Player rating gap to the candidate club’s squad")),
  "Retrieval dates of the transfer and games copies were not recorded; the checksums identify the files used. Source addresses are in coach_retry/perf/README.md.")
app_w <- function(i, n) { if (n == 5 && i == 3) return(c(1.5, 1.0, 1.4, 1.2, 1.4)); if (i == 5) return(c(.8, 1.3, 1.1, 3.3))
  switch(as.character(n), `4` = c(1.0, 2.0, 1.6, 1.9), `3` = if (i != 6) c(2.3, 1.6, 2.6) else c(1.8, 2.6, 2.1), `2` = c(5.4, 1.0), `5` = c(.9, .6, 1.5, 1.7, 1.8)) }
d2 <- build(fill(paste(readLines(file.path(paper, "online_appendix.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n"), at), "Online_Appendix_Twin_Matching", "Online Appendix", app_w)
for (d in c(d1, d2)) system2("soffice", c("--headless", "--convert-to", "pdf", "--outdir", shQuote(outdir), shQuote(d)), stdout = FALSE, stderr = FALSE)
cat("wrote", d1, "and", d2, "\n")
