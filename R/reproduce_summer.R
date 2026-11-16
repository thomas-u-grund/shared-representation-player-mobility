# Re-estimate the summer models in R on data_pseudonymised/ and compare with the stored estimates.
# Usage (from the repository root): Rscript R/reproduce_summer.R [output_dir]
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE); out <- if (length(args)) args[1] else "reproduced_summer_R"
script_dir <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE))   # Rscript encodes spaces in the path as "~+~"
repo <- normalizePath(file.path(script_dir, ".."))
stopifnot(file.exists(file.path(repo, "data_pseudonymised", "perf_extended", "player_performance.csv")))
status <- system2("Rscript", c(shQuote(file.path(repo, "R", "analyse.R")), shQuote(file.path(repo, "data_pseudonymised")), shQuote(out)))
stopifnot(status == 0)
new <- fread(file.path(out, "model_results_R.csv")); old <- fread(file.path(repo, "agency_paper", "model_results.csv"))
m <- merge(old, new, by = c("model", "term"), suffixes = c("_py", "_r"))
stopifnot(nrow(m) == nrow(old), nrow(new) == nrow(old))
cmp <- function(a, b, tol) max(abs(a - b) / pmax(abs(a), 1e-8)) < tol
chk <- c(events = all(m$events_py == m$events_r), alternatives = all(m$alternatives_py == m$alternatives_r),
         odds_ratio = cmp(m$odds_ratio_py, m$odds_ratio_r, 0.002), label_lo = cmp(m$label_lo_py, m$label_lo_r, 0.003), label_hi = cmp(m$label_hi_py, m$label_hi_r, 0.003),
         player_lo = cmp(m$player_id_lo_py, m$player_id_lo_r, 0.003), player_hi = cmp(m$player_id_hi_py, m$player_id_hi_r, 0.003))
print(chk)
cat(sprintf("max relative difference in odds ratios: %.5f\n", max(abs(m$odds_ratio_py - m$odds_ratio_r) / m$odds_ratio_py)))
stopifnot(all(chk)); cat(nrow(new), "rows agree with the stored (Python) estimates\n")
