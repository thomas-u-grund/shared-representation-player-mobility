# Re-run every analysis of the paper on data/ and compare the results with results/.
# Usage (from the repository root):  Rscript run_all.R [output_dir] [paper_dir]
#   output_dir  where result tables and logs are written (default: results_reproduced/)
# Run time: about 12 minutes on a laptop with four free cores (about 30 minutes with N_JOBS=1).
#   N_JOBS      environment variable: number of scripts run in parallel (default: min(4, cores - 1); 1 on Windows)
args <- commandArgs(TRUE)
repo <- normalizePath(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)))
out <- normalizePath(if (length(args)) args[1] else file.path(repo, "results_reproduced"), mustWork = FALSE)
paper <- if (length(args) > 1) args[2] else file.path(repo, "paper")
data <- file.path(repo, "data", "study1"); stopifnot(dir.exists(data))
dir.create(file.path(out, "logs"), showWarnings = FALSE, recursive = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out, "session_info.txt"))

n_jobs <- if (.Platform$OS.type == "windows") 1L else as.integer(Sys.getenv("N_JOBS", min(4, max(1, parallel::detectCores() - 1))))
cache <- file.path(out, "cache"); dir.create(cache, showWarnings = FALSE); Sys.setenv(PREP_CACHE = cache)   # prepared data shared by the scripts

run_one <- function(script, ...) {
  t0 <- Sys.time(); log <- file.path(out, "logs", paste0(basename(script), ".log"))
  st <- system2("Rscript", c(shQuote(file.path(repo, "code", script)), ...), stdout = log, stderr = log)
  list(script = script, ok = st == 0, secs = as.numeric(difftime(Sys.time(), t0, units = "secs")), log = log)
}
run_jobs <- function(jobs) {                      # jobs: list of argument vectors (script first); runs up to n_jobs at a time
  res <- parallel::mclapply(jobs, function(j) do.call(run_one, as.list(j)), mc.cores = n_jobs, mc.preschedule = FALSE)
  for (r in res) { if (!is.list(r) || !r$ok) stop(sprintf("%s failed; see %s", if (is.list(r)) r$script else "a job", if (is.list(r)) r$log else out), call. = FALSE)
    cat(sprintf("== %-42s %4.0f s\n", r$script, r$secs)) }
}
run <- function(script, ...) run_jobs(list(c(script, ...)))

# 1. estimation
cat("Preparing the data of each sample (cached in", cache, ")\n")
run_jobs(lapply(c("extended", "extended_base", "extended_post", "archived"), function(s) c("lib/prepare_cache.R", shQuote(data), shQuote(out), s)))
analysis <- c("01_main_models.R", "02_timing_fees_contract.R", "03_matched_agencies.R", "04_choice_sets_specialisation.R", "05_league.R", "06_sample_variants.R",
              "07_season_comparison.R", "08_move_type.R", "09_matched_agencies_value.R", "10_label_audit.R", "11_club_resources.R", "12_co_client_background.R", "13_agency_concentration_leagues.R")
jobs <- lapply(analysis, function(s) c(file.path("study1", s), shQuote(data), shQuote(out)))
# Study 2 (English FA records): re-estimated from the harmonised transaction table data/study2/fa_transactions_long.csv (the PDF extraction is optional, see rebuild_from_raw/README.md)
jobs <- c(jobs, lapply(c("study2/fa_choice_model.R", "study2/fa_audit_model.R", "study2/fa_balance_model.R"), function(s) c(s, shQuote(out))))
run_jobs(jobs)

# 2. tables and the paper
run("report/make_tables.R", shQuote(out))
run("report/render_manuscript.R", shQuote(paper), shQuote(out), shQuote(data), shQuote(file.path(repo, "results", "download_manifest.csv")), shQuote(out))

# 3. comparison with the stored results
suppressPackageStartupMessages(library(data.table))
chk <- function(file, keys = c("model", "term"), col = "odds_ratio", tol = 0.002) {
  n <- fread(file.path(out, file)); o <- fread(file.path(repo, "results", file))
  n <- unique(n[!is.na(n[[col]])], by = keys); o <- unique(o[!is.na(o[[col]])], by = keys); m <- merge(o, n, by = keys, suffixes = c("_old", "_new"))
  d <- max(abs(m[[paste0(col, "_old")]] - m[[paste0(col, "_new")]]) / abs(m[[paste0(col, "_old")]]))
  cat(sprintf("%-38s %3d/%3d rows, max relative difference %.1e\n", file, nrow(m), nrow(o), d)); stopifnot(nrow(m) == nrow(o), d < tol)
}
cat("\nComparison with results/:\n")
for (f in c("model_results.csv", "timing_fees_results.csv", "contract_status_results.csv", "matched_agencies_results.csv", "matched_agencies_by_contract.csv", "matched_agencies_closest.csv",
            "choice_set_results.csv", "elo_decomposition_results.csv", "league_results.csv", "sample_variants_results.csv", "specialisation_results.csv",
            "move_type_results.csv", "matched_agencies_value_results.csv", "label_audit_results.csv", "club_resources_results.csv", "co_client_background_results.csv", "agency_concentration_results.csv", "league_results_by_league.csv")) chk(f)
chk("season_comparison_results.csv", c("model", "term", "cluster"))
chk("table_estimates.csv", c("block", "label"))
chk("baseline_share.csv", "sample", "observed_share")
chk("fa_choice_results.csv", c("run", "model", "fixed_effects", "term"))                      # Study 2, Table 4 and B13
chk("fa_audit_results.csv", c("item", "run", "spec", "fe", "cluster", "term"))                # Study 2 audit, renewal model and further specifications
chk("fa_balance_results.csv", c("item", "run", "term"))                                       # Study 2 matched intermediaries, leave-out and divisions
cat("All re-estimates agree with the stored results.\n")
if (!nzchar(Sys.getenv("KEEP_CACHE"))) unlink(cache, recursive = TRUE)
