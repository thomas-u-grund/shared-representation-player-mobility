# Rebuild the analysis tables from the archived raw inputs, entirely in R.
# Usage: Rscript R/rebuild_data.R <root> [output_dir]
#   <root> holds raw/kaggle (archived players.csv releases + manifest), raw/performance, coach_retry/{games.gz,transfers.gz,expanded/summer_transfers.csv},
#          winter_recovery/sources.csv and winter_proxy/proxy_sources.csv (the layout of the project folder / full data deposit)
args <- commandArgs(TRUE); root <- normalizePath(args[1]); out <- normalizePath(if (length(args) > 1) args[2] else "rebuilt_data_R", mustWork = FALSE)
script_dir <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)); B <- file.path(normalizePath(script_dir), "build")
stopifnot(!dir.exists(out)); dir.create(out, recursive = TRUE)
run <- function(script, ..., wd) { cat("Running", script, "\n"); old <- setwd(wd); on.exit(setwd(old)); st <- system2("Rscript", c(shQuote(file.path(B, script)), ...)); stopifnot(st == 0) }
d <- function(...) { p <- file.path(out, ...); dir.create(p, showWarnings = FALSE, recursive = TRUE); p }
kaggle <- file.path(root, "raw", "kaggle"); coach <- file.path(root, "coach_retry")
expanded <- d("expanded"); perf <- d("perf"); observed <- d("winter_recovery"); proxy <- d("winter_proxy"); alt <- d("winter_proxy", "version_606_sensitivity")
for (v in c(240, 435, 600, 660)) file.copy(file.path(kaggle, paste0("data_", v)), file.path(expanded, paste0(v, ".data")))
for (f in c("games.gz", "transfers.gz")) file.copy(file.path(coach, f), file.path(expanded, f))
run("count.R", shQuote(file.path(coach, "expanded", "summer_transfers.csv")), wd = expanded)   # keeps the published event order
run("fit.R", wd = expanded)
run("build_performance.R", shQuote(expanded), shQuote(file.path(root, "raw", "performance")), shQuote(perf), wd = perf)
file.copy(file.path(root, "winter_recovery", "sources.csv"), observed)
run("build_winter.R", shQuote(kaggle), shQuote(coach), wd = observed)
for (p in list(proxy, alt)) { file.copy(file.path(root, "winter_proxy", if (identical(p, alt)) "version_606_sensitivity" else ".", "proxy_sources.csv"), p, overwrite = TRUE)
  run("evaluate_winter_proxy.R", shQuote(kaggle), shQuote(coach), shQuote(observed), wd = p) }
run("rev8_fill.R", "pre", "--root", shQuote(root), "--expanded", shQuote(expanded), "--out", shQuote(file.path(out, "perf_extended")), wd = out)
oe <- d("winter_recovery_extended"); file.copy(file.path(root, "winter_recovery", "sources.csv"), oe)
run("build_winter_extended.R", shQuote(kaggle), shQuote(coach), wd = oe)
cat("Rebuilt data:", out, "\n")
