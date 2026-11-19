# Rebuild every model input (event panel, performance and candidate tables, winter rosters, backcast, extended sample) from the archived raw files.
# Usage: Rscript rebuild_from_raw/rebuild_data.R <root> [output_dir]
#   <root> is laid out like rebuild_from_raw/sources/: raw/kaggle (archived players.csv releases + manifest), raw/performance, coach_retry/{games.gz,transfers.gz,expanded/summer_transfers.csv},
#          winter_recovery/sources.csv and winter_proxy/proxy_sources.csv. The raw files are not redistributed (see rebuild_from_raw/README.md).
args <- commandArgs(TRUE); root <- normalizePath(args[1]); out <- normalizePath(if (length(args) > 1) args[2] else "rebuilt_data", mustWork = FALSE)
script_dir <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)); B <- normalizePath(script_dir)
stopifnot(!dir.exists(out)); dir.create(out, recursive = TRUE)
run <- function(script, ..., wd) { cat("Running", script, "\n"); old <- setwd(wd); on.exit(setwd(old)); st <- system2("Rscript", c(shQuote(file.path(B, script)), ...)); stopifnot(st == 0) }
d <- function(...) { p <- file.path(out, ...); dir.create(p, showWarnings = FALSE, recursive = TRUE); p }
kaggle <- file.path(root, "raw", "kaggle"); coach <- file.path(root, "coach_retry")
expanded <- d("expanded"); perf <- d("perf"); observed <- d("winter_recovery"); proxy <- d("winter_proxy"); alt <- d("winter_proxy", "version_606_sensitivity")
for (v in c(240, 435, 600, 660)) file.copy(file.path(kaggle, paste0("data_", v)), file.path(expanded, paste0(v, ".data")))
for (f in c("games.gz", "transfers.gz")) file.copy(file.path(coach, f), file.path(expanded, f))
run("01_summer_events.R", shQuote(file.path(coach, "expanded", "summer_transfers.csv")), wd = expanded)   # keeps the published event order
run("02_summer_fit.R", wd = expanded)
run("03_performance_inputs.R", shQuote(expanded), shQuote(file.path(root, "raw", "performance")), shQuote(perf), wd = perf)
file.copy(file.path(root, "winter_recovery", "sources.csv"), observed)
run("04_winter_rosters.R", shQuote(kaggle), shQuote(coach), wd = observed)
for (p in list(proxy, alt)) { file.copy(file.path(root, "winter_proxy", if (identical(p, alt)) "version_606_sensitivity" else ".", "proxy_sources.csv"), p, overwrite = TRUE)
  run("05_winter_backcast.R", shQuote(kaggle), shQuote(coach), shQuote(observed), wd = p) }
run("06_extended_sample.R", "pre", "--root", shQuote(root), "--expanded", shQuote(expanded), "--out", shQuote(file.path(out, "perf_extended")), wd = out)
oe <- d("winter_recovery_extended"); file.copy(file.path(root, "winter_recovery", "sources.csv"), oe)
run("07_winter_rosters_extended.R", shQuote(kaggle), shQuote(coach), wd = oe)
cat("Rebuilt data:", out, "\n")
