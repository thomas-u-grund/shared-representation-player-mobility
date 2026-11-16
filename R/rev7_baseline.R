# Observed vs uniform-choice share of connected destinations.  Usage: Rscript R/rev7_baseline.R <data_root> [output_dir]
args <- commandArgs(TRUE); root <- args[1]; outdir <- if (length(args) > 1) args[2] else "."
script_dir <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE))
source(file.path(script_dir, "prepare.R")); source(file.path(script_dir, "enrich.R"))
retained <- function(x, fixed = "candidate_club") { x <- valid_events(x); repeat { n <- nrow(x); x <- valid_events(x[get(fixed) %in% x[chosen == 1, get(fixed)]]); if (nrow(x) == n) return(x) } }
rows <- list()
for (smp in list(c("first-version sample", "archived"), c("main sample", "extended"))) {
  d <- prepare(root, smp[2]); s <- retained(d$main)
  g <- s[, .(k = sum(same), n = .N), by = event_id]; ex <- g[k > 0]
  ch <- s[chosen == 1, .(event_id, same)][ex, on = "event_id"]
  rows[[length(rows) + 1]] <- data.table(sample = smp[1], events = nrow(g), exposed_events = nrow(ex), chosen_connected = sum(ch$same), observed_share = mean(ch$same), uniform_share = mean(ex$k / ex$n))
}
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
fwrite(rbindlist(rows), file.path(outdir, "rev7_baseline_R.csv"))
