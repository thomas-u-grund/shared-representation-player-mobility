# Contract-expiry status, ratio tests and the no-club-fixed-effects check.  Usage: Rscript R/rev6_contract.R <data_root> [output_dir]
args <- commandArgs(TRUE); root <- args[1]; outdir <- if (length(args) > 1) args[2] else "."
script_dir <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE))
source(file.path(script_dir, "prepare.R")); source(file.path(script_dir, "enrich.R"))
d <- prepare(root, "extended"); en <- enrich(root, "extended")
m <- contract_status(add_flags(d$main, en$ev), root)
for (k in c("expiring", "one more year", "longer", "unknown")) set(m, j = paste0("same_", gsub(" ", "_", k)), value = m$same * (m$exp_status == k))
cc <- c("same_expiring", "same_one_more_year", "same_longer", "same_unknown", controls, flows)
fit_logit("R6 by contract status (main sample)", m, cc, fx, list("contract expiring by 30 June" = c(same_expiring = 1), "contract runs one more year" = c(same_one_more_year = 1),
          "contract runs longer" = c(same_longer = 1), "contract end unknown" = c(same_unknown = 1)))
fit_logit("R6 expiring vs longer contract (main sample)", m, cc, fx, list("difference, expiring vs longer contract (ratio)" = c(same_expiring = 1, same_longer = -1),
          "difference, expiring vs one more year (ratio)" = c(same_expiring = 1, same_one_more_year = -1)))
b <- c("same", controls, flows)
fit_logit("R6 expiring only (main sample)", m[exp_status == "expiring"], b, fx)
fit_logit("R6 not expiring (main sample)", m[exp_status %in% c("one more year", "longer")], b, fx)
fit_logit("R6 expiring, on turn dates only", m[exp_status == "expiring" & turn], b, fx)
fit_logit("R6 expiring, off turn dates", m[exp_status == "expiring" & !turn], b, fx)
fit_logit("R6 no club FE, with flows", m, b)
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
fwrite(rbindlist(records$rows, fill = TRUE), file.path(outdir, "rev6_misc_results_R.csv"))
