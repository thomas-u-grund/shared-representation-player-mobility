# Fee type, return to a former club, timing.  Usage: Rscript R/rev1_timing.R <data_root> [output_dir]
args <- commandArgs(TRUE); root <- args[1]; outdir <- if (length(args) > 1) args[2] else "."
script_dir <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE))
source(file.path(script_dir, "prepare.R")); source(file.path(script_dir, "enrich.R"))
d <- prepare(root, "extended"); en <- enrich(root, "extended")
m <- add_flags(d$main, en$ev)
m[, same_former := same * former_dest]; m[, same_paid := same * paid]
m[, same_turn_free := same * (turn & !paid)]; m[, same_turn_paid := same * (turn & paid)]
m[, same_off_free := same * (!turn & !paid)]; m[, same_off_paid := same * (!turn & paid)]
base <- c("same", controls, flows)
fit_logit("R1 baseline", m, base, fx)
fit_logit("R1 same x former-club destination", m, c("same", "same_former", controls, flows), fx, list("shared, not former club" = c(same = 1), "shared, former club" = c(same = 1, same_former = 1)))
m2 <- m[former_dest == FALSE]
fit_logit("R1 exclude former-club destinations", m2, base, fx)
turnm <- list("off-turn" = c(same = 1), "turn" = c(same = 1, same_turn_of_july = 1))
fit_logit("R1 exclude former, turn x same", m2, c("same", "same_turn_of_july", controls, flows), fx, turnm)
four <- list("turn, free/loan" = c(same_turn_free = 1), "turn, paid" = c(same_turn_paid = 1), "off-turn, free/loan" = c(same_off_free = 1), "off-turn, paid" = c(same_off_paid = 1))
fcols <- c("same_turn_free", "same_turn_paid", "same_off_free", "same_off_paid", controls, flows)
fit_logit("R1 four-way transaction type", m, fcols, fx, four)
fit_logit("R1 four-way, exclude former", m2, fcols, fx, four)
fit_logit("R1 paid only", m[paid == TRUE], base, fx)
fit_logit("R1 no fee and not former club", m2[paid == FALSE], base, fx)
strict <- m[!(former_dest & turn & !paid)]
fit_logit("R1 strict return screen", strict, base, fx)
fit_logit("R1 strict screen, turn x same", strict, c("same", "same_turn_of_july", controls, flows), fx, turnm)
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
fwrite(rbindlist(records$rows, fill = TRUE), file.path(outdir, "rev1_timing_results_R.csv"))
