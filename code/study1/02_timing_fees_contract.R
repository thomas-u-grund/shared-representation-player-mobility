# Fee type, returns to a former club, timing of the move and contract status (Table 5).
# Usage: Rscript code/study1/02_timing_fees_contract.R <data_root> [output_dir]
# Writes timing_fees_results.csv and contract_status_results.csv.
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "..", "lib", "setup.R"))
# ---- fee type, return to a former club and timing of the move
local({
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

save_records("timing_fees_results.csv")
})

# ---- contract status at the window
local({
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

save_records("contract_status_results.csv")
})
