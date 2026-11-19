# Loan-like moves: no recorded fee and a return to the origin club within 14 months (Online Appendix, Table B9).
# Usage: Rscript R/analysis/08_move_type.R <data_root> [output_dir]
# Writes move_type_results.csv. The transfer file has no loan flag, so the loan-like class is a proxy.
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "..", "lib", "setup.R"))
d <- prepare(root, "extended"); en <- enrich(root, "extended")
m <- add_flags(d$main, en$ev)
tr <- en$tr; tr[, fee := suppressWarnings(as.numeric(transfer_fee))]
cat("latest transfer observed by 1 November 2026:", tr[transfer_date <= "2026-11-01", max(transfer_date)], "\n")
ev <- unique(m[, .(event_id, summer, player_id, origin_club, destination_club, date, paid)])
# return move: the same player goes from the destination club back to the origin club within 760 days of the move
tr2 <- unique(tr[, .(player_id, rdate = transfer_date, rfrom = from_club_id, rto = to_club_id)])
j <- merge(ev, tr2, by = "player_id", allow.cartesian = TRUE)[rfrom == destination_club & rto == origin_club & rdate > date & as.Date(rdate) <= as.Date(date) + 760]
ev[, returned := event_id %in% j$event_id]
ev[, ret14 := event_id %in% j[as.Date(rdate) <= as.Date(date) + 427, event_id]]
ev[, cat := fifelse(paid, "paid", fifelse(ret14, "loan_like", "unpaid_kept"))]
cat("events by summer and move type:\n"); print(ev[, .N, by = .(summer, cat)][order(summer, cat)])
m <- merge(m, ev[, .(event_id, cat)], by = "event_id", all.x = TRUE, sort = FALSE)
m[, `:=`(same_paid2 = same * (cat == "paid"), same_loan = same * (cat == "loan_like"), same_free = same * (cat == "unpaid_kept"))]
base <- c("same", controls, flows)
fit_logit("L0 baseline", m, base, fx)
fit_logit("L1 by type", m, c("same_paid2", "same_loan", "same_free", controls, flows), fx,
          list("paid" = c(same_paid2 = 1), "loan-like" = c(same_loan = 1), "unpaid, kept" = c(same_free = 1), "loan-like minus paid" = c(same_loan = 1, same_paid2 = -1), "unpaid kept minus paid" = c(same_free = 1, same_paid2 = -1)))
fit_logit("L2 excluding loan-like", m[cat != "loan_like"], base, fx)
fit_logit("L3 loan-like only", m[cat == "loan_like"], base, fx)
fit_logit("L4 unpaid kept only", m[cat == "unpaid_kept"], base, fx)
fit_logit("L5 paid only", m[cat == "paid"], base, fx)
save_records("move_type_results.csv")
