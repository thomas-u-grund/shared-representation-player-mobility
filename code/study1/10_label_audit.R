# Audit of representative labels and snapshots: baseline-only labels, summers separately, club label coverage (Online Appendix, Table B9).
# Usage: Rscript code/study1/10_label_audit.R <data_root> [output_dir]
# Writes label_audit_results.csv.
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "..", "lib", "setup.R"))
base <- c("same", controls, flows)
D <- list(ext = prepare(root, "extended"), base = prepare(root, "extended_base"))
for (nm in names(D)) { m <- D[[nm]]$main; m[, turn_ := NULL]
  cat("\n==", nm, "\n"); print(m[chosen == 1, .(events = .N, mover_label_share = 1), by = summer][order(summer)]) }
# event overlap by summer
e1 <- D$ext$main[chosen == 1, .(summer, event_id)]; e2 <- D$base$main[chosen == 1, .(summer, event_id)]
cat("\nextended vs baseline-only events by summer:\n"); print(merge(e1[, .(ext = .N), by = summer], e2[, .(base_only = .N), by = summer], by = "summer"))
cat("\nclub label coverage (named players per candidate club), by summer:\n")
print(D$ext$main[, .(known = known[1]), by = .(summer, candidate_club)][, .(clubs = .N, p10 = as.numeric(quantile(known, .1)), median = as.numeric(median(known)), p90 = as.numeric(quantile(known, .9)), share_lt5 = mean(known < 5), share_lt10 = mean(known < 10)), by = summer][order(summer)])
cat("\nshare of alternatives with co-client, by summer and sample:\n")
print(D$ext$main[, .(share_same = mean(same)), by = summer][order(summer)])
fit_logit("B1 extended, all", D$ext$main, base, fx)
fit_logit("B2 extended, summers 2023-24", D$ext$main[summer <= 2024], base, fx)
fit_logit("B3 baseline-only labels, all", D$base$main, base, fx)
fit_logit("B4 baseline-only labels, summers 2023-24", D$base$main[summer <= 2024], base, fx)
for (s in 2023:2025) fit_logit(paste0("B5 extended, summer ", s), D$ext$main[summer == s], base, fx)
for (k in c(5, 10, 15)) fit_logit(paste0("B6 candidate clubs with >= ", k, " named players"), D$ext$main[known >= k], base, fx)
save_records("label_audit_results.csv")
