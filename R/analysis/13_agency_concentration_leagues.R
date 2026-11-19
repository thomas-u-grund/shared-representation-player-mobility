# Is the association driven by a few large agencies, and does it differ by destination league? (Online Appendix)
# Usage: Rscript R/analysis/13_agency_concentration_leagues.R <data_root> [output_dir]
# Writes agency_concentration.csv, agency_concentration_results.csv, league_heterogeneity.csv and league_results_by_league.csv.
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "..", "lib", "setup.R"))
d <- prepare(root, "extended"); main <- valid_events(copy(d$main))
base <- c("same", controls, flows)

# ---- the largest agencies: size is the number of labelled clients on the pre-window rosters, summed over the three summers
size <- d$counts[, .(clients = sum(cnt)), by = label][order(-clients)]
size[, rank := .I]
evs <- main[chosen == 1, .(event_id, label, connected = same == 1)]
inf <- main[, .(informative = any(same == 1) & any(same == 0)), by = .(event_id, label)]
top <- size[rank <= 20][, `:=`(events = evs[label == .BY$label, .N], informative_events = inf[label == .BY$label & informative == TRUE, .N], chosen_connected = evs[label == .BY$label & connected == TRUE, .N]), by = label]
top[, `:=`(share_of_events = events / nrow(evs), share_of_informative_events = informative_events / sum(inf$informative), share_of_connected_choices = chosen_connected / sum(evs$connected))]
cat("share of all events, informative events and connected choices:\n"); print(top[, lapply(.SD, function(x) round(sum(x[rank <= 10]), 3)), .SDcols = c("share_of_events", "share_of_informative_events", "share_of_connected_choices")])
fwrite(top, file.path(outdir, "agency_concentration.csv"))
fit_logit("G0 main sample", main, base, fx)
for (k in 1:10) fit_logit(sprintf("G1 leave out agency rank %d", k), main[label != size[rank == k, label]], base, fx)
for (k in c(3, 5, 10, 20)) fit_logit(sprintf("G2 leave out the %d largest agencies", k), main[!(label %in% size[rank <= k, label])], base, fx)
fit_logit("G3 only the 20 largest agencies", main[label %in% size[rank <= 20, label]], base, fx)
save_records("agency_concentration_results.csv")

# ---- destination leagues: one model per league, with the number of informative transfers (events with a connected and an unconnected club)
rows <- list()
for (lg in names(countries)) {
  m <- main[destination_league == lg]
  n_ev <- uniqueN(m[chosen == 1, event_id]); info <- m[, .(i = any(same == 1) & any(same == 0)), by = event_id][, sum(i)]; cc <- m[chosen == 1 & same == 1, .N]
  nrec <- length(records$rows)
  ok <- tryCatch({ fit_logit(sprintf("L %s", lg), m, base, fx, quiet = TRUE); TRUE }, error = function(e) FALSE)
  r <- if (ok && length(records$rows) > nrec) as.data.table(records$rows[[nrec + which(vapply(records$rows[(nrec + 1):length(records$rows)], function(x) x$term, "") == "same")[1]]]) else NULL
  rows[[lg]] <- data.table(league = lg, country = unname(countries[lg]), events = n_ev, informative_events = info, chosen_connected = cc,
                           odds_ratio = if (is.null(r)) NA_real_ else r$odds_ratio, lo = if (is.null(r)) NA_real_ else r$label_lo, hi = if (is.null(r)) NA_real_ else r$label_hi)
}
L <- rbindlist(rows)
L[, `:=`(logor = log(odds_ratio), se = (log(hi) - log(lo)) / (2 * 1.96))]
ok <- L[!is.na(logor) & is.finite(se) & se > 0]
w <- 1 / ok$se^2; bbar <- sum(w * ok$logor) / sum(w); Q <- sum(w * (ok$logor - bbar)^2); df <- nrow(ok) - 1
cat(sprintf("heterogeneity across %d leagues: Q = %.2f, df = %d, p = %.3f, I2 = %.0f%%\n", nrow(ok), Q, df, pchisq(Q, df, lower.tail = FALSE), 100 * max(0, (Q - df) / Q)))
L[, `:=`(Q = Q, Q_df = df, Q_p = pchisq(Q, df, lower.tail = FALSE), I2 = max(0, (Q - df) / Q))]
fwrite(L[, .(league, country, events, informative_events, chosen_connected, odds_ratio, lo, hi, Q, Q_df, Q_p, I2)], file.path(outdir, "league_heterogeneity.csv"))
save_records("league_results_by_league.csv")
