# League model with nationality x destination-league fixed effects and agency concentration.
# Usage: Rscript R/rev5_league.R <data_root> [output_dir]
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "hdr.R"))
out <- list()
for (smp in list(c("first-version sample", "archived"), c("main sample", "extended"))) {
  tag <- smp[1]; d <- prepare(root, smp[2]); lg <- copy(d$lg)
  tot <- d$counts[, .(agency_total = sum(cnt)), by = .(summer, label)]
  lg <- merge(lg, tot, by = c("summer", "label"), all.x = TRUE, sort = FALSE); setorder(lg, event_id)
  cn <- expm1(lg$agency_log)
  lg[, agency_share := fifelse(is.na(agency_total) | agency_total == 0, 0, cn / agency_total) * 10]
  nat_n <- lg[chosen == 1, .N, by = nation]; keep <- nat_n[N >= 25, nation]
  cells <- lg[chosen == 1, .N, by = .(nation, alternative)]
  cols <- character()
  for (n in keep) for (a in names(countries)) {
    if (cells[nation == n & alternative == a, sum(N)] < 5) next
    nm <- paste0("nl_", length(cols) + 1); set(lg, j = nm, value = as.numeric(lg$nation == n & lg$alternative == a)); cols <- c(cols, nm)
  }
  lf <- function(name, x, terms, metrics = NULL) fit_logit(paste0(tag, ": ", name), x, terms, "route_fe", metrics)
  lf("baseline (agency log count, local nationality)", lg, c("agency_log", "log_roster", "local_nationality"))
  lf("+ nationality x destination-league FE", lg, c("agency_log", "log_roster", cols), list(agency_log = c(agency_log = 1)))
  lf("agency share, local nationality", lg, c("agency_share", "log_roster", "local_nationality"), list(agency_share = c(agency_share = 1)))
  lf("agency share + nationality x league FE", lg, c("agency_share", "log_roster", cols), list(agency_share = c(agency_share = 1)))
  lc <- lg[route == "between" & origin_league != alternative]
  lf("between-league, + nationality x league FE", lc, c("agency_log", "log_roster", cols), list(agency_log = c(agency_log = 1)))
}
r <- rbindlist(records$rows, fill = TRUE)
r[, or_per_doubling := fifelse(term == "agency_log", odds_ratio^log(2), NA_real_)]
r[, lo_doubling := fifelse(term == "agency_log", label_lo^log(2), NA_real_)]; r[, hi_doubling := fifelse(term == "agency_log", label_hi^log(2), NA_real_)]
fwrite(r, file.path(outdir, "rev5_league_results_R.csv"))
