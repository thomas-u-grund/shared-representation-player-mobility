# Sample variants and label harmonisation (Table 6, Online Appendix) and the observed versus uniform-choice share of connected destinations.
# Usage: Rscript code/study1/06_sample_variants.R <data_root> [output_dir]
# Writes sample_variants_results.csv and baseline_share.csv.
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "..", "lib", "setup.R"))
# ---- sample variants (first-version linkage, player-level linkage, carry-forward labels, look-ahead labels) and label harmonisation
nm <- c(archived = "first-version linkage", extended_base = "player-level linkage, baseline labels", extended = "MAIN: + carry-forward labels", extended_post = "+ look-ahead labels (later releases)")
b <- c("same", controls, flows)
for (smp in names(nm)) {
  d <- prepare(root, smp); en <- enrich(root, smp); m <- add_flags(d$main, en$ev); n <- nm[[smp]]
  fit_logit(paste0("V ", n), m, b, fx)
  fit_logit(paste0("V ", n, " | omit 30 June and 1 July"), m[turn == FALSE], b, fx)
  fit_logit(paste0("V ", n, " | club-by-summer FE"), m, b, "club_summer")
  if (smp == "extended") {
    pl <- d$players[label != ""]
    lab_h <- unique(pl, by = "label")[, .(label, hlabel)]
    mm <- merge(m, lab_h, by = "label", all.x = TRUE, sort = FALSE)
    cnt <- pl[, .(n = .N), by = .(summer, candidate_club = club_id, hlabel)]
    mm <- merge(mm, cnt, by = c("summer", "candidate_club", "hlabel"), all.x = TRUE, sort = FALSE)
    mm[is.na(n), n := 0L]; mm[, same := as.integer(n - as.integer(candidate_club == snapshot_club) > 0)]
    cat("harmonised: connected share", round(mean(m$same), 4), "->", round(mean(mm$same), 4), "\n")
    fit_logit("V MAIN | harmonised labels", mm, b, fx)
    fit_logit("V MAIN | no club fixed effects", m, b)
  }
}
save_records("sample_variants_results.csv")


# ---- observed versus uniform-choice share of connected destinations
local({
rows <- list()
for (smp in list(c("first-version sample", "archived"), c("main sample", "extended"))) {
  d <- prepare(root, smp[2]); s <- retained(d$main)
  g <- s[, .(k = sum(same), n = .N), by = event_id]; ex <- g[k > 0]
  ch <- s[chosen == 1, .(event_id, same)][ex, on = "event_id"]
  rows[[length(rows) + 1]] <- data.table(sample = smp[1], events = nrow(g), exposed_events = nrow(ex), chosen_connected = sum(ch$same), observed_share = mean(ch$same), uniform_share = mean(ex$k / ex$n))
}
fwrite(rbindlist(rows), file.path(outdir, "baseline_share.csv"))

})
