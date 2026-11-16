# Plausible choice sets: tier/affordability controls and restrictions of the alternatives (incl. the same-event Elo decomposition).
# Usage: Rscript R/rev4_choiceset.R <data_root> [output_dir]
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "hdr.R"))
d <- prepare(root, "extended"); en <- enrich(root, "extended")
m <- add_flags(d$main, en$ev); pl <- copy(d$players)
pl[, mv := as.numeric(market_value_eur)]; pl[, age := as.numeric(age_at_cutoff)]
club <- pl[, .(club_med_mv = median(mv, na.rm = TRUE), club_med_age = median(age, na.rm = TRUE)), by = .(summer, candidate_club = club_id)]
m <- merge(m, club, by = c("summer", "candidate_club"), all.x = TRUE, sort = FALSE)
m[, mv_mover := as.numeric(market_value_eur)]
m[, value_gap := log(mv_mover + 1e5) - log(club_med_mv + 1e5)]
m[, value_gap_unknown := as.integer(is.na(value_gap))]
m[, `:=`(abs_value_gap = fifelse(is.na(value_gap), 0, abs(value_gap)), sq_value_gap = fifelse(is.na(value_gap), 0, value_gap^2), signed_value_gap = fifelse(is.na(value_gap), 0, value_gap))]
m[, abs_age_gap := fifelse(is.na(player_age - club_med_age), 0, abs(player_age - club_med_age)) / 5]
m[, abs_elo_gap := fifelse(is.na(candidate_club_elo - origin_elo), 0, abs(candidate_club_elo - origin_elo) / 100)]
m[, elo_gap_unknown := as.integer(is.na(candidate_club_elo) | is.na(origin_elo))]
fitc <- c("abs_value_gap", "sq_value_gap", "signed_value_gap", "value_gap_unknown", "abs_age_gap", "abs_elo_gap", "elo_gap_unknown")
b <- c("same", controls, flows)
fit_logit("R4 main sample", m, b, fx)
fit_logit("R4 + tier/affordability fit controls", m, c(b, fitc), fx)
for (w in c(1.5, 1.0)) fit_logit(sprintf("R4 alternatives within |value gap|<=%s", format(w, nsmall = 1)), m[abs(value_gap) <= w | is.na(value_gap)], c(b, fitc), fx)
ek <- m[abs_elo_gap <= 2 | elo_gap_unknown == 1]
fit_logit("R4 alternatives within 200 Elo points of origin club", ek, c(b, fitc), fx)
save_records("rev4_choiceset_results_R.csv")
fit_logit("R4b restricted alternatives, fit controls", ek, c(b, fitc), fx)
fit_logit("R4b restricted alternatives, no fit controls", ek, b, fx)
se <- m[event_id %in% ek[chosen == 1, event_id]]
fit_logit("R4b same events, all alternatives, no fit controls", se, b, fx)
fit_logit("R4b same events, all alternatives, fit controls", se, c(b, fitc), fx)
save_records("rev4b_elo_results_R.csv")
