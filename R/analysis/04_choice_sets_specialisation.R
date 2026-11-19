# Plausible choice sets, tier and specialisation controls (Table 6 and Online Appendix).
# Usage: Rscript R/analysis/04_choice_sets_specialisation.R <data_root> [output_dir]
# Writes choice_set_results.csv, elo_decomposition_results.csv and specialisation_results.csv.
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "..", "lib", "setup.R"))
# ---- tier and affordability controls, restricted alternatives and the same-event Elo decomposition
local({
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
save_records("choice_set_results.csv")
fit_logit("R4b restricted alternatives, fit controls", ek, c(b, fitc), fx)
fit_logit("R4b restricted alternatives, no fit controls", ek, b, fx)
se <- m[event_id %in% ek[chosen == 1, event_id]]
fit_logit("R4b same events, all alternatives, no fit controls", se, b, fx)
fit_logit("R4b same events, all alternatives, fit controls", se, c(b, fitc), fx)
save_records("elo_decomposition_results.csv")

})

# ---- specialisation: distance between the candidate club's tier and the tier of the representative's other clients
local({
d <- prepare(root, "extended"); en <- enrich(root, "extended")
m <- add_flags(d$main, en$ev); pl <- d$players[label != ""]
# club tier (Elo) and squad value by summer
ce <- unique(rbind(d$c[, .(summer, club = candidate_club, elo = candidate_club_elo)], d$c[, .(summer, club = origin_club, elo = origin_elo)]))[!is.na(elo)]
ce <- ce[, .(elo = mean(elo)), by = .(summer, club)]
pl <- merge(pl, ce, by.x = c("summer", "club_id"), by.y = c("summer", "club"), all.x = TRUE, sort = FALSE)
pl[, mv := as.numeric(market_value_eur)]; pl[, lmv := log(mv + 1e5)]
agg <- pl[, .(n_elo = sum(!is.na(elo)), s_elo = sum(elo, na.rm = TRUE), n_mv = sum(!is.na(lmv)), s_mv = sum(lmv, na.rm = TRUE)), by = .(summer, label)]
focal <- unique(pl[, .(summer, player_id, f_elo = elo, f_mv = lmv)])
m <- merge(m, agg, by = c("summer", "label"), all.x = TRUE, sort = FALSE); m <- merge(m, focal, by = c("summer", "player_id"), all.x = TRUE, sort = FALSE)
m[, `:=`(n1 = n_elo - !is.na(f_elo), s1 = s_elo - fifelse(is.na(f_elo), 0, f_elo), n2 = n_mv - !is.na(f_mv), s2 = s_mv - fifelse(is.na(f_mv), 0, f_mv))]
m[, client_elo := fifelse(n1 > 0, s1 / n1, NA_real_)]
clubmv <- pl[, .(club_lmv = median(lmv, na.rm = TRUE)), by = .(summer, candidate_club = club_id)]
m <- merge(m, clubmv, by = c("summer", "candidate_club"), all.x = TRUE, sort = FALSE)
m[, client_lmv := fifelse(n2 > 0, s2 / n2, NA_real_)]
m[, elo_gap := abs(candidate_club_elo - client_elo) / 100]; m[, elo_gap_unknown := as.integer(is.na(elo_gap))]; m[is.na(elo_gap), elo_gap := 0]
m[, mv_gap := abs(club_lmv - client_lmv)]; m[, mv_gap_unknown := as.integer(is.na(mv_gap))]; m[is.na(mv_gap), mv_gap := 0]
m[, `:=`(elo_gap2 = elo_gap^2, mv_gap2 = mv_gap^2)]
sp <- c("elo_gap", "elo_gap2", "elo_gap_unknown", "mv_gap", "mv_gap2", "mv_gap_unknown")
b <- c("same", controls, flows)
cat("mean tier gap, chosen vs alternatives:", m[chosen == 1, mean(elo_gap)], m[chosen == 0, mean(elo_gap)], "\n")
fit_logit("R9 baseline", m, b, fx)
fit_logit("R9 + client-tier match (Elo, squad value)", m, c(b, sp), fx)
fit_logit("R9 + client-tier match, by gap", m[, same_close := same * (elo_gap <= median(elo_gap[same == 1]))][, same_far := same - same_close], c("same_close", "same_far", controls, flows, sp), fx,
          list("shared label, close tier match" = c(same_close = 1), "shared label, distant tier match" = c(same_far = 1)))
fit_logit("R9 events with known client tier", m[elo_gap_unknown == 0], c(b, sp), fx)
save_records("specialisation_results.csv")

})
