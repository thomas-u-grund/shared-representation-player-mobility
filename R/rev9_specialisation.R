# Specialisation control: distance between the candidate club's tier and the tier of the representative's other clients.
# Usage: Rscript R/rev9_specialisation.R <data_root> [output_dir]
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "hdr.R"))
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
save_records("rev9_specialisation_results_R.csv")
