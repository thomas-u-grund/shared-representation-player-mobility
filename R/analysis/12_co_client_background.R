# Does the association run through co-clients who resemble the mover? Candidate clubs are classified by whether their co-clients (other clients of the
# mover's agency on the pre-window roster) share the mover's background (Online Appendix).
# Usage: Rscript R/analysis/12_co_client_background.R <data_root> [output_dir]
# Writes co_client_background_results.csv (models) and co_client_background_counts.csv (how many choice sets distinguish the categories).
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "..", "lib", "setup.R"))
d <- prepare(root, "extended"); en <- enrich(root, "extended")
main <- add_flags(d$main, en$ev); main <- valid_events(main); pl <- d$players[label != ""]
base <- c(controls, flows)

# ---- co-client pairs: every other client of the mover's agency on the candidate club's pre-window roster
mv <- unique(d$players[, .(summer, player_id, snapshot_club = club_id, nat_m = nation, pos_m = position, age_m = age_at_cutoff)])
ev <- merge(unique(main[, .(event_id, summer, player_id, snapshot_club, label)]), mv, by = c("summer", "player_id", "snapshot_club"), all.x = TRUE)
ro <- pl[, .(summer, candidate_club = club_id, label, co_id = player_id, nat_c = nation, pos_c = position, age_c = age_at_cutoff)]
pairs <- merge(unique(main[, .(event_id, summer, candidate_club, label)]), ro, by = c("summer", "candidate_club", "label"), allow.cartesian = TRUE)
pairs <- merge(pairs, ev[, .(event_id, mover = player_id, nat_m, pos_m, age_m)], by = "event_id")[co_id != mover]
band <- function(a) as.integer(cut(a, c(0, 21, 25, 29, 99), labels = FALSE))
pairs[, `:=`(sim_nat = nat_c != "" & nat_c == nat_m, sim_pos = !is.na(pos_c) & pos_c == pos_m, sim_age = !is.na(age_c) & !is.na(band(age_c)) & band(age_c) == band(age_m))]
cat("co-client pairs:", nrow(pairs), "; unknown nationality (counted as different):", pairs[, sum(nat_c == "" | nat_m == "", na.rm = TRUE)], "\n")

# ---- shared earlier club: the mover and the co-client both appeared for or against a common club in the transfer record before the window (the candidate club itself excluded)
tr <- en$tr[!is.na(player_id) & !is.na(from_club_id) & !is.na(to_club_id) & !is.na(transfer_date), .(player_id, from_club_id, to_club_id, transfer_date)]
hl <- rbind(tr[, .(player_id, club = from_club_id, transfer_date)], tr[, .(player_id, club = to_club_id, transfer_date)])
hist <- rbindlist(lapply(sort(unique(main$summer)), function(s) unique(rbind(hl[transfer_date < paste0(s, "-06-01"), .(player_id, club)],
                  d$players[summer == s, .(player_id, club = club_id)]))[, summer := s]))
pairs[, pid := .I]
j <- merge(pairs[, .(pid, summer, mover, co_id, candidate_club)], hist[, .(summer, mover = player_id, club)], by = c("summer", "mover"), allow.cartesian = TRUE)
j <- merge(j, hist[, .(summer, co_id = player_id, club)], by = c("summer", "co_id", "club"))[club != candidate_club]
pairs[, sim_club := pid %in% j$pid]
nh <- hist[, .N, by = .(summer, player_id)]
pairs <- merge(pairs, nh[, .(summer, mover = player_id, n_m = N)], by = c("summer", "mover"), all.x = TRUE); pairs <- merge(pairs, nh[, .(summer, co_id = player_id, n_c = N)], by = c("summer", "co_id"), all.x = TRUE)
cat("pairs where both players have an earlier club on record:", round(mean(!is.na(pairs$n_m) & !is.na(pairs$n_c) & pairs$n_m > 1 & pairs$n_c > 1), 3), "\n")

# ---- classify each candidate club
dims <- list(nationality = "sim_nat", `shared earlier club` = "sim_club", position = "sim_pos", age = "sim_age")
counts <- list()
classify <- function(dim) {
  v <- dims[[dim]]
  a <- pairs[, .(n_sim = sum(get(v)), n_dif = sum(!get(v))), by = .(event_id, candidate_club)]
  m <- merge(main, a, by = c("event_id", "candidate_club"), all.x = TRUE, sort = FALSE); m[is.na(n_sim), n_sim := 0L]; m[is.na(n_dif), n_dif := 0L]
  m[, cat := fcase(n_sim > 0 & n_dif == 0, "same_only", n_sim == 0 & n_dif > 0, "diff_only", n_sim > 0 & n_dif > 0, "both", default = "none")]
  m[, `:=`(same_only = as.numeric(cat == "same_only"), diff_only = as.numeric(cat == "diff_only"), both = as.numeric(cat == "both"))]
  stopifnot(mean(m$n_sim + m$n_dif == m$co_clients) > 0.99)
  m
}
for (dim in names(dims)) {
  m <- classify(dim)
  tab <- m[, .(alternatives = .N, chosen = sum(chosen)), by = cat][match(c("none", "same_only", "diff_only", "both"), cat)]
  ev_ok <- m[, .(has_same = any(cat == "same_only"), has_diff = any(cat == "diff_only"), has_both = any(cat == "both")), by = event_id]
  counts[[dim]] <- cbind(dimension = dim, tab, events = uniqueN(m$event_id), events_with_same_only_and_diff_only = sum(ev_ok$has_same & ev_ok$has_diff),
                         chosen_share = tab$chosen / tab$alternatives)
  tm <- list("same-background co-clients only" = c(same_only = 1), "different-background co-clients only" = c(diff_only = 1), "both" = c(both = 1),
             "different only minus same only (ratio)" = c(diff_only = 1, same_only = -1))
  fit_logit(paste("X1", dim, "four categories"), m, c("same_only", "diff_only", "both", base), fx, tm)
  # exactly one co-client: clubs with larger agency footprints cannot end up in "both"
  m1 <- m[n_sim + n_dif <= 1]; m1[, `:=`(one_sim = as.numeric(n_sim == 1), one_dif = as.numeric(n_dif == 1))]
  fit_logit(paste("X2", dim, "exactly one co-client"), m1, c("one_sim", "one_dif", base), fx,
            list("one same-background co-client" = c(one_sim = 1), "one different-background co-client" = c(one_dif = 1), "different minus same (ratio)" = c(one_dif = 1, one_sim = -1)))
  if (dim == "nationality") {   # clubs with and without recorded transfers in either direction
    m[, np := as.numeric(any_prior_flow == 0)]
    for (k in c("same_only", "diff_only", "both")) { set(m, j = paste0(k, "_np"), value = m[[k]] * m$np); set(m, j = paste0(k, "_p"), value = m[[k]] * (1 - m$np)) }
    tt <- c(paste0(c("same_only", "diff_only", "both"), "_np"), paste0(c("same_only", "diff_only", "both"), "_p"))
    fit_logit("X3 nationality by prior transfers", m, c(tt, base), fx, setNames(lapply(tt, function(z) setNames(1, z)), c(
      "same-background only, no prior transfers", "different-background only, no prior transfers", "both, no prior transfers",
      "same-background only, prior transfers", "different-background only, prior transfers", "both, prior transfers")))
  }
}
save_records("co_client_background_results.csv")
fwrite(rbindlist(counts), file.path(outdir, "co_client_background_counts.csv"))
