# Build the January rosters, transfers and candidate sets from the December snapshots.  Usage (from the output directory, which holds sources.csv):
#   Rscript 04_winter_rosters.R <kaggle_dir> <transfer_dir>
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "winter_common.R"))
args <- commandArgs(TRUE); stopifnot(length(args) == 2)
source_folder <- args[1]; transfer_folder <- args[2]
sources <- fread("sources.csv", colClasses = list(character = "version"), encoding = "UTF-8")

members <- load_members(read_table(file.path(transfer_folder, "games.gz"), select = c("season", "competition_id", "home_club_id", "away_club_id")))
e <- load_moves(file.path(transfer_folder, "transfers.gz"))

snapshots <- list(); arrivals <- list(); candidates <- list(); counts <- list()
for (s in seq_len(nrow(sources))) {
  year <- sources$january[s]; version <- sources$version[s]; modified <- sources$file_last_modified[s]
  stopifnot(substr(modified, 1, 10) < sprintf("%d-01-01", year))
  p <- read_table(file.path(source_folder, paste0("data_", version)),
               select = c("player_id", "name", "last_season", "current_club_id", "agent_name"))
  clubs <- members[[as.character(year - 1)]]
  roster <- p[current_club_id %in% clubs$club & last_season %in% (year - 1)]
  stopifnot(!anyDuplicated(roster[, .(player_id, current_club_id)]))
  roster[, agent_label := clean_label(agent_name)]
  roster[, agent_key := make_key(agent_label)]
  roster[, league := clubs$league[match(current_club_id, clubs$club)]]
  roster <- roster[, .(january = year, snapshot_version = version, snapshot_date = substr(modified, 1, 10), player_id,
                       player_name = name, club_id = current_club_id, league, agent_label, agent_key)]
  snapshots[[s]] <- roster
  jan <- sprintf(c("%d-01-01", "%d-01-31"), year)
  z <- e[transfer_date >= jan[1] & transfer_date <= jan[2] & from_club_id %in% clubs$club & to_club_id %in% clubs$club]
  z[, `:=`(origin_league = clubs$league[match(from_club_id, clubs$club)],
           destination_league = clubs$league[match(to_club_id, clubs$club)], january = year)]
  z[, event_id := sprintf("W%d-%04d", year, seq_len(.N))]
  r2 <- roster[, .(player_id, from_club_id = club_id, club_id, agent_label, agent_key)]
  z <- r2[z, on = .(player_id, from_club_id)]
  setcolorder(z, c("player_id", "transfer_date", "from_club_id", "to_club_id", "positive_fee", "origin_league",
                   "destination_league", "january", "event_id", "club_id", "agent_label", "agent_key"))
  z[, origin_snapshot_match := !is.na(club_id)]
  z[is.na(agent_label), `:=`(agent_label = "", agent_key = "")]
  z[, reverse_move_in_prior_year := reverse_in_prior_year(z, e)]
  z[, zero_or_unknown_fee_reverse := !positive_fee & reverse_move_in_prior_year]
  labelled <- roster[agent_key != ""]
  cand <- winter_candidates(z, clubs, labelled, year)
  candidates[[s]] <- cand
  arrivals[[s]] <- z[, club_id := NULL]
  counts[[s]] <- data.table(january = year, clubs = nrow(clubs), roster_players = nrow(roster), named_roster_players = nrow(labelled),
    january_moves = nrow(z), between_league = sum(z$origin_league != z$destination_league),
    origin_matches = sum(z$origin_snapshot_match), named_movers = sum(z$agent_key != ""),
    named_excluding_zero_or_unknown_fee_reversals = sum(z$agent_key != "" & !z$zero_or_unknown_fee_reverse),
    eligible_choices = uniqueN(cand$event_id))
}
write_py_csv(rbindlist(snapshots), "winter_player_snapshots.csv")
write_py_csv(rbindlist(arrivals), "january_transfers.csv")
cc <- rbindlist(candidates)
stopifnot(!anyDuplicated(cc[, .(event_id, candidate_club)]), all(cc[, sum(chosen), by = event_id]$V1 == 1))
write_py_csv(cc, "january_destination_candidates.csv")
cov <- rbindlist(counts)
write_py_csv(cov, "coverage.csv")
print(cov)
