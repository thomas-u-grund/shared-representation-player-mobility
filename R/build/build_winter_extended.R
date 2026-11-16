# R port of winter_recovery/build_winter_extended.py.  Usage (from the output directory, which holds sources.csv):
#   Rscript build_winter_extended.R <kaggle_dir> <transfer_dir>
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "winter_common.R"))
args <- commandArgs(TRUE); stopifnot(length(args) == 2)
source_folder <- args[1]; transfer_folder <- args[2]
EARLIER <- list(`2024` = "240", `2025` = c("240", "325", "355", "435"))   # releases preceding each December snapshot
sources <- fread("sources.csv", colClasses = list(character = "version"), encoding = "UTF-8")

members <- load_members(read_py(file.path(transfer_folder, "games.gz"), select = c("season", "competition_id", "home_club_id", "away_club_id")))
e <- load_moves(file.path(transfer_folder, "transfers.gz"))

# fill still-empty labels from earlier releases (latest release first)
fill_labels <- function(d, year) {
  for (v in sort(EARLIER[[as.character(year)]], decreasing = TRUE)) {
    old <- read_py(file.path(source_folder, paste0("data_", v)), select = c("player_id", "agent_name"))
    old <- unique(old, by = "player_id")
    old[, lab := clean_label(agent_name)]
    fill <- old$lab[match(d$player_id, old$player_id)]; fill[is.na(fill)] <- ""
    use <- d$agent_label == "" & fill != ""
    d[use, agent_label := fill[use]]
  }
  d
}

snapshots <- list(); arrivals <- list(); candidates <- list(); counts <- list()
for (s in seq_len(nrow(sources))) {
  year <- sources$january[s]; version <- sources$version[s]; modified <- sources$file_last_modified[s]
  stopifnot(substr(modified, 1, 10) < sprintf("%d-01-01", year))
  p <- read_py(file.path(source_folder, paste0("data_", version)),
               select = c("player_id", "name", "last_season", "current_club_id", "agent_name"))
  clubs <- members[[as.character(year - 1)]]
  roster <- p[current_club_id %in% clubs$club & last_season %in% (year - 1)]
  stopifnot(!anyDuplicated(roster[, .(player_id, current_club_id)]))
  roster[, agent_label := clean_label(agent_name)]
  roster <- fill_labels(roster, year)
  roster[, `:=`(agent_key = make_key(agent_label), league = clubs$league[match(current_club_id, clubs$club)])]
  jan <- sprintf(c("%d-01-01", "%d-01-31"), year)
  inclub <- function(x) x %in% clubs$club
  z0 <- e[transfer_date >= jan[1] & transfer_date <= jan[2] & inclub(from_club_id) & inclub(to_club_id)]
  extra <- p[player_id %in% setdiff(z0$player_id, roster$player_id) & !(current_club_id %in% clubs$club)]
  extra[, agent_label := clean_label(agent_name)]
  extra <- fill_labels(extra, year)
  extra[, `:=`(agent_key = make_key(agent_label), league = "OUTSIDE")]
  keep <- function(d) d[, .(january = year, snapshot_version = version, snapshot_date = substr(modified, 1, 10), player_id,
                            player_name = name, club_id = current_club_id, league, agent_label, agent_key)]
  roster_all <- rbindlist(list(keep(roster), keep(extra)))
  snapshots[[s]] <- roster_all
  z <- copy(z0)
  z[, `:=`(origin_league = clubs$league[match(from_club_id, clubs$club)],
           destination_league = clubs$league[match(to_club_id, clubs$club)], january = year)]
  z[, event_id := sprintf("W%d-%04d", year, seq_len(.N))]
  stopifnot(!anyDuplicated(roster_all$player_id))
  z <- roster_all[, .(player_id, snapshot_club = club_id, agent_label, agent_key)][z, on = "player_id"]
  setcolorder(z, c("player_id", "transfer_date", "from_club_id", "to_club_id", "positive_fee", "origin_league",
                   "destination_league", "january", "event_id", "snapshot_club", "agent_label", "agent_key"))
  z[, `:=`(origin_snapshot_match = !is.na(snapshot_club) & snapshot_club == from_club_id, snapshot_club_listed = !is.na(snapshot_club))]
  z[is.na(agent_label), `:=`(agent_label = "", agent_key = "")]
  z[, reverse_move_in_prior_year := reverse_in_prior_year(z, e)]
  z[, zero_or_unknown_fee_reverse := !positive_fee & reverse_move_in_prior_year]
  labelled <- keep(roster)[agent_key != ""]
  cand <- winter_candidates(z, clubs, labelled, year)
  candidates[[s]] <- cand
  arrivals[[s]] <- z
  counts[[s]] <- data.table(january = year, clubs = nrow(clubs), roster_players = nrow(roster), named_roster_players = nrow(labelled),
    january_moves = nrow(z), between_league = sum(z$origin_league != z$destination_league),
    origin_matches = sum(z$origin_snapshot_match), player_listed = sum(z$snapshot_club_listed), named_movers = sum(z$agent_key != ""),
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
