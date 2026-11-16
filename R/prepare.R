# Data preparation shared by all analyses: mirrors the first part of agency_paper/analyse.py.
suppressPackageStartupMessages({library(data.table)})
source(file.path(script_dir, "clogit_helpers.R"))
idcols <- c("date","transfer_date","player_id","club_id","candidate_club","origin_club","snapshot_club","destination_club","from_club_id","to_club_id")
rd <- function(p) { hd <- names(fread(p, nrows = 0)); fread(p, colClasses = list(character = intersect(idcols, hd)), na.strings = c("", "NA")) }
tf <- function(x) x %in% c("True", "TRUE", "true", TRUE)
countries <- c(BE1 = "Belgium", DK1 = "Denmark", ES1 = "Spain", FR1 = "France", GB1 = "England", GR1 = "Greece", IT1 = "Italy", L1 = "Germany",
               NL1 = "Netherlands", PO1 = "Portugal", RU1 = "Russia", SC1 = "Scotland", TR1 = "Turkey", UKR1 = "Ukraine")
controls <- c("log_known","nationality_share_10pp","nationality_unknown","ppg_rank","ppg_missing","elo100","elo_missing","position_count","position_minutes",
              "position_unknown","position_minutes_unknown","rating_gap","rating_gap_unknown")
flows <- c("forward_3", "reverse_3", "own_history_3")
fx <- "candidate_club"

# sample: "extended" (main), "extended_base", "extended_post", or "archived" (first-version linkage)
prepare <- function(root, sample = "extended") {
  perf <- file.path(root, if (sample == "archived") "perf" else paste0("perf_", sample))
  anchor <- if (sample == "archived") "origin_club" else "snapshot_club"

  players <- rd(file.path(perf, "player_performance.csv"))
  players[is.na(agent_label), agent_label := ""]; players[is.na(nationality), nationality := ""]
  players[, label := key(agent_label)]; players[, nation := key(nationality)]
  e <- rd(file.path(perf, "transfer_performance.csv"))
  for (cn in c("known_agent", "likely_return", "baseline_match")) e[[cn]] <- tf(e[[cn]])
  pl1 <- unique(players[, .(summer, player_id, club_id, label, nation, market_value_eur)])
  stopifnot(!anyDuplicated(pl1[, .(summer, player_id, club_id)]))
  e <- merge(e[known_agent == TRUE], pl1, by.x = c("summer", "player_id", anchor), by.y = c("summer", "player_id", "club_id"), sort = FALSE)
  if (sample != "archived") e <- e[snapshot_club != destination_club]   # loan-to-buy conversions: the loan, not a search, fixed the destination
  cand <- rd(file.path(perf, "within_league_candidates.csv"))
  cand[, chosen := as.integer(chosen)]
  c <- merge(cand, e, by = c("event_id", "summer"))
  stopifnot(!anyDuplicated(c[, .(event_id, candidate_club)]))

  # ---- prior club-to-club flows from the full transfer record
  h <- as.data.table(readr::read_csv(file.path(root, "transfers.gz"), col_select = c("player_id","transfer_date","from_club_id","to_club_id","transfer_fee"),
                                     col_types = readr::cols(.default = "c"), na = c("", "NA"), progress = FALSE))
  h <- h[!is.na(player_id) & !is.na(transfer_date) & !is.na(from_club_id) & !is.na(to_club_id) & from_club_id != to_club_id & transfer_date < "2025-06-01"]
  h[, positive_fee := fifelse(is.na(transfer_fee), 0, as.numeric(transfer_fee)) > 0]
  h <- h[, .(positive_fee = max(positive_fee)), by = .(player_id, transfer_date, from_club_id, to_club_id)]
  rc <- h[positive_fee == 0 & substr(transfer_date, 6, 10) %in% c("06-30", "07-01")]
  rc[, lower := paste0(as.integer(substr(transfer_date, 1, 4)) - 1, "-06-01")]
  opp <- h[, .(player_id, from_club_id = to_club_id, to_club_id = from_club_id, d = transfer_date)]   # opposite-direction moves
  j <- merge(rc, opp, by = c("player_id", "from_club_id", "to_club_id"), allow.cartesian = TRUE)
  flag <- unique(j[d >= lower & d < transfer_date, .(player_id, transfer_date, from_club_id, to_club_id)])
  h <- h[!flag, on = .(player_id, transfer_date, from_club_id, to_club_id)]

  add_flows <- function(fr, windows) {
    fr <- copy(fr)
    for (yrs in windows) {
      for (cn in c("forward", "reverse", "own_history")) fr[[paste0(cn, "_", yrs)]] <- 0
      for (yr in unique(fr$summer)) {
        past <- h[transfer_date >= paste0(yr - yrs, "-06-01") & transfer_date < paste0(yr, "-06-01")]
        pair <- past[, .(n = .N), by = .(from_club_id, to_club_id)]
        pp <- past[, .(m = .N), by = .(from_club_id, to_club_id, player_id)]
        idx <- which(fr$summer == yr); sub <- fr[idx]; own <- numeric(length(idx))
        for (dir in c("forward", "reverse")) {
          a <- if (dir == "forward") sub$origin_club else sub$candidate_club
          b2 <- if (dir == "forward") sub$candidate_club else sub$origin_club
          cnt <- pair[data.table(from_club_id = a, to_club_id = b2), on = .(from_club_id, to_club_id), n]; cnt[is.na(cnt)] <- 0
          foc <- pp[data.table(from_club_id = a, to_club_id = b2, player_id = sub$player_id), on = .(from_club_id, to_club_id, player_id), m]; foc[is.na(foc)] <- 0
          stopifnot(all(cnt >= foc))
          set(fr, idx, paste0(dir, "_", yrs), log1p(cnt - foc)); own <- own + foc
        }
        set(fr, idx, paste0("own_history_", yrs), as.numeric(own > 0))
      }
    }
    fr
  }
  c <- copy(add_flows(c, c(1, 3, 5)))
  c[, any_prior_flow := as.integer(forward_3 > 0 | reverse_3 > 0)]
  c[, same_prior_flow := as.integer(co_clients > 0) * any_prior_flow]
  known <- players[label != "", .(known = uniqueN(player_id)), by = .(summer, club_id)]
  nat <- players[nation != "", .(nat_peers = uniqueN(player_id)), by = .(summer, club_id, nation)]
  nat_total <- players[nation != "", .(nat_total = uniqueN(player_id)), by = .(summer, club_id)]
  c <- merge(c, known, by.x = c("summer", "candidate_club"), by.y = c("summer", "club_id"), all.x = TRUE)
  c <- merge(c, nat, by.x = c("summer", "candidate_club", "nation"), by.y = c("summer", "club_id", "nation"), all.x = TRUE)
  c <- merge(c, nat_total, by.x = c("summer", "candidate_club"), by.y = c("summer", "club_id"), all.x = TRUE)
  for (cn in c("known", "nat_peers", "nat_total")) c[is.na(get(cn)), (cn) := 0]
  c[, nationality_share_10pp := fifelse(nat_total == 0, NA_real_, 10 * nat_peers / nat_total)]
  c[, log_known := log1p(known)]
  c[, same := as.integer(co_clients > 0)]
  c[, co_client_count_log := log1p(co_clients)]
  c[, between := route == "between"]
  c[, same_between := same * between]
  c[, same_2024 := same * (summer == 2024)]; c[, same_2025 := same * (summer == 2025)]
  c[, turn_of_july := substr(date, 6, 10) %in% c("06-30", "07-01")]
  c[, same_turn_of_july := same * turn_of_july]
  c[, ppg_rank := fifelse(is.na(candidate_ppg_percentile), 0.5, candidate_ppg_percentile)]
  c[, ppg_missing := as.integer(is.na(candidate_ppg_percentile))]
  c[, position_count := log1p(fifelse(is.na(same_position_players), 0, same_position_players))]
  c[, position_minutes := log1p(fifelse(is.na(same_position_minutes), 0, same_position_minutes))]
  c[, position_unknown := as.integer(is.na(same_position_players))]
  c[, position_minutes_unknown := as.integer(is.na(same_position_minutes))]
  c[, rating_gap := fifelse(is.na(player_minus_median_rating), 0, player_minus_median_rating) / 10]
  c[, rating_gap_unknown := as.integer(is.na(player_minus_median_rating))]
  c[, elo100 := (fifelse(is.na(candidate_club_elo), 1500, candidate_club_elo) - 1500) / 100]
  c[, elo_missing := as.integer(is.na(candidate_club_elo))]
  c[, value_elo := elo100 * log1p(fifelse(is.na(market_value_eur), 0, market_value_eur) / 1e6)]
  c[, minutes_elo := elo100 * log1p(fifelse(is.na(player_minutes), 0, player_minutes) / 900)]
  c[, rating_elo := elo100 * (fifelse(is.na(player_ea_overall), 65, as.numeric(player_ea_overall)) - 65) / 10]
  c[, value_unknown_elo := elo100 * as.numeric(is.na(market_value_eur))]
  c[, rating_unknown_elo := elo100 * as.numeric(is.na(player_ea_overall))]
  c[, nationality_unknown := as.integer(is.na(nationality_share_10pp))]
  c[is.na(nationality_share_10pp), nationality_share_10pp := 0]
  c <- c[candidate_club != origin_club]
  c[, club_summer := paste0(candidate_club, "_", summer)]
  c[, roster_unavailable := as.integer(known == 0)]
  all_clubs <- copy(c); c <- c[known > 0]
  main <- c[likely_return == FALSE]
  # ---- league frame
  pl_l <- players[label != ""]
  counts <- pl_l[, .(cnt = .N), by = .(summer, league, label)]
  count_known <- pl_l[, .(ck = .N), by = .(summer, league)]
  snap <- unique(players[, .(summer, player_id, club_id, snapshot_league = league)])
  e <- merge(e, snap, by.x = c("summer", "player_id", anchor), by.y = c("summer", "player_id", "club_id"), all.x = TRUE)
  lg <- e[likely_return == FALSE]
  lg <- lg[, .(alternative = names(countries)), by = names(lg)]
  lg[, chosen := as.integer(destination_league == alternative)]
  lg <- merge(lg, counts, by.x = c("summer", "alternative", "label"), by.y = c("summer", "league", "label"), all.x = TRUE); lg[is.na(cnt), cnt := 0]
  lg <- merge(lg, count_known, by.x = c("summer", "alternative"), by.y = c("summer", "league"), all.x = TRUE)
  lg[, agency_log := log1p(pmax(0, cnt - (alternative == snapshot_league)))]
  lg[, log_roster := log(ck)]
  lg[, local_nationality := as.integer(nation == key(countries[alternative]))]
  lg[alternative == "TR1" & nation == "turkiye", local_nationality := 1L]
  lg[, route_fe := paste0(origin_league, "_", alternative)]
  setorder(lg, event_id)
  list(players = players, e = e, h = h, c = c, all_clubs = all_clubs, main = main, lg = lg, counts = counts)
}
turn <- function(d) d[!(substr(date, 6, 10) %in% c("06-30", "07-01"))]
