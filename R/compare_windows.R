# Winter vs summer destination sorting on harmonised risk sets.  Usage: Rscript R/compare_windows.R <data_root> [output_dir] [extended|archived]
args <- commandArgs(TRUE); root <- args[1]; outdir <- if (length(args) > 1) args[2] else "."; sample <- if (length(args) > 2) args[3] else "extended"
script_dir <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE))
source(file.path(script_dir, "prepare.R")); source(file.path(script_dir, "enrich.R"))
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
perf <- file.path(root, if (sample == "extended") "perf_extended" else "perf")
wdir <- file.path(root, if (sample == "extended") "winter_recovery_extended" else "winter_recovery")
anchor_sum <- if (sample == "extended") "player" else "origin"

raw <- as.data.table(readr::read_csv(file.path(root, "transfers.gz"), col_select = c("player_id", "from_club_id", "to_club_id", "transfer_date", "transfer_fee"),
                                      col_types = readr::cols(.default = "c"), na = c("", "NA"), progress = FALSE))
raw <- raw[!is.na(player_id) & !is.na(from_club_id) & !is.na(to_club_id) & !is.na(transfer_date) & from_club_id != to_club_id]
raw[, positive_fee := fifelse(is.na(suppressWarnings(as.numeric(transfer_fee))), 0, as.numeric(transfer_fee)) > 0]
history <- raw[, .(positive_fee = max(positive_fee)), by = .(player_id, transfer_date, from_club_id, to_club_id)]
opp <- history[, .(player_id, from_club_id = to_club_id, to_club_id = from_club_id, rd = transfer_date)]
jj <- merge(history[, .(player_id, transfer_date, from_club_id, to_club_id)], opp, by = c("player_id", "from_club_id", "to_club_id"), allow.cartesian = TRUE)
jj <- unique(jj[rd < transfer_date & as.Date(rd) >= as.Date(transfer_date) - 365, .(player_id, transfer_date, from_club_id, to_club_id)])
history[, possible_return := FALSE]; history[jj, possible_return := TRUE, on = .(player_id, transfer_date, from_club_id, to_club_id)]
history[, possible_return := possible_return & positive_fee == FALSE]
labelled <- history[possible_return == FALSE]

roster_covariates <- function(c, p, anchor) {
  p <- copy(p); p[is.na(agent_label), agent_label := ""]; p[is.na(nationality), nationality := ""]
  known <- p[agent_label != "", .(known = .N), by = club_id]
  nat <- p[nationality != ""]; n <- nat[, .(base = .N), by = club_id]; same <- nat[, .(co = .N), by = .(club_id, nationality)]
  pp <- unique(p[, .(player_id, club_id, agent_label, nationality)])
  if (anchor == "player") { stopifnot(!anyDuplicated(pp$player_id)); c <- merge(c, pp, by = "player_id"); c <- c[club_id != destination_club] }
  else c <- merge(c, pp, by.x = c("player_id", "origin_club"), by.y = c("player_id", "club_id"), sort = FALSE)
  c <- merge(c, known, by.x = "candidate_club", by.y = "club_id", all.x = TRUE, sort = FALSE); c[is.na(known), known := 0L]
  c <- merge(c, n, by.x = "candidate_club", by.y = "club_id", all.x = TRUE, sort = FALSE); c[is.na(base), base := 0L]
  c <- merge(c, same, by.x = c("candidate_club", "nationality"), by.y = c("club_id", "nationality"), all.x = TRUE, sort = FALSE); c[is.na(co), co := 0L]
  c[, rsize := log1p(known)]
  c[, nationality_missing := as.numeric(base == 0 | nationality == "")]
  c[, nationality_share := fifelse(base > 0, co / base, 0) * 10]
  c[known > 0]
}
annotate <- function(c, period, yr) {
  c <- copy(c); c[, season := yr]; c[, winter := as.integer(period == "winter")]
  c[, event_id := paste0(substr(period, 1, 1), yr, "-", event_id)]
  c[, same := as.integer(co_clients > 0)]
  c[, turn_of_july := substr(date, 6, 10) %in% c("06-30", "07-01")]
  c <- merge(c, history[, .(player_id, date = transfer_date, origin_club = from_club_id, destination_club = to_club_id, possible_return)], by = c("player_id", "date", "origin_club", "destination_club"), all.x = TRUE, sort = FALSE)
  stopifnot(!anyNA(c$possible_return))
  cut <- if (period == "summer") as.Date(paste0(yr, "-06-01")) else as.Date(paste0(yr + 1, "-01-01"))
  lo <- seq(cut, by = "-3 years", length.out = 2)[2]
  past <- labelled[as.Date(transfer_date) >= lo & as.Date(transfer_date) < cut]
  pair <- past[, .(a = .N), by = .(from_club_id, to_club_id)]; own <- past[, .(b = .N), by = .(from_club_id, to_club_id, player_id)]
  ownc <- numeric(nrow(c))
  for (dir in c("forward", "reverse")) {
    f <- if (dir == "forward") c$origin_club else c$candidate_club; t <- if (dir == "forward") c$candidate_club else c$origin_club
    a <- pair[data.table(from_club_id = f, to_club_id = t), on = .(from_club_id, to_club_id), a]; a[is.na(a)] <- 0
    b <- own[data.table(from_club_id = f, to_club_id = t, player_id = c$player_id), on = .(from_club_id, to_club_id, player_id), b]; b[is.na(b)] <- 0
    stopifnot(all(a >= b)); set(c, j = dir, value = log1p(a - b)); ownc <- ownc + b
  }
  c[, own_history := as.numeric(ownc > 0)]
  c[, label := key(agent_label)]; stopifnot(!any(c$label == ""))
  c[, club_season := paste0(candidate_club, "_", season)]
  c
}

sp <- rd(file.path(perf, "player_performance.csv")); se <- rd(file.path(perf, "transfer_performance.csv"))
sc <- merge(rd(file.path(perf, "within_league_candidates.csv")), se[, .(event_id, player_id, origin_club, destination_club, date)], by = "event_id")
summer <- lapply(sort(unique(sc$summer)), function(y) annotate(roster_covariates(sc[summer == y], sp[summer == y], anchor_sum), "summer", y))

winter <- function(path, versions, rosters, transfers, candidates, anchor) {
  p <- rd(file.path(path, paste0(rosters, ".csv"))); p[, january := as.integer(january)]
  e <- rd(file.path(path, paste0(transfers, ".csv"))); setnames(e, c("from_club_id", "to_club_id", "transfer_date"), c("origin_club", "destination_club", "date")); e[, january := as.integer(january)]
  c <- rd(file.path(path, paste0(candidates, ".csv")))[, .(event_id, january, candidate_club, chosen, co_clients)]; c[, january := as.integer(january)]
  c <- merge(c, e[, .(event_id, january, player_id, origin_club, destination_club, date)], by = c("event_id", "january"))
  lapply(intersect(sort(unique(c$january)), as.integer(names(versions))), function(y) annotate(roster_covariates(c[january == y], p[january == y], anchor), "winter", y - 1))
}
observed <- winter(wdir, c(`2024` = 325, `2025` = 547), "winter_player_snapshots", "january_transfers", "january_destination_candidates", anchor_sum)
backcast <- function(path) winter(path, c(`2026` = 0), "backcast_rosters", "backcast_january_transfers", "backcast_candidates", "origin")[[1]]
w26a <- backcast(file.path(root, "winter_proxy")); w26b <- backcast(file.path(root, "winter_proxy", "version_606_sensitivity"))

fit_cw <- function(name, x, fixed = "candidate_club") {
  x <- retained(x, fixed)
  x[, same_winter := same * winter]
  nm0 <- length(records$rows)
  fit_logit(name, x, c("same", "same_winter", "rsize", "nationality_share", "nationality_missing", "forward", "reverse", "own_history"), fixed,
            list(summer = c(same = 1), winter = c(same = 1, same_winter = 1), winter_to_summer = c(same_winter = 1)), quiet = TRUE)
  ch <- x[chosen == 1]
  for (i in (nm0 + 1):length(records$rows)) { records$rows[[i]]$summer_events <- sum(ch$winter == 0); records$rows[[i]]$winter_events <- sum(ch$winter) }
}
primary <- rbindlist(c(summer[1:2], observed), fill = TRUE); mainw <- primary[possible_return == FALSE]
fit_cw("Observed paired seasons", mainw)
fit_cw("Observed paired seasons club-by-season FE", mainw, "club_season")
fit_cw("Observed paired seasons excluding 30 June and 1 July", mainw[turn_of_july == FALSE])
fit_cw("Observed paired seasons all linked moves", primary)
for (v in list(c(603, "a"), c(606, "b"))) {
  f <- rbindlist(list(mainw, summer[[3]], if (v[2] == "a") w26a else w26b), fill = TRUE)
  fit_cw(sprintf("Add summer 2025 and January 2026 proxy %s", v[1]), f[possible_return == FALSE])
}
res <- rbindlist(records$rows, fill = TRUE)[term %in% c("summer", "winter", "winter_to_summer")]
long <- rbindlist(lapply(c("label", "player_id"), function(cl) res[, .(model, term, cluster = cl, events, summer_events, winter_events, odds_ratio, lo = get(paste0(cl, "_lo")), hi = get(paste0(cl, "_hi")))]))
fwrite(long, file.path(outdir, "season_comparison_results_R.csv"))
