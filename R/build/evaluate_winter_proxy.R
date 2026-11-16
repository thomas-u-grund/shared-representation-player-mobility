# R port of winter_proxy/evaluate.py.  Usage (from the output directory, which holds proxy_sources.csv):
#   Rscript evaluate_winter_proxy.R <kaggle_dir> <transfer_dir> <observed_winter_dir>
# The conditional logits are fitted by Newton-Raphson with the same cluster-robust (influence-function)
# variance as the Python estimator (event-stratified McFadden logit; clustered by label and by player).
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "winter_common.R"))
args <- commandArgs(TRUE); stopifnot(length(args) == 3)
snap_dir <- args[1]; match_dir <- args[2]; obs_dir <- args[3]
sources <- fread("proxy_sources.csv", colClasses = list(character = "version", character = "snapshot_date"), encoding = "UTF-8")

key2 <- function(label) {                      # evaluate.py key(): strip/casefold, missing markers -> ''
  k <- make_key(label)
  k[stri_trans_casefold(stri_trim_both(label)) %in% BAD] <- ""
  k
}

g <- read_py(file.path(match_dir, "games.gz"), select = c("season", "competition_id", "home_club_id", "away_club_id"))
members <- load_members(g)
moves <- load_moves(file.path(match_dir, "transfers.gz"), need_player = TRUE)
obs <- read_py(file.path(obs_dir, "winter_player_snapshots.csv"))
obs[is.na(agent_label), agent_label := ""]; obs[is.na(agent_key), agent_key := ""]; obs[is.na(player_name), player_name := ""]

# Reverse a unique chain on each date, newest date first.
backcast1 <- function(current, d, a, b) {
  for (dt in sort(unique(d), decreasing = TRUE, method = "radix")) {
    idx <- which(d == dt); ea <- a[idx]; eb <- b[idx]
    while (length(ea)) {
      m <- which(eb == current)
      if (length(m) != 1L) return(NA_integer_)
      current <- ea[m]; ea <- ea[-m]; eb <- eb[-m]
    }
  }
  current
}

january_moves <- function(year, clubs) {
  z <- moves[transfer_date >= sprintf("%d-01-01", year) & transfer_date <= sprintf("%d-01-31", year) &
             from_club_id %in% clubs$club & to_club_id %in% clubs$club]
  z[, january := year]
  z[, event_id := sprintf("W%d-%04d", year, seq_len(.N))]
  z[, reverse_flag := !positive_fee & reverse_in_prior_year(z, moves)]
  z
}

make_candidates <- function(z, roster, clubs) {
  lab <- unique(roster[agent_key != "", .(player_id, club_id, agent_key)], by = c("player_id", "club_id"))
  size <- lab[, .(known_roster_labels = .N), by = club_id]
  co <- lab[, .(n = .N), by = .(club_id, agent_key)]
  ev <- copy(z); ev[, k := .I]
  ev[lab, on = .(player_id, from_club_id = club_id), a := i.agent_key]
  ev[size, on = .(to_club_id = club_id), tsize := i.known_roster_labels]
  ev <- ev[!is.na(a) & !is.na(tsize)]
  ev[, dest := clubs$league[match(to_club_id, clubs$club)]]
  cl <- merge(clubs, size, by.x = "club", by.y = "club_id")
  cand <- cl[ev, on = .(league = dest), allow.cartesian = TRUE, nomatch = NULL]
  cand <- cand[club != from_club_id]
  cand[, n := .N, by = k]
  cand <- cand[n >= 2]
  setorder(cand, k, club)
  cand[co, on = .(club = club_id, a = agent_key), cn := i.n]
  cand[is.na(cn), cn := 0L]
  cand[lab, on = .(player_id, club = club_id), foc := i.agent_key == a]
  cand[is.na(foc), foc := FALSE]
  cand[, .(event_id, january, player_id, origin_club = from_club_id, candidate_club = club,
           chosen = as.integer(club == to_club_id), agent_key = a, reverse_flag,
           known_roster_labels, co_clients = cn - as.integer(foc))]
}

# directional three-year club-pair flow counts excluding the focal player
flow_counts <- function(frame) {
  frame <- copy(frame)
  years <- sort(unique(frame$january))
  lo <- sprintf("%d-01-01", min(years) - 3L); hi <- sprintf("%d-01-01", max(years))
  rec <- moves[transfer_date >= lo & transfer_date < hi]
  rec[, suspect := !positive_fee & reverse_in_prior_year(rec, moves)]
  frame[, `:=`(forward = 0, reverse = 0)]
  for (yr in years) {
    past <- rec[transfer_date >= sprintf("%d-01-01", yr - 3L) & transfer_date < sprintf("%d-01-01", yr) & !suspect]
    pair <- past[, .(N = .N), by = .(from_club_id, to_club_id)]
    own <- past[, .(N = .N), by = .(from_club_id, to_club_id, player_id)]
    idx <- which(frame$january == yr); x <- frame[idx]
    cnt <- function(f, t) {
      q <- data.table(from_club_id = f, to_club_id = t, player_id = x$player_id)
      a <- pair[q, on = c("from_club_id", "to_club_id"), x.N]
      b <- own[q, on = c("from_club_id", "to_club_id", "player_id"), x.N]
      a[is.na(a)] <- 0L; b[is.na(b)] <- 0L
      log1p(as.numeric(a - b))
    }
    set(frame, idx, "forward", cnt(x$origin_club, x$candidate_club))
    set(frame, idx, "reverse", cnt(x$candidate_club, x$origin_club))
  }
  frame
}

# ---- conditional logit ----
fit_model <- function(frame, name, specification) {
  x <- copy(frame)
  if (specification == "flow_adjusted") repeat {
    n <- nrow(x)
    x <- x[candidate_club %in% x[chosen == 1, candidate_club]]
    f <- x[, .(size = .N, s = sum(chosen)), by = event_id]
    x <- x[event_id %in% f[size >= 2 & s == 1, event_id]]
    if (nrow(x) == n) break
  }
  if (!nrow(x)) return(NULL)
  setorder(x, event_id)
  gi <- match(x$event_id, unique(x$event_id)); ng <- max(gi); y <- x$chosen
  stopifnot(all(rowsum(y, gi) == 1))
  Z <- cbind(as.numeric(x$co_clients > 0), log1p(x$known_roster_labels))
  if (specification == "flow_adjusted") {
    lev <- sort(unique(x$candidate_club))
    D <- outer(x$candidate_club, lev[-1], "==") * 1                 # full dummy set is collinear within events: drop one level
    Z <- cbind(Z, x$forward, x$reverse, D)
  }
  probs <- function(b) { v <- drop(Z %*% b); vm <- as.numeric(tapply(v, gi, max)); ex <- exp(v - vm[gi]); tot <- as.numeric(rowsum(ex, gi)); list(nll = sum(log(tot) + vm) - sum(y * v), p = ex / tot[gi]) }
  b <- numeric(ncol(Z)); cur <- probs(b)
  hess <- function(p) { M <- rowsum(Z * p, gi); crossprod(Z, Z * p) - crossprod(M) }
  for (it in 1:200) {
    gr <- drop(crossprod(Z, cur$p - y))
    if (max(abs(gr)) < 1e-11) break
    H <- hess(cur$p)
    step <- tryCatch(solve(H, gr), error = function(e) drop(MASS::ginv(H) %*% gr))
    t <- 1; ok <- FALSE
    while (t > 1e-10) { nb <- b - t * step; nw <- probs(nb); if (is.finite(nw$nll) && nw$nll <= cur$nll + 1e-14) { ok <- TRUE; break }; t <- t / 2 }
    if (!ok) break
    done <- abs(cur$nll - nw$nll) < 1e-15 * max(1, abs(cur$nll)); b <- nb; cur <- nw
    if (done && max(abs(drop(crossprod(Z, cur$p - y)))) < 1e-6) break
  }
  H <- hess(cur$p)
  influence <- MASS::ginv(H)[, 1]
  sc <- (y - cur$p) * drop(Z %*% influence)
  out <- list()
  for (col in c("agent_key", "player_id")) {
    s <- as.numeric(rowsum(sc, as.character(x[[col]])))
    G <- length(s); se <- sqrt(sum(s^2) * G / (G - 1))
    out[[length(out) + 1]] <- data.table(sample = name, specification = specification, cluster = col, events = ng,
      alternatives = nrow(x), odds_ratio = exp(b[1]), lo = exp(b[1] - 1.96 * se), hi = exp(b[1] + 1.96 * se))
  }
  rbindlist(out)
}

summary_rows <- list(); roster_files <- list(); transfers <- list(); proxy_candidates <- list()
actual_candidates <- list(); common_actual <- list(); common_proxy <- list(); paired_out <- list()
for (s in seq_len(nrow(sources))) {
  year <- sources$january[s]; version <- sources$version[s]; snapdate <- sources$snapshot_date[s]
  clubs <- members[[as.character(year - 1)]]
  p <- read_py(file.path(snap_dir, paste0("data_", version)),
               select = c("player_id", "name", "last_season", "current_club_id", "agent_name"))
  stopifnot(!anyDuplicated(p$player_id))
  p <- p[!is.na(current_club_id)]
  p[, agent_label := fifelse(is.na(agent_name), "", agent_name)]
  p[, agent_key := key2(agent_label)]
  e <- moves[transfer_date >= sprintf("%d-01-01", year) & transfer_date < snapdate]
  p[, in_ch := player_id %in% e$player_id]
  pf <- p[(!is.na(last_season) & last_season == year - 1) | in_ch]
  pf[, `:=`(baseline = as.integer(current_club_id), traced = 0L)]
  ch <- split(e, e$player_id)
  unresolved <- 0L
  for (i in which(pf$in_ch)) {
    cp <- ch[[as.character(pf$player_id[i])]]
    r <- backcast1(as.integer(pf$current_club_id[i]), cp$transfer_date, cp$from_club_id, cp$to_club_id)
    pf$traced[i] <- nrow(cp)
    if (is.na(r)) {
      pf$baseline[i] <- NA_integer_
      if (pf$current_club_id[i] %in% clubs$club || any(cp$from_club_id %in% clubs$club | cp$to_club_id %in% clubs$club)) unresolved <- unresolved + 1L
    } else pf$baseline[i] <- r
  }
  pf <- pf[!is.na(baseline) & baseline %in% clubs$club]
  roster <- pf[, .(january = year, player_id, club_id = baseline, post_snapshot_club_id = as.integer(current_club_id),
                   league = clubs$league[match(baseline, clubs$club)], player_name = fifelse(is.na(name), "", name),
                   agent_label, agent_key, traced_moves = traced)]
  stopifnot(!anyDuplicated(roster$player_id))
  z <- january_moves(year, clubs)
  z <- roster[, .(player_id, from_club_id = club_id, club_id, agent_label, agent_key)][z, on = .(player_id, from_club_id)]
  setcolorder(z, c("player_id", "transfer_date", "from_club_id", "to_club_id", "positive_fee", "january", "event_id",
                   "reverse_flag", "club_id", "agent_label", "agent_key"))
  z[is.na(agent_label), `:=`(agent_label = "", agent_key = "")]
  transfers[[s]] <- z
  cand <- make_candidates(z, roster, clubs)
  roster_files[[s]] <- roster; proxy_candidates[[s]] <- cand
  row <- list(january = year, raw_moves = nrow(z), named_origin_matches = sum(z$agent_key != ""),
              named_origin_matches_screened = sum(z$agent_key != "" & !z$reverse_flag),
              reconstructed_roster = nrow(roster), changed_club = sum(roster$club_id != roster$post_snapshot_club_id),
              unresolved_chains = unresolved)
  if (year %in% c(2024, 2025)) {
    a <- obs[january == year]
    ap <- make_candidates(z, a, clubs); actual_candidates[[length(actual_candidates) + 1]] <- ap
    stopifnot(!anyDuplicated(a$player_id))
    # q: inner merge of December and backcast rosters on player_id
    qa <- a[match(a$player_id, roster$player_id, nomatch = 0L) > 0L]
    qp <- roster[match(qa$player_id, roster$player_id)]
    both <- qa$agent_key != "" & qp$agent_key != ""
    kk <- function(d) paste(d$event_id, d$candidate_club)
    ix <- match(kk(ap), kk(cand)); stopifnot(!anyDuplicated(kk(cand)), !anyDuplicated(kk(ap)))
    ap2 <- ap[!is.na(ix)]; c2 <- cand[ix[!is.na(ix)]]
    paired <- data.table(event_id = ap2$event_id, january_december = ap2$january, player_id_december = ap2$player_id,
      origin_club_december = ap2$origin_club, candidate_club = ap2$candidate_club, chosen_december = ap2$chosen,
      agent_key_december = ap2$agent_key, reverse_flag_december = ap2$reverse_flag,
      known_roster_labels_december = ap2$known_roster_labels, co_clients_december = ap2$co_clients,
      january_proxy = c2$january, player_id_proxy = c2$player_id, origin_club_proxy = c2$origin_club,
      chosen_proxy = c2$chosen, agent_key_proxy = c2$agent_key, reverse_flag_proxy = c2$reverse_flag,
      known_roster_labels_proxy = c2$known_roster_labels, co_clients_proxy = c2$co_clients)
    paired <- paired[!(reverse_flag_december)]
    adm <- paired[, .(sm = sum(chosen_december), sz = .N), by = event_id]
    paired <- paired[event_id %in% adm[sm == 1 & sz >= 2, event_id]]
    ks <- kk(paired)
    common_actual[[length(common_actual) + 1]] <- ap[kk(ap) %in% ks]
    common_proxy[[length(common_proxy) + 1]] <- cand[kk(cand) %in% ks]
    ad <- ap[chosen == 1, .(event_id, agent_key_december = agent_key)]
    pr <- cand[chosen == 1, .(event_id, agent_key_proxy = agent_key, reverse_flag)]
    az <- merge(ad, pr, by = "event_id")[!(reverse_flag)]
    ax <- paired$co_clients_december > 0; bx <- paired$co_clients_proxy > 0
    selected <- paired$chosen_december == 1
    paired_out[[as.character(year)]] <- paired
    row <- c(row, list(december_roster = nrow(a), overlapping_players = nrow(qa),
      same_club = sum(qa$club_id == qp$club_id), agent_comparable = sum(both),
      same_agent = sum(both & qa$agent_key == qp$agent_key),
      paired_screened_events = uniqueN(paired$event_id), paired_candidates = nrow(paired),
      same_shared_indicator = sum(ax == bx), shared_december = sum(ax), shared_proxy = sum(bx), shared_in_both = sum(ax & bx),
      mover_agent_comparable = nrow(az), mover_agent_same = sum(az$agent_key_december == az$agent_key_proxy),
      chosen_shared_december = sum(ax & selected), chosen_shared_proxy = sum(bx & selected),
      chosen_shared_in_both = sum(ax & bx & selected)))
  }
  summary_rows[[s]] <- row
}

p_all <- flow_counts(rbindlist(proxy_candidates))
a_all <- flow_counts(rbindlist(actual_candidates))
a_common <- flow_counts(rbindlist(common_actual))
p_common <- flow_counts(rbindlist(common_proxy))
models <- list()
samples <- list(
  list("matched 2024–25 December", a_common), list("matched 2024–25 backcast", p_common),
  list("2024–25 December", a_all), list("2026 backcast", p_all[january == 2026]),
  list("2024–25 December plus 2026 backcast", rbindlist(list(a_all, p_all[january == 2026]))))
for (sm in samples) for (spec in c("roster_adjusted", "flow_adjusted"))
  models[[length(models) + 1]] <- fit_model(sm[[2]][!(reverse_flag)], sm[[1]], spec)
summ <- rbindlist(summary_rows, fill = TRUE)
mod <- rbindlist(models)
write_py_csv(summ, "validation.csv")
write_py_csv(mod, "winter_models.csv")
for (yr in names(paired_out)) write_py_csv(paired_out[[yr]], sprintf("paired_validation_%s.csv", yr))
write_py_csv(rbindlist(roster_files), "backcast_rosters.csv")
write_py_csv(rbindlist(transfers), "backcast_january_transfers.csv")
write_py_csv(p_all, "backcast_candidates.csv")
print(summ); print(mod)
