# English FA reports: does the destination club tend to be one with which the player-side intermediary already has a documented earlier dealing?
# Usage: Rscript study2_fa_reports/fa_choice_model.R [output_dir]     Needs data.table, survival, stringi. Reads fa_transactions_long.csv and club_division.csv from this folder.
# Writes fa_choice_informative_events.csv and fa_choice_results.csv to output_dir (default: this folder).
suppressPackageStartupMessages({ library(data.table); library(survival); library(stringi) })
setDTthreads(2)
if (!exists("here")) { here <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)); outdir <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else here }
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
D <- fread(file.path(here, "fa_transactions_long.csv"), colClasses = "character", na.strings = NULL)
PER <- c("2015-16","2016-17","2017-18","2018-19","2019-20","2020-21","2021-22","2022-23","2023-24","2024-25","2025-26")
D[, ord := match(period, PER)]
for (cn in c("side_player","side_registering_club","side_former_club")) set(D, j = cn, value = D[[cn]] == "True")
norm <- function(s, club = FALSE) {
  s <- stri_trans_general(s, "Latin-ASCII"); s <- tolower(s); s <- gsub("\\(.*?\\)", " ", s); s <- gsub("[^a-z0-9& ]+", " ", s)
  stop <- c("ltd","limited","llp","plc","the","uk","sl","gmbh", if (club) c("fc","afc","cf","a","f","c"))
  vapply(strsplit(trimws(s), "\\s+"), function(w) paste(setdiff(w, stop), collapse = " "), "")
}
pkey <- function(s) vapply(strsplit(norm(s), " "), function(t) if (length(t) == 0 || !nzchar(t[1])) "" else paste(t[1], t[length(t)]), "")
D[, `:=`(club_k = norm(registering_club, TRUE), former_k = norm(former_club, TRUE), player_k = pkey(player),
         nm1 = norm(agent_name), nm2 = norm(agent_individual))]
D[nm1 %in% c("-", ""), nm1 := NA_character_]; D[nm2 %in% c("-", ""), nm2 := NA_character_]
D[agent_id == "", agent_id := NA_character_]
D[, strict := tolower(reg_type) %in% c("permanent transfer", "perm transfer in")]

# ---- division of each club in each period: club_division.csv (build_divisions.py); tier groups merge League One and League Two
CD <- fread(file.path(here, "club_division.csv"), colClasses = "character"); CD <- CD[division != ""]
CD[, group := fifelse(division %in% c("EFL League One", "EFL League Two"), "EFL League One/Two", division)]
division <- function(p, grouping = "division") CD[period == p, .(club_k, division = get(if (grouping == "tier") "group" else "division"))]

# ---- history table: one row per earlier dealing (agent key x club x other player x period x side)
H <- unique(D[, .(ord, period, club_k, player_k, id = agent_id, nm1, nm2, club_side = side_registering_club, club_only = side_registering_club & !side_player, player_side = side_player)])
Hk <- rbind(H[!is.na(id), .(ord, club_k, player_k, club_side, club_only, player_side, key = id, ktype = "id", has_id = TRUE)],
            H[, .(ord, club_k, player_k, club_side, club_only, player_side, key = nm1, ktype = "name", has_id = !is.na(id))],
            H[!is.na(nm2) & nm2 != nm1 | (is.na(nm1) & !is.na(nm2)), .(ord, club_k, player_k, club_side, club_only, player_side, key = nm2, ktype = "name", has_id = !is.na(id))])
Hk <- Hk[!is.na(key)]
amb <- D[ord %in% 5:8 & !is.na(agent_id) & !is.na(nm1), .(n = uniqueN(agent_id)), by = nm1][n > 1, nm1]   # names that map to more than one registration number in the ID years
cat("names with more than one registration number in the ID years:", length(amb), "\n")

build <- function(periods, match_rule, grouping = "division", drop_ambiguous = FALSE) {   # match_rule: "id" = ID where the history row has one, otherwise name; "name" = normalised name only
  out <- list(); info <- list()
  for (p in periods) {
    po <- match(p, PER)
    ev <- unique(D[period == p & strict & side_player, .(club_k, former_k, player_k, registering_club, agent_id, nm1, nm2, both_sides = side_registering_club)],
                 by = c("club_k", "player_k", "agent_id", "nm1"))
    ev[, event_id := paste(p, .I, sep = "_")]
    ev[, agent_key := fifelse(!is.na(agent_id), agent_id, fifelse(!is.na(nm1), nm1, nm2))]
    ev <- ev[!is.na(agent_key)]
    dv <- division(p, grouping); cl <- unique(D[period == p, .(club_k)]); cl <- merge(cl, dv, by = "club_k")
    ev <- merge(ev, cl, by = "club_k")          # focal club must have a division
    sets <- merge(ev[, .(event_id, division, club_k_chosen = club_k, former_k, player_k)], cl, by = "division", allow.cartesian = TRUE, suffixes = c("", "_c"))
    setnames(sets, "club_k", "club_c"); sets <- sets[is.na(former_k) | former_k == "" | club_c == club_k_chosen | club_c != former_k]
    sets[, chosen := as.integer(club_c == club_k_chosen)]
    # agent keys of each event
    ek <- rbind(ev[!is.na(agent_id), .(event_id, key = agent_id, ktype = "id")], ev[!is.na(nm1), .(event_id, key = nm1, ktype = "name")], ev[!is.na(nm2), .(event_id, key = nm2, ktype = "name")])
    ek <- unique(ek)
    hh <- Hk[ord < po]
    if (drop_ambiguous) { ek <- ek[!(ktype == "name" & key %in% amb)]; hh <- hh[!(ktype == "name" & key %in% amb)] }
    mt <- if (match_rule == "id") rbind(merge(ek[ktype == "id"], hh[ktype == "id"], by = "key", allow.cartesian = TRUE),
                                         merge(ek[ktype == "name"], hh[ktype == "name" & !has_id], by = "key", allow.cartesian = TRUE))
          else merge(ek[ktype == "name"], hh[ktype == "name"], by = "key", allow.cartesian = TRUE)
    mt <- merge(mt, unique(sets[, .(event_id, player_k)]), by = "event_id")[player_k.x != player_k.y]       # other players only
    mt[, tx := paste(ord, club_k, player_k.x == player_k.y)]
    ex <- mt[, .(any_prev = any(ord == po - 1), any_old = any(ord <= po - 2), n_prior = uniqueN(paste(ord, player_k.y)), club_side = any(club_side), club_only = any(club_only), player_side = any(player_side)), by = .(event_id, club_c = club_k)]
    sets <- merge(sets, ex, by = c("event_id", "club_c"), all.x = TRUE)
    for (cn in c("club_side","club_only","player_side")) set(sets, which(is.na(sets[[cn]])), cn, FALSE)
    sets[is.na(n_prior), n_prior := 0L]; sets[is.na(any_prev), any_prev := FALSE]; sets[is.na(any_old), any_old := FALSE]
    cd <- unique(Hk[ord < po & ktype == "id" | ord < po & ktype == "name" & !has_id, .(club_c = club_k, key)])[, .(n_club_agents = .N), by = club_c]
    sets <- merge(sets, cd, by = "club_c", all.x = TRUE); sets[is.na(n_club_agents), n_club_agents := 0L]; sets[, log_club_agents := log1p(n_club_agents)]
    sets[, `:=`(any_prev = as.numeric(any_prev), any_old = as.numeric(any_old))]
    sets[, `:=`(any_prior = as.numeric(n_prior > 0), club_side = as.numeric(club_side), club_only = as.numeric(club_only),
                player_only = as.numeric(n_prior > 0 & !club_side), log_n = log1p(n_prior), period = p)]
    sets <- merge(sets, ev[, .(event_id, agent_key, both_sides)], by = "event_id")
    out[[p]] <- sets
  }
  rbindlist(out)
}

informative <- function(S, cn) S[, .(inf = any(get(cn) > 0) & any(get(cn) == 0), chosen_exp = sum(get(cn) * chosen) > 0), by = .(event_id, period)]

fit <- function(S, cols, fe, label) {
  S <- copy(S); setnames(S, "club_c", "club")
  ok <- S[, .(n = .N, k = sum(chosen)), by = event_id]; S <- S[event_id %in% ok[k == 1 & n >= 2, event_id]]
  if (fe) repeat { n <- nrow(S); S <- S[club %in% S[chosen == 1, club]]; ok <- S[, .(n = .N, k = sum(chosen)), by = event_id]; S <- S[event_id %in% ok[k == 1 & n >= 2, event_id]]; if (nrow(S) == n) break }
  vr <- vapply(cols, function(cn) S[, any(abs(get(cn) - get(cn)[1]) > 1e-9), by = event_id][, any(V1)], logical(1)); cols <- cols[vr]
  if (!length(cols)) return(NULL)
  S[, fe__ := factor(club)]
  f <- as.formula(paste("chosen ~", paste(c(cols, if (fe) "fe__"), collapse = " + "), "+ strata(event_id) + cluster(agent_key)"))
  m <- tryCatch(suppressWarnings(clogit(f, data = as.data.frame(S), method = "efron", control = coxph.control(iter.max = 100, eps = 1e-10))), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  cf <- summary(m)$coefficients; cf <- cf[rownames(cf) %in% cols, , drop = FALSE]
  data.table(model = label, term = rownames(cf), events = uniqueN(S$event_id), alternatives = nrow(S), agents = uniqueN(S[chosen == 1, agent_key]),
             odds_ratio = exp(cf[, "coef"]), lo = exp(cf[, "coef"] - 1.96 * cf[, "robust se"]), hi = exp(cf[, "coef"] + 1.96 * cf[, "robust se"]), p = cf[, "Pr(>|z|)"], fixed_effects = fe)
}

specs <- list(
  list("A any prior dealing", c("any_prior")),
  list("B club-side vs player-side-only prior dealing", c("club_side", "player_only")),
  list("C club-side-only (agent not on player side) vs other prior dealing", c("club_only", "player_only")),
  list("D number of prior dealings (log1p)", c("log_n")),
  list("E recency: dealing in the previous period vs two or more periods earlier", c("any_prev", "any_old")),
  list("F any prior dealing, controlling for the club's number of earlier intermediaries", c("any_prior", "log_club_agents")))
core <- 1:4
runs <- list(
  list(tag = "1 primary: ID match, 2019-20 to 2022-23, same division", periods = PER[5:8], rule = "id", filter = NULL, grouping = "division", amb = FALSE, specs = 1:6),
  list(tag = "2 primary, agent not also on the club side of the focal deal", periods = PER[5:8], rule = "id", filter = "player_only_deal", grouping = "division", amb = FALSE, specs = 1:4),
  list(tag = "3 name match, 2019-20 to 2022-23", periods = PER[5:8], rule = "name", filter = NULL, grouping = "division", amb = FALSE, specs = c(1:4, 5, 6)),
  list(tag = "4 name match, 2019-20 to 2025-26", periods = PER[5:11], rule = "name", filter = NULL, grouping = "division", amb = FALSE, specs = c(1:4, 5, 6)),
  list(tag = "5 primary, tier-group choice sets", periods = PER[5:8], rule = "id", filter = NULL, grouping = "tier", amb = FALSE, specs = 1:4),
  list(tag = "6 name match, 2019-20 to 2022-23, ambiguous names removed", periods = PER[5:8], rule = "name", filter = NULL, grouping = "division", amb = TRUE, specs = 1:4))
res <- list(); infos <- list()
for (r in runs) {
  S <- build(r$periods, r$rule, r$grouping, r$amb)
  cat("events whose registering club has no division (dropped):", "see sample summary\n")
  if (!is.null(r$filter)) S <- S[both_sides == FALSE]
  cat("\n==", r$tag, ": events", uniqueN(S$event_id), ", alternatives", nrow(S), "\n")
  for (cn in c("any_prior", "club_side", "club_only", "player_only")) {
    i <- informative(S, cn); i[, `:=`(run = r$tag, exposure = cn)]; infos[[length(infos) + 1]] <- i
  }
  ia <- rbindlist(infos)[run == r$tag & exposure == "any_prior"]
  print(ia[, .(events = .N, informative = sum(inf), chosen_exposed = sum(chosen_exp)), by = period])
  for (sp in specs[r$specs]) for (fe in c(FALSE, TRUE)) { z <- fit(S, sp[[2]], fe, sp[[1]]); if (!is.null(z)) { z[, run := r$tag]; res[[length(res) + 1]] <- z } }
}
I <- rbindlist(infos)
fwrite(I[, .(events = .N, informative_events = sum(inf), events_where_chosen_club_exposed = sum(chosen_exp)), by = .(run, exposure, period)], file.path(outdir, "fa_choice_informative_events.csv"))
R <- rbindlist(res); R[, `:=`(odds_ratio = round(odds_ratio, 6), lo = round(lo, 6), hi = round(hi, 6), p = signif(p, 3))]
fwrite(R[, .(run, model, fixed_effects, term, events, alternatives, agents, odds_ratio, lo, hi, p)], file.path(outdir, "fa_choice_results.csv"))
print(R[, .(run = substr(run, 1, 34), model = substr(model, 1, 26), fe = fixed_effects, term, events, or = odds_ratio, lo, hi)], nrows = 200)
