# R port of revision/build_performance_relaxed.py
# Relaxed-link variant of coach_retry/perf/build_performance.py: an event whose mover is in the pre-window release at
# a different club takes the mover's attributes and agent label from that row (link_type == 'player_only').
# Usage: Rscript build_performance_relaxed.R <base_dir> <raw_performance_dir> <out_dir>
suppressWarnings(suppressPackageStartupMessages({library(data.table); library(stringi)}))
invisible(Sys.setlocale("LC_COLLATE", "C"))
setDTthreads(1)

# ---- helpers ---------------------------------------------------------------
is_zip <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  b <- readBin(con, "raw", 4L)
  length(b) == 4L && all(b == as.raw(c(0x50, 0x4b, 0x03, 0x04)))
}
fread_str <- function(file = NULL, cmd = NULL, select = NULL, encoding = "UTF-8") {
  opts <- list(colClasses = "character", na.strings = NULL, strip.white = FALSE, showProgress = FALSE,
               select = select, encoding = encoding, data.table = TRUE, quote = "\"", blank.lines.skip = TRUE)
  d <- if (is.null(cmd)) do.call(fread, c(list(file = file), opts)) else do.call(fread, c(list(cmd = cmd), opts))
  # this fread keeps the doubling of embedded quotes (""), the csv module collapses it
  for (j in names(d)) if (any(grepl('""', d[[j]], fixed = TRUE))) set(d, j = j, value = gsub('""', '"', d[[j]], fixed = TRUE))
  d
}
read_any <- function(path, select = NULL, encoding = "UTF-8") {
  latin <- encoding == "Latin-1"
  if (encoding == "Latin-1") encoding <- "unknown"   # bytes are re-declared as ISO-8859-1 below
  d <- if (grepl("\\.gz$", path)) fread_str(cmd = paste("gzip -dc", shQuote(path)), select = select, encoding = encoding)
       else fread_str(path, select = select, encoding = encoding)
  if (latin) for (j in names(d)) set(d, j = j, value = stri_conv(d[[j]], "ISO-8859-1", "UTF-8"))
  d
}
archive <- function(path, member, select = NULL) {
  if (is_zip(path)) fread_str(cmd = paste("unzip -p", shQuote(path), shQuote(member)), select = select)
  else read_any(path, select = select)
}
col_or_blank <- function(d, nm) if (nm %in% names(d)) d[[nm]] else rep("", nrow(d))
write_csv <- function(d, path) {
  d <- copy(d)
  for (j in names(d)) if (is.logical(d[[j]])) set(d, j = j, value = ifelse(d[[j]], "True", "False"))
  fwrite(d, path, na = "", quote = "auto")
}
folded <- function(s) {
  u <- unique(s)
  f <- stri_replace_all_regex(stri_trans_nfkd(stri_trans_casefold(u)), "[^\\x{00}-\\x{7F}]", "")
  f[match(s, u)]
}
key <- function(s) {
  u <- unique(s)
  k <- gsub("[^a-z0-9]", "", folded(u))
  k[match(s, u)]
}
club_key <- function(s) {
  u <- unique(s)
  f <- stri_replace_all_regex(folded(u), "\\b(?:football club|fussball club|gesellschaft mit beschrankter haftung|fc|cf|afc|ac|ssc|sc|as|sv|vfb|vfl|tsg|fsv|ogc|rc|losc|calcio|spa|sad|gmbh)\\b", " ")
  k <- gsub("[^a-z0-9]", "", folded(f))
  k[match(s, u)]
}
int0 <- function(s) { x <- suppressWarnings(as.numeric(s)); x[is.na(x)] <- 0; trunc(x) }
isdig <- function(s) grepl("^[0-9]+$", s)
# top name per group: highest count, ties to the earliest inserted (Counter.most_common)
top_names <- function(tn, by) {   # tn: columns by..., name, ord
  cnt <- tn[, .(n = .N, first = min(ord)), by = c(by, "name")]
  setorderv(cnt, c(by, "n", "first"), c(rep(1L, length(by)), -1L, 1L))
  cnt
}

build_performance_relaxed <- function(base, raw, out) {
dir.create(out, showWarnings = FALSE, recursive = TRUE)
country <- setNames(strsplit('BEL DEN ESP FRA ENG GRE ITA GER NED POR RUS SCO TUR UKR', ' ')[[1]],
                    strsplit('BE1 DK1 ES1 FR1 GB1 GR1 IT1 L1 NL1 PO1 RU1 SC1 TR1 UKR1', ' ')[[1]])
versions <- c('2023' = '240', '2024' = '435', '2025' = '600')
yrs <- names(versions)

snapshots <- read_any(file.path(base, "player_snapshots.csv"))
snapshots <- snapshots[summer %in% yrs]
snapshots[, ridx := .I]
A <- rbindlist(lapply(yrs, function(y) {
  d <- archive(file.path(base, paste0(versions[[y]], ".data")), "players.csv")
  d[, year := y]; d
}), fill = TRUE)
A <- A[, .SD[.N], by = .(year, player_id)]   # later duplicates override

# ---- games -----------------------------------------------------------------
games <- read_any(file.path(base, "games.gz"),
                  select = c("game_id", "competition_id", "season", "date", "home_club_id", "away_club_id",
                             "home_club_goals", "away_club_goals", "home_club_name", "away_club_name"))
G <- games[competition_id %in% names(country) & isdig(season)]
G[, gi := .I]
G[, entering := as.character(as.integer(season))]
G[, year := as.character(as.integer(season) + 1L)]
# next_names: names recorded for the entering summer
NG <- G[entering %in% yrs]
NN <- rbindlist(list(NG[, .(year = entering, club = home_club_id, name = home_club_name, ord = 2L * gi)],
                     NG[, .(year = entering, club = away_club_id, name = away_club_name, ord = 2L * gi + 1L)]))
GK <- G[year %in% yrs & date != "" & date < paste0(year, "-06-01") & isdig(home_club_goals) & isdig(away_club_goals)]
gdict <- GK[, .SD[.N], by = game_id][, .(game_id, gyear = year, home = home_club_id, away = away_club_id)]
long <- rbindlist(list(
  GK[, .(year, club = home_club_id, league = competition_id, gf = as.integer(home_club_goals), ga = as.integer(away_club_goals), name = home_club_name, ord = 2L * gi)],
  GK[, .(year, club = away_club_id, league = competition_id, gf = as.integer(away_club_goals), ga = as.integer(home_club_goals), name = away_club_name, ord = 2L * gi + 1L)]))
long[, pts := fifelse(gf > ga, 3L, as.integer(gf == ga))]
teams <- long[, .(games = .N, points = sum(pts), goals_for = sum(gf), goals_against = sum(ga)), by = .(year, club)]
setorder(long, ord)
tleague <- long[, .(league = league[.N]), by = .(year, club)]
teams <- merge(teams, tleague, by = c("year", "club"), sort = FALSE)
teams[, ppg := points / games]
tn <- long[, .(year, club, name, ord)]
sn <- merge(snapshots, A[, .(year, player_id, cur = col_or_blank(.SD, "current_club_name"))],
            by.x = c("summer", "player_id"), by.y = c("year", "player_id"), sort = FALSE)
stopifnot(nrow(sn) == nrow(snapshots))
sn <- sn[cur != ""][order(ridx)]
tn <- rbindlist(list(tn, sn[, .(year = summer, club = club_id, name = cur, ord = 1e9 + ridx)]))
team_names <- top_names(tn, c("year", "club"))
nn_names <- top_names(NN, c("year", "club"))

# ---- appearances -----------------------------------------------------------
ap <- read_any(file.path(raw, "appearances.gz"),
               select = c("appearance_id", "game_id", "player_id", "player_club_id", "minutes_played", "goals", "assists"))
ap[, aidx := .I]
ap <- merge(ap, gdict, by = "game_id", sort = FALSE)
setorder(ap, aidx)
ap <- ap[player_club_id == home | player_club_id == away]
ap <- ap[!duplicated(appearance_id)]
ap[, `:=`(m = int0(minutes_played), g = int0(goals), a = int0(assists))]
stats <- ap[, .(matches = sum(m > 0), minutes = sum(m), goals = sum(g), assists = sum(a)),
            by = .(year = gyear, club = player_club_id, player_id)]
appearance_clubs <- ap[, .(n = .N), by = .(year = gyear, club = player_club_id)]
rm(ap)

# ---- ratings ---------------------------------------------------------------
r23 <- read_any(file.path(raw, "2023.ratings.csv"), select = c("ID", "Name", "FullName", "Overall", "Age"))
game23 <- unique(rbindlist(list(r23[Name != "", .(k = key(Name), id = ID, ov = Overall, ag = Age)],
                                r23[FullName != "", .(k = key(FullName), id = ID, ov = Overall, ag = Age)])))
r24 <- read_any(file.path(raw, "2024.ratings.csv"), select = c("player_id", "name", "overall_rating", "dob"), encoding = "Latin-1")
slash <- grepl("/", r24$dob, fixed = TRUE)
m <- regmatches(r24$dob, regexec("^([0-9]{1,2})/([0-9]{1,2})/([0-9]{4})$", r24$dob))
parsed <- vapply(seq_along(m), function(i) {
  if (!slash[i]) return(substr(r24$dob[i], 1L, 10L))
  if (length(m[[i]]) != 4L) return(NA_character_)
  d <- suppressWarnings(as.Date(sprintf("%s-%s-%s", m[[i]][4], m[[i]][2], m[[i]][3]), format = "%Y-%m-%d"))
  if (is.na(d)) NA_character_ else format(d, "%Y-%m-%d")
}, "")
r24[, dobn := parsed]
game24 <- unique(r24[!is.na(dobn), .(k = key(name), dob = dobn, id = player_id, ov = overall_rating)])
r25 <- archive(file.path(raw, "sofifa.zip"), "all_fifa_players.csv", select = c("Player ID", "Overall Score"))
setnames(r25, c("id", "ov"))
r25 <- r25[grepl("^[0-9]", ov)][, ov := sub("^([0-9]+).*", "\\1", ov)]
game25 <- unique(r25)

# ---- players ---------------------------------------------------------------
P <- merge(snapshots, A, by.x = c("summer", "player_id"), by.y = c("year", "player_id"), sort = FALSE, suffixes = c("", ".a"))
setorder(P, ridx)
stopifnot(nrow(P) == nrow(snapshots))
P[, dob := substr(col_or_blank(.SD, "date_of_birth"), 1L, 10L)]
P[, k1 := key(name)]
P[, k2 := key(paste(col_or_blank(.SD, "first_name"), col_or_blank(.SD, "last_name")))]
okdob <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", P$dob)
P[, age := NA_real_]
P[okdob, age := as.numeric(sprintf("%.2f", as.numeric(as.Date(paste0(summer, "-06-01")) - as.Date(dob)) / 365.25))]

L <- unique(rbindlist(list(P[, .(ridx, k = k1)], P[, .(ridx, k = k2)])))[k != ""]
L <- merge(L, P[, .(ridx, summer, dob)], by = "ridx")
h23 <- merge(L[summer == "2023" & dob != ""], game23, by = "k", allow.cartesian = TRUE)
h23 <- h23[isdig(ag) & (2022L - as.integer(substr(dob, 1L, 4L)) - suppressWarnings(as.integer(ag))) %in% c(0L, 1L)]
h24 <- merge(L[summer != "2023" & dob != ""], game24, by = c("k", "dob"), allow.cartesian = TRUE)
hits <- unique(rbind(h23[, .(ridx, id, ov)], h24[, .(ridx, id, ov)]))
hits <- hits[, if (.N == 1L) .(gid = id, rating = ov), by = ridx]
P <- merge(P, hits, by = "ridx", all.x = TRUE, sort = FALSE); setorder(P, ridx)
P[is.na(gid), `:=`(gid = "", rating = "")]
g25 <- game25[, if (uniqueN(ov) == 1L) .(r25 = ov[1L]), by = id]
P[summer == "2025", rating := {
  r <- g25$r25[match(gid, g25$id)]
  fifelse(is.na(r), "", r)
}]
P[, method := fifelse(summer == "2023", "fifa23_name_age", fifelse(summer == "2024", "fc24_name_dob", "fc25_archive_via_fc24_id"))]
P[, pos := col_or_blank(.SD, "position")]
P <- merge(P, stats[, .(summer = year, club_id = club, player_id, s_matches = matches, s_minutes = minutes, s_goals = goals, s_assists = assists)],
           by = c("summer", "club_id", "player_id"), all.x = TRUE, sort = FALSE)
P <- merge(P, appearance_clubs[, .(summer = year, club_id = club, has_app = n > 0)], by = c("summer", "club_id"), all.x = TRUE, sort = FALSE)
setorder(P, ridx)
P[is.na(has_app), has_app := FALSE]
for (j in c("s_matches", "s_minutes", "s_goals", "s_assists")) set(P, which(is.na(P[[j]])), j, 0L)
snapcols <- setdiff(names(snapshots), "ridx")
players <- cbind(P[, ..snapcols], P[, .(
  position = pos, sub_position = col_or_blank(.SD, "sub_position"), nationality = col_or_blank(.SD, "country_of_citizenship"),
  age_at_cutoff = age, market_value_eur = col_or_blank(.SD, "market_value_in_eur"),
  prior_league_minutes = fifelse(has_app, as.integer(s_minutes), NA_integer_),
  prior_league_appearances = fifelse(has_app, as.integer(s_matches), NA_integer_),
  prior_league_goals = fifelse(has_app, as.integer(s_goals), NA_integer_),
  prior_league_assists = fifelse(has_app, as.integer(s_assists), NA_integer_),
  ea_id = gid, ea_overall = rating, rating_match = fifelse(rating != "", method, ""))])
players[, `:=`(ridx = .I, kl = key(agent_label))]
sq <- players[position != "" & !position %in% c("Missing", "-")]

# ---- Elo -------------------------------------------------------------------
elo <- read_any(file.path(raw, "elo.csv"))
elo[, y := substr(date, 1L, 4L)]
elo <- elo[y %in% yrs & date >= paste0(y, "-05-01") & date < paste0(y, "-06-01")]
elo[, `:=`(ck = club_key(club), eloval = as.numeric(elo))]
elo_t <- elo[, .(y, country, ck, day = date, team = club, eloval)]
alias_path <- file.path(raw, "elo_aliases.csv")
aliases <- if (file.exists(alias_path)) {
  a <- read_any(alias_path)
  a <- data.table(country = a$country, ck = club_key(a$transfermarkt), ek = club_key(a$elo))
  a[, .SD[.N], by = .(country, ck)]
} else data.table(country = character(), ck = character(), ek = character())

destinations <- read_any(file.path(base, "destination_clubs.csv"))[summer %in% yrs]
destinations <- destinations[, .SD[.N], by = .(summer, club_id)]   # dict: last wins
setorder(destinations, summer, club_id)

# ---- clubs -----------------------------------------------------------------
# group percentile of points per match among teams in the same prior league
teams[, pct := {
  n <- .N; v <- ppg
  if (n > 1L) vapply(v, function(x) (sum(v < x) + 0.5 * (sum(v == x) - 1)) / (n - 1), 0) else NA_real_
}, by = .(year, league)]
ck_all <- unique(rbind(teams[, .(year, club)], destinations[, .(year = summer, club = club_id)]))
setorder(ck_all, year, club)
ck_all <- merge(ck_all, teams[, .(year, club, prior = league, matches = games, points, ppg, pct, goal_difference = goals_for - goals_against, has_t = TRUE)],
                by = c("year", "club"), all.x = TRUE, sort = FALSE)
ck_all <- merge(ck_all, destinations[, .(year = summer, club = club_id, nxt = league)], by = c("year", "club"), all.x = TRUE, sort = FALSE)
setorder(ck_all, year, club)
ck_all[is.na(prior), prior := ""]; ck_all[is.na(nxt), nxt := ""]
ck_all[, ct := unname(country[fifelse(prior != "", prior, nxt)])]
ck_all[is.na(ct), ct := ""]
# names per club (fallback to the entering-summer names when a club has none)
nm_eff <- rbind(team_names[, .(year, club, name, n, first)],
                nn_names[!team_names, on = c("year", "club")][, .(year, club, name, n, first)])
best <- nm_eff[, .SD[1L], by = .(year, club)]   # already ordered by count desc, first asc within key
ck_all <- merge(ck_all, best[, .(year, club, cname = name)], by = c("year", "club"), all.x = TRUE, sort = FALSE)
ck_all[is.na(cname), cname := ""]
setorder(ck_all, year, club)
# Elo match
nm_ck <- unique(nm_eff[, .(year, club, name)])
nm_ck[, ck := club_key(name)]
nm_ck <- merge(nm_ck, ck_all[, .(year, club, ct)], by = c("year", "club"))
nm_ck <- merge(nm_ck, aliases, by.x = c("ct", "ck"), by.y = c("country", "ck"), all.x = TRUE, sort = FALSE)
nm_ck[, ck2 := fifelse(is.na(ek), ck, ek)]
fnd <- merge(nm_ck[, .(year, club, ct, ck2)], elo_t, by.x = c("year", "ct", "ck2"), by.y = c("y", "country", "ck"), allow.cartesian = TRUE)
fnd <- fnd[, if (uniqueN(team) == 1L) {
  o <- order(day, eloval); .(elo_label = team[1L], elo_date = day[o[.N]], club_elo = eloval[o[.N]])
}, by = .(year, club)]
ck_all <- merge(ck_all, fnd, by = c("year", "club"), all.x = TRUE, sort = FALSE)
setorder(ck_all, year, club)
club_rows <- ck_all[, .(summer = year, club_id = club, club = cname, prior_league = prior, next_league = nxt,
  entered_tracked_top_flight = (nxt != "" & prior == ""), matches, points, points_per_match = ppg,
  ppg_percentile = pct, goal_difference, club_elo, elo_date, elo_label)]

# ---- transfers -------------------------------------------------------------
ev <- read_any(file.path(base, "summer_transfers.csv"))
ev[, event_id := .I]
Pby <- players[, .SD[.N], by = .(summer, player_id, club_id)]
Pany <- players[, .SD[1L], by = .(summer, player_id)]
ev[, r1 := Pby[.(ev$summer, ev$player_id, ev$origin_club), on = .(summer, player_id, club_id), ridx]]
ev[, r2 := Pany[.(ev$summer, ev$player_id), on = .(summer, player_id), ridx]]
lt <- if ("link_type" %in% names(ev)) ev$link_type else rep("", nrow(ev))
ev[, plrow := fifelse(!is.na(r1), r1, fifelse(lt == "player_only", r2, NA_integer_))]
cr <- club_rows[, .(summer, club_id, ppg = points_per_match, elo = club_elo)]
c0 <- cr[.(ev$summer, ev$origin_club), on = .(summer, club_id)]
c1 <- cr[.(ev$summer, ev$destination_club), on = .(summer, club_id)]
stopifnot(!anyNA(c0$summer), !anyNA(c1$summer))
pl <- players[ev$plrow]
moves <- cbind(ev[, c("event_id", setdiff(names(ev), c("event_id", "r1", "r2", "plrow"))), with = FALSE],
  data.table(origin_ppg = c0$ppg, destination_ppg = c1$ppg, origin_elo = c0$elo, destination_elo = c1$elo,
             elo_change = c1$elo - c0$elo,
             snapshot_club = pl$club_id, player_position = pl$position, player_age = pl$age_at_cutoff,
             player_minutes = pl$prior_league_minutes, player_ea_overall = pl$ea_overall))
for (j in c("snapshot_club", "player_position", "player_ea_overall")) set(moves, which(is.na(moves[[j]])), j, "")
ev[, `:=`(pl_pos = pl$position, pl_ea = pl$ea_overall, pl_kl = pl$kl, pl_ridx = pl$ridx)]

# ---- within-league candidates ---------------------------------------------
K <- ev[known_agent == "True"]
stopifnot(!anyNA(K$pl_ridx))
Cn <- merge(K[, .(event_id, summer, origin_club, player_id, destination_club, destination_league, pos = pl_pos, ea = pl_ea, kl = pl_kl)],
            destinations[, .(summer, candidate_club = club_id, candidate_league = league)], by = "summer", allow.cartesian = TRUE)
Cn <- Cn[candidate_league == destination_league & candidate_club != origin_club]
setorder(Cn, event_id, candidate_club)
Cn[, cid := .I]
roster_n <- players[, .(roster_n = .N), by = .(summer, club_id)]
lab_n <- players[, .(nlab = .N), by = .(summer, club_id, kl)]
mine_n <- players[, .(nmine = .N), by = .(summer, club_id, player_id, kl)]
Cn[, roster_n := roster_n[.(Cn$summer, Cn$candidate_club), on = .(summer, club_id), roster_n]]
Cn[is.na(roster_n), roster_n := 0L]
Cn[, nlab := lab_n[.(Cn$summer, Cn$candidate_club, Cn$kl), on = .(summer, club_id, kl), nlab]]
Cn[is.na(nlab), nlab := 0L]
Cn[, nmine := mine_n[.(Cn$summer, Cn$candidate_club, Cn$player_id, Cn$kl), on = .(summer, club_id, player_id, kl), nmine]]
Cn[is.na(nmine), nmine := 0L]
pe <- merge(Cn[, .(cid, summer, candidate_club, pos, player_id)],
            sq[, .(summer, candidate_club = club_id, pos = position, peer = player_id, mins = prior_league_minutes, ea = ea_overall)],
            by = c("summer", "candidate_club", "pos"), allow.cartesian = TRUE)
pe <- pe[peer != player_id]
pe[, rated := fifelse(ea != "", suppressWarnings(as.numeric(ea)), NA_real_)]
ps <- pe[, .(npeers = .N, nobs = sum(!is.na(mins)), summin = sum(mins, na.rm = TRUE),
             nrated = sum(!is.na(rated)), med = if (any(!is.na(rated))) median(rated[!is.na(rated)]) else NA_real_), by = cid]
Cn <- merge(Cn, ps, by = "cid", all.x = TRUE, sort = FALSE); setorder(Cn, cid)
Cn[is.na(npeers), `:=`(npeers = 0L, nobs = 0L, summin = 0L, nrated = 0L)]
cr2 <- club_rows[, .(summer, candidate_club = club_id, candidate_club_elo = club_elo, candidate_points_per_match = points_per_match,
                     candidate_ppg_percentile = ppg_percentile, candidate_entered_top_flight = entered_tracked_top_flight)]
Cn <- merge(Cn, cr2, by = c("summer", "candidate_club"), all.x = TRUE, sort = FALSE); setorder(Cn, cid)
choices <- Cn[, .(event_id, summer, candidate_club, candidate_league, chosen = as.integer(candidate_club == destination_club),
  candidate_club_elo, candidate_points_per_match, candidate_ppg_percentile, candidate_entered_top_flight,
  co_clients = nlab - nmine, candidate_roster_observed = roster_n > 0L,
  same_position_players = fifelse(roster_n > 0L & pos != "", npeers, NA_integer_),
  same_position_minutes = fifelse(nobs > 0L, as.integer(summin), NA_integer_),
  same_position_rated_players = nrated, same_position_median_rating = med,
  player_minus_median_rating = fifelse(nrated > 0L & ea != "", suppressWarnings(as.numeric(ea)) - med, NA_real_))]

write_csv(players[, !c("ridx", "kl")], file.path(out, "player_performance.csv"))
write_csv(club_rows, file.path(out, "club_performance.csv"))
write_csv(moves, file.path(out, "transfer_performance.csv"))
write_csv(choices, file.path(out, "within_league_candidates.csv"))

audit <- rbindlist(lapply(yrs, function(y) {
  ps_ <- players[summer == y]
  es <- moves[summer == y & known_agent == "True" & likely_return == "False"]
  ch <- choices[chosen == 1L & summer == y]
  data.table(summer = y, players = nrow(ps_), player_minutes = sum(!is.na(ps_$prior_league_minutes)),
             rated_players = sum(ps_$ea_overall != ""), destination_clubs = sum(destinations$summer == y),
             clubs_with_ppg = sum(!is.na(club_rows[destinations[summer == y], on = .(summer, club_id), points_per_match])),
             clubs_with_elo = sum(!is.na(club_rows[destinations[summer == y], on = .(summer, club_id), club_elo])),
             screened_transfers = nrow(es), screened_elo_change = sum(!is.na(es$elo_change)),
             screened_player_rating = sum(es$player_ea_overall != ""),
             chosen_with_rating_fit = sum(!is.na(ch$player_minus_median_rating)))
}))
print(audit)
write_csv(audit, file.path(out, "coverage.csv"))
cat("events", nrow(moves), "agent-linked", sum(moves$known_agent == "True"), "player_only",
    if ("link_type" %in% names(moves)) sum(moves$link_type == "player_only") else 0L, "\n")
invisible(NULL)
}

if (sys.nframe() == 0L) {
  a <- commandArgs(TRUE)
  if (length(a) != 3L) stop("usage: Rscript build_performance_relaxed.R <base_dir> <raw_performance_dir> <out_dir>")
  build_performance_relaxed(a[1], a[2], a[3])
}
