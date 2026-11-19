# Build the performance inputs (rosters, candidate clubs, ratings, Elo) for the first-version summer sample.
# Usage:   Rscript 03_performance_inputs.R BASE RAW OUT
#   BASE: directory with player_snapshots.csv, summer_transfers.csv,
#         destination_clubs.csv, 240.data 435.data 600.data, games.gz
#   RAW : directory with appearances.gz, 2023.ratings.csv, 2024.ratings.csv,
#         sofifa.zip, elo.csv, elo_aliases.csv
#   OUT : output directory (created if missing)
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(TRUE)
if (length(args) != 3) stop("usage: Rscript 03_performance_inputs.R BASE RAW OUT")
base <- args[1]; raw <- args[2]; out <- args[3]
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---------------- helpers ----------------
# Shortest round-trip decimal string of a double, as written in the shipped files. R's own parser is
# not correctly rounded, so round-trip is tested with exact decimal digit strings.
repr_one <- function(v) {
  if (v == 0) return("0.0")
  ex <- sprintf("%.45e", v)
  for (d in 15:16) {
    s <- sprintf(paste0("%.", d - 1, "e"), v)
    pe <- function(z) as.integer(sub(".*e", "", z))
    if (pe(s) != pe(ex)) next
    digs <- function(z) { m <- gsub("[-.]", "", sub("e.*", "", z)); substr(paste0(m, strrep("0", 50)), 1, 46) }
    a <- digs(ex); b <- digs(s)
    diff <- (as.numeric(substr(a, 1, 15)) - as.numeric(substr(b, 1, 15))) * 1e31 +
            (as.numeric(paste0("0.", substr(a, 16, 46))) - as.numeric(paste0("0.", substr(b, 16, 46)))) * 1e31
    halfulp <- 2^(floor(log2(abs(v))) - 53)
    if (abs(diff) * 10^(pe(ex) - 45) < halfulp) { return(fin(sprintf(paste0("%.", d, "g"), v))) }
  }
  fin(sprintf("%.17g", v))
}
fin <- function(s) if (grepl("[.e]", s)) s else paste0(s, ".0")
pyfloat <- function(x) { r <- rep("", length(x)); ok <- !is.na(x); r[ok] <- vapply(x[ok], repr_one, ""); r }
pybool <- function(x) ifelse(x, "True", "False")
csv_field <- function(x) {
  x[is.na(x)] <- ""
  q <- grepl("[\",\r\n]", x)
  x[q] <- paste0("\"", gsub("\"", "\"\"", x[q], fixed = TRUE), "\"")
  x
}
write_csv_py <- function(dt, path) {
  cols <- lapply(dt, function(v) csv_field(as.character(v)))
  lines <- c(paste(csv_field(names(dt)), collapse = ","), do.call(paste, c(unname(cols), sep = ",")))
  con <- file(path, "wb"); on.exit(close(con))
  writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
}
# fread keeps doubled quotes of quoted fields; undo that to match csv.DictReader
unescape_quotes <- function(d) {
  for (n in names(d)) { v <- d[[n]]; w <- grep("\"\"", v, fixed = TRUE)
    if (length(w)) set(d, i = w, j = n, value = gsub("\"\"", "\"", v[w], fixed = TRUE)) }
  d
}
read_chr <- function(path, select = NULL, encoding = "UTF-8") {
  opts <- list(colClasses = "character", na.strings = NULL, strip.white = FALSE,
               encoding = if (encoding == "Latin-1") "unknown" else encoding, showProgress = FALSE, select = select)
  d <- if (grepl("\\.gz$", path)) do.call(fread, c(list(cmd = paste("gzip -dc", shQuote(path))), opts))
       else do.call(fread, c(list(path), opts))
  if (encoding == "Latin-1") for (n in names(d)) set(d, j = n, value = stringi::stri_encode(d[[n]], "ISO-8859-1", "UTF-8"))
  unescape_quotes(d)
}
read_member <- function(path, member, ...) {
  if (!inherits(try(utils::unzip(path, list = TRUE), silent = TRUE), "try-error")) {
    td <- tempfile(); dir.create(td); on.exit(unlink(td, recursive = TRUE))
    utils::unzip(path, files = member, exdir = td)
    read_chr(file.path(td, member), ...)
  } else read_chr(path, ...)
}
folded <- function(s) {
  s <- stringi::stri_trans_nfkd(stringi::stri_trans_casefold(s))
  stringi::stri_replace_all_regex(s, "[^\\x00-\\x7F]", "")
}
key <- function(s) gsub("[^a-z0-9]", "", folded(s))
club_key <- function(s) key(stringi::stri_replace_all_regex(folded(s),
  "\\b(?:football club|fussball club|gesellschaft mit beschrankter haftung|fc|cf|afc|ac|ssc|sc|as|sv|vfb|vfl|tsg|fsv|ogc|rc|losc|calcio|spa|sad|gmbh)\\b", " "))
int0 <- function(s) { v <- suppressWarnings(as.numeric(s)); v[is.na(v)] <- 0; trunc(v) }
isdig <- function(s) grepl("^[0-9]+$", s)

country <- setNames(strsplit("BEL DEN ESP FRA ENG GRE ITA GER NED POR RUS SCO TUR UKR", " ")[[1]],
                    strsplit("BE1 DK1 ES1 FR1 GB1 GR1 IT1 L1 NL1 PO1 RU1 SC1 TR1 UKR1", " ")[[1]])
versions <- c("2023" = "240", "2024" = "435", "2025" = "600")

snapshots <- read_chr(file.path(base, "player_snapshots.csv"))[summer %in% names(versions)]
attrs <- rbindlist(lapply(names(versions), function(y) {
  a <- read_member(file.path(base, paste0(versions[[y]], ".data")), "players.csv")
  a <- a[!duplicated(player_id, fromLast = TRUE)]
  a[, year := y]; a
}), fill = TRUE)
setkey(attrs, year, player_id)

# ---------------- games ----------------
G <- read_chr(file.path(base, "games.gz"))
G <- G[competition_id %in% names(country) & isdig(season)]
G[, gi := .I]
G[, year := as.character(as.integer(season) + 1L)]
G[, entering := season]
# names seen in the entering season (order of appearance kept through sequence numbers)
ent <- G[entering %in% names(versions)]
next_occ <- rbind(ent[, .(year = entering, club = home_club_id, nm = home_club_name, seq = gi * 2L)],
                  ent[, .(year = entering, club = away_club_id, nm = away_club_name, seq = gi * 2L + 1L)])
Gp <- G[year %in% names(versions)]
Gp <- Gp[date != "" & date < paste0(year, "-06-01") & isdig(home_club_goals) & isdig(away_club_goals)]
Gp[, `:=`(hg = as.integer(home_club_goals), ag = as.integer(away_club_goals))]
games <- Gp[!duplicated(game_id, fromLast = TRUE), .(game_id, year, home_club_id, away_club_id)]
side <- rbind(Gp[, .(gi, s = 0L, year, club = home_club_id, nm = home_club_name, league = competition_id, gf = hg, ga = ag)],
              Gp[, .(gi, s = 1L, year, club = away_club_id, nm = away_club_name, league = competition_id, gf = ag, ga = hg)])
setorder(side, gi, s)
tm <- side[, .(games = .N, points = sum(ifelse(gf > ga, 3L, as.integer(gf == ga))),
               goals_for = sum(gf), goals_against = sum(ga), league = league[.N]), by = .(year, club)]
setkey(tm, year, club)

# team names: Counter semantics (count, ties -> first inserted)
occ <- rbind(side[, .(year, club, nm, seq = gi * 2L + s)],
             data.table(year = snapshots$summer, club = snapshots$club_id,
                        nm = attrs[.(snapshots$summer, snapshots$player_id), current_club_name],
                        seq = 1e9 + seq_len(nrow(snapshots))))[!is.na(nm) & nm != ""]
# (snapshot names are only counted when current_club_name is non-empty, as in the original)
snap_names <- occ[seq >= 1e9]
names_tab <- function(o) {
  o <- o[, .(n = .N, first = min(seq)), by = .(year, club, nm)]
  setorder(o, year, club, -n, first)
  o
}
team_names <- names_tab(occ); setkey(team_names, year, club)
next_names <- names_tab(next_occ[nm != "" | TRUE]); setkey(next_names, year, club)

# ---------------- appearances ----------------
ap <- read_chr(file.path(raw, "appearances.gz"),
               select = c("appearance_id", "game_id", "player_id", "player_club_id", "minutes_played", "goals", "assists"))
ap <- merge(ap, games, by = "game_id", sort = FALSE)
ap <- ap[player_club_id == home_club_id | player_club_id == away_club_id]
ap <- ap[!duplicated(appearance_id)]
ap[, m := int0(minutes_played)]
stats <- ap[, .(matches = sum(m > 0), minutes = sum(m), goals = sum(int0(goals)), assists = sum(int0(assists))),
            by = .(year, club = player_club_id, player_id)]
setkey(stats, year, club, player_id)
app_clubs <- unique(ap[, .(year, club = player_club_id)]); setkey(app_clubs, year, club)

# ---------------- rating sources ----------------
r23 <- read_chr(file.path(raw, "2023.ratings.csv"))
g23 <- unique(rbind(r23[Name != "", .(k = key(Name), id = ID, overall = Overall, age = Age)],
                    r23[FullName != "", .(k = key(FullName), id = ID, overall = Overall, age = Age)]))
r24 <- read_chr(file.path(raw, "2024.ratings.csv"), encoding = "Latin-1")
dob24 <- r24$dob
sl <- grepl("/", dob24, fixed = TRUE)
mm <- regmatches(dob24, regexec("^([0-9]{1,2})/([0-9]{1,2})/([0-9]{4})$", dob24))
iso <- vapply(mm, function(z) {
  if (length(z) != 4) return(NA_character_)
  d <- suppressWarnings(as.Date(sprintf("%s-%02d-%02d", z[4], as.integer(z[2]), as.integer(z[3])), format = "%Y-%m-%d"))
  if (is.na(d)) NA_character_ else format(d)
}, "")
dob24 <- ifelse(sl, iso, dob24)
g24 <- unique(data.table(k = key(r24$name), dob = substr(dob24, 1, 10), id = r24$player_id, overall = r24$overall_rating)[!is.na(dob)])
r25 <- read_member(file.path(raw, "sofifa.zip"), "all_fifa_players.csv")
m25 <- regmatches(r25[["Overall Score"]], regexpr("^[0-9]+", r25[["Overall Score"]]))
g25 <- unique(data.table(id = r25[["Player ID"]][grepl("^[0-9]+", r25[["Overall Score"]])], overall = m25))

# ---------------- player rows ----------------
S <- copy(snapshots)
A <- attrs[.(S$summer, S$player_id)]
S[, `:=`(position = A$position, sub_position = A$sub_position, nationality = A$country_of_citizenship,
         market_value_eur = A$market_value_in_eur)]
dob <- substr(A$date_of_birth, 1, 10)
okdob <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", dob)
days <- as.numeric(as.Date(paste0(S$summer, "-06-01")) - as.Date(ifelse(okdob, dob, NA)))
age <- sprintf("%.2f", days / 365.25)
age <- sub("0$", "", age); age <- sub("\\.$", ".0", age)   # repr of a 2-decimal float
S[, age_at_cutoff := ifelse(okdob, age, "")]
S[, ri := .I]
keys <- rbind(data.table(ri = S$ri, k = key(A$name)), data.table(ri = S$ri, k = key(paste(A$first_name, A$last_name))))[k != ""]
keys <- unique(keys)
keys[, `:=`(year = S$summer[ri], dob = dob[ri])]
# FIFA 23: name + age window
h23 <- merge(keys[year == "2023" & dob != ""], g23, by = "k", allow.cartesian = TRUE)
h23 <- h23[isdig(age) & (2022L - as.integer(substr(dob, 1, 4)) - suppressWarnings(as.integer(age))) %in% c(0L, 1L)]
# FC24: name + dob (also used for 2025 to obtain the id)
h24 <- merge(keys[year != "2023" & dob != ""], g24, by = c("k", "dob"), allow.cartesian = TRUE)
hits <- unique(rbind(h23[, .(ri, id, overall)], h24[, .(ri, id, overall)]))
hits <- hits[, if (.N == 1L) .SD, by = ri]
S[, `:=`(ea_id = "", ea_overall = "", rating_match = "")]
S[hits$ri, `:=`(ea_id = hits$id, ea_overall = hits$overall)]
S[, method := ifelse(summer == "2023", "fifa23_name_age", "fc24_name_dob")]
i25 <- which(S$summer == "2025")
r25v <- g25[, .(n = .N, v = overall[1]), by = id]
mi <- match(S$ea_id[i25], r25v$id)
S[i25, `:=`(ea_overall = ifelse(!is.na(mi) & r25v$n[mi] == 1L, r25v$v[mi], ""), method = "fc25_archive_via_fc24_id")]
S[, rating_match := ifelse(ea_overall != "", method, "")]
st <- stats[.(S$summer, S$club_id, S$player_id)]
has_app <- !is.na(app_clubs[.(S$summer, S$club_id), which = TRUE])
S[, `:=`(prior_league_minutes = ifelse(has_app, as.character(ifelse(is.na(st$minutes), 0L, st$minutes)), ""),
         prior_league_appearances = ifelse(has_app, as.character(ifelse(is.na(st$matches), 0L, st$matches)), ""),
         prior_league_goals = ifelse(has_app, as.character(ifelse(is.na(st$goals), 0L, st$goals)), ""),
         prior_league_assists = ifelse(has_app, as.character(ifelse(is.na(st$assists), 0L, st$assists)), ""))]
players <- S[, .(summer, version, league, player_id, club_id, player, agent_label, position, sub_position, nationality,
                 age_at_cutoff, market_value_eur, prior_league_minutes, prior_league_appearances,
                 prior_league_goals, prior_league_assists, ea_id, ea_overall, rating_match)]
players[, key_label := key(agent_label)]

# ---------------- Elo and clubs ----------------
el <- read_chr(file.path(raw, "elo.csv"))
el[, y := substr(date, 1, 4)]
el <- el[y %in% names(versions) & date >= paste0(y, "-05-01") & date < paste0(y, "-06-01")]
el[, `:=`(ck = club_key(club), eloN = as.numeric(elo))]
el_idx <- split(seq_len(nrow(el)), paste(el$y, el$country, el$ck, sep = "\r"))
al <- read_chr(file.path(raw, "elo_aliases.csv"))
alias <- al[, .(k = paste(country, club_key(transfermarkt), sep = "\r"), v = club_key(elo))]
alias <- alias[!duplicated(k, fromLast = TRUE)]
dest <- read_chr(file.path(base, "destination_clubs.csv"))[summer %in% names(versions)]
dest_key <- paste(dest$summer, dest$club_id, sep = "\r")
tm_key <- paste(tm$year, tm$club, sep = "\r")
all_keys <- sort(union(dest_key, tm_key), method = "radix")
ak <- tstrsplit(all_keys, "\r", fixed = TRUE)
year_v <- ak[[1]]; club_v <- ak[[2]]
ti <- match(all_keys, tm_key); di <- match(all_keys, dest_key)
prior <- ifelse(is.na(ti), "", tm$league[ti]); nxt <- ifelse(is.na(di), "", dest$league[di])
ppg <- ifelse(is.na(ti), NA_real_, tm$points[ti] / tm$games[ti])
grp <- tm[, .(year, league, v = points / games)]
pct <- rep(NA_real_, length(all_keys))
for (i in which(!is.na(ti) & prior != "")) {
  v <- grp[year == year_v[i] & league == prior[i], v]
  if (length(v) > 1) pct[i] <- (sum(v < ppg[i]) + 0.5 * (sum(v == ppg[i]) - 1)) / (length(v) - 1)
}
ctry <- ifelse(ifelse(prior != "", prior, nxt) %in% names(country), country[ifelse(prior != "", prior, nxt)], "")
ctry[is.na(ctry)] <- ""
club_name <- character(length(all_keys)); elo_val <- rep(NA_real_, length(all_keys))
elo_date <- character(length(all_keys)); elo_label <- character(length(all_keys))
for (i in seq_along(all_keys)) {
  nm <- team_names[.(year_v[i], club_v[i])]
  nm <- nm[!is.na(nm)]
  if (nrow(nm) == 0) nm <- next_names[.(year_v[i], club_v[i])][!is.na(nm)]
  if (nrow(nm) == 0) next
  club_name[i] <- nm$nm[1]
  idx <- integer()
  for (n in nm$nm) {                          # insertion order of the Counter is irrelevant here
    ck <- club_key(n); a <- alias$v[match(paste(ctry[i], ck, sep = "\r"), alias$k)]
    if (!is.na(a)) ck <- a
    idx <- c(idx, el_idx[[paste(year_v[i], ctry[i], ck, sep = "\r")]])
  }
  if (length(idx) && length(unique(el$club[idx])) == 1) {
    e <- el[idx]; e <- e[order(date, eloN, decreasing = TRUE)][1]
    elo_val[i] <- e$eloN; elo_date[i] <- e$date; elo_label[i] <- e$club
  }
}
club_rows <- data.table(summer = year_v, club_id = club_v, club = club_name, prior_league = prior, next_league = nxt,
  entered_tracked_top_flight = pybool(nxt != "" & prior == ""),
  matches = ifelse(is.na(ti), "", as.character(tm$games[ti])), points = ifelse(is.na(ti), "", as.character(tm$points[ti])),
  points_per_match = pyfloat(ppg), ppg_percentile = pyfloat(pct),
  goal_difference = ifelse(is.na(ti), "", as.character(tm$goals_for[ti] - tm$goals_against[ti])),
  club_elo = pyfloat(elo_val), elo_date = elo_date, elo_label = elo_label)
club_elo_num <- elo_val
ckey <- paste(club_rows$summer, club_rows$club_id, sep = "\r")

# ---------------- transfers and candidate sets ----------------
ev <- read_chr(file.path(base, "summer_transfers.csv"))
ev[, event_id := .I]
pkey <- paste(players$summer, players$player_id, players$club_id, sep = "\r")
pl_i <- { u <- !duplicated(pkey, fromLast = TRUE); setNames(which(u), pkey[u]) }
pe <- unname(pl_i[paste(ev$summer, ev$player_id, ev$origin_club, sep = "\r")])
c0 <- match(paste(ev$summer, ev$origin_club, sep = "\r"), ckey)
c1 <- match(paste(ev$summer, ev$destination_club, sep = "\r"), ckey)
if (anyNA(c0) || anyNA(c1)) stop("event club missing from club table")
delta <- club_elo_num[c1] - club_elo_num[c0]
g_or <- function(col) ifelse(is.na(pe), "", players[[col]][pe])
moves <- cbind(ev[, .(event_id)], ev[, !"event_id"],
  data.table(origin_ppg = club_rows$points_per_match[c0], destination_ppg = club_rows$points_per_match[c1],
             origin_elo = club_rows$club_elo[c0], destination_elo = club_rows$club_elo[c1], elo_change = pyfloat(delta),
             player_position = g_or("position"), player_age = g_or("age_at_cutoff"),
             player_minutes = g_or("prior_league_minutes"), player_ea_overall = g_or("ea_overall")))

# squads (by year, club, position) and rosters (by year, club)
pos_ok <- players$position != "" & !(players$position %in% c("Missing", "-"))
sq <- players[pos_ok]
sq[, `:=`(rating = suppressWarnings(as.numeric(ifelse(ea_overall == "", NA, ea_overall))),
          mins = suppressWarnings(as.numeric(ifelse(prior_league_minutes == "", NA, prior_league_minutes))))]
sq_idx <- split(seq_len(nrow(sq)), paste(sq$summer, sq$club_id, sq$position, sep = "\r"))
ro_idx <- split(seq_len(nrow(players)), paste(players$summer, players$club_id, sep = "\r"))
med <- function(x) { x <- sort(x); n <- length(x); if (n %% 2) x[(n + 1) / 2] else (x[n / 2] + x[n / 2 + 1]) / 2 }

dsort <- dest[order(summer, club_id, method = "radix")]
dest_by <- split(seq_len(nrow(dsort)), dsort$summer)
ch <- vector("list", 0)
known_ev <- which(ev$known_agent == "True")
for (i in known_ev) {
  pi_ <- pe[i]; if (is.na(pi_)) stop("assertion: focal player missing")
  y <- ev$summer[i]; p <- ev$player_id[i]; o <- ev$origin_club[i]; d <- ev$destination_club[i]
  pl <- players[pi_]
  label <- pl$key_label
  dd <- dsort[dest_by[[y]]]
  dd <- dd[league == ev$destination_league[i] & club_id != o]
  n <- nrow(dd); if (n == 0) next
  rows <- vector("list", n)
  cc <- dd$club_id
  cr <- match(paste(y, cc, sep = "\r"), ckey)
  res <- data.table(event_id = i, summer = y, candidate_club = cc, candidate_league = dd$league,
                    chosen = as.integer(cc == d),
                    candidate_club_elo = club_rows$club_elo[cr], candidate_points_per_match = club_rows$points_per_match[cr],
                    candidate_ppg_percentile = club_rows$ppg_percentile[cr],
                    candidate_entered_top_flight = club_rows$entered_tracked_top_flight[cr])
  co <- integer(n); obs <- logical(n); npl <- character(n); smin <- character(n); nrt <- integer(n)
  smed <- character(n); gap <- character(n)
  for (j in seq_len(n)) {
    r <- ro_idx[[paste(y, cc[j], sep = "\r")]]
    co[j] <- sum(players$player_id[r] != p & players$key_label[r] == label)
    obs[j] <- length(r) > 0
    s <- sq_idx[[paste(y, cc[j], pl$position, sep = "\r")]]
    if (!is.null(s)) s <- s[sq$player_id[s] != p]
    peers <- length(s)
    npl[j] <- if (obs[j] && pl$position != "") as.character(peers) else ""
    om <- sq$mins[s]; om <- om[!is.na(om)]
    smin[j] <- if (length(om)) as.character(sum(om)) else ""
    rt <- sq$rating[s]; rt <- rt[!is.na(rt)]
    nrt[j] <- length(rt)
    if (length(rt)) {
      m <- med(rt); smed[j] <- pyfloat(m)
      if (pl$ea_overall != "") gap[j] <- pyfloat(as.numeric(pl$ea_overall) - m)
    }
  }
  res[, `:=`(co_clients = co, candidate_roster_observed = pybool(obs), same_position_players = npl,
             same_position_minutes = smin, same_position_rated_players = nrt,
             same_position_median_rating = smed, player_minus_median_rating = gap)]
  ch[[length(ch) + 1]] <- res
}
choices <- rbindlist(ch)

players_out <- players[, !"key_label"]
write_csv_py(players_out, file.path(out, "player_performance.csv"))
write_csv_py(club_rows, file.path(out, "club_performance.csv"))
write_csv_py(moves, file.path(out, "transfer_performance.csv"))
write_csv_py(choices, file.path(out, "within_league_candidates.csv"))

audit <- rbindlist(lapply(names(versions), function(y) {
  ps <- players[summer == y]; cs <- club_rows[summer == y & ckey %in% paste(dest$summer, dest$club_id, sep = "\r")]
  cs <- club_rows[match(paste(dest$summer[dest$summer == y], dest$club_id[dest$summer == y], sep = "\r"), ckey)]
  es <- moves[summer == y & known_agent == "True" & likely_return == "False"]
  chosen <- choices[chosen == 1L & summer == y]
  row <- data.table(summer = y, players = nrow(ps), player_minutes = sum(ps$prior_league_minutes != ""),
    rated_players = sum(ps$ea_overall != ""), destination_clubs = nrow(cs), clubs_with_ppg = sum(cs$points_per_match != ""),
    clubs_with_elo = sum(cs$club_elo != ""), screened_transfers = nrow(es), screened_elo_change = sum(es$elo_change != ""),
    screened_player_rating = sum(es$player_ea_overall != ""), chosen_with_rating_fit = sum(chosen$player_minus_median_rating != ""))
  print(row); row
}))
write_csv_py(audit, file.path(out, "coverage.csv"))
stopifnot(nrow(moves) == 4706, nrow(players) == 19549, sum(choices$chosen) == 2721)
