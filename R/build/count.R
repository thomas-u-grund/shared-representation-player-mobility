# R port of coach_retry/expanded/count.py
# Usage (run in the directory holding 240.data 435.data 600.data 660.data,
# games.gz and transfers.gz):   Rscript count.R [published_summer_transfers.csv]
# The optional argument is the published summer_transfers.csv; if given, the
# generated event rows are put in its order (as rebuild_data.py does). Without
# it, events are sorted by date, player_id, origin_club, destination_club.
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(TRUE)

# ---- helpers (csv writing equivalent to Python csv.writer, CRLF line ends) ----
# Shortest round-trip decimal string of a double (Python repr). R's own parser is
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
fmt_num <- function(x) vapply(x, repr_one, "")
csv_field <- function(x) {
  x[is.na(x)] <- ""
  q <- grepl("[\",\r\n]", x)
  x[q] <- paste0("\"", gsub("\"", "\"\"", x[q], fixed = TRUE), "\"")
  x
}
write_csv_py <- function(dt, path) {
  cols <- lapply(dt, function(v) {
    if (is.logical(v)) ifelse(v, "True", "False")
    else if (is.double(v)) fmt_num(v)
    else csv_field(as.character(v))
  })
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
read_chr <- function(path) {
  opts <- list(colClasses = "character", na.strings = NULL, strip.white = FALSE, encoding = "UTF-8", showProgress = FALSE)
  d <- if (grepl("\\.gz$", path)) do.call(fread, c(list(cmd = paste("gzip -dc", shQuote(path))), opts))
       else do.call(fread, c(list(path), opts))
  unescape_quotes(d)
}
read_zip_or_csv <- function(path, member) {
  if (!inherits(try(utils::unzip(path, list = TRUE), silent = TRUE), "try-error")) {
    td <- tempfile(); dir.create(td); on.exit(unlink(td, recursive = TRUE))
    utils::unzip(path, files = member, exdir = td)
    read_chr(file.path(td, member))
  } else read_chr(path)
}

L <- strsplit("BE1 DK1 ES1 FR1 GB1 GR1 IT1 L1 NL1 PO1 RU1 SC1 TR1 UKR1", " ")[[1]]
V <- c("2023" = 240, "2024" = 435, "2025" = 600, "2026" = 660)

# ---- club -> league per season, in order of first appearance ----
g <- read_chr("games.gz")
g <- g[competition_id %in% L & grepl("^[0-9]+$", season)]  # isdigit
g[, gi := .I]
long <- rbind(g[, .(gi, side = 1L, season = as.integer(season), club = home_club_id, comp = competition_id)],
              g[, .(gi, side = 2L, season = as.integer(season), club = away_club_id, comp = competition_id)])
setorder(long, gi, side)
clubs <- long[, .(league = comp[.N]), by = .(season, club)]   # first-appearance order, last value
cl_get <- function(s, ids) {
  sub <- clubs[season == s]; sub$league[match(ids, sub$club)]
}
cat("league coverage", paste(sprintf("(%d, %d, %d)", 2022:2025,
    sapply(2022:2025, function(s) uniqueN(clubs[season == s, league])),
    sapply(2022:2025, function(s) clubs[season == s, .N])), collapse = ", "), "\n")

# ---- player snapshots ----
no_agent <- c("", "null", "none", "-", "nan", "n/a", "unknown", "without agent")
profiles <- list(); observed <- list()
for (y in as.integer(names(V))) {
  p <- read_zip_or_csv(paste0(V[[as.character(y)]], ".data"), "players.csv")
  if (!"agent_name" %in% names(p)) p[, agent_name := ""]
  lg <- if ("current_club_domestic_competition_id" %in% names(p)) p$current_club_domestic_competition_id else ""
  if ("domestic_competition_id" %in% names(p)) lg <- ifelse(lg != "", lg, p$domestic_competition_id)
  p[, league := lg]
  keep <- p$last_season == as.character(y - 1) &
    (p$league %in% L | (y == 2026 & p$current_club_domestic_competition_id != ""))
  rr <- p[keep]
  known_row <- !(tolower(trimws(rr$agent_name)) %in% no_agent)
  key <- paste(rr$player_id, rr$current_club_id, sep = "\r")
  # dict semantics: the last row for a (player, club) key wins
  last <- tapply(known_row, key, function(v) v[length(v)])
  observed[[as.character(y)]] <- list(key = names(last), val = as.logical(last))
  known <- as.logical(last[key])
  cat(y, "roster", nrow(rr), sum(as.logical(last)), "\n")
  profiles[[as.character(y)]] <- data.table(summer = y, version = as.integer(V[[as.character(y)]]), league = rr$league,
      player_id = rr$player_id, club_id = rr$current_club_id, player = rr$name,
      agent_label = ifelse(known, rr$agent_name, ""), known = known)
}
profiles <- rbindlist(profiles)

# ---- transfers ----
tr <- read_chr("transfers.gz")
tr <- tr[from_club_id != "" & to_club_id != "" & from_club_id != to_club_id,
         .(d = transfer_date, p = player_id, a = from_club_id, b = to_club_id, fee = transfer_fee)]
hist <- tr[, .(p, a = b, b = a, d)]             # hist[p, to, from] keyed by reverse look-up
setkey(hist, p, a, b)
tr[, y := suppressWarnings(as.integer(substr(d, 1, 4)))]
raw <- unique(tr[y %in% as.integer(names(V)) & d >= paste0(y, "-06-01") & d <= paste0(y, "-09-01")])
dd <- raw[, .(fees = list(fee)), by = .(d, p, a, b)]
dd[, y := as.integer(substr(d, 1, 4))]
dd <- dd[y != 2026]
dd[, A := NA_character_]; dd[, B := NA_character_]
for (s in unique(dd$y)) {
  dd[y == s, A := cl_get(s - 1L, a)]
  dd[y == s, B := cl_get(s, b)]
}
dd <- dd[!is.na(A) & !is.na(B)]
dd[, route := ifelse(A == B, "within", "between")]
okey <- function(s) paste(s$player_id, s$club_id, sep = "\r")
dd[, obs_idx := {
  r <- integer(.N)
  for (s in unique(y)) {
    i <- which(y == s); o <- observed[[as.character(s)]]
    r[i] <- match(paste(p[i], a[i], sep = "\r"), o$key)
  }
  r }]
dd[, obs_val := {
  r <- rep(NA, .N)
  for (s in unique(y)) {
    i <- which(y == s); o <- observed[[as.character(s)]]
    r[i] <- o$val[obs_idx[i]]
  }
  r }]
dd[, fee0 := vapply(fees, function(f) all(f == "" | suppressWarnings(as.numeric(f)) == 0, na.rm = FALSE), NA)]
dd[, ret := FALSE]
cand <- which(substr(dd$d, 6, 10) %in% c("06-30", "07-01") & dd$fee0)
for (i in cand) {
  h <- hist[.(dd$p[i], dd$a[i], dd$b[i]), d, nomatch = NULL]
  dd$ret[i] <- any(h >= paste0(dd$y[i] - 1, "-06-01") & h < dd$d[i])
}
events <- dd[, .(summer = y, date = d, player_id = p, origin_club = a, destination_club = b,
                 origin_league = A, destination_league = B, route,
                 baseline_match = !is.na(obs_val), known_agent = obs_val %in% TRUE, likely_return = ret)]
setorder(events, date, player_id, origin_club, destination_club)
if (length(args) >= 1) {
  pub <- read_chr(args[1])
  kf <- function(x) paste(x$date, x$player_id, x$origin_club, x$destination_club, sep = "\r")
  ord <- match(kf(pub), kf(events))
  if (anyNA(ord) || nrow(pub) != nrow(events) || anyDuplicated(ord))
    stop("Rebuilt summer event keys differ from the archived panel")
  events <- events[ord]
}

summ <- function(y, grp, rt) {
  s <- events[summer == y]
  s <- switch(grp, all = s, roster = s[(baseline_match)], agent = s[(known_agent)],
              agent_screened = s[known_agent & !likely_return])
  sum(s$route == rt)
}
counts <- rbindlist(lapply(2023:2025, function(y) rbindlist(lapply(c("all", "roster", "agent", "agent_screened"),
  function(gr) data.table(summer = y, screen = gr, within = summ(y, gr, "within"), between = summ(y, gr, "between"))))))
for (y in 2023:2025) cat(y, paste(sprintf("('%s', %d, %d)", counts[summer == y]$screen, counts[summer == y]$within, counts[summer == y]$between), collapse = ", "), "\n")

lc <- profiles[, .(players = .N, named_agent = sum(known)), by = .(summer, league)]
lc <- lc[order(match(summer, as.integer(names(V))), league, method = "radix")]
dc <- clubs[season %in% 2023:2025, .(summer = season, club_id = club, league)]

write_csv_py(profiles[, known := NULL][], "player_snapshots.csv")
write_csv_py(events, "summer_transfers.csv")
write_csv_py(counts, "counts.csv")
write_csv_py(lc, "league_coverage.csv")
write_csv_py(dc, "destination_clubs.csv")
