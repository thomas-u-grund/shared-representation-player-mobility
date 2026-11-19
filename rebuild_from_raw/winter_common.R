# Shared helpers for the winter-data scripts (04_winter_rosters.R, 07_winter_rosters_extended.R, 05_winter_backcast.R).
# Conventions for reading and writing csv files (missing-value strings, number formatting) and for normalising labels.
suppressPackageStartupMessages({library(data.table); library(stringi)})
Sys.setlocale("LC_COLLATE", "C")
setDTthreads(1)

LEAGUES <- strsplit("BE1 DK1 ES1 FR1 GB1 GR1 IT1 L1 NL1 PO1 RU1 SC1 TR1 UKR1", " ")[[1]]
BAD <- c("", "null", "none", "-", "nan", "n/a", "unknown", "without agent", "no agent")
NA_STRINGS <- c("", "#N/A", "#N/A N/A", "#NA", "-1.#IND", "-1.#QNAN", "-NaN", "-nan", "1.#IND", "1.#QNAN",
          "<NA>", "N/A", "NA", "NULL", "NaN", "None", "n/a", "nan", "null")

# read a csv / zip / gz, inferring the compression from the file content
read_table <- function(path, select = NULL) {
  con <- file(path, "rb"); magic <- readBin(con, "raw", 2); close(con)
  cmd <- if (identical(magic, as.raw(c(0x50, 0x4b)))) paste("unzip -p", shQuote(path))
         else if (identical(magic, as.raw(c(0x1f, 0x8b)))) paste("gzip -dc", shQuote(path))
         else paste("cat", shQuote(path))
  fread(cmd = cmd, na.strings = NA_STRINGS, encoding = "UTF-8", select = select, strip.white = FALSE,
        showProgress = FALSE, data.table = TRUE)
}

# normalised label key (casefold, NFKD, keep [a-z0-9])
make_key <- function(v) {
  v <- as.character(v); u <- unique(v)
  k <- stri_replace_all_regex(stri_trans_nfkd(stri_trans_casefold(u)), "[^a-z0-9]", "")
  k[is.na(k)] <- ""
  k[match(v, u)]
}

# strip and blank out missing markers in representative labels
clean_label <- function(v) {
  l <- ifelse(is.na(v), "", stri_trim_both(v))
  l[stri_trans_tolower(l) %in% BAD] <- ""
  l
}

# club -> league membership by season (the last assignment wins)
load_members <- function(games) {
  g <- games[competition_id %in% LEAGUES]
  long <- data.table(season = rep(g$season, each = 2), league = rep(g$competition_id, each = 2),
                     club = as.vector(rbind(g$home_club_id, g$away_club_id)))
  long <- unique(long, by = c("season", "club"), fromLast = TRUE)
  split(long[, .(club, league)], long$season)
}

# unique player/date/from/to moves with the positive-fee flag
load_moves <- function(path, need_player = FALSE) {
  raw <- read_table(path)
  raw <- raw[!is.na(from_club_id) & !is.na(to_club_id) & !is.na(transfer_date) & (!need_player | !is.na(player_id))]
  raw <- raw[from_club_id != to_club_id]
  raw[, positive_fee := fifelse(is.na(transfer_fee), 0, transfer_fee) > 0]
  e <- raw[!is.na(player_id), .(positive_fee = any(positive_fee)), by = .(player_id, transfer_date, from_club_id, to_club_id)]
  setorder(e, player_id, transfer_date, from_club_id, to_club_id)
  e
}

# TRUE when a zero/unknown-fee-agnostic reverse move exists in (date year - 1)-01-01 <= d < date
reverse_in_prior_year <- function(z, e) {
  if (!nrow(z)) return(logical(0))
  zz <- data.table(i = seq_len(nrow(z)), player_id = z$player_id, a = z$to_club_id, b = z$from_club_id,
                   d = z$transfer_date, lo = paste0(as.integer(substr(z$transfer_date, 1, 4)) - 1L, "-01-01"))
  h <- e[, .(player_id, a = from_club_id, b = to_club_id, hd = transfer_date)]
  m <- h[zz, on = .(player_id, a, b), allow.cartesian = TRUE, nomatch = NULL]
  m <- m[hd >= lo & hd < d]
  seq_len(nrow(z)) %in% m$i
}

# ---- csv writer ---------------------------------------------------------------
fmt_float <- function(x) {
  out <- character(length(x)); ok <- !is.na(x) & is.finite(x)
  v <- x[ok]
  f <- sprintf("%.15g", v)
  bad <- as.numeric(f) != v; f[bad] <- sprintf("%.16g", v[bad])
  bad <- as.numeric(f) != v; f[bad] <- sprintf("%.17g", v[bad])
  f <- ifelse(grepl("[.e]", f), f, paste0(f, ".0"))
  f <- sub("e([+-])([0-9])$", "e\\10\\2", f)
  out[ok] <- f
  out[!is.na(x) & is.infinite(x)] <- ifelse(x[!is.na(x) & is.infinite(x)] > 0, "inf", "-inf")
  out
}
fmt_col <- function(x) {
  if (is.logical(x)) { o <- ifelse(x, "True", "False"); o[is.na(x)] <- ""; return(o) }
  if (is.integer(x)) { if (anyNA(x)) { o <- fmt_float(as.numeric(x)) } else o <- as.character(x); o[is.na(x)] <- ""; return(o) }
  if (is.numeric(x)) { o <- fmt_float(as.numeric(x)); o[is.na(x)] <- ""; return(o) }
  x <- as.character(x); x[is.na(x)] <- ""
  q <- grepl("[\",\r\n]", x)
  x[q] <- paste0("\"", gsub("\"", "\"\"", x[q], fixed = TRUE), "\"")
  x
}
write_py_csv <- function(dt, path) {
  dt <- as.data.table(dt)
  out <- as.data.table(lapply(dt, fmt_col)); setnames(out, fmt_col(names(dt)))
  fwrite(out, path, quote = FALSE, sep = ",", eol = "\n", na = "")
}

# script directory (Rscript encodes spaces in the path as "~+~")
script_dir <- function() dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE))

# club sets / candidate construction shared by build_winter*.R ------------------------------
# z: moves with event_id, from/to, destination_league, agent_key; labelled: labelled roster (club_id, player_id, agent_key)
winter_candidates <- function(z, clubs, labelled, year) {
  zz <- z[agent_key != ""]
  if (!nrow(zz)) return(data.table())
  size <- labelled[, .(known_roster_labels = .N), by = club_id]
  co <- labelled[, .(n = .N), by = .(club_id, agent_key)]
  pl <- unique(labelled[, .(player_id, club_id, lab = agent_key)], by = c("player_id", "club_id"))
  ev <- zz[, .(k = .I, event_id, player_id, from_club_id, to_club_id, destination_league, agent_key)]
  cl <- merge(clubs, size, by.x = "club", by.y = "club_id")      # clubs with at least one label
  cand <- cl[ev, on = .(league = destination_league), allow.cartesian = TRUE, nomatch = NULL]
  cand <- cand[club != from_club_id]
  cand[, `:=`(n = .N, has = any(club == to_club_id)), by = k]
  cand <- cand[has & n >= 2]
  setorder(cand, k, club)
  cand[co, on = .(club = club_id, agent_key), c_n := i.n]
  cand[is.na(c_n), c_n := 0L]
  cand[pl, on = .(player_id, club = club_id), focal := i.lab == agent_key]
  cand[is.na(focal), focal := FALSE]
  cand[, .(event_id, january = year, candidate_club = club, chosen = as.integer(club == to_club_id),
           known_roster_labels = known_roster_labels, co_clients = c_n - as.integer(focal))]
}
