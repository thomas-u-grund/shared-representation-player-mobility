# Event-level enrichment: transfer fee, return to a former club, paid/free, turn-of-July flag, contract status.
enrich <- function(root, sample = "extended") {
  perf <- file.path(root, if (sample == "archived") "perf" else paste0("perf_", sample))
  tr <- as.data.table(readr::read_csv(file.path(root, "transfers.gz"), col_types = readr::cols(.default = "c"), na = c("", "NA"), progress = FALSE))
  tr[, fee := suppressWarnings(as.numeric(transfer_fee))]
  tp <- rd(file.path(perf, "transfer_performance.csv"))
  g <- tr[, .(fee = if (all(is.na(fee))) NA_real_ else max(fee, na.rm = TRUE)), by = .(player_id, transfer_date, from_club_id, to_club_id)]
  tp <- merge(tp, g, by.x = c("player_id", "date", "origin_club", "destination_club"), by.y = c("player_id", "transfer_date", "from_club_id", "to_club_id"), all.x = TRUE)
  hist <- unique(tr[, .(player_id, transfer_date, from_club_id, to_club_id)])
  j <- merge(tp[, .(event_id, player_id, date, destination_club)], hist, by = "player_id", allow.cartesian = TRUE)
  j <- j[transfer_date < date & (from_club_id == destination_club | to_club_id == destination_club)]
  tp[, former_dest := event_id %in% j$event_id]
  tp[, paid := !is.na(fee) & fee > 0]
  list(ev = tp[, .(event_id, fee, former_dest, paid)], tr = tr)
}
add_flags <- function(frame, ev) {
  f <- merge(frame, ev, by = "event_id", all.x = TRUE, sort = FALSE)
  f[, turn := substr(date, 6, 10) %in% c("06-30", "07-01")]
  f
}
contract_status <- function(m, root) {
  ce <- fread(file.path(root, "contract_dates.csv"), colClasses = list(character = c("player_id", "contract_expiration_date")), na.strings = c("", "NA"))
  ce[, summer := as.integer(summer)]; ce <- unique(ce, by = c("summer", "player_id"))
  m <- merge(m, ce, by = c("summer", "player_id"), all.x = TRUE, sort = FALSE)
  m[, cexp := as.Date(contract_expiration_date)]
  win <- as.Date(paste0(m$summer, "-06-30"))
  m[, exp_status := fifelse(is.na(cexp), "unknown", fifelse(cexp <= win, "expiring", fifelse(cexp <= win + 365, "one more year", "longer")))]
  m
}

retained <- function(x, fixed = "candidate_club") {
  x <- valid_events(x)
  repeat { n <- nrow(x); x <- valid_events(x[get(fixed) %in% x[chosen == 1, get(fixed)]]); if (nrow(x) == n) return(x) }
}
