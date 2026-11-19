# Create the pseudonymised analysis inputs in data/study1/ (read by the analysis scripts).
# Usage: Rscript rebuild_from_raw/make_pseudonymised_data.R <coach_retry dir> <raw/kaggle dir> <output dir>
# (winter_recovery_extended/ and winter_proxy/ are read from the parent of <coach_retry dir>.)
# Player identifiers and representative labels are replaced by random codes drawn with a fresh random permutation; the mapping is
# not stored, so the files cannot be linked back to named individuals from this repository. Player names and EA identifiers are dropped.
# Added columns: `hlabel` (labels that differ only by generic words such as group, agency or management share a code), `nationality` on the
# winter rosters, and contract_dates.csv (contract end dates by summer and player).
suppressPackageStartupMessages({library(data.table); library(stringi)})
args <- commandArgs(TRUE); src <- normalizePath(args[1]); kag <- normalizePath(args[2]); out <- normalizePath(args[3], mustWork = FALSE); dir.create(out, showWarnings = FALSE, recursive = TRUE)
proj <- dirname(src); set.seed(NULL)
asc <- function(s) gsub("[^\\x01-\\x7F]", "", stri_trans_nfkd(tolower(as.character(s))), perl = TRUE)
key <- function(s) gsub("[^a-z0-9]", "", asc(s))
STOP <- strsplit("gmbh ltd limited llc sl srl spa sa bv ag oy ab as inc co company group agency agencia agentur management managment sports sport football futbol soccer consulting consultancy international intl world the of and de la el", " ")[[1]]
hkey <- function(s, orig) vapply(seq_along(s), function(i) { t <- strsplit(asc(s[i]), "[^a-z0-9]+")[[1]]; t <- t[nzchar(t) & !(t %in% STOP)]; r <- paste(sort(unique(t)), collapse = ""); if (nzchar(r)) r else orig[i] }, "")
rdc <- function(p) fread(p, colClasses = "character", na.strings = NULL, encoding = "UTF-8")
FILES <- c("player_performance", "transfer_performance", "within_league_candidates")
DIRS <- c("perf", "perf_extended", "perf_extended_base", "perf_extended_post")
data <- setNames(lapply(DIRS, function(d) setNames(lapply(FILES, function(f) rdc(file.path(src, d, paste0(f, ".csv")))), FILES)), DIRS)
hist <- as.data.table(readr::read_csv(file.path(src, "transfers.gz"), col_select = c("player_id", "transfer_date", "from_club_id", "to_club_id", "transfer_fee"), col_types = readr::cols(.default = "c"), na = character(), progress = FALSE))
rd_kaggle <- function(v) { p <- file.path(kag, paste0("data_", v))
  if (system2("unzip", c("-tq", shQuote(p)), stdout = FALSE, stderr = FALSE) == 0) fread(cmd = paste("unzip -p", shQuote(p), "players.csv"), colClasses = "character", na.strings = c("", "NA")) else fread(p, colClasses = "character", na.strings = c("", "NA")) }
ce <- rbindlist(lapply(list(c(2023, 240), c(2024, 435), c(2025, 600)), function(sv) rd_kaggle(sv[2])[, .(player_id, contract_expiration_date = fifelse(is.na(contract_expiration_date), "", substr(contract_expiration_date, 1, 10)), summer = as.character(sv[1]))]))
WIN <- list(winter_recovery_extended = list(winter_player_snapshots = c(`2024` = 325, `2025` = 547), january_transfers = NULL, january_destination_candidates = NULL),
            winter_proxy = list(backcast_rosters = c(`2026` = 603), backcast_january_transfers = NULL, backcast_candidates = NULL),
            `winter_proxy/version_606_sensitivity` = list(backcast_rosters = c(`2026` = 606), backcast_january_transfers = NULL, backcast_candidates = NULL))
wdata <- lapply(names(WIN), function(d) setNames(lapply(names(WIN[[d]]), function(f) rdc(file.path(proj, d, paste0(f, ".csv")))), names(WIN[[d]]))); names(wdata) <- names(WIN)
ids <- c(hist$player_id, ce$player_id)
for (d in data) ids <- c(ids, d$player_performance$player_id, d$transfer_performance$player_id)
for (d in wdata) for (t in d) if ("player_id" %in% names(t)) ids <- c(ids, t$player_id)
mapping <- function(values, fmt) { u <- sort(unique(values)); perm <- sample(length(u)); setNames(sprintf(fmt, perm), u) }
pid <- mapping(ids, "%d")
rawlabels <- unlist(lapply(data, function(d) d$player_performance$agent_label)); winlabels <- unlist(lapply(wdata, function(d) lapply(d, function(t) if ("agent_label" %in% names(t)) t$agent_label)))
allk <- key(c(rawlabels, winlabels)); lab <- mapping(allk[nzchar(allk)], "rep%05d")
hk <- hkey(rawlabels, key(rawlabels)); hl <- mapping(hk[nzchar(key(rawlabels))], "hrep%05d")
for (dn in names(data)) {
  pp <- data[[dn]]$player_performance; tp <- data[[dn]]$transfer_performance
  pp[, player_id := pid[player_id]]; tp[, player_id := pid[player_id]]
  k <- key(pp$agent_label); h <- ifelse(nzchar(k), hkey(pp$agent_label, k), "")
  first <- h[!duplicated(k)]; names(first) <- k[!duplicated(k)]       # first row of each label defines its harmonised label (as in the original analysis)
  pp[, hlabel := ifelse(nzchar(k), hl[first[k]], "")]
  pp[, agent_label := ifelse(nzchar(k), lab[k], "")]
  pp <- pp[, setdiff(names(pp), c("player", "ea_id")), with = FALSE]
  dir.create(file.path(out, dn), showWarnings = FALSE, recursive = TRUE)
  fwrite(pp, file.path(out, dn, "player_performance.csv")); fwrite(tp, file.path(out, dn, "transfer_performance.csv")); fwrite(data[[dn]]$within_league_candidates, file.path(out, dn, "within_league_candidates.csv"))
}
hist[, player_id := pid[player_id]]; fwrite(hist, file.path(out, "transfers.gz"), compress = "gzip")
ce[, player_id := pid[player_id]]; fwrite(ce[, .(summer, player_id, contract_expiration_date)], file.path(out, "contract_dates.csv"))
nat <- lapply(c(`325` = 325, `547` = 547, `603` = 603, `606` = 606), function(v) { x <- rd_kaggle(v); x <- unique(x, by = "player_id"); setNames(x$country_of_citizenship, x$player_id) })
for (d in names(WIN)) { dir.create(file.path(out, d), showWarnings = FALSE, recursive = TRUE)
  for (f in names(WIN[[d]])) { t <- copy(wdata[[d]][[f]]); vers <- WIN[[d]][[f]]; has <- "player_id" %in% names(t); raw <- if (has) t$player_id
    if (!is.null(vers)) { nv <- vapply(seq_len(nrow(t)), function(i) { j <- t$january[i]; if (j %in% names(vers)) { x <- nat[[as.character(vers[[j]])]][raw[i]]; if (is.na(x)) "" else x } else "" }, ""); t[, nationality := nv] }
    if (has) t[, player_id := pid[raw]]
    if ("player_name" %in% names(t)) t[, player_name := NULL]
    if ("agent_label" %in% names(t)) { k <- key(t$agent_label); t[, agent_label := ifelse(nzchar(k), lab[k], "")] }
    if ("agent_key" %in% names(t)) t[, agent_key := ""]
    fwrite(t, file.path(out, d, paste0(f, ".csv"))) } }
cat("written to", out, "\n")
