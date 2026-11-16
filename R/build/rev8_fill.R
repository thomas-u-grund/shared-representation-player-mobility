# R port of revision/rev8_fill.py
# Recover missing representative labels from the archived releases and rebuild the linkage.
# variants (cumulative):
#   base  baseline release only
#   pre   + latest earlier PRE-window release label for the same player (no look-ahead),
#         + movers listed at clubs outside the 14 leagues
#   post  + earliest label in a release up to ~15 months AFTER the window (look-ahead; sensitivity only)
# Usage: Rscript rev8_fill.R [pre|base|post] [--expanded DIR] [--out DIR] [--root DIR]
suppressWarnings(suppressPackageStartupMessages(library(data.table)))
invisible(Sys.setlocale("LC_COLLATE", "C"))
script_dir <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE))
source(file.path(script_dir, "build_performance_relaxed.R"))   # defines helpers and build_performance_relaxed()

args <- commandArgs(TRUE)
variant <- "pre"; EXP <- NULL; OUT <- NULL
ROOT <- Sys.getenv("FOOTBALL_ROOT", normalizePath(file.path(script_dir, "..", ".."), mustWork = FALSE))
i <- 1L
while (i <= length(args)) {
  if (args[i] == "--expanded") { EXP <- args[i + 1L]; i <- i + 2L }
  else if (args[i] == "--out") { OUT <- args[i + 1L]; i <- i + 2L }
  else if (args[i] == "--root") { ROOT <- args[i + 1L]; i <- i + 2L }
  else { variant <- args[i]; i <- i + 1L }
}
stopifnot(variant %in% c("base", "pre", "post"))
CR <- file.path(ROOT, "coach_retry"); K <- file.path(ROOT, "raw", "kaggle")
if (is.null(EXP)) EXP <- file.path(CR, "expanded")
dirname_ <- c(pre = "perf_extended", base = "perf_extended_base", post = "perf_extended_post")[[variant]]
if (is.null(OUT)) OUT <- file.path(CR, dirname_)

vs <- c(240L, 325L, 355L, 435L, 547L, 566L, 600L, 603L, 606L, 660L)
dates <- c(`240` = "2023-05-23", `325` = "2023-12-29", `355` = "2024-02-09", `435` = "2024-05-31", `547` = "2024-12-27",
           `566` = "2025-02-11", `600` = "2025-04-11", `603` = "2026-02-14", `606` = "2026-02-14", `660` = "2026-05-26")
base_v <- c(`2023` = 240L, `2024` = 435L, `2025` = 600L)
yrs <- names(base_v)
bad <- c("", "null", "none", "-", "nan", "n/a", "unknown", "without agent")
# strings pandas.read_csv turns into NaN (the releases are read with pandas in the reference)
pd_na <- c("", "#N/A", "#N/A N/A", "#NA", "-1.#IND", "-1.#QNAN", "-NaN", "-nan", "1.#IND", "1.#QNAN", "<NA>", "N/A", "NA",
           "NULL", "NaN", "None", "n/a", "nan", "null")
clean <- function(x) { s <- trimws(x, whitespace = "[ \t\r\n\f\v]"); fifelse(tolower(s) %in% bad, "", s) }
clean_pd <- function(x) fifelse(x %in% pd_na, "", clean(x))

rd <- function(v) {
  p <- file.path(K, paste0("data_", v))
  d <- if (is_zip(p)) fread_str(cmd = paste("unzip -p", shQuote(p), "players.csv")) else fread_str(p)
  d[!duplicated(player_id)]
}
rel <- setNames(lapply(vs, rd), as.character(vs))
relclean <- lapply(rel, function(d) setNames(clean_pd(col_or_blank(d, "agent_name")), d$player_id))

# vectorised fill_label: returns list(label, source) for vectors of summers and player ids
fill_label <- function(s, pid) {
  n <- length(pid); lab <- rep("", n); src <- rep(NA_integer_, n)
  for (sy in unique(s)) {
    idx <- which(s == sy); win <- paste0(sy, "-06-01"); b <- base_v[[as.character(sy)]]
    order_v <- integer()
    if (variant %in% c("pre", "post"))
      order_v <- c(order_v, sort(vs[dates[as.character(vs)] < win & vs != b], decreasing = TRUE))
    if (variant == "post") {
      lim <- paste0(as.integer(sy) + 1L, "-09-01")
      order_v <- c(order_v, sort(vs[dates[as.character(vs)] >= win & dates[as.character(vs)] < lim]))
    }
    for (v in order_v) {
      todo <- idx[lab[idx] == ""]
      if (!length(todo)) break
      cl <- relclean[[as.character(v)]][pid[todo]]
      hit <- !is.na(cl) & cl != ""
      lab[todo[hit]] <- unname(cl[hit]); src[todo[hit]] <- v
    }
  }
  list(label = lab, source = src)
}

stage <- file.path(tempdir(), "stage"); dir.create(stage, recursive = TRUE)
out <- normalizePath(OUT, mustWork = FALSE); dir.create(out, showWarnings = FALSE, recursive = TRUE)
for (v in c(240, 435, 600)) invisible(file.copy(file.path(K, paste0("data_", v)), file.path(stage, paste0(v, ".data"))))
invisible(file.copy(file.path(CR, "games.gz"), file.path(stage, "games.gz")))
invisible(file.copy(file.path(EXP, "destination_clubs.csv"), file.path(stage, "destination_clubs.csv")))
snap <- read_any(file.path(EXP, "player_snapshots.csv"))
fields <- names(snap)
audit <- list()
in_y <- snap$summer %in% yrs
have <- paste(snap$summer[in_y], snap$player_id[in_y], sep = "\r")
miss <- which(in_y & clean(snap$agent_label) == "")
if (length(miss)) {
  fl <- fill_label(as.integer(snap$summer[miss]), snap$player_id[miss])
  ok <- fl$label != ""
  set(snap, miss[ok], "agent_label", fl$label[ok])
  audit[[1]] <- data.table(summer = as.integer(snap$summer[miss[ok]]), player_id = snap$player_id[miss[ok]],
                           source = fl$source[ok], kind = "roster_fill")
}
rows <- read_any(file.path(EXP, "summer_transfers.csv"))
if (variant %in% c("pre", "post")) {
  # movers listed at clubs outside the 14 leagues
  ek <- paste(rows$summer, rows$player_id, sep = "\r")
  cand <- which(!ek %in% have & !duplicated(ek))
  add <- list(); aud2 <- list()
  if (length(cand)) {
    s_ <- as.integer(rows$summer[cand]); pid <- rows$player_id[cand]
    vv <- base_v[as.character(s_)]
    inrel <- mapply(function(v, p) p %in% rel[[as.character(v)]]$player_id, vv, pid)
    cand <- cand[inrel]; s_ <- s_[inrel]; pid <- pid[inrel]; vv <- vv[inrel]
    lab0 <- mapply(function(v, p) unname(relclean[[as.character(v)]][p]), vv, pid)
    fl <- fill_label(s_, pid)
    lab <- fifelse(lab0 != "", lab0, fl$label)
    pyval <- function(x) fifelse(x %in% pd_na, "nan", x)   # str(NaN) in the reference
    recs <- rbindlist(lapply(seq_along(cand), function(j) {
      d <- rel[[as.character(vv[j])]]; r <- match(pid[j], d$player_id)
      data.table(summer = rows$summer[cand[j]], version = as.character(vv[j]),
                 league = pyval(col_or_blank(d, "current_club_domestic_competition_id")[r]), player_id = pid[j],
                 club_id = pyval(col_or_blank(d, "current_club_id")[r]), player = pyval(col_or_blank(d, "name")[r]),
                 agent_label = lab[j])
    }))
    setcolorder(recs, fields)
    snap <- rbind(snap, recs)
    in_y <- snap$summer %in% yrs
    k <- lab != ""
    if (any(k)) audit[[length(audit) + 1L]] <- data.table(summer = s_[k], player_id = pid[k], source = as.integer(vv[k]), kind = "outside_14_leagues")
  }
}
audit <- rbindlist(audit)
write_csv(snap, file.path(stage, "player_snapshots.csv"))
sy <- snap[summer %in% yrs]
lab_by <- sy[, .(lab = (clean(agent_label) != "")[.N]), by = .(summer, player_id)]
club_by <- unique(snap[, .(summer, player_id, club_id)])
at_origin <- rows[, paste(summer, player_id, origin_club, sep = "\r")] %in% club_by[, paste(summer, player_id, club_id, sep = "\r")]
m <- lab_by[.(rows$summer, rows$player_id), on = .(summer, player_id), lab]
inlab <- !is.na(m); known <- fifelse(inlab, m, FALSE)
rows[, baseline_match := ifelse(at_origin | inlab, "True", "False")]
rows[, known_agent := ifelse(known, "True", "False")]
rows[, link_type := fifelse(at_origin & known, "origin", fifelse(known, "player_only", ""))]
write_csv(rows, file.path(stage, "summer_transfers.csv"))
if (nrow(audit)) fwrite(audit, file.path(out, "fill_audit.csv"), na = "") else cat("\n", file = file.path(out, "fill_audit.csv"))
cat(variant, "filled rows:", if (nrow(audit)) paste(capture.output(print(audit[, .N, by = kind])), collapse = " ") else "{}", "\n")
sink(tempfile()); build_performance_relaxed(stage, file.path(ROOT, "raw", "performance"), out); sink()
t <- fread(file.path(out, "transfer_performance.csv"), colClasses = "character", na.strings = NULL)
cat(variant, "events", nrow(t), "agent-linked", sum(t$known_agent == "True"), "link types:",
    paste(names(table(t$link_type)), table(t$link_type), collapse = ", "), "\n")
unlink(stage, recursive = TRUE)
