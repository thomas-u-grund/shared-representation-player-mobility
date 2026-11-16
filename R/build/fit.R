# R port of coach_retry/expanded/fit.py
# Usage: run in the directory holding the output of count.R (player_snapshots.csv,
# summer_transfers.csv, destination_clubs.csv):   Rscript fit.R
# Conditional logits by Newton-Raphson (exact gradient/Hessian) with
# player-clustered sandwich standard errors, identical in form to fit.py.
suppressPackageStartupMessages(library(data.table))

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
  cols <- lapply(dt, function(v) if (is.double(v)) fmt_num(v) else csv_field(as.character(v)))
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
read_chr <- function(p) unescape_quotes(fread(p, colClasses = "character", na.strings = NULL, strip.white = FALSE,
                              encoding = "UTF-8", showProgress = FALSE))
# orthographic key: casefold, NFKD, drop non-ASCII, keep [a-z0-9]
key <- function(s) {
  s <- stringi::stri_trans_nfkd(stringi::stri_trans_casefold(s))
  gsub("[^a-z0-9]", "", stringi::stri_replace_all_regex(s, "[^\\x00-\\x7F]", ""))
}

roster <- read_chr("player_snapshots.csv")
events <- read_chr("summer_transfers.csv")[known_agent == "True"]
roster[, k := key(agent_label)]
lab_key <- paste(roster$summer, roster$player_id, roster$club_id, sep = "\r")
lab_last <- !duplicated(lab_key, fromLast = TRUE)
labels <- setNames(roster$k[lab_last], lab_key[lab_last])        # dict: last row wins
named <- roster[agent_label != ""]
cnt <- function(...) { t <- table(paste(..., sep = "\r")); setNames(as.integer(t), names(t)) }
market <- cnt(named$summer, named$league, named$k)
club <- cnt(named$summer, named$club_id, named$k)
known <- cnt(named$summer, named$club_id)
dest <- read_chr("destination_clubs.csv")
clubs <- split(dest$club_id, paste(dest$summer, dest$league, sep = "\r"))
leagues <- sort(unique(roster[summer == "2023", league]), method = "radix")
get0n <- function(tab, k) { v <- tab[k]; v[is.na(v)] <- 0L; as.numeric(v) }

newton <- function(X, id, y, ncl) {
  # X: stacked candidate rows; id: event index; y: row index of chosen alternative
  k <- ncol(X); b <- numeric(k); n <- max(id)
  stats <- function(b) {
    v <- drop(X %*% b)
    m <- as.vector(tapply(v, id, max))[id]
    e <- exp(v - m)
    den <- rowsum(e, id)[, 1]
    p <- as.vector(e / den[id])
    list(p = p, mu = rowsum(p * X, id), nll = sum(log(den) + as.vector(tapply(v, id, max))) - sum(v[y]))
  }
  for (it in 1:100) {
    s <- stats(b)
    grad <- colSums(s$mu) - colSums(X[y, , drop = FALSE])
    H <- crossprod(X * s$p, X) - crossprod(s$mu)
    step <- solve(H, grad)
    t <- 1                                  # step halving keeps Newton monotone
    while (t > 1e-10 && stats(b - t * step)$nll > s$nll + 1e-12 * abs(s$nll)) t <- t / 2
    b <- b - t * step
    if (max(abs(t * step)) < 1e-13) break
  }
  s <- stats(b)
  grad <- colSums(s$mu) - colSums(X[y, , drop = FALSE])
  stopifnot(max(abs(grad)) < 1e-3)
  H <- crossprod(X * s$p, X) - crossprod(s$mu)
  bread <- solve(H)
  sc <- X[y, , drop = FALSE] - s$mu                                   # per-event score
  S <- rowsum(sc, ncl)
  V <- bread %*% crossprod(S) %*% bread * nrow(S) / (nrow(S) - 1)
  list(b = b[1], se = sqrt(V[1, 1]))
}

results <- list()
for (sample in c("broad", "return_screened")) {
  for (model in c("league", "cross_league", "club_within_destination_league")) {
    Xl <- list(); idl <- list(); yl <- integer(); pl <- character(); n <- 0L; pos <- 0L; ys <- integer()
    for (i in seq_len(nrow(events))) {
      e <- events[i]
      if (sample == "return_screened" && e$likely_return == "True") next
      if (model == "cross_league" && e$origin_league == e$destination_league) next
      y <- e$summer; p <- e$player_id; cc <- e$origin_club; d <- e$destination_club
      h <- e$origin_league; l <- e$destination_league
      a <- labels[[paste(y, p, cc, sep = "\r")]]
      if (model == "club_within_destination_league") {
        cand <- sort(setdiff(clubs[[paste(y, l, sep = "\r")]], cc), method = "radix")
        cand <- cand[get0n(known, paste(y, cand, sep = "\r")) > 0]
        if (!(d %in% cand) || length(cand) < 2) next
        x <- cbind(as.numeric(get0n(club, paste(y, cand, a, sep = "\r")) > 0),
                   log1p(get0n(known, paste(y, cand, sep = "\r"))))
        chosen <- match(d, cand)
      } else {
        cand <- if (model == "league") leagues else leagues[leagues != h]
        mk <- pmax(0, get0n(market, paste(y, cand, a, sep = "\r")) - (cand == h))
        x <- cbind(log1p(mk), if (model == "league") as.numeric(cand == h),
                   sapply(leagues[-1], function(s) as.numeric(cand == s)))
        chosen <- match(l, cand)
      }
      n <- n + 1L
      Xl[[n]] <- x; idl[[n]] <- rep(n, nrow(x)); yl[n] <- pos + chosen; pos <- pos + nrow(x); pl[n] <- p
    }
    X <- do.call(rbind, Xl); id <- unlist(idl)
    f <- newton(X, id, yl, pl)
    se <- f$se; b <- f$b
    results[[length(results) + 1]] <- data.table(sample = sample, model = model, events = n,
      odds_ratio = exp(b), ci_low = exp(b - 1.96 * se), ci_high = exp(b + 1.96 * se))
    print(results[[length(results)]])
  }
}
res <- rbindlist(results)
res[, events := as.character(events)]
write_csv_py(res, "model_results.csv")
