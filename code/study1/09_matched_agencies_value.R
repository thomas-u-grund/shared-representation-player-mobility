# Matched agencies when the profile includes player market value, and with the mover left out of the focal profile (Table 3, Online Appendix B8).
# Usage: Rscript code/study1/09_matched_agencies_value.R <data_root> [output_dir]
# Writes matched_agencies_value_results.csv.
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "..", "lib", "setup.R"))
d <- prepare(root, "extended"); en <- enrich(root, "extended")
main <- add_flags(d$main, en$ev); pl <- d$players[label != ""]
pl[, pos := fifelse(is.na(position), "NA", position)]
pl[, ageband := as.character(cut(age_at_cutoff, c(0, 21, 25, 29, 99), labels = FALSE))]; pl[is.na(ageband), ageband := "NA"]
pl[, nat2 := fifelse(nation == "", "NA", nation)]
pl[, mv := as.numeric(market_value_eur)]
pl[, vband := NA_character_]
for (s in unique(pl$summer)) { q <- quantile(pl[summer == s & !is.na(mv), mv], c(.2, .4, .6, .8)); pl[summer == s, vband := as.character(findInterval(mv, q) + 1L)] }
pl[is.na(mv), vband := "NA"]
cnt <- pl[, .(n = .N), by = .(summer, candidate_club = club_id, label)]
mv_med <- pl[, .(med = median(log(mv + 1e5), na.rm = TRUE), spread = IQR(log(mv + 1e5), na.rm = TRUE)), by = .(summer, label)]
mover <- unique(pl[, .(summer, player_id, club_id, label, nat2, pos, ageband, vband)])
setkey(mover, summer, player_id, club_id)
# one row per event with the mover's profile
evs <- unique(main[, .(event_id, summer, player_id, snapshot_club, label)])
evs <- merge(evs, mover[, .(summer, player_id, snapshot_club = club_id, nat2, pos, ageband, vband)], by = c("summer", "player_id", "snapshot_club"), all.x = TRUE)
cat("events with mover profile:", sum(!is.na(evs$nat2)), "of", nrow(evs), "\n")
select_twins <- function(blocks, loo, sizes_rule = TRUE) {
  res <- list()
  for (s in sort(unique(pl$summer))) {
    x <- pl[summer == s]
    C <- lapply(blocks, function(b) unclass(table(x$label, x[[b]])))      # counts, labels x categories
    labs <- rownames(C[[1]]); nn <- as.integer(table(x$label)[labs])
    mk <- function(Cs) { sh <- lapply(Cs, function(m) m / rowSums(m)); F <- do.call(cbind, sh); F / sqrt(rowSums(F^2)) }
    F0 <- mk(C)
    es <- evs[summer == s]
    for (k in seq_len(nrow(es))) {
      L <- es$label[k]; i <- match(L, labs); if (is.na(i)) next
      nf <- nn[i]; f <- F0[i, ]
      if (loo) { if (nf < 4) next; nf <- nf - 1
        Cs <- lapply(seq_along(blocks), function(bi) { v <- C[[bi]][i, ]; cat_ <- as.character(es[[blocks[bi]]][k]); if (!is.na(cat_) && cat_ %in% names(v)) v[cat_] <- v[cat_] - 1; v / sum(v) })
        f <- unlist(Cs); f <- f / sqrt(sum(f^2)) } else if (nf < 3) next
      ok <- nn >= max(4, 0.5 * nf) & nn <= 2 * nf; ok[i] <- FALSE
      if (sum(ok) < 3) next
      cand <- which(ok); sim <- as.numeric(F0[cand, , drop = FALSE] %*% f); top <- cand[order(-sim)[1:3]]
      res[[length(res) + 1]] <- data.table(event_id = es$event_id[k], t1 = labs[top[1]], t2 = labs[top[2]], t3 = labs[top[3]], summer = s)
    }
  }
  rbindlist(res)
}
presence <- function(tt, tag) {
  j <- merge(main[, .(event_id, candidate_club, summer)], tt, by = c("event_id", "summer"))
  h <- function(col) { z <- merge(j[, .(event_id, candidate_club, summer, tw = get(col))], cnt, by.x = c("summer", "candidate_club", "tw"), by.y = c("summer", "candidate_club", "label"), all.x = TRUE); z[, .(event_id, candidate_club, hit = !is.na(n))] }
  a <- h("t1"); b <- h("t2"); c3 <- h("t3")
  any3 <- merge(merge(a, b, by = c("event_id", "candidate_club")), c3, by = c("event_id", "candidate_club"))[, .(event_id, candidate_club, any3 = as.numeric(hit.x | hit.y | hit))]
  out <- merge(any3, a[, .(event_id, candidate_club, one = as.numeric(hit))], by = c("event_id", "candidate_club"))
  setnames(out, c("any3", "one"), paste0(c("any_", "one_"), tag)); out
}
B3 <- c("nat2", "pos", "ageband"); B4 <- c(B3, "vband")
variants <- list(orig = list(B3, FALSE), value = list(B4, FALSE), loo = list(B3, TRUE), loo_value = list(B4, TRUE))
tw <- list()
for (v in names(variants)) { cat("selecting", v, "\n"); t <- select_twins(variants[[v]][[1]], variants[[v]][[2]]); cat("  events with twins:", nrow(t), "\n"); tw[[v]] <- t; main <- merge(main, presence(t, v), by = c("event_id", "candidate_club"), all.x = TRUE, sort = FALSE) }
# balance: similarity of value distribution between focal and its twins (portfolio median log value gap)
for (v in names(tw)) { t <- merge(tw[[v]], evs[, .(event_id, label)], by = "event_id"); t <- merge(t, mv_med[, .(summer, label, m0 = med)], by = c("summer", "label"))
  g <- sapply(1:3, function(k) { z <- merge(t[, .(summer, lab = get(paste0("t", k)), m0)], mv_med[, .(summer, lab = label, med)], by = c("summer", "lab")); abs(z$med - z$m0) })
  cat(v, "median abs gap in portfolio median log value, twins 1-3 mean:", round(median(unlist(g), na.rm = TRUE), 3), "\n") }

base <- c("same", controls, flows)
vars <- c("orig", "value", "loo", "loo_value")
ok_all <- main[, .(ok = !anyNA(.SD)), by = event_id, .SDcols = paste0("any_", vars)][ok == TRUE, event_id]
cat("common-sample events (before restrictions):", length(ok_all), "\n")
cs <- main[event_id %in% ok_all]
fit_logit("C0 common sample: own only", cs, base, fx)
for (v in vars) for (k in c("any", "one")) {
  col <- paste0(k, "_", v); cs[[paste0("tw__", col)]] <- cs[[col]]
  fit_logit(paste0("C ", k, " ", v), cs, c("same", col, controls, flows), fx,
            setNames(list(c(same = 1), setNames(1, col), setNames(c(1, -1), c("same", col))), c("own", "matched", "own minus matched")))
}
# each variant on its own full sample
for (v in vars) { o <- main[!is.na(get(paste0("any_", v)))]
  for (k in c("any", "one")) { col <- paste0(k, "_", v)
    fit_logit(paste0("Cfull ", k, " ", v), o, c("same", col, controls, flows), fx, setNames(list(c(same = 1), setNames(1, col), setNames(c(1, -1), c("same", col))), c("own", "matched", "own minus matched"))) } }
save_records("matched_agencies_value_results.csv")
