# Study 2 extensions to balance Study 1 (see rebuild_from_raw/study2/audit/PRESPEC.md, section "Extensions to balance Study 2 with Study 1").
# Usage: Rscript code/study2/fa_balance_model.R [output_dir]     Needs data.table, survival, stringi. Writes fa_balance_results.csv and fa_balance_counts.csv to output_dir.
suppressPackageStartupMessages({ library(data.table); library(survival); library(stringi) })
here <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)); outdir <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "."
src <- readLines(file.path(here, "fa_audit_model.R")); eval(parse(text = src[1:(grep("^term_info <- function", src)[1] - 1)]))   # data preparation, Hk2 and build2
src2 <- readLines(file.path(here, "fa_audit_model.R")); eval(parse(text = src2[grep("^term_info <- function", src2)[1]:(grep("^fit2 <- function", src2)[1] - 1)]))

prep <- function(S, fe = "club") {
  S <- copy(S); setnames(S, "club_c", "club")
  ok <- S[, .(n = .N, k = sum(chosen)), by = event_id]; S <- S[event_id %in% ok[k == 1 & n >= 2, event_id]]
  if (fe != "none") repeat { n <- nrow(S); S <- S[club %in% S[chosen == 1, club]]; ok <- S[, .(n = .N, k = sum(chosen)), by = event_id]; S <- S[event_id %in% ok[k == 1 & n >= 2, event_id]]; if (nrow(S) == n) break }
  S[, fe__ := factor(club)]; S[, cl__ := agent_key]; S
}
fitm <- function(S, cols, fe = "club") {
  S <- prep(S, fe)
  vr <- vapply(cols, function(cn) S[, any(abs(get(cn) - get(cn)[1]) > 1e-9), by = event_id][, any(V1)], logical(1)); keep <- cols[vr]
  f <- as.formula(paste("chosen ~", paste(c(keep, if (fe != "none") "fe__"), collapse = " + "), "+ strata(event_id) + cluster(cl__)"))
  m <- suppressWarnings(clogit(f, data = as.data.frame(S), method = "efron", control = coxph.control(iter.max = 100, eps = 1e-10)))
  list(m = m, S = S, keep = keep)
}
row_of <- function(item, run, term, fit, i = term) {
  cf <- summary(fit$m)$coefficients; b <- cf[i, "coef"]; se <- cf[i, "robust se"]
  data.table(item = item, run = run, term = term, events = uniqueN(fit$S$event_id), clusters = uniqueN(fit$S$cl__), odds_ratio = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se), p = cf[i, "Pr(>|z|)"])
}
RES <- list(); CNT <- list(); add <- function(z) { RES[[length(RES) + 1]] <<- z; invisible(z) }

# ---------------- B1 matched intermediaries
Hid <- Hk2[ktype == "id"]; Hid[, period := PER[ord]]
dv <- unique(CD[, .(period, club_k, division)], by = c("period", "club_k")); Hid <- merge(Hid, dv, by = c("period", "club_k"), all.x = TRUE); Hid[is.na(division) | division == "", division := "other"]
divs <- sort(unique(Hid$division))
SB <- build2(PER[6:8], "idstrict", hist_from = 5)   # 2020-21..2022-23, ID at both ends
add_match <- function(S, k = 3) {
  out <- list(); info <- list()
  for (p in unique(S$period)) {
    po <- match(p, PER); Sp <- S[period == p]; h <- Hid[ord < po & ord >= 5]
    prof <- h[, c(list(ncl = uniqueN(club_k), cs = mean(club_side)), setNames(lapply(divs, function(d) mean(division == d)), divs)), by = key][ncl >= 3]
    X <- as.matrix(prof[, c("cs", divs), with = FALSE]); X <- X / sqrt(rowSums(X^2)); X[is.nan(X)] <- 0
    fk <- unique(Sp$agent_key); fk_in <- intersect(fk, prof$key)
    mt <- rbindlist(lapply(fk_in, function(f) { i <- match(f, prof$key); cand <- which(prof$key != f & abs(log2(prof$ncl / prof$ncl[i])) <= 1)
      if (length(cand) < k) return(NULL); s <- as.numeric(X[cand, , drop = FALSE] %*% X[i, ]); o <- order(-s)[1:k]
      data.table(agent_key = f, mkey = prof$key[cand[o]], rank = 1:k, sim = s[o]) }))
    info[[p]] <- data.table(period = p, events = uniqueN(Sp$event_id), focal_intermediaries = length(fk), with_3clubs = length(fk_in), matched = uniqueN(mt$agent_key),
                            events_matched = uniqueN(Sp[agent_key %in% mt$agent_key, event_id]), mean_sim = mean(mt$sim), mean_sim_closest = mean(mt[rank == 1, sim]))
    ex <- merge(mt, h[, .(mkey = key, club_c = club_k, mplayer = player_k)], by = "mkey", allow.cartesian = TRUE)
    ex <- merge(ex, unique(Sp[, .(event_id, agent_key, player_k)]), by = "agent_key", allow.cartesian = TRUE)[mplayer != player_k]
    e3 <- unique(ex[, .(event_id, club_c, m3 = 1)]); e1 <- unique(ex[rank == 1, .(event_id, club_c, m1 = 1)])
    Sp <- Sp[agent_key %in% mt$agent_key]; Sp <- merge(Sp, e3, by = c("event_id", "club_c"), all.x = TRUE); Sp <- merge(Sp, e1, by = c("event_id", "club_c"), all.x = TRUE)
    Sp[is.na(m3), m3 := 0]; Sp[is.na(m1), m1 := 0]; out[[p]] <- Sp
  }
  list(S = rbindlist(out), info = rbindlist(info))
}
M <- add_match(SB); CNT[[1]] <- cbind(item = "B1", M$info); print(M$info)
SM <- M$S
cat("\nB1: alternatives with matched exposure:", mean(SM$m3), " own:", mean(SM$any_prior), "\n")
f0 <- fitm(SM, "any_prior"); add(row_of("B1", "own indicator alone, matched sample", "any_prior", f0))
f3 <- fitm(SM, c("any_prior", "m3")); f1 <- fitm(SM, c("any_prior", "m1"))
for (nm in c("3", "1")) { f <- get(paste0("f", nm)); lab <- c("3" = "own + any of three matched intermediaries", "1" = "own + closest matched intermediary")[nm]
  add(row_of("B1", lab, "own", f, "any_prior")); add(row_of("B1", lab, "matched", f, paste0("m", nm)))
  b <- coef(f$m); V <- f$m$var; ii <- match(c("any_prior", paste0("m", nm)), names(b)); d <- b[ii[1]] - b[ii[2]]; se <- sqrt(V[ii[1], ii[1]] + V[ii[2], ii[2]] - 2 * V[ii[1], ii[2]])
  add(data.table(item = "B1", run = lab, term = "ratio own/matched", events = uniqueN(f$S$event_id), clusters = uniqueN(f$S$cl__), odds_ratio = exp(d), lo = exp(d - 1.96 * se), hi = exp(d + 1.96 * se), p = 2 * pnorm(-abs(d / se)))) }

# ---------------- B2 leave out largest intermediaries
S1 <- build2(PER[5:8], "id")
tp <- unique(S1[chosen == 1, .(event_id, agent_key)])[, .N, by = agent_key][order(-N)][1:10]; print(tp)
CNT[[2]] <- cbind(item = "B2", tp)
add(row_of("B2", "all intermediaries (primary)", "any_prior", fitm(S1, "any_prior")))
for (a in tp$agent_key) add(row_of("B2", paste("dropping", a), "any_prior", fitm(S1[agent_key != a], "any_prior")))
add(row_of("B2", "dropping all ten", "any_prior", fitm(S1[!agent_key %in% tp$agent_key], "any_prior")))

# ---------------- B3 heterogeneity and fitted probability
S1[, grp := fifelse(division == "Premier League", "PL", fifelse(grepl("^EFL", division), "EFL", "NLS"))]
for (g in c("PL", "EFL", "NLS")) S1[, (paste0("any_", g)) := any_prior * (grp == g)]
fh <- fitm(S1, c("any_PL", "any_EFL", "any_NLS"))
for (g in c("PL", "EFL", "NLS")) if (paste0("any_", g) %in% fh$keep) add(row_of("B3", "by division, primary", paste0("any_prior in ", g), fh, paste0("any_", g)))
nn <- fh$S[, .(events = uniqueN(event_id)), by = grp]; print(nn)
kk <- intersect(c("any_PL", "any_EFL", "any_NLS"), fh$keep); b <- coef(fh$m)[kk]; V <- fh$m$var; dimnames(V) <- list(names(coef(fh$m)), names(coef(fh$m))); V <- V[kk, kk]
C <- cbind(diag(length(kk) - 1), -1); Wd <- as.numeric(t(C %*% b) %*% solve(C %*% V %*% t(C)) %*% (C %*% b)); pw <- pchisq(Wd, length(kk) - 1, lower.tail = FALSE)
add(data.table(item = "B3", run = "by division, primary", term = "Wald test of equality across divisions", events = uniqueN(fh$S$event_id), odds_ratio = NA, lo = NA, hi = NA, p = pw, clusters = Wd))
fp <- fitm(S1, "any_prior"); S <- fp$S; bc <- coef(fp$m)["any_prior"]; S[, lp := as.numeric(predict(fp$m, type = "lp"))]; S[, lp0 := lp - bc * any_prior]
S[, `:=`(p1 = exp(lp) / sum(exp(lp)), p0 = exp(lp0) / sum(exp(lp0))), by = event_id]
ev <- S[, .(pe = sum(p1 * any_prior), pe0 = sum(p0 * any_prior), nexp = sum(any_prior)), by = event_id][nexp > 0]
CNT[[3]] <- data.table(item = "B3", measure = "mean fitted probability of choosing an exposed club (events with an exposed alternative)", events = nrow(ev), with_coefficient = mean(ev$pe), coefficient_zero = mean(ev$pe0), chosen_exposed_share = S[event_id %in% ev$event_id & chosen == 1, mean(any_prior)])
print(CNT[[3]])
R <- rbindlist(RES, fill = TRUE); R[, `:=`(odds_ratio = round(odds_ratio, 6), lo = round(lo, 6), hi = round(hi, 6), p = signif(p, 3))]
fwrite(R, file.path(outdir, "fa_balance_results.csv")); fwrite(rbindlist(CNT, fill = TRUE), file.path(outdir, "fa_balance_counts.csv"))
options(width = 250); print(R)
