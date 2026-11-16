# Own agency vs matched ("twin") agencies, recent vs long-standing co-clients, sample flow, cross-tab, closest twin,
# matching quality, coverage and worked example.   Usage: Rscript R/rev2_twin.R <data_root> [output_dir]
args <- commandArgs(TRUE); root <- args[1]; outdir <- if (length(args) > 1) args[2] else "."
script_dir <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE))
source(file.path(script_dir, "prepare.R")); source(file.path(script_dir, "enrich.R"))
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
d <- prepare(root, "extended"); en <- enrich(root, "extended")
main <- add_flags(d$main, en$ev); pl <- d$players[label != ""]
base <- c("same", controls, flows)
out <- function(x, f) fwrite(x, file.path(outdir, f))

# ---- (b) recently placed vs long-standing co-clients
arr <- en$tr[!is.na(player_id) & !is.na(to_club_id) & !is.na(transfer_date), .(player_id, club_id = to_club_id, transfer_date)]
pl[, lim := paste0(summer, "-06-01")]
a <- arr[pl[, .(summer, player_id, club_id, lim)], on = .(player_id, club_id), allow.cartesian = TRUE, nomatch = NA][is.na(transfer_date) | transfer_date < lim]
arrival <- a[, .(arrival = if (all(is.na(transfer_date))) NA_character_ else max(transfer_date, na.rm = TRUE)), by = .(summer, player_id, club_id)]
pl <- merge(pl, arrival, by = c("summer", "player_id", "club_id"), all.x = TRUE, sort = FALSE)
pl[, recent := !is.na(arrival) & arrival >= paste0(summer - 3, "-06-01")]
n_all <- pl[, .(n_all = .N), by = .(summer, candidate_club = club_id, label)]
n_rec <- pl[recent == TRUE, .(n_rec = .N), by = .(summer, candidate_club = club_id, label)]
main <- merge(main, n_all, by = c("summer", "candidate_club", "label"), all.x = TRUE, sort = FALSE)
main <- merge(main, n_rec, by = c("summer", "candidate_club", "label"), all.x = TRUE, sort = FALSE)
main[is.na(n_all), n_all := 0L]; main[is.na(n_rec), n_rec := 0L]
foc_rec <- unique(pl[, .(summer, player_id, candidate_club = club_id, foc_recent = recent)])
main <- merge(main, foc_rec, by = c("summer", "player_id", "candidate_club"), all.x = TRUE, sort = FALSE)
foc <- main$candidate_club == main$snapshot_club
main[, n_all := n_all - as.integer(foc)]; main[, n_rec := n_rec - as.integer(foc & foc_recent %in% TRUE)]
stopifnot(mean(main$n_all == main$co_clients) > 0.995)
main[, same_recent := as.integer(n_rec > 0)]; main[, same_old := as.integer(n_all > 0 & n_rec == 0)]

# ---- (a) twin agencies
pl[, pos := fifelse(is.na(position), "NA", position)]
pl[, ageband := as.character(cut(age_at_cutoff, c(0, 21, 25, 29, 99), labels = FALSE))]; pl[is.na(ageband), ageband := "NA"]
pl[, nat2 := fifelse(nation == "", "NA", nation)]
shares <- function(lab, v) { m <- unclass(table(lab, v)); m / rowSums(m) }
twins <- list(); eligible <- list(); blocks_by_summer <- list()
for (s in sort(unique(pl$summer))) {
  x <- pl[summer == s]
  B <- list(nationality = shares(x$label, x$nat2), position = shares(x$label, x$pos), `age band` = shares(x$label, x$ageband))
  blocks_by_summer[[as.character(s)]] <- B
  F <- do.call(cbind, B); F <- F / sqrt(rowSums(F^2)); labs <- rownames(F)
  nn <- as.integer(table(x$label)[labs]); S <- F %*% t(F); diag(S) <- -1
  for (i in seq_along(labs)) {
    if (nn[i] < 3) next
    ok <- nn >= max(4, 0.5 * nn[i]) & nn <= 2 * nn[i]; ok[i] <- FALSE
    if (sum(ok) < 3) next
    cand <- which(ok); top <- cand[order(-S[i, cand])[1:3]]
    twins[[paste(s, labs[i])]] <- list(summer = s, label = labs[i], t = labs[top]); eligible[[paste(s, labs[i])]] <- labs[cand]
  }
}
cat("focal labels with twins:", length(twins), "\n")
tt <- rbindlist(lapply(twins, function(z) data.table(summer = z$summer, label = z$label, t1 = z$t[1], t2 = z$t[2], t3 = z$t[3])))
cnt <- pl[, .(n = .N), by = .(summer, candidate_club = club_id, label)]
has <- function(col) { j <- merge(main[, .(event_id, candidate_club, summer, label)][tt, on = .(summer, label), nomatch = NULL][, tw := get(col)][, .(event_id, candidate_club, summer, tw)],
                                  cnt, by.x = c("summer", "candidate_club", "tw"), by.y = c("summer", "candidate_club", "label"), all.x = TRUE); j[, .(event_id, candidate_club, hit = !is.na(n))] }
tw_any <- Reduce(function(a, b) merge(a, b, by = c("event_id", "candidate_club"))[, .(event_id, candidate_club, hit = hit.x | hit.y)], list(has("t1"), has("t2"), has("t3")))
t1 <- has("t1")
main <- merge(main, tw_any[, .(event_id, candidate_club, twin_any = as.numeric(hit))], by = c("event_id", "candidate_club"), all.x = TRUE, sort = FALSE)
main <- merge(main, t1[, .(event_id, candidate_club, twin1 = as.numeric(hit))], by = c("event_id", "candidate_club"), all.x = TRUE, sort = FALSE)
ok_ev <- main[, .(ok = !anyNA(twin_any)), by = event_id][ok == TRUE, event_id]
tw <- main[event_id %in% ok_ev]
cat("twin sample events:", uniqueN(tw$event_id), "\n")
cm <- function(a) list("own agency" = c(same = 1), "twin agencies (placebo)" = c(twin_any = 1), "difference (own - twin)" = c(same = 1, twin_any = -1))
fit_logit("R2 twin sample: own agency only", tw, base, fx)
fit_logit("R2 twin sample: own + twin agencies", tw, c("same", "twin_any", controls, flows), fx, cm())
fit_logit("R2 twin sample: twin only", tw, c("twin_any", controls, flows), fx, list("twin agencies (placebo)" = c(twin_any = 1)))
fit_logit("R2 recent vs long-standing co-clients", main, c("same_recent", "same_old", controls, flows), fx,
          list("recently placed co-client" = c(same_recent = 1), "long-standing co-client" = c(same_old = 1), "difference (recent - old)" = c(same_recent = 1, same_old = -1)))
out(rbindlist(records$rows, fill = TRUE), "rev2_identification_results_R.csv")

# ---- twin placebo within contract-status groups
records$rows <- list()
twc <- contract_status(tw, root)
for (g in list(c("expiring contract", "expiring"), c("contract runs longer", "longer")))
  fit_logit(paste0("R2 twin placebo, ", g[1]), twc[exp_status == g[2]], c("same", "twin_any", controls, flows), fx, cm())
out(rbindlist(records$rows, fill = TRUE), "rev2b_twin_by_contract_results_R.csv")

# ---- sample flow, cross-tab, closest single twin
records$rows <- list()
b0 <- retained(main); nB <- uniqueN(b0$event_id)
nlab <- pl[, .(clients = .N), by = .(summer, label)]
ev1 <- unique(b0[, .(event_id, summer, label)]); ev1 <- merge(ev1, nlab, by = c("summer", "label"), all.x = TRUE); ev1[is.na(clients), clients := 0L]
ev1[, has_twin := paste(summer, label) %in% names(twins)]
no_t <- ev1[has_twin == FALSE]; r_small <- sum(no_t$clients < 3); r_elig <- sum(no_t$clients >= 3)
final <- retained(b0[event_id %in% ev1[has_twin == TRUE, event_id]]); nC <- sum(ev1$has_twin); nF <- uniqueN(final$event_id)
out(data.table(step = c("Events in the club model", "Representative has fewer than three clients on pre-window rosters, so no twin is defined", "Fewer than three agencies meet the size rule",
    "Dropped after twin restriction (single-alternative or never-chosen clubs)", "Events in the twin model"), events = c(nB, -r_small, -r_elig, -(nC - nF), nF)), "rev2c_sample_flow_R.csv")
fx2 <- copy(final)
fx2[, cat := fcase(same == 1 & twin_any == 0, "Own agency only", same == 0 & twin_any == 1, "Twin only", same == 1 & twin_any == 1, "Both", default = "Neither")]
ct <- fx2[, .(alternatives = .N, chosen = sum(chosen)), by = cat][match(c("Own agency only", "Twin only", "Both", "Neither"), cat)]
ct[, `:=`(share_of_alternatives = alternatives / sum(alternatives), chosen_share = chosen / alternatives)]
out(ct, "rev2c_crosstab_R.csv")
he <- fx2[, .(own = any(cat == "Own agency only"), twin = any(cat == "Twin only")), by = event_id]
out(data.table(events = nF, with_own_only = sum(he$own), with_twin_only = sum(he$twin), with_both_types = sum(he$own & he$twin)), "rev2c_choice_sets_R.csv")
fit_logit("R2c own + closest single twin", fx2, c("same", "twin1", controls, flows), fx,
          list("own agency" = c(same = 1), "closest twin (placebo)" = c(twin1 = 1), "difference (own - closest twin)" = c(same = 1, twin1 = -1)))
out(rbindlist(records$rows, fill = TRUE), "rev2c_closest_twin_results_R.csv")

# ---- matching quality, coverage, worked example
rows <- list()
for (z in twins) {
  B <- blocks_by_summer[[as.character(z$summer)]]; el <- eligible[[paste(z$summer, z$label)]]
  for (k in names(B)) { Fm <- B[[k]]; Fm <- Fm / sqrt(rowSums(Fm^2)); cs <- function(o) mean(as.numeric(Fm[o, , drop = FALSE] %*% Fm[z$label, ]))
    rows[[length(rows) + 1]] <- data.table(block = k, twin = cs(z$t), eligible_pool = cs(el)) }
}
out(rbindlist(rows)[, .(twin = round(mean(twin), 3), eligible_pool = round(mean(eligible_pool), 3)), by = block][order(block)], "rev2_matching_quality_R.csv")
sz <- rbindlist(lapply(twins, function(z) { n <- pl[summer == z$summer, .N, by = label]; nf <- n[label == z$label, N]; nt <- n[match(z$t, label), N]
  data.table(summer = z$summer, focal_clients = nf, mean_twin_clients = mean(nt), min_ratio = min(nt) / nf, max_ratio = max(nt) / nf) }))
cov <- sz[, .(`focal labels with twins` = .N, `median twin/focal size` = round(median(mean_twin_clients / focal_clients), 2)), by = summer]
cov <- merge(cov, pl[, .N, by = .(summer, label)][, .(`labels with at least 3 clients` = sum(N >= 3)), by = summer], by = "summer")
out(cov, "rev2_matching_coverage_R.csv")
x24 <- pl[summer == 2024]; n24 <- x24[, .N, by = label]
cands <- sort(vapply(twins[vapply(twins, function(z) z$summer == 2024, NA)], function(z) z$label, ""))
cands <- cands[n24[match(cands, label), N] >= 18 & n24[match(cands, label), N] <= 26]
szc <- n24[match(cands, label), N]; focl <- cands[order(abs(szc - median(szc)))[1]]
prof <- function(l) { x <- x24[label == l]; pc <- function(v, k) sprintf("%.0f%%", 100 * mean(v == k))
  u <- unique(x$nat2); ct <- as.integer(table(factor(x$nat2, levels = u))); top <- order(-ct, seq_along(u))[seq_len(min(3, length(u)))]
  nt <- setNames(ct[top], u[top])
  data.table(clients = nrow(x), top_nationalities = paste0(names(nt), " ", sprintf("%.0f%%", 100 * as.numeric(nt) / nrow(x)), collapse = ", "), forwards = pc(x$pos, "Attack"), midfield = pc(x$pos, "Midfield"),
             defenders = pc(x$pos, "Defender"), goalkeepers = pc(x$pos, "Goalkeeper"), age_21_or_under = pc(x$ageband, "1"), age_22_25 = pc(x$ageband, "2"), age_26_29 = pc(x$ageband, "3"), age_30_plus = pc(x$ageband, "4")) }
ex <- rbindlist(c(list(cbind(agency = "Focal agency", prof(focl))), lapply(seq_along(twins[[paste(2024, focl)]]$t), function(k) cbind(agency = paste("Twin", k), prof(twins[[paste(2024, focl)]]$t[k])))))
out(ex, "rev2_worked_example_R.csv")
