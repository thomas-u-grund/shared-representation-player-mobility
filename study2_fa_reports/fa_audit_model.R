# Audit and extensions of the Study 2 analysis (see PRESPEC.md, section "Audit and extensions").
# Usage: Rscript study2_fa_reports/fa_audit_model.R [output_dir]     Needs data.table, survival, stringi.
# Writes fa_audit_results.csv and fa_audit_counts.csv to output_dir (default: this folder). Includes the renewal (three-exposure) model of the paper.
suppressPackageStartupMessages({ library(data.table); library(survival); library(stringi) })
here <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)); outdir <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else here
src <- readLines(file.path(here, "fa_choice_model.R")); eval(parse(text = src[1:(grep("^build <- function", src)[1] - 1)]))   # data preparation shared with fa_choice_model.R
D[, rt := tolower(reg_type)]
D[, ren := grepl("^updated (registration|contract)$|^new contract$|^contract amendment$", rt)]
PLEFL <- c("Premier League", "EFL Championship", "EFL League One", "EFL League Two")
H2 <- unique(D[, .(ord, period, club_k, player_k, id = agent_id, nm1, nm2, club_side = side_registering_club, club_only = side_registering_club & !side_player, player_side = side_player, rt, ren)])
cols_h <- function(x, key, ktype, has_id) x[, .(ord, club_k, player_k, club_side, club_only, player_side, rt, ren, key = key, ktype = ktype, has_id = has_id)]
Hk2 <- rbind(cols_h(H2[!is.na(id)], H2[!is.na(id)]$id, "id", TRUE), cols_h(H2, H2$nm1, "name", !is.na(H2$id)),
             cols_h(H2[(!is.na(nm2) & nm2 != nm1) | (is.na(nm1) & !is.na(nm2))], H2[(!is.na(nm2) & nm2 != nm1) | (is.na(nm1) & !is.na(nm2))]$nm2, "name", !is.na(H2[(!is.na(nm2) & nm2 != nm1) | (is.na(nm1) & !is.na(nm2))]$id)))
Hk2 <- Hk2[!is.na(key)]

build2 <- function(periods, rule = "id", choice = "report", signing = "pair", hist_from = 1, only_plefl = FALSE) {
  out <- list()
  for (p in periods) {
    po <- match(p, PER)
    ev0 <- D[period == p & strict & side_player, .(club_k, former_k, player_k, registering_club, agent_id, nm1, nm2, both_sides = side_registering_club)]
    ev0[, rn := .I]; ev0 <- unique(ev0, by = c("club_k", "player_k", "agent_id", "nm1"))
    ev0[, agent_key := fifelse(!is.na(agent_id), agent_id, fifelse(!is.na(nm1), nm1, nm2))]; ev0 <- ev0[!is.na(agent_key)]
    ev0[, `:=`(sid = paste(p, club_k, player_k, sep = "|"))]; ev0[, n_agents := .N, by = sid]
    ev0[, pair_id := paste(sid, agent_key, sep = "|")]
    if (signing == "pair") { ev <- copy(ev0); ev[, event_id := pair_id]; evk <- ev }
    else { setorder(ev0, rn); ev <- ev0[!duplicated(sid)]; ev[, event_id := sid]; ev0[, event_id := sid]; evk <- if (signing == "any") ev0 else ev
           if (signing == "any") ev[, both_sides := ev0[, any(both_sides), by = sid][match(ev$sid, sid), V1]] }
    cl <- switch(choice, report = unique(D[period == p, .(club_k)]), incoming = unique(D[period == p & !grepl("\\(out\\)|out$|cancel", rt), .(club_k)]),
                 members = unique(CD[period == p & division %in% PLEFL & club_k != "", .(club_k)]))
    dv <- CD[period == p, .(club_k, division)]; dv <- dv[!duplicated(club_k)]
    cl <- merge(cl, dv, by = "club_k")
    if (only_plefl || choice == "members") cl <- cl[division %in% PLEFL]
    ev <- merge(ev, cl[, .(club_k, division)], by = "club_k")
    sets <- merge(ev[, .(event_id, division, club_k_chosen = club_k, former_k, player_k)], cl, by = "division", allow.cartesian = TRUE, suffixes = c("", "_c"))
    setnames(sets, "club_k", "club_c"); sets <- sets[is.na(former_k) | former_k == "" | club_c == club_k_chosen | club_c != former_k]
    sets[, chosen := as.integer(club_c == club_k_chosen)]
    ek <- unique(rbind(evk[!is.na(agent_id), .(event_id, key = agent_id, ktype = "id")], evk[!is.na(nm1), .(event_id, key = nm1, ktype = "name")], evk[!is.na(nm2), .(event_id, key = nm2, ktype = "name")]))
    ek <- ek[event_id %in% ev$event_id]
    hh <- Hk2[ord < po & ord >= hist_from]
    mt <- switch(rule, id = rbind(merge(ek[ktype == "id"], hh[ktype == "id"], by = "key", allow.cartesian = TRUE), merge(ek[ktype == "name"], hh[ktype == "name" & !has_id], by = "key", allow.cartesian = TRUE)),
                 idstrict = merge(ek[ktype == "id"], hh[ktype == "id"], by = "key", allow.cartesian = TRUE), name = merge(ek[ktype == "name"], hh[ktype == "name"], by = "key", allow.cartesian = TRUE))
    mt <- merge(mt, unique(sets[, .(event_id, player_k)]), by = "event_id")[player_k.x != player_k.y]
    ex <- mt[, .(any_prev = any(ord == po - 1), any_old = any(ord <= po - 2), n_prior = uniqueN(paste(ord, player_k.x)), club_side = any(club_side), club_only = any(club_only),
                 club_renew = any(club_side & ren), club_other = any(club_side & !ren & rt != ""), via_id = any(ktype.x == "id")), by = .(event_id, club_c = club_k)]
    sets <- merge(sets, ex, by = c("event_id", "club_c"), all.x = TRUE)
    for (cn in c("club_side", "club_only", "club_renew", "club_other", "via_id")) set(sets, which(is.na(sets[[cn]])), cn, FALSE)
    sets[is.na(n_prior), n_prior := 0L]
    sets[, `:=`(any_prior = as.numeric(n_prior > 0), club_side = as.numeric(club_side), club_only = as.numeric(club_only), player_only = as.numeric(n_prior > 0 & !club_side),
                club_renew = as.numeric(club_renew), club_other = as.numeric(club_other), name_only_link = as.numeric(n_prior > 0 & !via_id), period = p)]
    sets <- merge(sets, ev[, .(event_id, agent_key, both_sides, n_agents)], by = "event_id")
    out[[p]] <- sets
  }
  rbindlist(out)
}

term_info <- function(S, cn) { i <- S[, .(inf = any(get(cn) > 0) & any(get(cn) == 0), ce = sum(get(cn) * chosen) > 0), by = event_id]; c(informative_sets = sum(i$inf), chosen_exposed = sum(i$ce)) }
fit2 <- function(S, cols, fe = "none", cluster = "agent_key", item, run, spec) {
  S <- copy(S); setnames(S, "club_c", "club"); if (fe == "clubperiod") S[, club := paste(club, period)]
  ok <- S[, .(n = .N, k = sum(chosen)), by = event_id]; S <- S[event_id %in% ok[k == 1 & n >= 2, event_id]]
  if (fe != "none") repeat { n <- nrow(S); S <- S[club %in% S[chosen == 1, club]]; ok <- S[, .(n = .N, k = sum(chosen)), by = event_id]; S <- S[event_id %in% ok[k == 1 & n >= 2, event_id]]; if (nrow(S) == n) break }
  vr <- vapply(cols, function(cn) S[, any(abs(get(cn) - get(cn)[1]) > 1e-9), by = event_id][, any(V1)], logical(1)); keep <- cols[vr]
  if (!length(keep) || nrow(S) == 0) return(data.table(item = item, run = run, spec = spec, fe = fe, cluster = cluster, term = cols, events = uniqueN(S$event_id), note = "no variation"))
  S[, fe__ := factor(club)]; S[, cl__ := get(if (cluster == "club") "club_k_chosen" else "agent_key")]
  f <- as.formula(paste("chosen ~", paste(c(keep, if (fe != "none") "fe__"), collapse = " + "), "+ strata(event_id) + cluster(cl__)"))
  m <- tryCatch(suppressWarnings(clogit(f, data = as.data.frame(S), method = "efron", control = coxph.control(iter.max = 100, eps = 1e-10))), error = function(e) NULL)
  if (is.null(m)) return(data.table(item = item, run = run, spec = spec, fe = fe, cluster = cluster, term = keep, events = uniqueN(S$event_id), note = "model failed"))
  cf <- summary(m)$coefficients; cf <- cf[rownames(cf) %in% keep, , drop = FALSE]
  ti <- t(vapply(rownames(cf), function(cn) term_info(S, cn), c(0, 0)))
  data.table(item = item, run = run, spec = spec, fe = fe, cluster = cluster, term = rownames(cf), events = uniqueN(S$event_id), alternatives = nrow(S), clusters = uniqueN(S$cl__),
             informative_sets = ti[, 1], chosen_exposed = ti[, 2], odds_ratio = exp(cf[, "coef"]), lo = exp(cf[, "coef"] - 1.96 * cf[, "robust se"]), hi = exp(cf[, "coef"] + 1.96 * cf[, "robust se"]), p = cf[, "Pr(>|z|)"], note = "")
}
RES <- list(); CNT <- list()
add <- function(...) { z <- fit2(...); RES[[length(RES) + 1]] <<- z; invisible(z) }
SPA <- c("any_prior"); SPB <- c("club_side", "player_only")
runspec <- function(S, item, run, fes = c("none", "club"), cluster = "agent_key", specs = list(A = SPA, B = SPB)) for (sp in names(specs)) for (fe in fes) add(S, specs[[sp]], fe, cluster, item, run, sp)

# ---- 1a link audit: share of exposed (event, club) pairs whose link is name-only, by cohort
S1 <- build2(PER[5:8], "id")
CNT[[1]] <- S1[any_prior == 1, .(item = "1a", measure = "exposed event-club pairs", n = .N, name_only_links = sum(name_only_link), share_name_only = mean(name_only_link), club_side_pairs = sum(club_side), club_side_name_only = sum(club_side * name_only_link)), by = period]
cat("\n1a link audit\n"); print(CNT[[1]])
ev_c <- unique(S1[chosen == 1, .(event_id, period, club_side_chosen = club_side, any_chosen = any_prior)])
# ---- 1c signings by number of player-side intermediaries
sg <- unique(D[period %in% PER[5:8] & strict & side_player, .(period, club_k, player_k, agent_key = fifelse(!is.na(agent_id), agent_id, nm1))][!is.na(agent_key)])[, .(n = .N), by = .(period, club_k, player_k)]
CNT[[2]] <- sg[, .(item = "1c", signings = .N, with1 = sum(n == 1), with2 = sum(n == 2), with3plus = sum(n >= 3), signing_intermediary_pairs = sum(n)), by = period]
cat("\n1c signings by number of player-side intermediaries\n"); print(CNT[[2]])
# ---- 1b
runspec(S1, "1b", "primary 2019-20..2022-23 (ID where available, else name)")
S1b <- build2(PER[6:8], "id"); runspec(S1b, "1b", "primary without 2019-20 cohort (2020-21..2022-23)")
S1c <- build2(PER[5:8], "idstrict", hist_from = 5); runspec(S1c, "1b", "ID at both ends (history from ID periods only; 2019-20 has no history)")
S1d <- build2(PER[6:8], "idstrict", hist_from = 5); runspec(S1d, "1b", "ID at both ends, without 2019-20 cohort")
# ---- 1c checks
for (sg_ in c("pair", "first", "any")) { S <- build2(PER[5:8], "id", signing = sg_); for (cl in c("agent_key", "club")) runspec(S, "1c", paste("one event per", c(pair = "signing-intermediary pair", first = "signing (first-listed intermediary)", any = "signing (exposure if any intermediary)")[sg_]), cluster = cl, specs = list(A = SPA)) }
# ---- 4 not also on the club side
runspec(S1[both_sides == FALSE], "4", "primary, intermediary not also on the club side of the focal deal", specs = list(A = SPA))
# ---- 2 choice sets
ids_ok <- function(S) unique(S$event_id)
Pm <- PER[c(5, 7, 8)]
Sm <- build2(Pm, "id", "members"); Sr <- build2(Pm, "id", "report", only_plefl = TRUE); Si <- build2(Pm, "id", "incoming", only_plefl = TRUE)
common <- Reduce(intersect, list(ids_ok(Sm), ids_ok(Sr), ids_ok(Si)))
CNT[[3]] <- data.table(item = "2", measure = c("events in membership run", "events in report run", "events in incoming run", "events in all three"), n = c(length(ids_ok(Sm)), length(ids_ok(Sr)), length(ids_ok(Si)), length(common)))
CNT[[4]] <- rbind(Sm[, .(set = "members", alternatives_per_event = .N / uniqueN(event_id))], Sr[, .(set = "report", alternatives_per_event = .N / uniqueN(event_id))], Si[, .(set = "incoming", alternatives_per_event = .N / uniqueN(event_id))])
for (nm in c("members", "report", "incoming")) { S <- get(c(members = "Sm", report = "Sr", incoming = "Si")[nm])[event_id %in% common]; runspec(S, "2", paste("same signings, PL/EFL 2019-20, 2021-22, 2022-23: alternatives =", nm), specs = list(A = SPA)) }
Sr4 <- build2(PER[5:8], "id", "report"); Si4 <- build2(PER[5:8], "id", "incoming")
runspec(Si4, "2", "primary periods, alternatives = clubs with an incoming row (drops foreign clubs in 2020-21)", specs = list(A = SPA))
runspec(Sr4, "2", "primary periods, club-by-period fixed effects", fes = "clubperiod", specs = list(A = SPA))
# ---- 3 renewals
cat("\nrenewal counts\n")
RC <- D[ord >= 5 & !is.na(agent_id), .(rows = .N, renewal_rows = sum(ren & side_registering_club), clubside_renewal_rows = sum(ren & side_registering_club)), by = period]
RCn <- D[ord >= 5, .(registration_rows = .N, renewal_rows = sum(ren), renewal_rows_club_side = sum(ren & side_registering_club), untyped_rows = sum(rt == "")), by = period]
CNT[[5]] <- cbind(item = "3", RCn); print(RCn)
S3a <- build2(PER[6:8], "idstrict", hist_from = 5); S3b <- build2(PER[6:11], "name", hist_from = 5)
for (nm in c("S3a", "S3b")) { S <- get(nm); lab <- c(S3a = "ID-matched 2020-21..2022-23, history from 2019-20", S3b = "name-matched 2020-21..2025-26, history from 2019-20")[nm]
  cat(lab, ": events", uniqueN(S$event_id), "; exposed event-club pairs: club-side renewal", sum(S$club_renew), ", club-side other", sum(S$club_other), ", player-side only", sum(S$player_only), "\n")
  CNT[[length(CNT) + 1]] <- data.table(item = "3", measure = lab, n = uniqueN(S$event_id), pairs_club_renewal = sum(S$club_renew), pairs_club_other = sum(S$club_other), pairs_player_only = sum(S$player_only),
                                      chosen_club_renewal = sum(S$club_renew * S$chosen), chosen_club_other = sum(S$club_other * S$chosen), chosen_player_only = sum(S$player_only * S$chosen))
  runspec(S, "3", lab, specs = list(A = SPA, R = c("club_renew", "club_other", "player_only")))
}
R <- rbindlist(RES, fill = TRUE); R[, `:=`(odds_ratio = round(odds_ratio, 3), lo = round(lo, 3), hi = round(hi, 3), p = signif(p, 3))]
fwrite(R, file.path(outdir, "fa_audit_results.csv")); fwrite(rbindlist(CNT, fill = TRUE), file.path(outdir, "fa_audit_counts.csv"))
options(width = 250); print(R[, .(item, run = substr(run, 1, 70), spec, fe, cluster, term, events, informative_sets, chosen_exposed, or = odds_ratio, lo, hi, note)], nrows = 300)
