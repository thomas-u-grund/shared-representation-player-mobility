# Conditional logit models used throughout the paper.
# fit_logit() estimates an event-stratified (McFadden) conditional logit with survival::clogit. An event is one transfer and its
# alternatives are the candidate destination clubs. Events need exactly one chosen alternative and at least two alternatives;
# destination club fixed effects are entered as dummies, levels that are never chosen are pruned, and columns that do not vary
# within events are dropped. Standard errors are cluster-robust (sandwich) by the mover's representative label (the reported
# intervals) and by player, with the G/(G-1) small-sample factor. Each call appends one row per reported term to records$rows.
suppressPackageStartupMessages({library(data.table); library(survival)})

key <- function(s) {                      # normalise labels and nationalities: ASCII letters and digits only, lower case
  s <- tolower(as.character(s)); s[is.na(s)] <- ""
  gsub("[^a-z0-9]", "", stringi::stri_trans_nfkd(s))
}

valid_events <- function(x) {
  f <- x[, .(n = .N, k = sum(chosen)), by = event_id]
  x[event_id %in% f[k == 1 & n >= 2, event_id]]
}

records <- new.env(); records$rows <- list(); records$summaries <- list()

fit_logit <- function(name, x, cols, fixed = NULL, metrics = NULL, quiet = FALSE) {
  x <- valid_events(as.data.table(x))
  if (!is.null(fixed)) repeat {           # drop never-chosen levels of the fixed effect, then re-validate
    n <- nrow(x)
    x <- valid_events(x[get(fixed) %in% x[chosen == 1, get(fixed)]])
    if (nrow(x) == n) break
  }
  setorder(x, event_id)
  # drop columns that do not vary within events (absorbed by stratification)
  varies <- vapply(cols, function(cn) x[, any(abs(get(cn) - get(cn)[1]) > 1e-9), by = event_id][, any(V1)], logical(1))
  cols <- cols[varies]
  rhs <- cols
  if (!is.null(fixed)) { x[, fe__ := factor(get(fixed))]; rhs <- c(rhs, "fe__") }
  f <- as.formula(paste("chosen ~", paste(rhs, collapse = " + "), "+ strata(event_id)"))
  fit <- suppressWarnings(clogit(f, data = as.data.frame(x), method = "efron", control = coxph.control(iter.max = 100, eps = 1e-10)))
  b <- coef(fit); use <- names(b)[!is.na(b)]
  keepc <- intersect(cols, use)
  D <- residuals(fit, type = "dfbeta")      # one row per observation
  D <- D[, !is.na(b), drop = FALSE]; colnames(D) <- names(b)[!is.na(b)]
  vc <- list()
  for (cl in c("player_id", "label")) {
    A <- rowsum(D, as.character(x[[cl]]))
    G <- nrow(A); vc[[cl]] <- crossprod(A) * G / (G - 1)
  }
  targets <- list()
  for (cn in keepc) if (cn %in% c("same","co_client_count_log","same_between","same_2024","same_2025","agency_log","local_nationality",
                                  "forward_3","reverse_3","same_prior_flow","same_turn_of_july","agency_share","twin_any","twin1")) targets[[cn]] <- setNames(1, cn)
  if (!is.null(metrics)) for (m in names(metrics)) targets[[m]] <- metrics[[m]]
  for (term in names(targets)) {
    parts <- targets[[term]]; w <- setNames(numeric(length(use)), use); w[names(parts)] <- parts
    bhat <- sum(w * b[use]); rec <- list(model = name, term = term, events = uniqueN(x$event_id), alternatives = nrow(x),
                                          coefficient = bhat, odds_ratio = exp(bhat))
    for (cl in names(vc)) {
      se <- sqrt(drop(t(w) %*% vc[[cl]] %*% w))
      rec[[paste0(cl, "_se")]] <- se; rec[[paste0(cl, "_lo")]] <- exp(bhat - 1.96 * se)
      rec[[paste0(cl, "_hi")]] <- exp(bhat + 1.96 * se); rec[[paste0(cl, "_p")]] <- 2 * pnorm(-abs(bhat / se))
    }
    records$rows[[length(records$rows) + 1]] <- as.data.frame(rec)
  }
  chose <- x[chosen == 1]
  s <- list(events = uniqueN(x$event_id), alternatives = nrow(x), players = uniqueN(chose$player_id), labels = uniqueN(chose$label))
  if ("same" %in% names(x)) {
    exposed <- x[, .(e = any(same == 1)), by = event_id]
    s$exposed_events <- sum(exposed$e); s$chosen_exposed <- sum(chose$same); s$candidate_share <- mean(x$same)
    if ("same" %in% keepc) {
      lp <- as.numeric(predict(fit, type = "lp")); soft <- function(v) { e <- exp(v - max(v)); e / sum(e) }
      x[, lp__ := lp]; x[, p__ := soft(lp__), by = event_id]
      lp0 <- lp; for (cn in intersect(c("same","same_between","same_2024","same_2025","same_prior_flow","same_turn_of_july"), keepc)) lp0 <- lp0 - x[[cn]] * b[[cn]]
      x[, lp0__ := lp0]; x[, p0__ := soft(lp0__), by = event_id]
      s$predicted_exposed_given_eligible <- sum(x$p__ * x$same) / sum(exposed$e)
      s$equalised_exposed_given_eligible <- sum(x$p0__ * x$same) / sum(exposed$e)
    }
  }
  records$summaries[[name]] <- s
  if (!quiet) cat(name, "events", s$events, "alternatives", s$alternatives, "\n")
  invisible(fit)
}
