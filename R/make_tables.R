# Collect the revision estimates into revision_results.csv and draw Figure 1 (compact) and Figure A1 (specification ladder).
# Usage: Rscript R/make_tables.R <results_dir>   (the directory holding the *_R.csv files written by the analysis scripts; outputs go there too)
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
dir <- commandArgs(TRUE)[1]
rf <- function(f) fread(file.path(dir, sub("\\.csv$", "_R.csv", f)))
get <- function(f, model, term) { d <- rf(f); r <- d[model == m & term == t_, env = list(m = model, t_ = term)]; stopifnot(nrow(r) == 1); c(r$events, r$odds_ratio, r$label_lo, r$label_hi) }
get <- function(f, mod, trm) { d <- rf(f); r <- d[d$model == mod & d$term == trm]; stopifnot(nrow(r) == 1); c(r$events, r$odds_ratio, r$label_lo, r$label_hi) }
rows <- list()
add <- function(block, label, v, src = "") rows[[length(rows) + 1]] <<- data.table(block = block, label = label, events = as.integer(v[1]), odds_ratio = v[2], lo = v[3], hi = v[4], source = src)
f <- "rev6_misc_results.csv"; M <- "R6 by contract status (main sample)"
add("mechanism", "Contract ends by 30 June of the window year", get(f, M, "contract expiring by 30 June"), f)
add("mechanism", "Contract runs one more year", get(f, M, "contract runs one more year"), f)
add("mechanism", "Contract runs longer", get(f, M, "contract runs longer"), f)
add("mechanism", "Ratio: expiring contract to longer contract", get(f, "R6 expiring vs longer contract (main sample)", "difference, expiring vs longer contract (ratio)"), f)
f <- "rev1_timing_results.csv"; M <- "R1 four-way transaction type"
for (p in list(c("turn, free/loan", "30 June or 1 July, no fee"), c("turn, paid", "30 June or 1 July, fee paid"), c("off-turn, free/loan", "Other dates, no fee"), c("off-turn, paid", "Other dates, fee paid")))
  add("mechanism", p[2], get(f, M, p[1]), f)
add("mechanism", "Excluding moves to a former club", get(f, "R1 exclude former-club destinations", "same"), f)
f <- "rev2_identification_results.csv"; T <- "R2 twin sample: own + twin agencies"
add("mechanism", "Own agency (twin-agency sample)", get(f, T, "own agency"), f)
add("mechanism", "Twin agencies with similar portfolios (placebo)", get(f, T, "twin agencies (placebo)"), f)
add("mechanism", "Difference: own minus twin agencies", get(f, T, "difference (own - twin)"), f)
f <- "rev2c_closest_twin_results.csv"
add("mechanism", "Closest single twin (placebo)", get(f, "R2c own + closest single twin", "closest twin (placebo)"), f)
add("mechanism", "Difference: own minus closest single twin", get(f, "R2c own + closest single twin", "difference (own - closest twin)"), f)
f <- "rev2b_twin_by_contract_results.csv"
add("mechanism", "Own agency, contract expiring", get(f, "R2 twin placebo, expiring contract", "own agency"), f)
add("mechanism", "Own agency, contract runs longer", get(f, "R2 twin placebo, contract runs longer", "own agency"), f)
add("mechanism", "Twin agencies, contract runs longer (placebo)", get(f, "R2 twin placebo, contract runs longer", "twin agencies (placebo)"), f)
add("mechanism", "Difference, contract runs longer: own minus twin agencies", get(f, "R2 twin placebo, contract runs longer", "difference (own - twin)"), f)
add("mechanism", "Twin agencies, contract expiring (placebo)", get(f, "R2 twin placebo, expiring contract", "twin agencies (placebo)"), f)
add("mechanism", "Difference, contract expiring: own minus twin agencies", get(f, "R2 twin placebo, expiring contract", "difference (own - twin)"), f)
f <- "rev2_identification_results.csv"
add("mechanism", "Recently placed co-client", get(f, "R2 recent vs long-standing co-clients", "recently placed co-client"), f)
add("mechanism", "Long-standing co-client", get(f, "R2 recent vs long-standing co-clients", "long-standing co-client"), f)
f <- "rev8_fill_results.csv"
add("sensitivity", "First-version linkage (1,514 events)", get(f, "V first-version linkage", "same"), f)
add("sensitivity", "First-version linkage, omit 30 June and 1 July", get(f, "V first-version linkage | omit 30 June and 1 July", "same"), f)
add("sensitivity", "Player-level linkage, baseline labels only", get(f, "V player-level linkage, baseline labels", "same"), f)
add("sensitivity", "Main sample, omit 30 June and 1 July", get(f, "V MAIN: + carry-forward labels | omit 30 June and 1 July", "same"), f)
add("sensitivity", "Main sample, no club fixed effects", get(f, "V MAIN | no club fixed effects", "same"), f)
add("sensitivity", "Main sample, harmonised representative labels", get(f, "V MAIN | harmonised labels", "same"), f)
add("sensitivity", "Adding look-ahead labels from later releases", get(f, "V + look-ahead labels (later releases)", "same"), f)
f <- "rev4_choiceset_results.csv"
add("sensitivity", "Tier and affordability controls", get(f, "R4 + tier/affordability fit controls", "same"), f)
add("sensitivity", "Alternatives within a 4.5-fold squad-value gap", get(f, "R4 alternatives within |value gap|<=1.5", "same"), f)
f <- "rev4b_elo_results.csv"
add("sensitivity", "Same 1,643 events, all clubs as alternatives", get(f, "R4b same events, all alternatives, no fit controls", "same"), f)
add("sensitivity", "Same 1,643 events, all clubs, + tier controls", get(f, "R4b same events, all alternatives, fit controls", "same"), f)
add("sensitivity", "Same 1,643 events, alternatives within 200 Elo", get(f, "R4b restricted alternatives, no fit controls", "same"), f)
add("sensitivity", "Same 1,643 events, within 200 Elo, + tier controls", get(f, "R4b restricted alternatives, fit controls", "same"), f)
d <- rf("rev5_league_results.csv")
for (p in list(c("first-version sample: baseline (agency log count, local nationality)", "League model, first-version sample (per doubling)"),
               c("main sample: + nationality x destination-league FE", "League model, main sample, nationality-by-league effects (per doubling)"))) {
  r <- d[model == p[1] & term == "agency_log"]
  rows[[length(rows) + 1]] <- data.table(block = "league", label = p[2], events = as.integer(r$events), odds_ratio = r$or_per_doubling, lo = r$lo_doubling, hi = r$hi_doubling, source = "rev5_league_results.csv")
}
r <- d[model == "main sample: agency share + nationality x league FE" & term == "agency_share"]
rows[[length(rows) + 1]] <- data.table(block = "league", label = "League model, main sample, agency concentration (per 10 pp of clients), nationality-by-league effects", events = as.integer(r$events), odds_ratio = r$odds_ratio, lo = r$label_lo, hi = r$label_hi, source = "rev5_league_results.csv")
R <- rbindlist(rows); fwrite(R, file.path(dir, "revision_results_R.csv"))

theme_fig <- theme_minimal(base_size = 9) + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(colour = "black"))
draw <- function(items, file, w, h, xlim, colours, xlab = "Odds ratio or ratio of odds ratios (95% CI); null value = 1") {
  items[, y := rev(seq_len(.N))]
  p <- ggplot(items, aes(y = y, colour = kind)) + geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40", linewidth = .3) +
    geom_linerange(aes(xmin = lo, xmax = hi), linewidth = .7) + geom_point(aes(x = odds_ratio, shape = kind), size = 2) +
    scale_y_continuous(breaks = items$y, labels = items$label) + scale_colour_manual(values = colours) + scale_shape_manual(values = c(a = 16, b = 17, c = 18, main = 16, sens = 16)) +
    coord_cartesian(xlim = xlim) + labs(x = xlab, y = NULL) + theme_fig + theme(legend.position = "none", axis.line.x = element_line(colour = "black", linewidth = .3), axis.ticks.x = element_line(colour = "black", linewidth = .3))
  ggsave(file.path(dir, file), p, width = w, height = h, dpi = 200)
}
b <- fread(file.path(dir, "model_results_R.csv"))[term == "same"]
pick <- function(m) { r <- b[model == m]; c(r$odds_ratio, r$label_lo, r$label_hi) }
spec <- list(c("Roster coverage only", "Club baseline"), c("+ nationality and fit", "Club adjusted"), c("+ club fixed effects", "Club fixed effects"), c("Club-by-summer FE", "Club by summer fixed effects"),
             c("+ three-year flows (preferred)", "Club past flows"), c("Club-by-summer FE + flows", "Club by summer past flows"), c("Omit 30 June / 1 July + flows", "Club past flows excluding turn of July"))
it <- rbindlist(lapply(spec, function(s) { v <- pick(s[2]); data.table(label = s[1], odds_ratio = v[1], lo = v[2], hi = v[3], kind = "main") }))
sens <- R[block == "sensitivity" & !startsWith(label, "Main sample, omit"), .(label, odds_ratio, lo, hi, kind = "sens")]
draw(rbind(it, sens), "figureA1_specifications_R.png", 7.2, 6.2, c(0.85, 2.3), c(main = "#1f4e79", sens = "#b5651d"))
g <- function(lbl) { r <- R[label == lbl]; c(r$odds_ratio, r$lo, r$hi) }
pref <- pick("Club past flows")
its <- list(c("Preferred club model, 2,000 moves", "a", pref), c("Own agency, joint model (1,674 moves)", "a", g("Own agency (twin-agency sample)")),
            c("Three matched agencies, same model", "b", g("Twin agencies with similar portfolios (placebo)")), c("Ratio: own to three matched agencies", "c", g("Difference: own minus twin agencies")),
            c("Closest matched agency, separate model", "b", g("Closest single twin (placebo)")), c("Ratio: own to closest matched agency", "c", g("Difference: own minus closest single twin")))
it2 <- rbindlist(lapply(its, function(s) data.table(label = s[1], kind = s[2], odds_ratio = as.numeric(s[3]), lo = as.numeric(s[4]), hi = as.numeric(s[5]))))
draw(it2, "figure1_main_R.png", 7.2, 3.4, c(0.75, 2.0), c(a = "#1f4e79", b = "#b5651d", c = "#000000"))
cat("wrote revision_results_R.csv and two figures to", dir, "\n")
