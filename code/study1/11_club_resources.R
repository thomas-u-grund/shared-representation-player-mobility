# Association by club resources, proxied by squad market value within league and summer (Online Appendix, Table B9).
# Usage: Rscript code/study1/11_club_resources.R <data_root> [output_dir]
# Writes club_resources_results.csv.
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "..", "lib", "setup.R"))
d <- prepare(root, "extended"); m <- copy(d$main); pl <- d$players
base <- c("same", controls, flows)
# club resources proxy: squad market value (sum of listed values) and Elo, ranked within league-summer
cv <- pl[, .(squad_value = sum(as.numeric(market_value_eur), na.rm = TRUE)), by = .(summer, league, club_id)]
cv[, vrank := frank(squad_value) / .N, by = .(summer, league)]
m <- merge(m, cv[, .(summer, candidate_club = club_id, vrank)], by = c("summer", "candidate_club"), all.x = TRUE, sort = FALSE)
m <- m[!is.na(vrank)]
m[, low_res := as.integer(vrank <= 0.5)]
m[, `:=`(same_low = same * low_res, same_high = same * (1 - low_res), same_vr = same * (vrank - 0.5))]
fit_logit("R0 baseline (sample with value)", m, base, fx)
fit_logit("R1 same x lower half of league by squad value", m, c("same_low", "same_high", controls, flows), fx,
          list("lower-valued clubs" = c(same_low = 1), "higher-valued clubs" = c(same_high = 1), "lower minus higher" = c(same_low = 1, same_high = -1)))
fit_logit("R2 same x squad-value rank (continuous)", m, c("same", "same_vr", controls, flows), fx, list("at median rank" = c(same = 1), "bottom (rank 0) vs top (rank 1)" = c(same_vr = -1)))
save_records("club_resources_results.csv")
