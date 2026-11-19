# Club and league conditional logits for the main sample (Tables 2 and 4, Figure A1) and their specification variants.
# Usage: Rscript code/study1/01_main_models.R <data_root> [output_dir]
# Writes model_results.csv and sample_summary.json.
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "..", "lib", "setup.R"))
suppressPackageStartupMessages(library(jsonlite))
d <- prepare(root, "extended"); main <- d$main; c <- d$c; all_clubs <- d$all_clubs; lg <- d$lg
fit_logit("Club baseline", main, c("same", "log_known"))
fit_logit("Club adjusted", main, c("same", controls))
fit_logit("Club fixed effects", main, c("same", controls), fx)
fit_logit("Club by summer fixed effects", main, c("same", controls), "club_summer")
fit_logit("Club past flows", main, c("same", controls, flows), fx)
fit_logit("Club by summer past flows", main, c("same", controls, flows), "club_summer")
fit_logit("Club short past flows", main, c("same", controls, "forward_1", "reverse_1", "own_history_1"), fx)
fit_logit("Club long past flows", main, c("same", controls, "forward_5", "reverse_5", "own_history_5"), fx)
fit_logit("Club past flows excluding turn of July", turn(main), c("same", controls, flows), fx)
fit_logit("Club timing interaction", main, c("same", "same_turn_of_july", controls, flows), fx,
          list("shared label on 30 June or 1 July" = c(same = 1, same_turn_of_july = 1)))
fit_logit("Club past flows interaction", main, c("same", "same_prior_flow", controls, flows, "any_prior_flow"), fx,
          list("shared label with past flows" = c(same = 1, same_prior_flow = 1)))
fit_logit("Club including unobserved rosters", all_clubs[likely_return == FALSE], c("same", "roster_unavailable", controls), fx)
fit_logit("Club broad sample", c, c("same", controls), fx)
fit_logit("Club excluding turn of July", turn(main), c("same", controls), fx)
fit_logit("Club player quality by Elo", main, c("same", controls, "value_elo", "minutes_elo", "rating_elo", "value_unknown_elo", "rating_unknown_elo"), fx)
fit_logit("Club dose", main, c("co_client_count_log", controls), fx)
fit_logit("Club route interaction", main, c("same", "same_between", controls), fx, list("between league total" = c(same = 1, same_between = 1)))
fit_logit("Club year interaction", main, c("same", "same_2024", "same_2025", controls), fx,
          list("2024 total" = c(same = 1, same_2024 = 1), "2025 total" = c(same = 1, same_2025 = 1)))

fit_logit("League fixed routes", lg, c("agency_log", "log_roster", "local_nationality"), "route_fe")
lc <- lg[route == "between" & origin_league != alternative]
fit_logit("League fixed cross routes", lc, c("agency_log", "log_roster", "local_nationality"), "route_fe")

res <- rbindlist(records$rows, fill = TRUE)
fwrite(res, file.path(outdir, "model_results.csv"))
write(toJSON(records$summaries, auto_unbox = TRUE, pretty = TRUE), file.path(outdir, "sample_summary.json"))
cat("wrote", nrow(res), "rows\n")
