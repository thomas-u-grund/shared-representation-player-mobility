# compare(new_csv, old_csv): max relative difference in odds ratios over matching (model, term) rows
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(TRUE); n <- fread(args[1]); o <- fread(args[2])
m <- merge(o, n, by = c("model", "term"), suffixes = c("_py", "_r"))
cat(sprintf("%s: %d/%d rows matched; max rel diff OR %.2e; events equal: %s\n", basename(args[2]), nrow(m), nrow(o), max(abs(m$odds_ratio_py - m$odds_ratio_r) / m$odds_ratio_py), all(m$events_py == m$events_r)))
