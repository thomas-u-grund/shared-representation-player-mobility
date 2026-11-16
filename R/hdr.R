# Common header for the analysis scripts: arguments, script directory, shared code.
args <- commandArgs(TRUE); root <- args[1]; outdir <- if (length(args) > 1) args[2] else "."
script_dir <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE))
source(file.path(script_dir, "prepare.R")); source(file.path(script_dir, "enrich.R"))
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
save_records <- function(file) { fwrite(rbindlist(records$rows, fill = TRUE), file.path(outdir, file)); records$rows <- list() }
