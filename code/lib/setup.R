# Shared set-up for the analysis scripts (sourced at the top of each script in code/study1/).
# Reads the command line (<data_root> [output_dir]), loads the packages and the shared functions, and defines save_records().
suppressPackageStartupMessages(library(data.table))
.script_dir <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE))   # Rscript encodes spaces in the path as "~+~"
lib_dir <- normalizePath(file.path(.script_dir, "..", "lib"))
args <- commandArgs(TRUE)
root <- args[1]                                         # data root, e.g. data/study1/
outdir <- if (length(args) > 1) args[2] else "."        # where the result tables are written
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
for (f in c("models.R", "prepare.R", "enrich.R")) source(file.path(lib_dir, f))
# Optional disk cache for prepare() and enrich(): when the environment variable PREP_CACHE names a directory, the prepared data of each sample
# is stored there once (run_all.R does this) and later scripts read it instead of rebuilding it. Without the variable nothing is cached.
local({
  cache <- Sys.getenv("PREP_CACHE", "")
  wrap <- function(fun, tag) { force(fun); function(root, sample = "extended") {
    if (!nzchar(cache)) return(fun(root, sample))
    f <- file.path(cache, sprintf("%s_%s.rds", tag, sample))
    if (!file.exists(f)) { r <- fun(root, sample); tmp <- paste0(f, ".", Sys.getpid()); saveRDS(r, tmp, compress = FALSE); file.rename(tmp, f) }
    lapply(readRDS(f), function(x) if (is.data.table(x)) setalloccol(x) else x)   # readRDS drops the over-allocation that := needs
  } }
  assign("prepare", wrap(prepare, "prepare"), envir = globalenv()); assign("enrich", wrap(enrich, "enrich"), envir = globalenv())
})
# Write the model rows collected by fit_logit() since the last call to a csv file and start a new collection.
save_records <- function(file) { fwrite(rbindlist(records$rows, fill = TRUE), file.path(outdir, file)); records$rows <- list() }
