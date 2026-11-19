# Prepare and cache the data of one sample (run by run_all.R before the analyses; needs the PREP_CACHE environment variable).
# Usage: Rscript R/lib/prepare_cache.R <data_root> <output_dir> <sample>     sample: extended, extended_base, extended_post or archived
source(file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "setup.R"))
stopifnot(nzchar(Sys.getenv("PREP_CACHE")))
invisible(prepare(root, args[3])); invisible(enrich(root, args[3]))
