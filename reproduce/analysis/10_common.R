# Shared set-up of the benchmark scripts (config/benchmark.yaml).

suppressPackageStartupMessages({
  library(InsectTrack)
})
root <- here::here()
bcfg <- yaml::read_yaml(file.path(root, "config", "benchmark.yaml"))
set.seed(bcfg$seed)
bout <- function(...) {
  d <- file.path(root, bcfg$paths$output, "benchmark", ...)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}
write_csv <- function(x, file) utils::write.csv(x, file, row.names = FALSE, na = "")
log_msg <- function(...) message(format(Sys.time(), "%H:%M:%S "), sprintf(...))
`%||%` <- function(a, b) if (is.null(a)) b else a
