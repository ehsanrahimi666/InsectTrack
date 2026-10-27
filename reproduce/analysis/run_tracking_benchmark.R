# Runs analysis/12_tracking_benchmark.R with the development library of the package.
.libPaths(c(Sys.getenv("IT_RLIB", .libPaths()[1]), .libPaths()))
source(here::here("analysis", "12_tracking_benchmark.R"))
