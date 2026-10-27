# Runs all feasibility analyses in order. Usage: Rscript analysis/run_all.R [part]
# where part is "A" (example videos and Delosperma), "B" (validation data
# sets) or empty for both.
part <- commandArgs(trailingOnly = TRUE)
part <- if (length(part)) part[1] else "AB"
here_src <- function(f) {
  message("==== ", f, " ", format(Sys.time()))
  source(here::here("analysis", f), local = new.env())
}
if (grepl("A", part)) {
  here_src("01_web_videos.R")
  here_src("02_delosperma.R")
}
if (grepl("B", part)) {
  here_src("03_melipona.R")
  here_src("04_native_bee.R")
  here_src("05_polytrack.R")
}
writeLines(capture.output(sessionInfo()), here::here("logs", paste0("sessionInfo_", part, ".txt")))
message("==== finished ", format(Sys.time()))
