#!/usr/bin/env bash
# Re-runs a subset of the analyses in fresh R sessions into output_rerun/ and
# compares the result tables with output/.
set -euo pipefail
cd "$(dirname "$0")/.."
export IT_OUTPUT=output_rerun
rm -rf output_rerun
IT_VIDEOS=V04 IT_STABILITY=0 Rscript analysis/01_web_videos.R
Rscript analysis/02_delosperma.R
IT_ONLY=17h00_30fps Rscript analysis/03_melipona.R
Rscript analysis/04_native_bee.R
Rscript analysis/05_polytrack.R
Rscript - <<'R'
files <- c("web_videos/V04/V04_tracks.csv", "web_videos/V04/V04_zone_transitions.csv",
           "delosperma/detection_evaluation.csv", "delosperma/delosperma_summary.csv",
           "delosperma/visits_per_flower_comparison.csv", "delosperma/yolo_tracks_full_video.csv",
           "melipona/17h00_30fps_tracks.csv", "native_bee/native_bee_metrics_pooled.csv",
           "polytrack/polytrack_summary.csv")
cmp <- do.call(rbind, lapply(files, function(f) {
  a <- utils::read.csv(file.path("output", f)); b <- utils::read.csv(file.path("output_rerun", f))
  num <- vapply(a, is.numeric, TRUE) & vapply(b, is.numeric, TRUE)
  same_dim <- identical(dim(a), dim(b))
  maxdiff <- if (same_dim && any(num)) max(abs(as.matrix(a[num]) - as.matrix(b[num])), na.rm = TRUE) else NA
  data.frame(file = f, rows = nrow(a), same_dimensions = same_dim, max_abs_difference = maxdiff,
             identical_text = same_dim && isTRUE(all.equal(a[!num], b[!num], check.attributes = FALSE)))
}))
m <- utils::read.csv("output/melipona/melipona_metrics_by_sequence.csv")
r <- utils::read.csv("output_rerun/melipona/melipona_metrics_by_sequence.csv")
m <- m[m$id == "17h00_30fps", ]
cmp <- rbind(cmp, data.frame(file = "melipona/melipona_metrics_by_sequence.csv (17h00_30fps)", rows = nrow(r),
                             same_dimensions = nrow(m) == nrow(r),
                             max_abs_difference = max(abs(m$mota - r$mota), abs(m$idf1 - r$idf1)),
                             identical_text = TRUE))
utils::write.csv(cmp, "logs/rerun_comparison.csv", row.names = FALSE)
print(cmp)
R
