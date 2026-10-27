# Australian native stingless bees at a hive entrance (frames with masked
# static background): tracking from annotated positions, i.e. a test of the
# association step alone.

source(here::here("analysis", "00_common.R"))

p <- cfg$native_bee
od <- out_dir("native_bee")
base <- file.path(root, cfg$paths$native_bee)
seqs <- list.dirs(base, full.names = FALSE, recursive = FALSE)
anomalies <- list()

res <- do.call(rbind, lapply(seqs, function(sq) {
  lab <- list.files(file.path(base, sq, "labels"), pattern = "\\.txt$", full.names = TRUE)
  gt <- it_read_yolo_labels(lab, 1280, 720, frame_regex = "_frame_(\\d+)", first_field = "id")
  gt$time <- gt$frame
  dup <- duplicated(gt[, c("frame", "id")])
  if (any(dup)) {
    anomalies[[sq]] <<- data.frame(sequence = sq, problem = sprintf("%d annotations repeat an identity already present in the same frame (later ones removed)", sum(dup)))
    gt <- gt[!dup, ]
  }
  tr <- run_tracking(gt[, c("frame", "time", "x", "y")], p$track, p$stitch)
  tr0 <- run_tracking(gt[, c("frame", "time", "x", "y")], p$track, NULL)
  write_csv(gt[, c("frame", "x", "y", "id")], file.path(od, paste0(sq, "_gt.csv")))
  write_csv(tr, file.path(od, paste0(sq, "_tracks_from_annotations.csv")))
  rbind(cbind(sequence = sq, frames = length(lab), stitched = TRUE, it_eval_tracks(tr, gt, p$max_dist)),
        cbind(sequence = sq, frames = length(lab), stitched = FALSE, it_eval_tracks(tr0, gt, p$max_dist)))
}))
write_csv(res, file.path(od, "native_bee_metrics_by_sequence.csv"))
if (length(anomalies)) write_csv(do.call(rbind, anomalies), file.path(od, "data_anomalies.csv"))
pool <- do.call(rbind, lapply(split(res, res$stitched), function(d) {
  GT <- sum(d$gt_objects); P <- sum(d$pred_objects)
  idtp <- sum(d$idf1 * (d$gt_objects + d$pred_objects) / 2)
  data.frame(stitched = d$stitched[1], sequences = nrow(d), frames = sum(d$frames), gt_objects = GT,
             gt_tracks = sum(d$gt_tracks), pred_tracks = sum(d$pred_tracks),
             mota = 1 - sum(d$fn + d$fp + d$id_switches) / GT, idf1 = 2 * idtp / (GT + P),
             id_switches = sum(d$id_switches))
}))
write_csv(pool, file.path(od, "native_bee_metrics_pooled.csv"))
log_msg("native bees finished: pooled MOTA %.3f, IDF1 %.3f (stitched)", pool$mota[pool$stitched], pool$idf1[pool$stitched])
