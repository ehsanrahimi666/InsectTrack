# Stingless bee hive entrance with a deep-learning detector: YOLO26n trained
# by the data set authors on Melipona capixaba frames (ONNX export, Zenodo
# 10.5281/zenodo.20276311), followed by (a) the InsectTrack tracker and (b) the
# ByteTrack tracker of Ultralytics, both evaluated against the MOTChallenge
# ground truth.

source(here::here("analysis", "00_common.R"))

p <- cfg$melipona
od <- out_dir("melipona_yolo")
base <- file.path(root, cfg$paths$melipona)
model <- file.path(root, "data", "raw", "validation", "melipona_yolo26n_fp32.onnx")

seqs <- list()
for (fps in c(120, 30)) {
  vids <- list.files(file.path(base, paste0(fps, "_fps"), "partes"), pattern = "\\.mp4$", full.names = TRUE)
  for (v in vids) {
    g <- file.path(sub("\\.mp4$", "_gt", v), "gt", "gt.txt")
    if (file.exists(g)) seqs[[length(seqs) + 1]] <- data.frame(sequence = substr(basename(v), 1, 5), fps = fps, video = v, gt = g)
  }
}
seqs[[length(seqs) + 1]] <- data.frame(sequence = "08h00v", fps = 120,
                                       video = file.path(base, "validacao_mota", "2026_03_06_h08_M00_S18.mp4"),
                                       gt = file.path(base, "validacao_mota", "gt", "gt.txt"))
seqs <- do.call(rbind, seqs)
seqs$id <- paste0(seqs$sequence, "_", seqs$fps, "fps")

bytetrack <- function(frames, conf) {
  ul <- reticulate::import("ultralytics", convert = FALSE)
  m <- ul$YOLO(model, task = "detect")
  out <- vector("list", nrow(frames))
  for (k in seq_len(nrow(frames))) {
    r <- m$track(source = frames$file[k], persist = TRUE, tracker = "bytetrack.yaml", imgsz = 640L,
                 conf = conf, verbose = FALSE)
    b <- reticulate::py_get_item(r, 0L)$boxes
    if (!reticulate::py_to_r(b$is_track)) next
    xy <- reticulate::py_to_r(b$xyxy$cpu()$numpy())
    id <- as.integer(reticulate::py_to_r(b$id$cpu()$numpy()))
    out[[k]] <- data.frame(frame = frames$frame[k], time = frames$time[k],
                           x = (xy[, 1] + xy[, 3]) / 2, y = (xy[, 2] + xy[, 4]) / 2, track = id)
  }
  do.call(rbind, out)
}

res <- list()
only <- Sys.getenv("IT_ONLY", "")
if (nzchar(only)) seqs <- seqs[seqs$id == only, ]
for (i in seq_len(nrow(seqs))) {
  sq <- seqs[i, ]
  wd <- file.path(tempdir(), paste0("yolo_", sq$id))
  frc <- it_extract_frames(sq$video, dir = wd, gray = FALSE, format = "jpg")
  frc$time <- (frc$frame - 1) / sq$fps
  g <- it_read_detections(sq$gt, format = "mot", fps = sq$fps)
  lab <- trimws(readLines(file.path(dirname(sq$gt), "labels.txt"), warn = FALSE))
  g <- g[g$V8 %in% which(tolower(lab[nzchar(lab)]) == "bee"), ]
  gt <- data.frame(frame = g$frame, time = (g$frame - 1) / sq$fps, x = g$x, y = g$y, id = g$track)
  dt <- timed(it_detect_yolo(frc, model, imgsz = 640, conf = 0.25, batch = 1, verbose = FALSE))
  det <- it_merge_detections(dt$value, 10)
  tr <- run_tracking(det, p$track, p$stitch)
  bt <- bytetrack(frc, conf = 0.1)
  ed <- it_eval_detections(det, gt, p$max_dist, frames = frc$frame)$summary
  res[[sq$id]] <- rbind(
    cbind(id = sq$id, sequence = sq$sequence, fps = sq$fps, tracker = "InsectTrack",
          det_f1 = ed$f1, det_precision = ed$precision, det_recall = ed$recall,
          it_eval_tracks(tr, gt, p$max_dist), s_per_frame_detect = dt$seconds / nrow(frc)),
    cbind(id = sq$id, sequence = sq$sequence, fps = sq$fps, tracker = "ByteTrack (Ultralytics)",
          det_f1 = ed$f1, det_precision = ed$precision, det_recall = ed$recall,
          it_eval_tracks(bt, gt, p$max_dist), s_per_frame_detect = NA_real_))
  write_csv(det, file.path(od, paste0(sq$id, "_yolo_detections.csv")))
  write_csv(tr, file.path(od, paste0(sq$id, "_tracks_insecttrack.csv")))
  write_csv(bt, file.path(od, paste0(sq$id, "_tracks_bytetrack.csv")))
  write_csv(gt, file.path(od, paste0(sq$id, "_gt.csv")))
  if (sq$id == "15h00_120fps") {
    it_render_video(frc[frc$frame %% 2 == 1, ], tr, file.path(od, paste0(sq$id, "_annotated_yolo.mp4")),
                    tail = 0.5, fps = 30)
  }
  unlink(wd, recursive = TRUE)
  log_msg("%s: YOLO F1 %.3f | InsectTrack MOTA %.3f IDF1 %.3f | ByteTrack MOTA %.3f IDF1 %.3f", sq$id, ed$f1,
          res[[sq$id]]$mota[1], res[[sq$id]]$idf1[1], res[[sq$id]]$mota[2], res[[sq$id]]$idf1[2])
}
res <- do.call(rbind, res)
if (nzchar(only) && file.exists(file.path(od, "melipona_yolo_metrics_by_sequence.csv"))) {
  old <- utils::read.csv(file.path(od, "melipona_yolo_metrics_by_sequence.csv"))
  res <- rbind(old[!old$id %in% res$id, ], res)
}
write_csv(res, file.path(od, "melipona_yolo_metrics_by_sequence.csv"))
pool <- do.call(rbind, lapply(split(res, list(res$fps, res$tracker), drop = TRUE), function(d) {
  GT <- sum(d$gt_objects); P <- sum(d$pred_objects)
  idtp <- sum(d$idf1 * (d$gt_objects + d$pred_objects) / 2)
  data.frame(fps = d$fps[1], tracker = d$tracker[1], sequences = nrow(d), gt_objects = GT,
             gt_tracks = sum(d$gt_tracks), pred_tracks = sum(d$pred_tracks),
             det_recall = sum(d$det_recall * d$gt_objects) / GT,
             mota = 1 - sum(d$fn + d$fp + d$id_switches) / GT, idf1 = 2 * idtp / (GT + P),
             id_switches = sum(d$id_switches))
}))
write_csv(pool, file.path(od, "melipona_yolo_metrics_pooled.csv"))
log_msg("Melipona YOLO finished")
