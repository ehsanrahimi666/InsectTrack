# Stingless bee (Melipona capixaba) hive entrance with MOTChallenge ground
# truth: detection and tracking accuracy under six light conditions at 120 and
# 30 frames per second, tracking from annotated positions, and planned
# sensitivity analyses.

source(here::here("analysis", "00_common.R"))

p <- cfg$melipona
od <- out_dir("melipona")
base <- file.path(root, cfg$paths$melipona)

seqs <- list()
for (fps in c(120, 30)) {
  vids <- list.files(file.path(base, paste0(fps, "_fps"), "partes"), pattern = "\\.mp4$", full.names = TRUE)
  for (v in vids) {
    g <- file.path(sub("\\.mp4$", "_gt", v), "gt", "gt.txt")
    if (file.exists(g)) {
      seqs[[length(seqs) + 1]] <- data.frame(sequence = substr(basename(v), 1, 5), fps = fps, video = v, gt = g)
    }
  }
}
seqs[[length(seqs) + 1]] <- data.frame(sequence = "08h00v", fps = 120,
                                       video = file.path(base, "validacao_mota", "2026_03_06_h08_M00_S18.mp4"),
                                       gt = file.path(base, "validacao_mota", "gt", "gt.txt"))
seqs <- do.call(rbind, seqs)
seqs$id <- paste0(seqs$sequence, "_", seqs$fps, "fps")
only <- Sys.getenv("IT_ONLY", "")
if (nzchar(only)) seqs <- seqs[seqs$id %in% strsplit(only, ",")[[1]], ]
write_csv(seqs[, c("id", "sequence", "fps")], file.path(od, "sequences.csv"))

read_gt <- function(f, fps) {
  g <- it_read_detections(f, format = "mot", fps = fps)
  lab <- trimws(readLines(file.path(dirname(f), "labels.txt"), warn = FALSE))
  lab <- lab[nzchar(lab)]
  g <- g[g$V8 %in% which(tolower(lab) == "bee"), ]   # class ids follow labels.txt; pollen loads are not bees
  g$time <- (g$frame - 1) / fps
  g$id <- g$track
  g[, c("frame", "time", "x", "y", "id", "width", "height")]
}

det_store <- list(); gt_store <- list(); frames_store <- list()
res <- list(); sens <- list()
for (i in seq_len(nrow(seqs))) {
  sq <- seqs[i, ]
  wd <- file.path(tempdir(), sq$id)
  fr <- it_extract_frames(sq$video, dir = wd, gray = TRUE)
  fr$time <- (fr$frame - 1) / sq$fps
  gt <- read_gt(sq$gt, sq$fps)
  stopifnot(max(gt$frame) <= nrow(fr))
  bg <- it_background(fr, n = p$detect$bg_frames)
  dt <- timed(run_detection(fr, p$detect, background = bg))
  det <- dt$value
  detB <- run_detection(fr, utils::modifyList(p$detect, list(k_pixel = cfg$variant_k_pixel)), background = bg)
  trB <- run_tracking(detB, p$track, p$stitch)
  edB <- it_eval_detections(detB, gt, p$max_dist, frames = fr$frame)$summary
  names(edB)[names(edB) %in% c("precision", "recall", "f1")] <- c("det_precision", "det_recall", "det_f1")
  pt <- p$track
  tr <- run_tracking(det, pt, p$stitch)
  tr_nostitch <- run_tracking(det, pt, NULL)
  orc <- run_tracking(gt[, c("frame", "time", "x", "y")], pt, p$stitch)
  ed <- it_eval_detections(det, gt, p$max_dist, frames = fr$frame)$summary
  names(ed)[names(ed) %in% c("precision", "recall", "f1")] <- c("det_precision", "det_recall", "det_f1")
  et <- it_eval_tracks(tr, gt, p$max_dist)
  en <- it_eval_tracks(tr_nostitch, gt, p$max_dist)
  eo <- it_eval_tracks(orc, gt, p$max_dist)
  res[[sq$id]] <- rbind(
    cbind(id = sq$id, sequence = sq$sequence, fps = sq$fps, input = "motion detections", stitched = TRUE,
          ed[, c("det_precision", "det_recall", "det_f1", "count_mae")], et),
    cbind(id = sq$id, sequence = sq$sequence, fps = sq$fps, input = "motion detections", stitched = FALSE,
          ed[, c("det_precision", "det_recall", "det_f1", "count_mae")], en),
    cbind(id = sq$id, sequence = sq$sequence, fps = sq$fps, input = "motion detections, pixel threshold",
          stitched = TRUE, edB[, c("det_precision", "det_recall", "det_f1", "count_mae")],
          it_eval_tracks(trB, gt, p$max_dist)),
    cbind(id = sq$id, sequence = sq$sequence, fps = sq$fps, input = "annotated positions", stitched = TRUE,
          det_precision = 1, det_recall = 1, det_f1 = 1, count_mae = 0, eo))
  write_csv(tr, file.path(od, paste0(sq$id, "_tracks.csv")))
  write_csv(orc, file.path(od, paste0(sq$id, "_tracks_from_annotations.csv")))
  write_csv(gt, file.path(od, paste0(sq$id, "_gt.csv")))
  # planned sensitivity: detector (k, minimum area) and tracker (gate, maximum gap)
  dv <- list(k3 = list(k = 3), k7 = list(k = 7), area_half = list(min_area = p$detect$min_area * 0.5),
             area_double = list(min_area = p$detect$min_area * 2))
  for (nm in if (sq$fps == 30) names(dv) else character()) {
    d2 <- run_detection(fr, utils::modifyList(p$detect, dv[[nm]]), background = bg)
    t2 <- run_tracking(d2, pt, p$stitch)
    sens[[paste(sq$id, nm)]] <- cbind(id = sq$id, fps = sq$fps, variant = nm,
                                      stats::setNames(it_eval_detections(d2, gt, p$max_dist, frames = fr$frame)$summary[, c("precision", "recall", "f1")], c("det_precision", "det_recall", "det_f1")),
                                      it_eval_tracks(t2, gt, p$max_dist)[, c("mota", "idf1", "id_switches")])
  }
  tv <- list(gate_half = list(gate = pt$gate * 0.5), gate_double = list(gate = pt$gate * 2),
             gap_half = list(max_gap = pt$max_gap * 0.5), gap_double = list(max_gap = pt$max_gap * 2))
  for (nm in names(tv)) {
    t2 <- run_tracking(det, utils::modifyList(pt, tv[[nm]]), p$stitch)
    sens[[paste(sq$id, nm)]] <- cbind(id = sq$id, fps = sq$fps, variant = nm,
                                      ed[, c("det_precision", "det_recall", "det_f1")],
                                      it_eval_tracks(t2, gt, p$max_dist)[, c("mota", "idf1", "id_switches")])
  }
  if (sq$id == "15h00_120fps") {
    frc <- it_extract_frames(sq$video, dir = file.path(wd, "col"), gray = FALSE, format = "jpg")
    frc$time <- (frc$frame - 1) / sq$fps
    it_render_video(frc[frc$frame %% 2 == 1, ], tr, file.path(od, paste0(sq$id, "_annotated.mp4")),
                    tail = 0.5, fps = 30)
    save_png(file.path(od, paste0(sq$id, "_trajectories.png")), 1600, 900,
             it_plot_tracks(tr, 1280, 720, background = it_read_frame(frc$file[1], gray = FALSE)))
  }
  unlink(wd, recursive = TRUE)
  log_msg("%s: F1 %.3f | MOTA %.3f IDF1 %.3f (annotated positions: MOTA %.3f IDF1 %.3f) | %.3f s/frame detection",
          sq$id, ed$det_f1, et$mota, et$idf1, eo$mota, eo$idf1, dt$seconds / nrow(fr))
}
res <- do.call(rbind, res)
sens <- do.call(rbind, sens)
if (nzchar(only) && file.exists(file.path(od, "melipona_metrics_by_sequence.csv"))) {   # re-run of selected sequences: replace their rows in the existing tables
  old <- utils::read.csv(file.path(od, "melipona_metrics_by_sequence.csv"))
  res <- rbind(old[!old$id %in% res$id, ], res)
  olds <- utils::read.csv(file.path(od, "melipona_sensitivity.csv"))
  sens <- rbind(olds[!olds$id %in% unique(res$id[res$id %in% seqs$id]), ], sens)
}
write_csv(res, file.path(od, "melipona_metrics_by_sequence.csv"))
write_csv(sens, file.path(od, "melipona_sensitivity.csv"))

# pooled metrics by frame rate and input
pool <- do.call(rbind, lapply(split(res, list(res$fps, res$input, res$stitched), drop = TRUE), function(d) {
  GT <- sum(d$gt_objects); P <- sum(d$pred_objects)
  idtp <- sum(d$idf1 * (d$gt_objects + d$pred_objects) / 2)
  data.frame(fps = d$fps[1], input = d$input[1], stitched = d$stitched[1], sequences = nrow(d),
             gt_objects = GT, gt_tracks = sum(d$gt_tracks), pred_tracks = sum(d$pred_tracks),
             precision_det = sum(d$tp) / P, recall_det = sum(d$tp) / GT,
             mota = 1 - sum(d$fn + d$fp + d$id_switches) / GT, idf1 = 2 * idtp / (GT + P),
             id_switches = sum(d$id_switches), mostly_tracked = sum(d$mostly_tracked),
             mostly_lost = sum(d$mostly_lost))
}))
write_csv(pool, file.path(od, "melipona_metrics_pooled.csv"))
log_msg("Melipona finished")
