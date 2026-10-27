# Post hoc check of track confirmation in BoxMOT: OC-SORT, Hybrid-SORT and
# BoostTrack return a box only when its track has been matched in min_hits
# consecutive frames (default 3), counting again after any frame without a
# match, and do not return the boxes before that. The annotated boxes of every
# sequence are tracked again with min_hits = 1, and the share of annotated
# boxes returned in tracks, HOTA, DetA and AssA are compared with the
# benchmark runs (default settings). Also counts the runs of consecutive
# frames of the annotated tracks, whose interruptions restart the count.

source(here::here("analysis", "10_common.R"))
source(here::here("analysis", "10_tracking_common.R"))
od <- bout("tracking")
check_trackers <- c("boxmot_ocsort", "boxmot_hybridsort", "boxmot_boosttrack")
thr <- tb$thresholds$ground_truth

rows <- list(); hres <- list(); runs <- list()
for (i in seq_len(nrow(all_seqs))) {
  s <- all_seqs[i, ]
  g <- read_gt(s)
  # interruptions of the annotated tracks: runs of consecutive frames per track, and frames of the
  # sequence that are missing from the image files (native bees)
  rl <- vapply(split(g$frame, g$id), function(f) sum(diff(sort(f)) > 1) + 1, 0)
  missing_frames <- if (s$dataset == "native_bee") {
    img <- list.files(file.path(root, bcfg$paths$native_bee, s$id, "images"))
    fr <- as.integer(sub(".*_frame_(\\d+).*", "\\1", img))
    length(setdiff(seq_len(max(g$frame)), fr))
  } else 0L
  runs[[length(runs) + 1]] <- data.frame(dataset = s$dataset, sequence = s$id, fps = s$fps, tracks = length(rl),
                                         runs = sum(rl), boxes = nrow(g), missing_frames = missing_frames,
                                         frames = max(g$frame))
  det <- gt_as_det(g)
  fs <- seq_frames(s, g, det)
  for (tn in check_trackers) {
    tr <- it_track(det[det$conf >= thr$low, , drop = FALSE], tn, frame_size = c(s$H, s$W), frames = fs,
                   params = utils::modifyList(boxmot_params(tn, thr, s$fps), list(min_hits = 1L)))
    h <- it_eval_hota(tr, g, "iou")
    rows[[length(rows) + 1]] <- data.frame(dataset = s$dataset, sequence = s$id, fps = s$fps, tracker = tn,
                                           min_hits = 1L, gt_objects = nrow(g), pred_objects = sum(!is.na(tr$track)))
    hres[[paste(s$dataset, s$id, tn)]] <- h
  }
  log_msg("%s done", s$id)
}
bys <- do.call(rbind, rows)
bys$fps_group <- ifelse(bys$dataset == "melipona", paste0(bys$fps, "fps"), "all")
pooled <- do.call(rbind, lapply(split(bys, bys[, c("dataset", "fps_group", "tracker")], drop = TRUE), function(z) {
  hc <- it_combine_hota(hres[paste(z$dataset, z$sequence, z$tracker)])
  data.frame(dataset = z$dataset[1], fps_group = z$fps_group[1], tracker = z$tracker[1], min_hits = 1L,
             sequences = nrow(z), boxes_returned = sum(z$pred_objects) / sum(z$gt_objects),
             hota = hc$hota, deta = hc$deta, assa = hc$assa)
}))
# benchmark runs with the default min_hits
bm <- utils::read.csv(file.path(od, "metrics_by_sequence.csv"))
bm <- bm[bm$input == "ground_truth" & bm$tracker %in% check_trackers, ]
bm$fps_group <- ifelse(bm$dataset == "melipona", paste0(bm$fps, "fps"), "all")
pm <- utils::read.csv(file.path(od, "metrics_pooled.csv"))
pm <- pm[pm$input == "ground_truth" & pm$tracker %in% check_trackers, ]
def <- do.call(rbind, lapply(split(bm, bm[, c("dataset", "fps_group", "tracker")], drop = TRUE), function(z) {
  q <- pm[pm$dataset == z$dataset[1] & pm$fps_group == z$fps_group[1] & pm$tracker == z$tracker[1], ]
  data.frame(dataset = z$dataset[1], fps_group = z$fps_group[1], tracker = z$tracker[1], min_hits = 3L,
             sequences = nrow(z), boxes_returned = sum(z$pred_objects) / sum(z$gt_objects),
             hota = q$hota, deta = q$deta, assa = q$assa)
}))
out <- rbind(def, pooled)
write_csv(out[order(out$dataset, out$fps_group, out$tracker, out$min_hits), ], file.path(od, "confirmation_check.csv"))
ru <- do.call(rbind, runs)
ru$fps_group <- ifelse(ru$dataset == "melipona", paste0(ru$fps, "fps"), "all")
ra <- stats::aggregate(cbind(sequences = 1, tracks = tracks, runs = runs, boxes = boxes, missing_frames = missing_frames,
                             frames = frames) ~ dataset + fps_group, data = ru, FUN = sum)
write_csv(ra, file.path(od, "annotated_track_runs.csv"))
log_msg("confirmation check: %d sequence-tracker runs", nrow(bys))
