# Post hoc diagnostic of the hive-entrance counts. For every movement into or
# out of the entrance zone in the annotated tracks, records whether each
# tracker, given the annotated boxes, returns a track with the same movement
# that holds the annotated box at the entrance (the last box of a movement
# in, the first box of a movement out), together with the largest
# displacement of the insect between consecutive frames.

source(here::here("analysis", "10_common.R"))
source(here::here("analysis", "10_tracking_common.R"))
od <- bout("tracking")
zones <- entrance_zones(all_seqs)
thr <- tb$thresholds$ground_truth
key <- function(d) paste(d$frame, round(d$x, 2), round(d$y, 2))

rows <- list()
for (i in seq_len(nrow(all_seqs))) {
  s <- all_seqs[i, ]
  g <- read_gt(s)
  gtr <- g; gtr$track <- as.integer(factor(gtr$id))
  zg <- it_zone_transitions(gtr, zones[[s$dataset]])
  ev <- zg[zg$type %in% c("entered", "exited"), , drop = FALSE]
  if (!nrow(ev)) next
  det <- gt_as_det(g)
  fs <- seq_frames(s, g)
  for (tn in trackers) {
    tr <- tryCatch(run_tracker(tn, det, thr, s, fs), error = function(e) {
      log_msg("%s %s: %s", s$id, tn, conditionMessage(e)); NULL })
    if (is.null(tr)) next
    zt <- it_zone_transitions(tr, zones[[s$dataset]])
    tk <- key(tr)
    for (k in seq_len(nrow(ev))) {
      r <- gtr[gtr$track == ev$track[k], ]
      r <- r[order(r$frame), ]
      b <- if (ev$type[k] == "entered") r[nrow(r), ] else r[1, ]
      j <- match(key(b), tk)
      ty <- if (is.na(j)) "not tracked" else zt$type[match(tr$track[j], zt$track)]
      step <- sqrt(diff(r$x)^2 + diff(r$y)^2) / pmax(1, diff(r$frame))
      rows[[length(rows) + 1]] <- data.frame(
        dataset = s$dataset, sequence = s$id, fps = s$fps, tracker = tn, gt_track = ev$track[k],
        direction = if (ev$type[k] == "entered") "in" else "out", gt_detections = nrow(r),
        max_step_px = if (length(step)) max(step) else 0, box_size_px = stats::median(sqrt(r$width * r$height)),
        tracker_track_type = ty, recovered = identical(ty, ev$type[k]))
    }
  }
  log_msg("%s: %d movements", s$id, nrow(ev))
}
ev <- do.call(rbind, rows)
ev$step_per_box <- ev$max_step_px / ev$box_size_px
ev$fps_group <- ifelse(ev$dataset == "melipona", paste0(ev$fps, "fps"), "all")
write_csv(ev, file.path(od, "inout_diagnostic_events.csv"))

ev$step_class <- ifelse(ev$step_per_box > 1, "more than one box length", "up to one box length")
sm <- stats::aggregate(cbind(movements = 1, recovered = recovered) ~ dataset + fps_group + tracker + step_class,
                       data = ev, FUN = sum)
sm$share_recovered <- sm$recovered / sm$movements
gt_steps <- unique(ev[, c("dataset", "fps_group", "sequence", "gt_track", "direction", "max_step_px",
                          "box_size_px", "step_per_box")])
st <- stats::aggregate(cbind(median_max_step_px = max_step_px, median_box_size_px = box_size_px,
                             median_step_per_box = step_per_box) ~ dataset + fps_group, data = gt_steps,
                       FUN = stats::median)
st$movements <- as.integer(table(paste(gt_steps$dataset, gt_steps$fps_group))[paste(st$dataset, st$fps_group)])
st$share_over_one_box <- as.numeric(tapply(gt_steps$step_per_box > 1, paste(gt_steps$dataset, gt_steps$fps_group),
                                           mean)[paste(st$dataset, st$fps_group)])
write_csv(sm[order(sm$dataset, sm$fps_group, sm$tracker, sm$step_class), ], file.path(od, "inout_diagnostic_summary.csv"))
write_csv(st, file.path(od, "inout_diagnostic_steps.csv"))
log_msg("in/out diagnostic: %d movement-tracker pairs", nrow(ev))
