# Post hoc diagnostic of the hive-entrance counts. The movements into and out
# of the entrance zone of every tracker are matched one-to-one to those of the
# annotated tracks (match_movements() in 10_tracking_common.R), so that each
# count splits into reproduced, missed and spurious movements. Inputs: the
# annotated boxes and the YOLO26n detections of the Melipona sequences, and the
# annotated boxes of the native-bee sequences. For every annotated movement,
# the largest displacement of the insect between consecutive annotated frames
# is recorded in box sizes (the square root of the box area), together with
# the number of annotated boxes outside the zone.

source(here::here("analysis", "10_common.R"))
source(here::here("analysis", "10_tracking_common.R"))
od <- bout("tracking")
zones <- entrance_zones(all_seqs)
diag_inputs <- list(melipona = c("ground_truth", "yolo_melipona"), native_bee = "ground_truth")
by_seq <- utils::read.csv(file.path(od, "metrics_by_sequence.csv"))

gt_rows <- list(); tr_rows <- list(); n_mismatch <- 0
for (i in seq_len(nrow(all_seqs))) {
  s <- all_seqs[i, ]
  zone <- zones[[s$dataset]]
  g <- read_gt(s)
  gtr <- g; gtr$track <- as.integer(factor(gtr$id))
  steps <- do.call(rbind, lapply(split(gtr, gtr$track), function(r) {
    r <- r[order(r$frame), ]
    st <- sqrt(diff(r$x)^2 + diff(r$y)^2) / pmax(1, diff(r$frame))
    data.frame(track = r$track[1], gt_detections = nrow(r), max_step_px = if (length(st)) max(st) else 0,
               box_size_px = stats::median(sqrt(r$width * r$height)),
               boxes_outside = sum(!it_point_in_polygon(r$x, r$y, zone$x, zone$y)))
  }))
  for (input in diag_inputs[[s$dataset]]) {
    det <- if (input == "ground_truth") gt_as_det(g) else read_det(s, input)
    if (is.null(det)) next
    fs <- seq_frames(s, g, det)
    thr <- tb$thresholds[[input]]
    for (tn in trackers) {
      tr <- tryCatch(run_tracker(tn, det, thr, s, fs), error = function(e) {
        log_msg("%s %s %s: %s", s$id, input, tn, conditionMessage(e)); NULL })
      if (is.null(tr)) next
      # the tracker output must reproduce the counts of the benchmark
      io <- inout(tr, zone)
      b <- by_seq[by_seq$sequence == s$id & by_seq$input == input & by_seq$tracker == tn, ]
      if (nrow(b) != 1 || b$n_in != io[["n_in"]] || b$n_out != io[["n_out"]]) n_mismatch <- n_mismatch + 1
      mm <- match_movements(tr, gtr, zone)
      key <- data.frame(dataset = s$dataset, sequence = s$id, fps = s$fps, input = input, tracker = tn)
      if (nrow(mm$gt)) {
        gm <- merge(mm$gt, steps, by = "track", sort = FALSE)
        gt_rows[[length(gt_rows) + 1]] <- cbind(key, gt_track = gm$track, direction = gm$direction,
          gt_detections = gm$gt_detections, boxes_outside = gm$boxes_outside, max_step_px = gm$max_step_px,
          box_size_px = gm$box_size_px,
          reproduced = gm$reproduced, tracker_track = gm$tracker_track, failure = gm$failure)
      }
      if (nrow(mm$tracker)) {
        tr_rows[[length(tr_rows) + 1]] <- cbind(key, track = mm$tracker$track, direction = mm$tracker$direction,
                                                matched = mm$tracker$matched, cause = mm$tracker$cause)
      }
    }
  }
  log_msg("%s done", s$id)
}
if (n_mismatch) stop(n_mismatch, " tracker runs did not reproduce the benchmark counts")

ev <- do.call(rbind, gt_rows)
ev$step_per_box <- ev$max_step_px / ev$box_size_px
ev$fps_group <- ifelse(ev$dataset == "melipona", paste0(ev$fps, "fps"), "all")
write_csv(ev, file.path(od, "inout_diagnostic_events.csv"))
tm <- do.call(rbind, tr_rows)
tm$fps_group <- ifelse(tm$dataset == "melipona", paste0(tm$fps, "fps"), "all")
write_csv(tm, file.path(od, "inout_diagnostic_tracker_movements.csv"))

# per data set, frame rate, input and tracker: reproduced, missed and spurious movements
keys <- c("dataset", "fps_group", "input", "tracker")
a <- stats::aggregate(cbind(movements = 1, reproduced = reproduced) ~ dataset + fps_group + input + tracker,
                      data = ev, FUN = sum)
b <- stats::aggregate(cbind(tracker_movements = 1, matched = matched) ~ dataset + fps_group + input + tracker,
                      data = tm, FUN = sum)
sm <- merge(a, b, by = keys, all = TRUE)
sm$tracker_movements[is.na(sm$tracker_movements)] <- 0; sm$matched[is.na(sm$matched)] <- 0
stopifnot(all(sm$matched == sm$reproduced))
sm$missed <- sm$movements - sm$reproduced
sm$spurious <- sm$tracker_movements - sm$matched
sm$recall <- sm$reproduced / sm$movements
sm$precision <- ifelse(sm$tracker_movements > 0, sm$matched / sm$tracker_movements, NA)
for (f in c("not tracked", "no tracked box inside the zone", "no tracked box outside the zone",
            "track broken at the zone edge", "other")) {
  n <- stats::aggregate(cbind(n = failure == f) ~ dataset + fps_group + input + tracker, data = ev, FUN = sum)
  names(n)[5] <- paste0("missed_", gsub(" ", "_", f))
  sm <- merge(sm, n, by = keys, all.x = TRUE)
}
for (f in c("duplicate of an annotated movement", "annotated bee without this movement", "false detections")) {
  n <- stats::aggregate(cbind(n = cause == f) ~ dataset + fps_group + input + tracker, data = tm, FUN = sum)
  names(n)[5] <- paste0("spurious_", gsub(" ", "_", f))
  sm <- merge(sm, n, by = keys, all.x = TRUE)
}
sm[is.na(sm)] <- 0
write_csv(sm[order(sm$dataset, sm$fps_group, sm$input, sm$tracker), ], file.path(od, "inout_diagnostic_summary.csv"))

# reproduced movements by the largest step of the insect
ev$step_class <- ifelse(ev$step_per_box > 1, "more than one box size", "up to one box size")
sc <- stats::aggregate(cbind(movements = 1, reproduced = reproduced) ~ dataset + fps_group + input + tracker +
                         step_class, data = ev, FUN = sum)
sc$share_reproduced <- sc$reproduced / sc$movements
write_csv(sc[order(sc$dataset, sc$fps_group, sc$input, sc$tracker, sc$step_class), ],
          file.path(od, "inout_diagnostic_step_classes.csv"))

# reproduced movements by the number of annotated boxes outside the zone (fewer than three, or three or more)
ev$outside_class <- ifelse(ev$boxes_outside < 3, "fewer than three boxes outside", "three or more boxes outside")
oc <- stats::aggregate(cbind(movements = 1, reproduced = reproduced) ~ dataset + fps_group + input + tracker +
                         outside_class, data = ev, FUN = sum)
oc$share_reproduced <- oc$reproduced / oc$movements
write_csv(oc[order(oc$dataset, oc$fps_group, oc$input, oc$tracker, oc$outside_class), ],
          file.path(od, "inout_diagnostic_outside_classes.csv"))

# largest steps and boxes outside the zone of the annotated movements (one row per movement)
gt_steps <- unique(ev[ev$input == "ground_truth", c("dataset", "fps_group", "sequence", "gt_track", "direction",
                                                    "max_step_px", "box_size_px", "step_per_box", "boxes_outside")])
st <- stats::aggregate(cbind(median_max_step_px = max_step_px, median_box_size_px = box_size_px,
                             median_step_per_box = step_per_box, median_boxes_outside = boxes_outside) ~
                         dataset + fps_group, data = gt_steps, FUN = stats::median)
grp <- paste(gt_steps$dataset, gt_steps$fps_group)
st$movements <- as.integer(table(grp)[paste(st$dataset, st$fps_group)])
st$share_over_one_box <- as.numeric(tapply(gt_steps$step_per_box > 1, grp, mean)[paste(st$dataset, st$fps_group)])
st$share_under_three_outside <- as.numeric(tapply(gt_steps$boxes_outside < 3, grp, mean)[paste(st$dataset, st$fps_group)])
write_csv(st, file.path(od, "inout_diagnostic_steps.csv"))
log_msg("in/out diagnostic: %d annotated movement-tracker pairs, %d tracker movements", nrow(ev), nrow(tm))
