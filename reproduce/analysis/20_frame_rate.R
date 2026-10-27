# Frame rate and tracking of stingless bees at the hive entrance: the five
# Melipona sequences annotated at 30, 40, 50, 60, 100 and 120 frames per
# second are tracked from their annotated boxes with every tracker, and
# HOTA, IDF1, MOTA, entrance counts and the displacement of the bees between
# frames are compared across frame rates. Post hoc check: the Hungarian
# tracker with its gate scaled to the frame interval.

source(here::here("analysis", "10_common.R"))
source(here::here("analysis", "10_tracking_common.R"))
od <- bout("frame_rate")
base <- file.path(root, bcfg$paths$melipona)
rates <- c(30, 40, 50, 60, 100, 120)
zone <- entrance_zones(all_seqs)[["melipona"]]
thr <- tb$thresholds$ground_truth

read_mot_gt <- function(f, fps) {
  g <- it_read_detections(f, format = "mot", fps = fps)
  lab <- trimws(readLines(file.path(dirname(f), "labels.txt"), warn = FALSE))
  lab <- lab[nzchar(lab)]
  g <- g[g$V8 %in% which(tolower(lab) == "bee"), ]   # class ids follow labels.txt; pollen loads are not bees
  g$time <- (g$frame - 1) / fps
  g$id <- g$track
  g[, c("frame", "time", "x", "y", "id", "width", "height")]
}

# sequences annotated at every frame rate
gts <- list()
for (r in rates) {
  d <- file.path(base, paste0(r, "_fps"), "partes")
  for (g in list.files(d, pattern = "_gt$", full.names = TRUE)) {
    f <- file.path(g, "gt", "gt.txt")
    if (file.exists(f)) gts[[length(gts) + 1]] <- data.frame(sequence = substr(basename(g), 1, 5), fps = r, gt = f)
  }
}
gts <- do.call(rbind, gts)
common <- names(which(table(gts$sequence) == length(rates)))
gts <- gts[gts$sequence %in% common, ]
log_msg("frame rates: %d sequences annotated at all of %s frames per second", length(common), paste(rates, collapse = ", "))

rows <- list(); hres <- list(); moves <- list(); scaled <- list(); hres_scaled <- list()
for (i in seq_len(nrow(gts))) {
  s <- data.frame(dataset = "melipona", id = paste0(gts$sequence[i], "_", gts$fps[i], "fps"), fps = gts$fps[i],
                  W = 1280, H = 720)
  g <- read_mot_gt(gts$gt[i], s$fps)
  gtr <- g; gtr$track <- as.integer(factor(gtr$id))
  gio <- inout(gtr, zone)
  # displacement between consecutive annotated frames of each movement into or out of the entrance
  zt <- it_zone_transitions(gtr, zone)
  for (k in which(zt$type %in% c("entered", "exited"))) {
    r <- gtr[gtr$track == zt$track[k], ]; r <- r[order(r$frame), ]
    step <- sqrt(diff(r$x)^2 + diff(r$y)^2) / pmax(1, diff(r$frame))
    moves[[length(moves) + 1]] <- data.frame(sequence = gts$sequence[i], fps = s$fps, gt_track = zt$track[k],
      direction = if (zt$type[k] == "entered") "in" else "out",
      max_step_px = if (length(step)) max(step) else 0, box_size_px = stats::median(sqrt(r$width * r$height)))
  }
  det <- gt_as_det(g)
  fs <- seq_len(max(10 * s$fps, max(g$frame)))
  for (tn in trackers) {
    tr <- tryCatch(run_tracker(tn, det, thr, s, fs), error = function(e) {
      log_msg("%s %s: %s", s$id, tn, conditionMessage(e)); NULL })
    if (is.null(tr)) next
    e <- it_eval_tracks(tr, g, tb$max_dist$melipona)
    h <- it_eval_hota(tr, g, "iou")
    io <- inout(tr, zone)
    rows[[length(rows) + 1]] <- cbind(data.frame(sequence = gts$sequence[i], fps = s$fps, tracker = tn,
                                                 n_in = io[["n_in"]], n_out = io[["n_out"]],
                                                 gt_in = gio[["n_in"]], gt_out = gio[["n_out"]]),
                                      e[, c("gt_objects", "pred_objects", "fp", "fn", "id_switches", "mota", "idf1")],
                                      h[, c("hota", "deta", "assa")])
    hres[[paste(s$fps, gts$sequence[i], tn)]] <- h
  }
  # post hoc check: the Hungarian tracker with its gate scaled to the frame interval (gate at 120 fps)
  p <- tb$trackers$native_hungarian
  tr <- it_track(det[det$conf >= thr$high, , drop = FALSE], "hungarian", gate = p$gate * 120 / s$fps,
                 max_gap = p$max_gap, min_length = p$min_length, velocity_weight = p$velocity_weight)
  e <- it_eval_tracks(tr, g, tb$max_dist$melipona)
  h <- it_eval_hota(tr, g, "iou")
  io <- inout(tr, zone)
  scaled[[length(scaled) + 1]] <- cbind(data.frame(sequence = gts$sequence[i], fps = s$fps,
                                                   tracker = "native_hungarian_scaled_gate", gate = p$gate * 120 / s$fps,
                                                   n_in = io[["n_in"]], n_out = io[["n_out"]],
                                                   gt_in = gio[["n_in"]], gt_out = gio[["n_out"]]),
                                        e[, c("gt_objects", "pred_objects", "fp", "fn", "id_switches", "mota", "idf1")],
                                        h[, c("hota", "deta", "assa")])
  hres_scaled[[paste(s$fps, gts$sequence[i])]] <- h
  log_msg("%s done", s$id)
}
bys <- do.call(rbind, rows)
write_csv(bys, file.path(od, "frame_rate_by_sequence.csv"))
mv <- do.call(rbind, moves)
mv$step_per_box <- mv$max_step_px / mv$box_size_px
write_csv(mv, file.path(od, "frame_rate_movements.csv"))

pool_rates <- function(bys, hl, hkey) {
  pooled <- do.call(rbind, lapply(split(bys, list(bys$fps, bys$tracker), drop = TRUE), function(z) {
    GT <- sum(z$gt_objects); P <- sum(z$pred_objects)
    idtp <- sum(z$idf1 * (z$gt_objects + z$pred_objects) / 2)
    hc <- it_combine_hota(hl[hkey(z)])
    data.frame(fps = z$fps[1], tracker = z$tracker[1], sequences = nrow(z), gt_objects = GT,
               mota = 1 - sum(z$fn + z$fp + z$id_switches) / GT, idf1 = 2 * idtp / (GT + P),
               id_switches = sum(z$id_switches), hota = hc$hota, deta = hc$deta, assa = hc$assa,
               n_in = sum(z$n_in), n_out = sum(z$n_out), gt_in = sum(z$gt_in), gt_out = sum(z$gt_out),
               inout_abs_error_seq = sum(abs(z$n_in - z$gt_in) + abs(z$n_out - z$gt_out)))
  }))
  pooled$inout_rel_error <- (abs(pooled$n_in - pooled$gt_in) + abs(pooled$n_out - pooled$gt_out)) /
    pmax(1, pooled$gt_in + pooled$gt_out)
  pooled$inout_rel_error_seq <- pooled$inout_abs_error_seq / pmax(1, pooled$gt_in + pooled$gt_out)
  pooled[order(pooled$tracker, pooled$fps), ]
}
write_csv(pool_rates(bys, hres, function(z) paste(z$fps, z$sequence, z$tracker)),
          file.path(od, "frame_rate_pooled.csv"))
sc <- do.call(rbind, scaled)
write_csv(sc, file.path(od, "frame_rate_scaled_gate_by_sequence.csv"))
write_csv(pool_rates(sc, hres_scaled, function(z) paste(z$fps, z$sequence)), file.path(od, "frame_rate_scaled_gate.csv"))
log_msg("frame-rate analysis: %d sequence-rate-tracker results", nrow(bys))
