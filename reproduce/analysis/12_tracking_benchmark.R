# Multi-object tracking benchmark: trackers of InsectTrack (Hungarian, SORT,
# ByteTrack) and of BoxMOT on the Melipona and native-bee sequences, with
# annotated boxes and with detections of trained and zero-shot detectors as
# input; CLEAR MOT, IDF1 and HOTA; hive-entrance in/out counts; planned
# sensitivity analysis of ByteTrack. Results are cached per sequence and
# input in output/benchmark/tracking/cache.

source(here::here("analysis", "10_common.R"))
source(here::here("analysis", "10_tracking_common.R"))
od <- bout("tracking")
cache <- file.path(od, "cache"); dir.create(cache, showWarnings = FALSE)

seqs <- all_seqs
only <- Sys.getenv("IT_ONLY", "")
if (nzchar(only)) seqs <- seqs[seqs$id %in% strsplit(only, ",")[[1]], ]
if (nzchar(Sys.getenv("IT_INPUTS", ""))) tb$inputs <- lapply(tb$inputs, intersect, strsplit(Sys.getenv("IT_INPUTS"), ",")[[1]])
if (nzchar(Sys.getenv("IT_CACHE", ""))) { cache <- Sys.getenv("IT_CACHE"); dir.create(cache, showWarnings = FALSE) }

evaluate <- function(tr, g, s) {
  md <- tb$max_dist[[s$dataset]]
  e <- it_eval_tracks(tr, g, md)
  h <- it_eval_hota(tr, g, "iou")
  list(row = cbind(e[, c("gt_objects", "pred_objects", "gt_tracks", "pred_tracks", "tp", "fp", "fn",
                         "id_switches", "mota", "idf1")], h[, c("hota", "deta", "assa", "loca")]), hota = h)
}

zones <- entrance_zones(all_seqs)
write_csv(do.call(rbind, lapply(names(zones), function(ds) data.frame(
  dataset = ds, x = attr(zones[[ds]], "centre")[["x"]], y = attr(zones[[ds]], "centre")[["y"]],
  radius = attr(zones[[ds]], "radius"), n_endpoints = attr(zones[[ds]], "n_endpoints")))),
  file.path(od, "entrance_zones.csv"))

# Main benchmark -----------------------------------------------------------------------
for (i in seq_len(nrow(seqs))) {
  s <- seqs[i, ]
  g <- read_gt(s)
  gt_tracks <- g; gt_tracks$track <- as.integer(factor(gt_tracks$id))
  gio <- inout(gt_tracks, zones[[s$dataset]])
  for (input in tb$inputs[[s$dataset]]) {
    cf <- file.path(cache, sprintf("%s__%s__%s.rds", s$dataset, s$id, input))
    if (file.exists(cf)) next
    det <- if (input == "ground_truth") gt_as_det(g) else read_det(s, input)
    if (is.null(det)) next
    fs <- seq_frames(s, g, det)
    thr <- tb$thresholds[[input]]
    dq <- if (input == "ground_truth") NULL else
      it_eval_detections(det[det$conf >= thr$high, ], g, iou = 0.5, frames = sort(unique(g$frame)))$summary
    rows <- list(); hres <- list()
    for (tn in trackers) {
      t0 <- Sys.time()
      tr <- tryCatch(run_tracker(tn, det, thr, s, fs), error = function(e) { log_msg("%s %s %s: %s", s$id, input, tn,
                                                                                  conditionMessage(e)); NULL })
      if (is.null(tr)) next
      secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      ev <- evaluate(tr, g, s)
      io <- inout(tr, zones[[s$dataset]])
      rows[[tn]] <- cbind(data.frame(dataset = s$dataset, sequence = s$id, fps = s$fps, input = input, tracker = tn,
                                     seconds = secs, n_in = io[["n_in"]], n_out = io[["n_out"]],
                                     gt_in = gio[["n_in"]], gt_out = gio[["n_out"]]), ev$row)
      hres[[tn]] <- ev$hota
    }
    saveRDS(list(rows = do.call(rbind, rows), hota = hres, det_quality = dq), cf)
    log_msg("%s %s %s done", s$dataset, s$id, input)
  }
}

# Collect and pool -------------------------------------------------------------------------
files <- list.files(cache, pattern = "^(melipona|native_bee)__.*\\.rds$", full.names = TRUE)
res <- lapply(files, readRDS)
by_seq <- do.call(rbind, lapply(res, `[[`, "rows"))
write_csv(by_seq, file.path(od, "metrics_by_sequence.csv"))
hota_list <- list()
for (r in res) for (tn in names(r$hota)) {
  k <- paste(r$rows$dataset[1], r$rows$sequence[1], r$rows$input[1], tn, sep = "|")
  hota_list[[k]] <- r$hota[[tn]]
}
dq <- do.call(rbind, lapply(seq_along(files), function(i) {
  if (is.null(res[[i]]$det_quality)) return(NULL)
  p <- strsplit(sub("\\.rds$", "", basename(files[i])), "__")[[1]]
  cbind(dataset = p[1], sequence = p[2], input = p[3], res[[i]]$det_quality)
}))
write_csv(dq, file.path(od, "detection_quality_by_sequence.csv"))

pool <- function(d, keys) {
  sp <- split(d, d[, keys], drop = TRUE)
  do.call(rbind, lapply(sp, function(z) {
    GT <- sum(z$gt_objects); P <- sum(z$pred_objects)
    idtp <- sum(z$idf1 * (z$gt_objects + z$pred_objects) / 2)
    hc <- it_combine_hota(hota_list[paste(z$dataset, z$sequence, z$input, z$tracker, sep = "|")])
    cbind(z[1, keys, drop = FALSE], sequences = nrow(z), gt_objects = GT, gt_tracks = sum(z$gt_tracks),
          pred_tracks = sum(z$pred_tracks), mota = 1 - sum(z$fn + z$fp + z$id_switches) / GT,
          idf1 = 2 * idtp / (GT + P), id_switches = sum(z$id_switches), hota = hc$hota, deta = hc$deta,
          assa = hc$assa, n_in = sum(z$n_in), n_out = sum(z$n_out), gt_in = sum(z$gt_in), gt_out = sum(z$gt_out),
          seconds = sum(z$seconds))
  }))
}
by_seq$fps_group <- ifelse(by_seq$dataset == "melipona", paste0(by_seq$fps, "fps"), "all")
pooled <- pool(by_seq, c("dataset", "fps_group", "input", "tracker"))
pooled$inout_abs_error <- abs(pooled$n_in - pooled$gt_in) + abs(pooled$n_out - pooled$gt_out)
pooled$inout_rel_error <- pooled$inout_abs_error / pmax(1, pooled$gt_in + pooled$gt_out)
rownames(pooled) <- NULL
write_csv(pooled, file.path(od, "metrics_pooled.csv"))
log_msg("tracking benchmark: %d sequence-input-tracker results", nrow(by_seq))

# Planned sensitivity of ByteTrack (detections of the primary trained model) ------------------
sens_inputs <- c(melipona = "yolo_melipona", native_bee = "yolo_bee")
variants <- list()
for (par in names(tb$sensitivity$native_bytetrack)) for (v in tb$sensitivity$native_bytetrack[[par]]) {
  variants[[paste0(par, "=", v)]] <- stats::setNames(list(v), par)
}
sf <- file.path(od, "sensitivity_bytetrack.csv")
if (!file.exists(sf) && !nzchar(Sys.getenv("IT_INPUTS", ""))) {
  sens <- list()
  for (i in seq_len(nrow(seqs))) {
    s <- seqs[i, ]; g <- read_gt(s); input <- sens_inputs[[s$dataset]]
    det <- read_det(s, input); if (is.null(det)) next
    thr <- tb$thresholds[[input]]
    fs <- seq_frames(s, g)
    for (vn in names(variants)) {
      ov <- variants[[vn]]
      if (!is.null(ov$track_thresh)) ov$new_track_thresh <- ov$track_thresh + (thr$new - thr$high)
      tr <- run_tracker("native_bytetrack", det, thr, s, fs, bt_override = ov)
      ev <- evaluate(tr, g, s)
      sens[[length(sens) + 1]] <- cbind(data.frame(dataset = s$dataset, sequence = s$id, fps = s$fps, input = input,
                                                   variant = vn), ev$row)
    }
  }
  write_csv(do.call(rbind, sens), sf)
}
log_msg("sensitivity done")
