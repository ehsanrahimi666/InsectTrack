# Sequences, annotations, detections, trackers and hive-entrance zones of the
# tracking benchmark (sourced after 10_common.R by 12_tracking_benchmark.R and
# 18_inout_diagnostic.R).

tb <- bcfg$tracking

mel_ids <- sub("_gt\\.csv$", "", list.files(file.path(root, "output", "melipona"), pattern = "_gt\\.csv$"))
all_seqs <- rbind(
  data.frame(dataset = "melipona", id = mel_ids, fps = as.numeric(sub(".*_(\\d+)fps$", "\\1", mel_ids)),
             W = 1280, H = 720),
  data.frame(dataset = "native_bee", id = sort(list.files(file.path(root, bcfg$paths$native_bee))),
             fps = tb$frame_rate$native_bee, W = 1280, H = 720))

read_gt <- function(s) {
  if (s$dataset == "melipona") {
    g <- utils::read.csv(file.path(root, "output", "melipona", paste0(s$id, "_gt.csv")))
  } else {
    lab <- list.files(file.path(root, bcfg$paths$native_bee, s$id, "labels"), pattern = "\\.txt$", full.names = TRUE)
    g <- it_read_yolo_labels(lab, s$W, s$H, frame_regex = "_frame_(\\d+)", first_field = "id")
    g <- g[!duplicated(g[, c("frame", "id")]), ]
    g$width <- g$w; g$height <- g$h
  }
  g$time <- (g$frame - 1) / s$fps
  g[, c("frame", "time", "x", "y", "id", "width", "height")]
}

read_det <- function(s, input) {
  f <- file.path(bout("detections", s$dataset), paste0(s$id, "__", input, ".csv"))
  if (!file.exists(f)) return(NULL)
  d <- utils::read.csv(f)
  data.frame(frame = d$frame, time = (d$frame - 1) / s$fps, x = (d$x1 + d$x2) / 2, y = (d$y1 + d$y2) / 2,
             x0 = d$x1, y0 = d$y1, x1 = d$x2, y1 = d$y2, conf = d$score, class = d$class)
}

gt_as_det <- function(g) {
  data.frame(frame = g$frame, time = g$time, x = g$x, y = g$y, x0 = g$x - g$width / 2, y0 = g$y - g$height / 2,
             x1 = g$x + g$width / 2, y1 = g$y + g$height / 2, conf = 1, class = 0)
}

# frames of a sequence, including frames without annotations or detections
seq_frames <- function(s, g, det = NULL) {
  nfr <- if (s$dataset == "melipona") 1200 / (120 / s$fps) else max(g$frame)
  seq_len(max(nfr, max(g$frame), if (is.null(det) || !nrow(det)) 0 else max(det$frame)))
}

boxmot_params <- function(name, thr, fps) {
  p <- tb$trackers[[name]] %||% list()
  m <- sub("^boxmot_", "", name)
  extra <- switch(m,
    bytetrack = list(track_thresh = thr$high, min_conf = thr$low, frame_rate = as.integer(fps)),
    ocsort = list(det_thresh = thr$high, min_conf = thr$low),
    botsort = list(track_high_thresh = thr$high, track_low_thresh = thr$low, new_track_thresh = thr$new,
                   frame_rate = as.integer(fps)),
    boosttrack = list(det_thresh = thr$high),
    sfsort = list(high_th = thr$high, low_th = thr$low, new_track_th = thr$new),
    hybridsort = list(track_thresh = thr$high, low_thresh = thr$low, det_thresh = thr$high))
  utils::modifyList(extra, p)
}

run_tracker <- function(name, det, thr, s, fs, bt_override = list()) {
  p <- tb$trackers[[sub("_stitch$", "", name)]]
  hi <- det[det$conf >= thr$high, , drop = FALSE]
  if (name == "native_hungarian") {
    return(it_track(hi, "hungarian", gate = p$gate, max_gap = p$max_gap, min_length = p$min_length,
                    velocity_weight = p$velocity_weight))
  }
  if (name == "native_sort") return(it_track(hi, "sort", iou_min = p$iou_min, min_hits = p$min_hits,
                                             max_age = p$max_age, frames = fs))
  if (name %in% c("native_bytetrack", "native_bytetrack_stitch")) {
    a <- utils::modifyList(list(track_thresh = thr$high, low_thresh = thr$low, new_track_thresh = thr$new,
                                match_iou = p$match_iou, second_match_iou = p$second_match_iou,
                                track_buffer = p$track_buffer, frame_rate = s$fps, frames = fs), bt_override)
    tr <- do.call(it_track, c(list(det[det$conf >= a$low_thresh, , drop = FALSE], "bytetrack"), a))
    if (name == "native_bytetrack_stitch") tr <- it_stitch(tr, max_gap = 1, max_dist = 0.05 * s$W)
    return(tr)
  }
  it_track(det[det$conf >= thr$low, , drop = FALSE], name, frame_size = c(s$H, s$W), frames = fs,
           params = boxmot_params(name, thr, s$fps))
}

trackers <- c("native_hungarian", "native_sort", "native_bytetrack", "native_bytetrack_stitch",
              paste0(names(tb$trackers)[grepl("^boxmot_", names(tb$trackers))]))

# Hive entrances: from the end points of the annotated tracks of all sequences, pooled per data set
entrance_zones <- function(seqs) {
  zones <- list()
  fe <- bcfg$in_out$find_entrance
  for (ds in unique(seqs$dataset)) {
    ss <- seqs[seqs$dataset == ds, ]
    gtall <- do.call(rbind, lapply(seq_len(nrow(ss)), function(i) {
      g <- read_gt(ss[i, ]); g$track <- paste(ss$id[i], g$id); g }))
    gtall$track <- as.integer(factor(gtall$track))
    zones[[ds]] <- it_find_entrance(gtall, ss$W[1], ss$H[1], cell = fe$cell, radius_quantile = fe$radius_quantile,
                                    min_radius = fe$min_radius)
  }
  zones
}

inout <- function(tr, zone) {
  ec <- it_entrance_counts(tr, zone)
  c(n_in = sum(ec$events$direction == "in"), n_out = sum(ec$events$direction == "out"))
}
