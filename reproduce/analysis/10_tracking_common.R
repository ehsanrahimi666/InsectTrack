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

# Movements into and out of the entrance zone, matched between tracks and annotations ----------
# Tracker boxes are matched one-to-one to annotated boxes in each frame (IoU >= iou_min). A tracker
# movement and an annotated movement in the same direction correspond when their tracks share
# matched boxes; the pairs that share most boxes are chosen by optimal assignment. Annotated
# movements without a counterpart are missed; tracker movements without one are spurious.

box_corners <- function(d) {
  if (all(c("x0", "y0", "x1", "y1") %in% names(d))) return(cbind(d$x0, d$y0, d$x1, d$y1))
  cbind(d$x - d$width / 2, d$y - d$height / 2, d$x + d$width / 2, d$y + d$height / 2)
}

iou_matrix <- function(a, b) {
  inter <- pmax(0, outer(a[, 3], b[, 3], pmin) - outer(a[, 1], b[, 1], pmax)) *
    pmax(0, outer(a[, 4], b[, 4], pmin) - outer(a[, 2], b[, 2], pmax))
  area_a <- (a[, 3] - a[, 1]) * (a[, 4] - a[, 2]); area_b <- (b[, 3] - b[, 1]) * (b[, 4] - b[, 2])
  inter / (outer(area_a, area_b, "+") - inter)
}

# one-to-one assignment of rows to columns maximising the total weight, over weights >= min_w
assign_max <- function(w, min_w) {
  w[w < min_w] <- 0
  if (!length(w) || max(w) <= 0) return(matrix(integer(), 0, 2))
  flip <- nrow(w) > ncol(w)
  m <- if (flip) t(w) else w
  p <- cbind(seq_len(nrow(m)), as.integer(clue::solve_LSAP(m, maximum = TRUE)))
  p <- p[m[p] > 0, , drop = FALSE]
  if (flip) p[, 2:1, drop = FALSE] else p
}

match_boxes <- function(tr, g, iou_min = 0.5) {
  bt <- box_corners(tr); bg <- box_corners(g)
  st <- split(seq_len(nrow(tr)), tr$frame); sg <- split(seq_len(nrow(g)), g$frame)
  pairs <- lapply(intersect(names(st), names(sg)), function(f) {
    i <- st[[f]]; j <- sg[[f]]
    p <- assign_max(iou_matrix(bt[i, , drop = FALSE], bg[j, , drop = FALSE]), iou_min)
    if (nrow(p)) cbind(i[p[, 1]], j[p[, 2]]) else NULL
  })
  m <- do.call(rbind, pairs)
  if (is.null(m)) m <- matrix(integer(), 0, 2)
  colnames(m) <- c("tr_row", "gt_row")
  m
}

zone_movements <- function(d, zone) {
  z <- it_zone_transitions(d, zone)
  z <- z[z$type %in% c("entered", "exited"), , drop = FALSE]
  data.frame(track = z$track, direction = ifelse(z$type == "entered", "in", "out"), stringsAsFactors = FALSE)
}

# gtr: annotated tracks (track column); tr: tracker output. Returns the annotated movements with
# reproduced / failure and the tracker movements with their counterpart or cause.
match_movements <- function(tr, gtr, zone, iou_min = 0.5) {
  tr <- tr[!is.na(tr$track), , drop = FALSE]
  mg <- zone_movements(gtr, zone); mt <- zone_movements(tr, zone)
  pm <- match_boxes(tr, gtr, iou_min)
  pt <- tr$track[pm[, "tr_row"]]; pg <- gtr$track[pm[, "gt_row"]]
  w <- matrix(0, nrow(mt), nrow(mg))
  if (nrow(mt) && nrow(mg) && nrow(pm)) {
    sh <- table(factor(pt, levels = mt$track), factor(pg, levels = mg$track))
    w <- unclass(sh)[, , drop = FALSE] * outer(mt$direction, mg$direction, "==")
  }
  a <- assign_max(w, 1)
  mg$reproduced <- seq_len(nrow(mg)) %in% a[, 2]
  mg$tracker_track <- rep(NA_integer_, nrow(mg)); mg$tracker_track[a[, 2]] <- mt$track[a[, 1]]
  inside_gt <- it_point_in_polygon(gtr$x, gtr$y, zone$x, zone$y)
  mg$failure <- vapply(seq_len(nrow(mg)), function(k) {
    if (mg$reproduced[k]) return("")
    rows <- pm[pg == mg$track[k], "gt_row"]
    if (!length(rows)) return("not tracked")
    tin <- unique(tr$track[pm[pg == mg$track[k] & inside_gt[pm[, "gt_row"]], "tr_row"]])
    tout <- unique(tr$track[pm[pg == mg$track[k] & !inside_gt[pm[, "gt_row"]], "tr_row"]])
    if (!length(tin)) return("no tracked box inside the zone")
    if (!length(tout)) return("no tracked box outside the zone")
    if (!length(intersect(tin, tout))) return("track broken at the zone edge")
    "other"
  }, "")
  mt$matched <- seq_len(nrow(mt)) %in% a[, 1]
  mt$cause <- vapply(seq_len(nrow(mt)), function(k) {
    if (mt$matched[k]) return("")
    gs <- unique(pg[pt == mt$track[k]])
    if (!length(gs)) return("false detections")
    same <- mg$track[mg$direction == mt$direction[k]]
    if (any(gs %in% same)) "duplicate of an annotated movement" else "annotated bee without this movement"
  }, "")
  list(gt = mg, tracker = mt)
}
