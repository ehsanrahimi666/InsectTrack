# Box-based trackers: SORT and ByteTrack implemented in R, and the trackers
# of the Python package 'BoxMOT' called through 'reticulate'.

#' Detection boxes as a matrix
#'
#' Boxes are taken from columns `x0`, `y0`, `x1`, `y1` (corners), else from
#' `xmin`, `ymin`, `xmax`, `ymax`, else from `width` and `height` (or `w` and
#' `h`) around the centre `x`, `y`. Without any box columns, squares of side `box_size`
#' centred on the detections are used.
#'
#' @param det Detections.
#' @param box_size Side (pixels) of the squares used when there are no boxes;
#'   default twice the square root of the median blob area, or 20.
#' @return A numeric matrix with columns `x0`, `y0`, `x1`, `y1`.
#' @export
it_det_boxes <- function(det, box_size = NULL) {
  b <- if (all(c("x0", "y0", "x1", "y1") %in% names(det))) {
    cbind(det$x0, det$y0, det$x1, det$y1)
  } else if (all(c("xmin", "ymin", "xmax", "ymax") %in% names(det))) {
    cbind(det$xmin, det$ymin, det$xmax, det$ymax)
  } else if (all(c("width", "height") %in% names(det))) {
    cbind(det$x - det$width / 2, det$y - det$height / 2, det$x + det$width / 2, det$y + det$height / 2)
  } else if (all(c("w", "h") %in% names(det))) {
    cbind(det$x - det$w / 2, det$y - det$h / 2, det$x + det$w / 2, det$y + det$h / 2)
  } else {
    if (is.null(box_size)) {
      box_size <- if ("area" %in% names(det) && nrow(det)) 2 * sqrt(stats::median(det$area)) else 20
    }
    h <- box_size / 2
    cbind(det$x - h, det$y - h, det$x + h, det$y + h)
  }
  colnames(b) <- c("x0", "y0", "x1", "y1")
  b
}

#' Intersection over union of two sets of boxes
#'
#' @param a,b Matrices with columns x0, y0, x1, y1.
#' @return A matrix of IoU values (rows `a`, columns `b`).
#' @examples
#' it_box_iou(rbind(c(0, 0, 10, 10)), rbind(c(0, 0, 10, 10), c(5, 0, 15, 10)))
#' @export
it_box_iou <- function(a, b) {
  a <- matrix(a, ncol = 4); b <- matrix(b, ncol = 4)
  if (!nrow(a) || !nrow(b)) return(matrix(0, nrow(a), nrow(b)))
  iw <- pmax(0, outer(a[, 3], b[, 3], pmin) - outer(a[, 1], b[, 1], pmax))
  ih <- pmax(0, outer(a[, 4], b[, 4], pmin) - outer(a[, 2], b[, 2], pmax))
  inter <- iw * ih
  aa <- (a[, 3] - a[, 1]) * (a[, 4] - a[, 2])
  ab <- (b[, 3] - b[, 1]) * (b[, 4] - b[, 2])
  u <- outer(aa, ab, "+") - inter
  ifelse(u > 0, inter / u, 0)
}

# Optimal one-to-one assignment accepting only pairs with cost <= thresh.
assign_lsap <- function(C, thresh) {
  n <- nrow(C); m <- ncol(C)
  if (!n || !m) return(matrix(integer(), 0, 2))
  ok <- C <= thresh
  if (!any(ok)) return(matrix(integer(), 0, 2))
  BIG <- 1e6
  M <- matrix(BIG, n + m, n + m)
  Cc <- C; Cc[!ok] <- BIG
  M[seq_len(n), seq_len(m)] <- Cc
  M[cbind(seq_len(n), m + seq_len(n))] <- thresh
  M[cbind(n + seq_len(m), seq_len(m))] <- thresh
  M[n + seq_len(m), m + seq_len(n)] <- 0
  s <- as.integer(clue::solve_LSAP(M))
  pr <- cbind(seq_len(n), s[seq_len(n)])
  pr <- pr[pr[, 2] <= m, , drop = FALSE]
  pr[ok[pr], , drop = FALSE]
}

# Constant-velocity Kalman filter on box centre, aspect ratio and height, as
# used by SORT-family trackers (state x, y, a, h and their velocities).
kf_w_pos <- 1 / 20
kf_w_vel <- 1 / 160
kf_F <- local({ F <- diag(8); F[cbind(1:4, 5:8)] <- 1; F })
kf_H <- cbind(diag(4), matrix(0, 4, 4))

box_to_xyah <- function(b) c((b[1] + b[3]) / 2, (b[2] + b[4]) / 2, (b[3] - b[1]) / max(b[4] - b[2], 1e-6), b[4] - b[2])
xyah_to_box <- function(m) { w <- m[3] * m[4]; c(m[1] - w / 2, m[2] - m[4] / 2, m[1] + w / 2, m[2] + m[4] / 2) }

kf_init <- function(b) {
  z <- box_to_xyah(b); h <- z[4]
  s <- c(2 * kf_w_pos * h, 2 * kf_w_pos * h, 1e-2, 2 * kf_w_pos * h,
         10 * kf_w_vel * h, 10 * kf_w_vel * h, 1e-5, 10 * kf_w_vel * h)
  list(mean = c(z, 0, 0, 0, 0), cov = diag(s^2))
}

kf_predict <- function(k) {
  h <- k$mean[4]
  q <- c(kf_w_pos * h, kf_w_pos * h, 1e-2, kf_w_pos * h, kf_w_vel * h, kf_w_vel * h, 1e-5, kf_w_vel * h)
  k$mean <- as.numeric(kf_F %*% k$mean)
  k$cov <- kf_F %*% k$cov %*% t(kf_F) + diag(q^2)
  k
}

kf_update <- function(k, b) {
  z <- box_to_xyah(b); h <- k$mean[4]
  r <- c(kf_w_pos * h, kf_w_pos * h, 1e-1, kf_w_pos * h)
  S <- kf_H %*% k$cov %*% t(kf_H) + diag(r^2)
  K <- k$cov %*% t(kf_H) %*% solve(S)
  k$mean <- as.numeric(k$mean + K %*% (z - as.numeric(kf_H %*% k$mean)))
  k$cov <- k$cov - K %*% S %*% t(K)
  k
}

frame_sequence <- function(det, frames) {
  if (!is.null(frames)) return(sort(unique(frames)))
  f <- sort(unique(det$frame))
  if (length(f) && all(f == round(f))) seq(min(f), max(f)) else f
}

finish_tracks <- function(det, track, min_length, info) {
  if (min_length > 1) {
    cnt <- table(track)
    track[track %in% as.integer(names(cnt)[cnt < min_length])] <- NA_integer_
  }
  det$track <- match(track, sort(unique(track[!is.na(track)])))
  attr(det, "tracking") <- info
  det
}

#' Track detections with SORT
#'
#' Simple Online and Realtime Tracking (Bewley et al. 2016): a constant-velocity
#' Kalman filter predicts the box of every track, and boxes are matched to
#' detections by intersection over union (Hungarian algorithm). A track is
#' confirmed after `min_hits` consecutive matches and deleted after `max_age`
#' frames without a match. All detections of a confirmed track, including
#' those before confirmation, receive its id.
#'
#' @param det Detections (`frame`, `time`, `x`, `y`, boxes; see
#'   [it_det_boxes()]).
#' @param iou_min Minimum IoU for a match.
#' @param min_hits Matches needed to confirm a track.
#' @param max_age Frames a track survives without a match.
#' @param min_length Minimum number of detections per track.
#' @param frames Frame numbers analysed (frames without detections still
#'   advance the filters); default all integers from the first to the last
#'   frame with detections.
#' @param box_size Passed to [it_det_boxes()].
#' @return The detections with an integer column `track` (`NA` when not
#'   assigned to a confirmed track).
#' @export
it_track_sort <- function(det, iou_min = 0.3, min_hits = 3, max_age = 30, min_length = 1,
                          frames = NULL, box_size = NULL) {
  det <- det[order(det$frame), , drop = FALSE]
  rownames(det) <- NULL
  B <- it_det_boxes(det, box_size)
  track <- rep(NA_integer_, nrow(det))
  rows_by_frame <- split(seq_len(nrow(det)), det$frame)
  tr <- list(); next_id <- 1L
  for (f in frame_sequence(det, frames)) {
    rows <- rows_by_frame[[as.character(f)]]
    if (is.null(rows)) rows <- integer()
    for (i in seq_along(tr)) tr[[i]]$kf <- kf_predict(tr[[i]]$kf)
    pb <- if (length(tr)) t(vapply(tr, function(z) xyah_to_box(z$kf$mean), numeric(4))) else matrix(0, 0, 4)
    m <- assign_lsap(1 - it_box_iou(pb, B[rows, , drop = FALSE]), 1 - iou_min)
    hit <- rep(FALSE, length(tr)); used <- rep(FALSE, length(rows))
    for (k in seq_len(nrow(m))) {
      i <- m[k, 1]; j <- m[k, 2]
      tr[[i]]$kf <- kf_update(tr[[i]]$kf, B[rows[j], ])
      tr[[i]]$hits <- tr[[i]]$hits + 1L; tr[[i]]$age <- 0L
      tr[[i]]$rows <- c(tr[[i]]$rows, rows[j])
      hit[i] <- TRUE; used[j] <- TRUE
    }
    for (i in which(!hit)) { tr[[i]]$age <- tr[[i]]$age + 1L; tr[[i]]$hits <- 0L }
    for (j in which(!used)) {
      tr[[length(tr) + 1]] <- list(kf = kf_init(B[rows[j], ]), hits = 1L, age = 0L, rows = rows[j],
                                   id = next_id, confirmed = FALSE)
      next_id <- next_id + 1L
    }
    for (i in seq_along(tr)) {
      if (!tr[[i]]$confirmed && tr[[i]]$hits >= min_hits) tr[[i]]$confirmed <- TRUE
      if (tr[[i]]$confirmed) track[tr[[i]]$rows] <- tr[[i]]$id
    }
    tr <- tr[vapply(tr, function(z) z$age <= max_age, TRUE)]
  }
  finish_tracks(det, track, min_length, list(method = "sort", iou_min = iou_min, min_hits = min_hits,
                                             max_age = max_age, min_length = min_length))
}

#' Track detections with ByteTrack
#'
#' ByteTrack (Zhang et al. 2022) associates every detection box, not only the
#' confident ones: tracks are first matched to detections with a score of at
#' least `track_thresh`, and tracks left unmatched are then matched to the
#' low-score detections (scores between `low_thresh` and `track_thresh`),
#' which recovers insects that are partly hidden or blurred. New tracks start
#' from unmatched detections with a score of at least `new_track_thresh` and
#' are confirmed by a second match; lost tracks are kept for `track_buffer`
#' frames (at 30 frames per second; scaled with `frame_rate`). Boxes are
#' predicted by a constant-velocity Kalman filter. All detections of a
#' confirmed track receive its id.
#'
#' @inheritParams it_track_sort
#' @param score Name of the detection score column; when missing all scores
#'   are 1.
#' @param track_thresh,low_thresh,new_track_thresh Score thresholds.
#' @param match_iou Minimum IoU in the first association.
#' @param second_match_iou Minimum IoU for low-score detections.
#' @param unconfirmed_match_iou Minimum IoU to confirm a new track.
#' @param track_buffer Frames (at 30 frames per second) a lost track is kept.
#' @param frame_rate Frame rate of the analysed frames; default from `time`.
#' @param fuse_score Multiply IoU by the detection score in the first
#'   association.
#' @export
it_track_bytetrack <- function(det, score = "conf", track_thresh = 0.5, low_thresh = 0.1,
                               new_track_thresh = 0.6, match_iou = 0.2, second_match_iou = 0.5,
                               unconfirmed_match_iou = 0.3, track_buffer = 30, frame_rate = NULL,
                               fuse_score = TRUE, min_length = 1, frames = NULL, box_size = NULL) {
  det <- det[order(det$frame), , drop = FALSE]
  rownames(det) <- NULL
  B <- it_det_boxes(det, box_size)
  sc <- if (score %in% names(det)) det[[score]] else rep(1, nrow(det))
  if (is.null(frame_rate)) {
    ft <- unique(det[, c("frame", "time")])
    frame_rate <- if (nrow(ft) > 1) 1 / stats::median(diff(ft$time) / diff(ft$frame)) else 30
  }
  max_lost <- max(1L, as.integer(round(track_buffer * frame_rate / 30)))
  track <- rep(NA_integer_, nrow(det))
  rows_by_frame <- split(seq_len(nrow(det)), det$frame)
  tr <- list(); next_id <- 1L; first <- TRUE
  state <- function() if (length(tr)) vapply(tr, function(z) z$state, "") else character()
  pred_boxes <- function(idx) if (length(idx)) t(vapply(tr[idx], function(z) xyah_to_box(z$kf$mean), numeric(4))) else matrix(0, 0, 4)
  update_track <- function(i, row, f) {
    tr[[i]]$kf <<- kf_update(tr[[i]]$kf, B[row, ])
    tr[[i]]$rows <<- c(tr[[i]]$rows, row)
    tr[[i]]$state <<- "tracked"; tr[[i]]$last <<- f
  }
  for (f in frame_sequence(det, frames)) {
    rows <- rows_by_frame[[as.character(f)]]
    if (is.null(rows)) rows <- integer()
    s <- sc[rows]
    high <- rows[s >= track_thresh]; low <- rows[s >= low_thresh & s < track_thresh]
    for (i in seq_along(tr)) tr[[i]]$kf <- kf_predict(tr[[i]]$kf)
    st <- state(); act <- if (length(tr)) vapply(tr, function(z) z$activated, TRUE) else logical()
    pool <- which(act & st %in% c("tracked", "lost"))
    unconf <- which(!act & st == "tracked")
    # first association: confirmed and lost tracks with high-score detections
    cost <- 1 - it_box_iou(pred_boxes(pool), B[high, , drop = FALSE])
    if (fuse_score && length(pool) && length(high)) cost <- 1 - (1 - cost) * rep(sc[high], each = length(pool))
    m1 <- assign_lsap(cost, 1 - match_iou)
    for (k in seq_len(nrow(m1))) update_track(pool[m1[k, 1]], high[m1[k, 2]], f)
    u_pool <- setdiff(seq_along(pool), m1[, 1]); u_high <- setdiff(seq_along(high), m1[, 2])
    # second association: remaining tracked (not lost) tracks with low-score detections
    r_tr <- pool[u_pool][vapply(tr[pool[u_pool]], function(z) z$state == "tracked", TRUE)]
    m2 <- assign_lsap(1 - it_box_iou(pred_boxes(r_tr), B[low, , drop = FALSE]), 1 - second_match_iou)
    for (k in seq_len(nrow(m2))) update_track(r_tr[m2[k, 1]], low[m2[k, 2]], f)
    for (i in setdiff(r_tr, r_tr[m2[, 1]])) if (tr[[i]]$state == "tracked") tr[[i]]$state <- "lost"
    # unconfirmed tracks with the remaining high-score detections
    rh <- high[u_high]
    m3 <- assign_lsap(1 - it_box_iou(pred_boxes(unconf), B[rh, , drop = FALSE]), 1 - unconfirmed_match_iou)
    for (k in seq_len(nrow(m3))) {
      i <- unconf[m3[k, 1]]
      update_track(i, rh[m3[k, 2]], f)
      tr[[i]]$activated <- TRUE
    }
    for (i in setdiff(unconf, unconf[m3[, 1]])) tr[[i]]$state <- "removed"
    # new tracks
    for (j in setdiff(seq_along(rh), m3[, 2])) {
      r <- rh[j]
      if (sc[r] < new_track_thresh) next
      tr[[length(tr) + 1]] <- list(kf = kf_init(B[r, ]), rows = r, state = "tracked", last = f,
                                   activated = first, id = next_id)
      next_id <- next_id + 1L
    }
    for (i in seq_along(tr)) if (tr[[i]]$state == "lost" && f - tr[[i]]$last > max_lost) tr[[i]]$state <- "removed"
    keep <- vapply(tr, function(z) z$state != "removed" || z$activated, TRUE)
    for (i in which(vapply(tr, function(z) z$activated, TRUE))) track[tr[[i]]$rows] <- tr[[i]]$id
    tr <- tr[keep & vapply(tr, function(z) z$state != "removed", TRUE)]
    if (length(rows)) first <- FALSE
  }
  finish_tracks(det, track, min_length,
                list(method = "bytetrack", track_thresh = track_thresh, low_thresh = low_thresh,
                     new_track_thresh = new_track_thresh, match_iou = match_iou,
                     second_match_iou = second_match_iou, unconfirmed_match_iou = unconfirmed_match_iou,
                     track_buffer = track_buffer, frame_rate = frame_rate, fuse_score = fuse_score,
                     min_length = min_length))
}

#' Track detections with a BoxMOT tracker
#'
#' Calls a tracker of the Python package 'BoxMOT' (Broström 2025) through
#' 'reticulate': `"bytetrack"`, `"ocsort"`, `"botsort"`, `"boosttrack"`,
#' `"sfsort"`, `"hybridsort"`, `"occluboost"`, and with re-identification
#' weights `"strongsort"` and `"deepocsort"`. Trackers that compensate camera
#' motion or use appearance need the frames (`files`).
#'
#' @param det Detections with boxes (see [it_det_boxes()]).
#' @param method Tracker name.
#' @param frame_size Frame height and width (pixels); not needed with `files`.
#' @param files Optional frame image files, one per analysed frame, in the
#'   order of `frames`.
#' @param params Named list of tracker arguments (see the BoxMOT
#'   documentation); `frame_rate` is set from the frame times when the tracker
#'   accepts it.
#' @param score Name of the score column (default all 1).
#' @param class Name of a class column (default all 0).
#' @inheritParams it_track_sort
#' @return The detections with an integer column `track`.
#' @export
it_track_boxmot <- function(det, method = "ocsort", frame_size = NULL, files = NULL, params = list(),
                            score = "conf", class = "class", min_length = 1, frames = NULL,
                            box_size = NULL) {
  be <- it_backend()
  det <- det[order(det$frame), , drop = FALSE]
  rownames(det) <- NULL
  fs <- frame_sequence(det, frames)
  if (!is.null(files) && length(files) != length(fs)) stop("'files' must have one file per analysed frame")
  B <- it_det_boxes(det, box_size)
  sc <- if (score %in% names(det)) det[[score]] else rep(1, nrow(det))
  cl <- if (class %in% names(det)) as.integer(factor(det[[class]])) - 1L else rep(0L, nrow(det))
  rows_by_frame <- split(seq_len(nrow(det)), det$frame)
  lst <- lapply(fs, function(f) {
    r <- rows_by_frame[[as.character(f)]]
    if (is.null(r)) return(matrix(0, 0, 6))
    cbind(B[r, , drop = FALSE], sc[r], cl[r])
  })
  if (is.null(frame_size) && is.null(files)) frame_size <- c(ceiling(max(B[, 4])) + 1, ceiling(max(B[, 3])) + 1)
  ft <- unique(det[, c("frame", "time")])
  fr <- if (nrow(ft) > 1) 1 / stats::median(diff(ft$time) / diff(ft$frame)) else 30
  if (method %in% c("bytetrack", "botsort") && is.null(params$frame_rate)) params$frame_rate <- as.integer(round(fr))
  out <- be$track_boxmot(lapply(lst, function(m) reticulate::np_array(m, dtype = "float32")),
                         frames = if (is.null(files)) NULL else as.list(files), method = method,
                         frame_size = if (is.null(frame_size)) NULL else as.integer(frame_size),
                         params = params)
  out <- reticulate::py_to_r(out)
  track <- rep(NA_integer_, nrow(det))
  if (length(out) && nrow(out)) {
    for (k in seq_len(nrow(out))) {
      di <- out[k, 9]
      if (di < 0) next
      r <- rows_by_frame[[as.character(fs[out[k, 1] + 1])]]
      if (!is.null(r) && di + 1 <= length(r)) track[r[di + 1]] <- as.integer(out[k, 2])
    }
  }
  finish_tracks(det, track, min_length, c(list(method = paste0("boxmot_", method), min_length = min_length), params))
}
