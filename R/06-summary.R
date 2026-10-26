#' Summary of each track
#'
#' @param tr Tracks from [it_track()].
#' @param width,height Frame size in pixels, used to classify where a track
#'   starts and ends (`left`, `right`, `top`, `bottom` edge or `interior`).
#' @param edge Distance (pixels) from the frame border counted as edge.
#' @param scale Pixels per unit of length (e.g. pixels per mm); lengths and
#'   speeds are divided by it.
#' @return One row per track: number of detections, start, end and duration
#'   (s), path length, net displacement, straightness (net / path), mean and
#'   maximum speed, start and end positions and edges, and the most frequent
#'   class when the tracks have a `class` column.
#' @export
it_track_summary <- function(tr, width = NULL, height = NULL, edge = 20, scale = 1) {
  d <- tr[!is.na(tr$track) & !is.na(tr$x) & !is.na(tr$y), , drop = FALSE]
  if (!nrow(d)) return(data.frame())
  d <- d[order(d$track, d$time), , drop = FALSE]
  where <- function(x, y) {
    if (is.null(width) || is.null(height)) return(rep(NA_character_, length(x)))
    e <- cbind(left = x, right = width - x, top = y, bottom = height - y)
    k <- max.col(-e, ties.method = "first")
    ifelse(apply(e, 1, min) <= edge, colnames(e)[k], "interior")
  }
  sp <- split(seq_len(nrow(d)), d$track)
  res <- do.call(rbind, lapply(sp, function(r) {
    x <- d$x[r]; y <- d$y[r]; t <- d$time[r]
    st <- sqrt(diff(x)^2 + diff(y)^2) / scale
    dtt <- diff(t)
    path <- sum(st)
    dur <- t[length(t)] - t[1]
    net <- sqrt((x[length(x)] - x[1])^2 + (y[length(y)] - y[1])^2) / scale
    spd <- if (length(st)) st[dtt > 0] / dtt[dtt > 0] else numeric()
    data.frame(track = d$track[r[1]], n = length(r), start = t[1], end = t[length(t)],
               duration = dur, path_length = path, net_displacement = net,
               straightness = if (path > 0) net / path else NA_real_,
               mean_speed = if (dur > 0) path / dur else NA_real_,
               max_speed = if (length(spd)) max(spd) else NA_real_,
               x_start = x[1], y_start = y[1], x_end = x[length(x)], y_end = y[length(y)])
  }))
  res$start_edge <- where(res$x_start, res$y_start)
  res$end_edge <- where(res$x_end, res$y_end)
  if ("class" %in% names(d)) {
    res$class <- vapply(sp, function(r) names(which.max(table(d$class[r]))), "")
  }
  rownames(res) <- NULL
  res
}

#' Activity time series
#'
#' Counts per time bin: mean and maximum number of tracked insects per frame
#' (the maximum is the MaxN abundance index of camera-trap studies), number
#' of tracks present and number of tracks that start in the bin.
#'
#' @param tr Tracks from [it_track()].
#' @param frames Data frame with all analysed frames (`frame`, `time`), so that
#'   frames without insects count as zero.
#' @param bin Bin width (s).
#' @param tracked_only Count only detections assigned to a track.
#' @return One row per bin.
#' @export
it_activity <- function(tr, frames, bin = 1, tracked_only = TRUE) {
  d <- if (tracked_only) tr[!is.na(tr$track), , drop = FALSE] else tr
  t0 <- min(frames$time)
  fb <- floor((frames$time - t0) / bin)
  nper <- table(factor(d$frame, levels = frames$frame))
  cnt <- as.numeric(nper)
  bins <- sort(unique(fb))
  firsts <- if (nrow(d) && "track" %in% names(d)) tapply(d$time, d$track, min) else numeric()
  fbt <- floor((firsts - t0) / bin)
  out <- data.frame(bin_start = t0 + bins * bin,
                    n_frames = as.integer(table(factor(fb, levels = bins))),
                    mean_count = as.numeric(tapply(cnt, factor(fb, levels = bins), mean)),
                    max_count = as.numeric(tapply(cnt, factor(fb, levels = bins), max)))
  db <- floor((d$time - t0) / bin)
  out$n_tracks <- if (nrow(d) && "track" %in% names(d)) {
    as.integer(vapply(bins, function(b) length(unique(d$track[db == b])), 1L))
  } else 0L
  out$n_new_tracks <- as.integer(vapply(bins, function(b) sum(fbt == b), 1L))
  out
}
