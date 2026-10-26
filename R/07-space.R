#' Activity heat map
#'
#' Positions are binned in square cells.
#'
#' * `"time"`: insect-seconds per cell; each detection is weighted by the time
#'   step to the next analysed frame (from `frames`).
#' * `"tracks"`: number of distinct tracks that entered the cell (visits).
#' * `"detections"`: number of detections.
#'
#' @param tr Tracks from [it_track()] (or detections; then `"tracks"` is not
#'   available).
#' @param width,height Frame size in pixels.
#' @param cell Cell size in pixels.
#' @param weight `"time"`, `"tracks"` or `"detections"`.
#' @param frames Data frame of analysed frames (`frame`, `time`) used for the
#'   time weights.
#' @param smooth Standard deviation (cells) of a Gaussian smoothing kernel;
#'   0 for none.
#' @param tracked_only Use only detections assigned to a track.
#' @return A list with matrix `z` (rows = y cells, from the top), cell centres
#'   `x` and `y`, and the settings.
#' @export
it_heatmap <- function(tr, width, height, cell = 10, weight = c("time", "tracks", "detections"),
                       frames = NULL, smooth = 0, tracked_only = TRUE) {
  weight <- match.arg(weight)
  d <- if (tracked_only && "track" %in% names(tr)) tr[!is.na(tr$track), , drop = FALSE] else tr
  d <- d[!is.na(d$x) & !is.na(d$y), , drop = FALSE]
  nx <- ceiling(width / cell); ny <- ceiling(height / cell)
  ix <- pmin(pmax(floor(d$x / cell) + 1, 1), nx)
  iy <- pmin(pmax(floor(d$y / cell) + 1, 1), ny)
  z <- matrix(0, ny, nx)
  if (nrow(d)) {
    w <- switch(weight,
      detections = rep(1, nrow(d)),
      time = {
        if (is.null(frames)) stop("'frames' is needed for weight = 'time'")
        ft <- sort(unique(frames$time))
        step <- c(diff(ft), stats::median(diff(ft)))
        step[match(d$time, ft)]
      },
      tracks = NULL)
    if (weight == "tracks") {
      key <- unique(data.frame(track = d$track, ix = ix, iy = iy))
      agg <- stats::aggregate(rep(1, nrow(key)), list(iy = key$iy, ix = key$ix), sum)
    } else {
      agg <- stats::aggregate(w, list(iy = iy, ix = ix), sum)
    }
    z[cbind(agg$iy, agg$ix)] <- agg$x
  }
  if (smooth > 0) z <- gauss_smooth(z, smooth)
  list(z = z, x = (seq_len(nx) - 0.5) * cell, y = (seq_len(ny) - 0.5) * cell,
       cell = cell, weight = weight, width = width, height = height, smooth = smooth)
}

gauss_smooth <- function(z, sigma) {
  r <- ceiling(3 * sigma)
  k <- stats::dnorm(-r:r, sd = sigma); k <- k / sum(k)
  pad <- function(v) c(rep(0, r), v, rep(0, r))
  conv <- function(v) {
    f <- stats::filter(pad(v), k, sides = 2)
    as.numeric(f[(r + 1):(r + length(v))])
  }
  z <- t(apply(z, 1, conv))
  apply(z, 2, conv)
}

#' Hot spots of activity
#'
#' Getis-Ord Gi* statistic of a heat map computed over a square
#' neighbourhood of half-width `radius` cells (including the cell itself),
#' with Benjamini-Hochberg adjusted one-sided p-values. Cells are not
#' independent samples (one insect contributes to many neighbouring cells
#' and frames), so the p-values are descriptive; inference about hot spots
#' should be made at the level of tracks or recording sessions.
#'
#' @param hm Heat map from [it_heatmap()].
#' @param radius Neighbourhood half-width in cells.
#' @param alpha Threshold for the adjusted p-value.
#' @return The heat map list with matrices `gi` (z-scores), `p_adj` and logical
#'   `hot`.
#' @export
it_hotspots <- function(hm, radius = 2, alpha = 0.05) {
  z <- hm$z
  n <- length(z)
  xbar <- mean(z)
  s <- sqrt(mean(z^2) - xbar^2)
  wcount <- box_count(nrow(z), ncol(z), radius)
  wsum <- it_box_mean(z, radius) * wcount
  gi <- (wsum - xbar * wcount) / (s * sqrt((n * wcount - wcount^2) / (n - 1)))
  gi[!is.finite(gi)] <- 0
  p <- stats::pnorm(gi, lower.tail = FALSE)
  padj <- matrix(stats::p.adjust(p, method = "BH"), nrow(z))
  hm$gi <- gi
  hm$p_adj <- padj
  hm$hot <- padj < alpha & gi > 0
  hm$radius <- radius
  hm
}

box_count <- function(nr, nc, r) {
  ci <- pmin(seq_len(nr) + r, nr) - pmax(seq_len(nr) - r, 1) + 1
  cj <- pmin(seq_len(nc) + r, nc) - pmax(seq_len(nc) - r, 1) + 1
  outer(ci, cj)
}
