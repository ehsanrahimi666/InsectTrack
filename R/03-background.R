#' Background image from the temporal median of frames
#'
#' Insects that move are removed by the per-pixel median of frames sampled
#' across the sequence. Insects that stay at one place for more than about
#' half of the sampled frames become part of the background and are not
#' detected by [it_detect_motion()].
#'
#' @param frames Data frame from [it_extract_frames()] or a character vector of
#'   image files.
#' @param n Number of frames sampled evenly across the sequence.
#' @param exclude Optional indices of frames that must not be used (e.g. the
#'   frame being analysed).
#' @return A grey-scale matrix with values 0-255, with the per-pixel temporal
#'   spread over the sampled frames as attribute `spread` (75th percentile of
#'   the absolute deviations from the median, scaled to equal the standard
#'   deviation for normal data; insects present in fewer than a quarter of the
#'   sampled frames do not inflate it).
#' @export
it_background <- function(frames, n = 31, exclude = NULL) {
  files <- if (is.data.frame(frames)) frames$file else frames
  pool <- setdiff(seq_along(files), exclude)
  if (!length(pool)) stop("no frames available for the background")
  idx <- pool[unique(round(seq(1, length(pool), length.out = min(n, length(pool)))))]
  first <- it_read_frame(files[idx[1]])
  d <- dim(first)
  arr <- array(0, c(d, length(idx)))
  arr[, , 1] <- first
  for (k in seq_along(idx)[-1]) arr[, , k] <- it_read_frame(files[idx[k]])
  ms <- it_median_spread_stack(arr, d[1] * d[2], length(idx))
  bg <- matrix(ms$median, d[1], d[2])
  attr(bg, "spread") <- matrix(ms$spread, d[1], d[2])
  bg
}

#' Check whether the camera moved
#'
#' Global shifts between sampled frames are estimated by phase correlation of
#' reduced grey-scale frames. Videos for which the camera moved (hand-held,
#' wind) are not suitable for background subtraction and violate the fixed
#' coordinate system assumed by heat maps and regions of interest.
#'
#' @param frames Data frame from [it_extract_frames()].
#' @param n Number of frame pairs compared.
#' @param size Width of the reduced frames used for the comparison.
#' @param lag Number of frames between the frames of a pair.
#' @return A list with the shift of each pair (in pixels of the original
#'   frame), the median and maximum shift, the share of pairs with a shift of
#'   more than `tolerance` pixels and a verdict.
#' @param tolerance Shift (pixels of the original frame) above which a pair is
#'   counted as moved.
#' @export
it_check_stability <- function(frames, n = 20, size = 160, lag = 10, tolerance = 2) {
  files <- frames$file
  if (length(files) <= lag) stop("too few frames")
  i1 <- unique(round(seq(1, length(files) - lag, length.out = min(n, length(files) - lag))))
  ref <- it_read_frame(files[1])
  f <- size / ncol(ref)
  shrink <- function(m) {
    rr <- unique(pmax(1, round(seq(1, nrow(m), by = 1 / f))))
    cc <- unique(pmax(1, round(seq(1, ncol(m), by = 1 / f))))
    m[rr, cc]
  }
  pc <- function(a, b) {
    a <- a - mean(a); b <- b - mean(b)
    wy <- 0.5 - 0.5 * cos(2 * pi * seq(0, 1, length.out = nrow(a)))
    wx <- 0.5 - 0.5 * cos(2 * pi * seq(0, 1, length.out = ncol(a)))
    w <- outer(wy, wx)
    A <- stats::fft(a * w); B <- stats::fft(b * w)
    R <- A * Conj(B); R <- R / pmax(Mod(R), 1e-12)
    r <- Re(stats::fft(R, inverse = TRUE))
    k <- which(r == max(r), arr.ind = TRUE)[1, ]
    dy <- k[1] - 1; dx <- k[2] - 1
    if (dy > nrow(a) / 2) dy <- dy - nrow(a)
    if (dx > ncol(a) / 2) dx <- dx - ncol(a)
    c(dx, dy)
  }
  sh <- t(vapply(i1, function(i) {
    pc(shrink(it_read_frame(files[i])), shrink(it_read_frame(files[i + lag]))) / f
  }, numeric(2)))
  mag <- sqrt(rowSums(sh^2))
  list(pairs = data.frame(frame1 = frames$frame[i1], frame2 = frames$frame[i1 + lag],
                          dx = sh[, 1], dy = sh[, 2], shift = mag),
       median_shift = stats::median(mag), max_shift = max(mag),
       share_moved = mean(mag > tolerance),
       stable = mean(mag > tolerance) <= 0.1)
}
