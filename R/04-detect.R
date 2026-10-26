#' Detect moving insects by background subtraction
#'
#' Each frame is compared with a background image (temporal median of frames,
#' see [it_background()]). The absolute difference is smoothed with a box
#' filter, thresholded, cleaned with binary morphology and split into
#' connected blobs, which are kept when their area lies within
#' `[min_area, max_area]`.
#'
#' The threshold is either fixed (`threshold`, grey levels 0-255) or set for
#' each frame as `median + k * MAD` of the difference image, but never lower
#' than `min_threshold`.
#'
#' @param frames Data frame from [it_extract_frames()].
#' @param background Optional background matrix; computed with
#'   [it_background()] when `NULL`.
#' @param bg_frames Number of frames used for each background.
#' @param bg_window Optional length (s) of consecutive segments that get their
#'   own background, for long videos with changing light.
#' @param threshold Fixed threshold (grey levels) or `NULL` for the adaptive
#'   rule.
#' @param k,min_threshold Parameters of the adaptive threshold.
#' @param k_pixel Optional multiplier of the per-pixel temporal spread of the
#'   background frames (see [it_background()]); the threshold of each pixel is then the larger of the
#'   frame threshold and `k_pixel` times its variability, which suppresses
#'   detections on moving vegetation, water or flickering light.
#' @param blur Half-width (pixels) of the box filter applied to the
#'   difference image; 0 for none.
#' @param close,open Half-widths (pixels) of binary closing and opening.
#' @param min_area,max_area Blob area limits in pixels.
#' @param mask Optional logical matrix (same size as the frames); only pixels
#'   with `TRUE` are analysed.
#' @param verbose Print progress.
#' @return A data frame with one row per detection: `frame`, `time`, `x`, `y`
#'   (contrast-weighted centroid), `area`, bounding box, contrast, orientation
#'   (radians), major and minor axis lengths and the threshold used.
#' @export
it_detect_motion <- function(frames, background = NULL, bg_frames = 31,
                             bg_window = NULL, threshold = NULL, k = 5, k_pixel = NULL,
                             min_threshold = 15, blur = 1, close = 1, open = 0,
                             min_area = 10, max_area = Inf, mask = NULL,
                             verbose = TRUE) {
  stopifnot(is.data.frame(frames), all(c("frame", "time", "file") %in% names(frames)))
  n <- nrow(frames)
  seg <- if (is.null(bg_window) || !is.null(background)) rep(1L, n) else
    as.integer(floor((frames$time - min(frames$time)) / bg_window)) + 1L
  bgs <- list()
  tms <- list()
  out <- vector("list", n)
  for (i in seq_len(n)) {
    s <- seg[i]
    if (is.null(background)) {
      key <- as.character(s)
      if (is.null(bgs[[key]])) bgs[[key]] <- it_background(frames[seg == s, , drop = FALSE], n = bg_frames)
      bg <- bgs[[key]]
    } else {
      key <- "given"
      bg <- background
    }
    if (!is.null(k_pixel) && is.null(tms[[key]])) {
      md <- attr(bg, "spread")
      if (is.null(md)) stop("the background has no 'spread' attribute; compute it with it_background()")
      tms[[key]] <- k_pixel * (if (blur > 0) it_box_mean(md, blur) else md)
    }
    attr(bg, "spread") <- NULL
    img <- it_read_frame(frames$file[i])
    if (!identical(dim(img), dim(bg))) stop("frame and background differ in size")
    d <- abs(img - bg)
    if (blur > 0) d <- it_box_mean(d, blur)
    thr <- if (is.null(threshold)) {
      sub <- d[seq.int(1L, length(d), by = 7L)]
      max(min_threshold, stats::median(sub) + k * stats::mad(sub))
    } else threshold
    m <- if (is.null(k_pixel)) d > thr else d > pmax(thr, tms[[key]])
    if (!is.null(mask)) m <- m & mask
    if (open > 0) m <- it_morph(it_morph(m, open, FALSE), open, TRUE)
    if (close > 0) m <- it_morph(it_morph(m, close, TRUE), close, FALSE)
    b <- it_label_blobs(m, d, as.integer(min_area), as.numeric(max_area))
    if (nrow(b)) {
      b <- cbind(frame = frames$frame[i], time = frames$time[i], b, threshold = thr)
    } else b <- NULL
    out[[i]] <- b
    if (verbose && i %% 250 == 0) message(sprintf("  detection: frame %d/%d", i, n))
  }
  det <- do.call(rbind, out)
  if (is.null(det)) {
    det <- data.frame(frame = integer(), time = numeric(), x = numeric(), y = numeric(),
                      area = numeric())
  }
  rownames(det) <- NULL
  det
}

#' Read detections or tracks from a table
#'
#' Imports detections of an external detector (e.g. 'YOLO') or tracks of
#' other tracking software. Boxes given by corners (`x0`, `y0`, `x1`, `y1`) or
#' by top-left corner and size (MOTChallenge format) are converted to box
#' centres.
#'
#' @param file CSV or text file.
#' @param format `"csv"` (named columns) or `"mot"` (MOTChallenge: frame, id,
#'   left, top, width, height, confidence, ...; no header).
#' @param columns For `"csv"`: named character vector mapping the standard
#'   names (`frame`, `time`, `x`, `y`, `x0`, `y0`, `x1`, `y1`, `conf`,
#'   `class`, `track`) to the column names in the file.
#' @param fps Frame rate used to compute `time` when the file has none.
#' @param scale Factor applied to all coordinates (e.g. 2/3 to go from
#'   1920 x 1080 to 1280 x 720 pixels).
#' @return A data frame with at least `frame`, `time`, `x` and `y`.
#' @export
it_read_detections <- function(file, format = c("csv", "mot"), columns = NULL,
                               fps = NULL, scale = 1) {
  format <- match.arg(format)
  if (format == "mot") {
    d <- utils::read.csv(file, header = FALSE)
    names(d)[1:7] <- c("frame", "track", "left", "top", "width", "height", "conf")
    d$x <- d$left + d$width / 2
    d$y <- d$top + d$height / 2
    d$x0 <- d$left; d$y0 <- d$top; d$x1 <- d$left + d$width; d$y1 <- d$top + d$height
    d$track[d$track < 0] <- NA
  } else {
    d <- utils::read.csv(file, stringsAsFactors = FALSE)
    if (!is.null(columns)) {
      for (nm in names(columns)) d[[nm]] <- d[[columns[[nm]]]]
    }
    if (!all(c("x", "y") %in% names(d))) {
      if (!all(c("x0", "y0", "x1", "y1") %in% names(d)))
        stop("need x and y, or box corners x0, y0, x1, y1")
      d$x <- (d$x0 + d$x1) / 2
      d$y <- (d$y0 + d$y1) / 2
    }
  }
  if (!"frame" %in% names(d)) stop("column 'frame' is required")
  if (!"time" %in% names(d)) {
    if (is.null(fps)) stop("no 'time' column: give fps")
    d$time <- d$frame / fps
  }
  for (v in intersect(c("x", "y", "x0", "y0", "x1", "y1", "left", "top", "width", "height"), names(d)))
    d[[v]] <- d[[v]] * scale
  d[order(d$frame), , drop = FALSE]
}

#' Read YOLO-format label files
#'
#' Reads one text file per frame with lines `class cx cy w h [conf]` in
#' coordinates relative to the image size. The first field can also be an
#' identity (e.g. `bee_5`) as in some tracking data sets.
#'
#' @param files Label files.
#' @param width,height Image size in pixels.
#' @param frame_regex Regular expression with one group that extracts the
#'   frame number from the file name.
#' @param first_field Meaning of the first field: `"class"` or `"id"`.
#' @return A data frame with `frame`, `x`, `y`, `w`, `h` (pixels), `class` or
#'   `id`, and `file`.
#' @export
it_read_yolo_labels <- function(files, width, height, frame_regex = "frame_(\\d+)",
                                first_field = c("class", "id")) {
  first_field <- match.arg(first_field)
  out <- lapply(files, function(f) {
    fr <- as.integer(sub(paste0(".*", frame_regex, ".*"), "\\1", basename(f)))
    ln <- readLines(f, warn = FALSE)
    ln <- ln[nzchar(trimws(ln))]
    if (!length(ln)) return(NULL)
    p <- strsplit(trimws(ln), "\\s+")
    data.frame(frame = fr,
               first = vapply(p, `[`, "", 1),
               x = as.numeric(vapply(p, `[`, "", 2)) * width,
               y = as.numeric(vapply(p, `[`, "", 3)) * height,
               w = as.numeric(vapply(p, `[`, "", 4)) * width,
               h = as.numeric(vapply(p, `[`, "", 5)) * height,
               file = basename(f), stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, out)
  if (is.null(d)) return(data.frame(frame = integer(), x = numeric(), y = numeric()))
  names(d)[names(d) == "first"] <- first_field
  d[order(d$frame), , drop = FALSE]
}
