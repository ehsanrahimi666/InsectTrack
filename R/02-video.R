#' Basic information about a video file
#'
#' Any container and codec that FFmpeg reads can be used (MP4, MOV, AVI,
#' MKV, WebM, OGV, MTS, ...). For MP4 and MOV files the display rotation
#' stored by smartphones is read from the track header; `width` and `height`
#' are the stored frame size and `display_width`, `display_height` the size
#' after rotation.
#'
#' @param video Path to a video file readable by FFmpeg.
#' @return A one-row data frame with duration (s), frame size, number of
#'   frames, frame rate, codec and rotation (degrees clockwise applied for
#'   display).
#' @export
it_video_info <- function(video) {
  if (!file.exists(video)) stop("file not found: ", video)
  i <- av::av_media_info(video)
  v <- i$video
  if (is.null(v) || !nrow(v)) stop("no video stream in ", video)
  rot <- video_rotation(video)
  swap <- rot %in% c(90, 270)
  data.frame(file = basename(video), duration_s = i$duration,
             width = v$width[1], height = v$height[1], frames = v$frames[1],
             fps = v$framerate[1], codec = v$codec[1], rotation = rot,
             display_width = if (swap) v$height[1] else v$width[1],
             display_height = if (swap) v$width[1] else v$height[1], stringsAsFactors = FALSE)
}

# Display rotation (0, 90, 180 or 270 degrees clockwise, as applied by FFmpeg
# players) from the matrix of the first video track header of an MP4 or MOV
# file; 0 for other files or when no matrix is found.
video_rotation <- function(file) {
  con <- file(file, "rb")
  on.exit(close(con))
  fsize <- file.size(file)
  rd32 <- function(raw, i) sum(as.numeric(raw[i:(i + 3)]) * 256^(3:0))
  # box types are four printable ASCII characters; anything else is not an MP4 or QuickTime file
  box_type <- function(raw) if (all(raw >= as.raw(0x20) & raw <= as.raw(0x7e))) rawToChar(raw) else NA_character_
  pos <- 0
  moov <- NULL
  while (pos + 8 <= fsize) {
    seek(con, pos)
    h <- readBin(con, "raw", 8)
    if (length(h) < 8) break
    size <- rd32(h, 1); type <- box_type(h[5:8])
    if (is.na(type)) return(0)
    hdr <- 8
    if (size == 1) { size <- sum(as.numeric(readBin(con, "raw", 8)) * 256^(7:0)); hdr <- 16 }
    if (size == 0) size <- fsize - pos
    if (size < hdr) break
    if (type == "moov") { seek(con, pos + hdr); moov <- readBin(con, "raw", size - hdr); break }
    pos <- pos + size
  }
  if (is.null(moov)) return(0)
  boxes <- function(raw) {
    out <- list(); i <- 1; n <- length(raw)
    while (i + 7 <= n) {
      size <- rd32(raw, i); type <- box_type(raw[(i + 4):(i + 7)])
      if (is.na(type) || size < 8 || i + size - 1 > n) break
      out[[length(out) + 1]] <- list(type = type, data = raw[(i + 8):(i + size - 1)])
      i <- i + size
    }
    out
  }
  s32 <- function(raw, i) { v <- rd32(raw, i); if (v >= 2^31) v - 2^32 else v }
  for (b in boxes(moov)) {
    if (b$type != "trak") next
    tk <- Filter(function(z) z$type == "tkhd", boxes(b$data))
    if (!length(tk)) next
    d <- tk[[1]]$data
    off <- if (as.integer(d[1]) == 1) 4 + 32 else 4 + 20
    m0 <- off + 8 + 8 + 1
    if (length(d) < m0 + 43) next
    a <- s32(d, m0) / 65536; bb <- s32(d, m0 + 4) / 65536
    w <- rd32(d, m0 + 36) / 65536; h <- rd32(d, m0 + 40) / 65536
    if (w <= 0 || h <= 0) next
    disp <- -atan2(bb, a) * 180 / pi
    theta <- (-round(disp)) %% 360
    return(c(0, 90, 180, 270)[(round(theta / 90) %% 4) + 1])
  }
  0
}

#' Extract frames from a video
#'
#' Frames are written as image files with FFmpeg (through the 'av' package).
#' A segment can be selected with `start` and `duration`, the frame rate can be
#' reduced with `fps` and frames can be scaled to `width` pixels.
#'
#' @param video Path to a video file.
#' @param dir Output directory (created if needed).
#' @param fps Output frame rate; `NULL` keeps every frame.
#' @param start,duration Segment to extract, in seconds; `NULL` for the whole
#'   video.
#' @param width Output width in pixels (height scaled to keep the aspect
#'   ratio); `NULL` keeps the original size.
#' @param gray Write grey-scale frames.
#' @param format `"png"` (lossless) or `"jpg"`.
#' @param times Optional numeric vector of frame times (s), used instead of
#'   times computed from the frame rate, for example presentation times of a
#'   variable-frame-rate video.
#' @param rotate Apply the display rotation stored in MP4 and MOV files (see
#'   [it_video_info()]), so that frames appear as in a video player.
#' @return A data frame with columns `frame` (1, 2, ...), `time` (s) and
#'   `file`.
#' @export
it_extract_frames <- function(video, dir = tempfile("frames_"), fps = NULL,
                              start = NULL, duration = NULL, width = NULL,
                              gray = TRUE, format = c("png", "jpg"),
                              times = NULL, rotate = TRUE) {
  format <- match.arg(format)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  old <- list.files(dir, pattern = "^frame_\\d{6}\\.(png|jpg)$", full.names = TRUE)
  if (length(old)) stop("output directory already contains frames: ", dir)
  f <- character()
  rot <- if (rotate) video_rotation(video) else 0
  if (rot == 90) f <- c(f, "transpose=clock")
  if (rot == 180) f <- c(f, "hflip", "vflip")
  if (rot == 270) f <- c(f, "transpose=cclock")
  if (!is.null(start) || !is.null(duration)) {
    tr <- c(if (!is.null(start)) sprintf("start=%.4f", start),
            if (!is.null(duration)) sprintf("duration=%.4f", duration))
    f <- c(f, paste0("trim=", paste(tr, collapse = ":")), "setpts=PTS-STARTPTS")
  }
  if (!is.null(fps)) f <- c(f, sprintf("fps=fps=%g", fps))
  if (!is.null(width)) f <- c(f, sprintf("scale=%d:-2", as.integer(width)))
  if (gray) f <- c(f, "format=gray")
  vfilter <- if (length(f)) paste(f, collapse = ",") else "null"
  info <- av::av_media_info(video)
  rate <- if (is.null(fps)) info$video$framerate else fps
  out <- file.path(dir, paste0("frame_%06d.", format))
  if (format == "jpg") vfilter <- paste0(vfilter, ",format=yuvj420p")
  av::av_encode_video(input = video, output = out, framerate = rate,
                      vfilter = vfilter,
                      codec = if (format == "jpg") "mjpeg" else "png",
                      verbose = FALSE)
  files <- list.files(dir, pattern = paste0("^frame_\\d{6}\\.", format, "$"),
                      full.names = TRUE)
  if (!length(files)) stop("no frames were extracted from ", video)
  if (!is.null(duration) && is.null(times)) {
    # the frame-rate filter can add a frame at the end of the segment
    extra <- (seq_along(files) - 1) / rate >= duration - 1e-6
    if (any(extra) && !all(extra)) { unlink(files[extra]); files <- files[!extra] }
  }
  t0 <- if (is.null(start)) 0 else start
  tt <- if (is.null(times)) t0 + (seq_along(files) - 1) / rate else times
  if (length(tt) != length(files))
    stop(sprintf("%d frames extracted but %d times given", length(files), length(tt)))
  data.frame(frame = seq_along(files), time = tt, file = files,
             stringsAsFactors = FALSE)
}

#' Read one frame as a numeric matrix
#'
#' @param file PNG or JPEG file.
#' @param gray Convert colour images to grey (luma, ITU-R BT.601 weights).
#' @return Grey: a matrix (rows = image rows) with values 0-255. Colour: an
#'   array of dimension rows x columns x 3 with values 0-1.
#' @export
it_read_frame <- function(file, gray = TRUE) {
  img <- if (grepl("\\.png$", file, ignore.case = TRUE)) png::readPNG(file) else jpeg::readJPEG(file)
  if (!gray) {
    if (length(dim(img)) == 2) img <- array(img, c(dim(img), 3))
    if (dim(img)[3] > 3) img <- img[, , 1:3]
    if (dim(img)[3] == 2) img <- array(img[, , 1], c(dim(img)[1:2], 3))
    return(img)
  }
  if (length(dim(img)) == 3) {
    img <- if (dim(img)[3] >= 3) {
      0.299 * img[, , 1] + 0.587 * img[, , 2] + 0.114 * img[, , 3]
    } else img[, , 1]
  }
  img * 255
}

#' Frames from a folder of images
#'
#' Uses an image sequence (for example frames exported by another program or
#' a time-lapse camera) instead of a video.
#'
#' @param dir Folder with PNG or JPEG images.
#' @param fps Frame rate used to compute frame times.
#' @param pattern Regular expression selecting the image files.
#' @param frame_regex Optional regular expression with one group that
#'   extracts the frame number from the file name; default the sorted order.
#' @return A data frame with `frame`, `time` and `file`.
#' @export
it_frames_from_images <- function(dir, fps = 30, pattern = "\\.(png|jpe?g)$", frame_regex = NULL) {
  files <- sort(list.files(dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE))
  if (!length(files)) stop("no images found in ", dir)
  fr <- if (is.null(frame_regex)) seq_along(files) else
    as.integer(sub(paste0(".*", frame_regex, ".*"), "\\1", basename(files)))
  o <- order(fr)
  data.frame(frame = fr[o], time = (fr[o] - min(fr)) / fps, file = files[o], stringsAsFactors = FALSE)
}
