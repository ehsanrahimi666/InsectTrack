#' Example video and detections
#'
#' `it_example_video()` returns the path of a 10-s video (10 frames per
#' second, 640 x 360 pixels) of honeybees visiting *Delosperma* flowers, cut
#' from a smartphone video recorded by the package authors.
#' `it_example_detections()` returns the detections of this video by a
#' YOLO26n bee detector (one class, `bee`), so that tracking, summaries,
#' plots and rendered videos can be tried without Python.
#'
#' @return `it_example_video()`: the file path. `it_example_detections()`: a
#'   data frame with `frame`, `time` (s), the box centre `x`, `y` and corners
#'   `x0`, `y0`, `x1`, `y1` (pixels), the detector score `conf` and `class`.
#' @seealso [it_example_gallery()] for tracked frames of other insects.
#' @examples
#' v <- it_example_video()
#' it_video_info(v)
#' det <- it_example_detections()
#' tr <- it_track(det, "bytetrack", track_thresh = 0.25, new_track_thresh = 0.3, frame_rate = 10)
#' head(it_track_summary(tr, width = 640, height = 360))
#' @export
it_example_video <- function() {
  system.file("extdata", "honeybees_delosperma.mp4", package = "InsectTrack", mustWork = TRUE)
}

#' @rdname it_example_video
#' @export
it_example_detections <- function() {
  f <- system.file("extdata", "example_detections.csv", package = "InsectTrack", mustWork = TRUE)
  utils::read.csv(f, stringsAsFactors = FALSE)
}

#' Gallery of tracked insects
#'
#' Lists one tracked frame from each of eight demonstration videos: honeybees
#' at a hive entrance, stingless bees, paper wasps, hornets, a yellowjacket
#' with a honeybee, a honeybee on a flower, leafcutter ants and fruit flies.
#' Every insect has its own colour and identity label. The images keep the
#' licences of their source videos (CC0, public domain or CC BY).
#'
#' @return A data frame with the image `path`, `species`, `scene`, the
#'   `detector` used, `author`, `licence`, `licence_url`, `source` (page of
#'   the source video) and `modification`.
#' @examples
#' g <- it_example_gallery()
#' g[, c("species", "detector", "licence")]
#' img <- jpeg::readJPEG(g$path[1])
#' plot.new(); plot.window(c(0, 1), c(0, dim(img)[1] / dim(img)[2]), asp = 1)
#' rasterImage(img, 0, 0, 1, dim(img)[1] / dim(img)[2])
#' @export
it_example_gallery <- function() {
  dir <- system.file("extdata", "gallery", package = "InsectTrack", mustWork = TRUE)
  g <- utils::read.csv(file.path(dir, "ATTRIBUTION.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
  g$path <- file.path(dir, g$file)
  g[, c("path", setdiff(names(g), c("path", "file")))]
}
