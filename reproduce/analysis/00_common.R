# Shared settings and helpers for the feasibility analyses.

suppressPackageStartupMessages({
  library(InsectTrack)
})

root <- here::here()
cfg <- yaml::read_yaml(file.path(root, "config", "feasibility.yaml"))
if (nzchar(Sys.getenv("IT_OUTPUT"))) cfg$paths$output <- Sys.getenv("IT_OUTPUT")
set.seed(cfg$seed)

out_dir <- function(...) {
  p <- file.path(root, cfg$paths$output, ...)
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}

write_csv <- function(x, file) utils::write.csv(x, file, row.names = FALSE)

log_msg <- function(...) message(format(Sys.time(), "%H:%M:%S  "), sprintf(...))

zone_poly <- function(z) {
  if (!is.null(z$ellipse)) {
    e <- unlist(z$ellipse)
    return(it_roi_ellipse(e[1], e[2], e[3], e[4]))
  }
  data.frame(x = unlist(z$x), y = unlist(z$y))
}

save_png <- function(file, width, height, expr, res = 150) {
  grDevices::png(file, width = width, height = height, res = res, type = "cairo", bg = "white")
  on.exit(grDevices::dev.off())
  force(expr)
  invisible(file)
}

run_detection <- function(frames, p, background = NULL) {
  det <- it_detect_motion(frames, background = background, bg_frames = p$bg_frames, k = p$k,
                          k_pixel = p$k_pixel,
                          min_threshold = p$min_threshold, blur = p$blur, close = p$close,
                          min_area = p$min_area, max_area = p$max_area, verbose = FALSE)
  it_merge_detections(det, p$merge_dist)
}

run_tracking <- function(det, pt, ps = NULL) {
  tr <- it_track(det, gate = pt$gate, max_speed = pt$max_speed %||% 0, max_gap = pt$max_gap,
                 min_length = pt$min_length)
  n_before <- length(unique(stats::na.omit(tr$track)))
  if (!is.null(ps)) {
    tr <- it_stitch(tr, max_gap = ps$max_gap, max_dist = ps$max_dist, max_speed = ps$max_speed %||% 0)
  }
  attr(tr, "n_tracks_before_stitch") <- n_before
  tr
}

timed <- function(expr) {
  t0 <- proc.time()[["elapsed"]]
  val <- force(expr)
  list(value = val, seconds = proc.time()[["elapsed"]] - t0)
}
