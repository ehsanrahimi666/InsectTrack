#' Render a video with track identities and trajectories
#'
#' Draws on every frame the trajectory of each track over the last `tail`
#' seconds and its current position with the track number, optionally with
#' boxes, regions of interest, counting lines, an entrance zone with running
#' in and out counts, and a counter, and encodes the frames as an H.264
#' video. By default every track has its own colour (see
#' [it_track_colours()]); labels have a dark outline so that they stay
#' readable on bright and dark backgrounds.
#'
#' @param frames Data frame (`frame`, `time`, `file`) of colour frames in the
#'   same pixel coordinates as the tracks (see [it_extract_frames()] with
#'   `gray = FALSE`).
#' @param tr Tracks from [it_track()].
#' @param output Output video file (`.mp4`).
#' @param tail Length (s) of the trajectory drawn behind each insect.
#' @param from,to Optional time range (s).
#' @param fps Frame rate of the output; default from the frame times.
#' @param colour `"track"` (one colour per track) or `"single"`.
#' @param col_tail,col_point Colours of trajectories and positions when
#'   `colour = "single"`.
#' @param lwd Line width of trajectories at a frame width of 1280 pixels
#'   (scaled with the frame width).
#' @param id_cex Size of the track labels at a frame width of 1280 pixels
#'   (scaled with the frame width).
#' @param show_class Add the class name to the label when the tracks have a
#'   `class` column with more than one value.
#' @param rois,lines Optional regions and counting lines (see
#'   [it_plot_tracks()]).
#' @param entrance Optional entrance polygon, drawn in white.
#' @param events Optional entrance events from [it_entrance_counts()]
#'   (`$events`); running totals of insects in and out are shown.
#' @param counter Show the time and the number of insects in view.
#' @param boxes Draw detection boxes when the tracks have box columns.
#' @param quality JPEG quality of the intermediate frames.
#' @param workdir Directory for the rendered frames.
#' @return The output path, invisibly.
#' @export
it_render_video <- function(frames, tr, output, tail = 2, from = NULL, to = NULL, fps = NULL,
                            colour = c("track", "single"), col_tail = "#1E5BFF", col_point = "#FFD400",
                            lwd = 3, id_cex = 1.6, show_class = TRUE, rois = NULL, lines = NULL,
                            entrance = NULL, events = NULL, counter = TRUE, boxes = FALSE,
                            quality = 90, workdir = tempfile("render_")) {
  colour <- match.arg(colour)
  sel <- frames
  if (!is.null(from)) sel <- sel[sel$time >= from, , drop = FALSE]
  if (!is.null(to)) sel <- sel[sel$time <= to, , drop = FALSE]
  if (!nrow(sel)) stop("no frames in the selected time range")
  if (is.null(fps)) fps <- if (nrow(sel) > 1) 1 / stats::median(diff(sel$time)) else 10
  dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
  files <- render_frames(sel, tr, workdir, offset = 0, tail = tail, colour = colour, col_tail = col_tail,
                         col_point = col_point, lwd = lwd, id_cex = id_cex, show_class = show_class,
                         rois = rois, lines = lines, entrance = entrance, events = events,
                         counter = counter, boxes = boxes, quality = quality)
  av::av_encode_video(files, output = output, framerate = fps, codec = "libx264",
                      vfilter = "format=yuv420p", verbose = FALSE)
  unlink(workdir, recursive = TRUE)
  invisible(output)
}

# Draw tracks on frames and write them as JPEG files numbered from offset + 1.
render_frames <- function(sel, tr, workdir, offset = 0, tail = 2, colour = "track", col_tail = "#1E5BFF",
                          col_point = "#FFD400", lwd = 3, id_cex = 1.6, show_class = TRUE, rois = NULL,
                          lines = NULL, entrance = NULL, events = NULL, counter = TRUE, boxes = FALSE,
                          quality = 90) {
  d <- tr[!is.na(tr$track) & !is.na(tr$x), , drop = FALSE]
  d <- d[order(d$track, d$time), , drop = FALSE]
  has_box <- nrow(d) > 0 && (all(c("x0", "y0", "x1", "y1") %in% names(d)) ||
                               all(c("xmin", "ymin", "xmax", "ymax") %in% names(d)))
  bx <- if (boxes && has_box) it_det_boxes(d) else NULL
  tcol <- if (colour == "track") it_track_colours(d$track) else rep(col_tail, nrow(d))
  pcol <- if (colour == "track") tcol else rep(col_point, nrow(d))
  lab <- as.character(d$track)
  if (show_class && "class" %in% names(d) && length(unique(stats::na.omit(d$class))) > 1) {
    cl <- tapply(as.character(d$class), d$track, function(z) names(which.max(table(z))))
    lab <- paste(lab, unname(cl[as.character(d$track)]))
  }
  by_frame <- split(seq_len(nrow(d)), d$frame)
  files <- character(nrow(sel))
  for (k in seq_len(nrow(sel))) {
    img <- it_read_frame(sel$file[k], gray = FALSE)
    H <- dim(img)[1]; W <- dim(img)[2]
    s <- max(0.8, W / 1280)
    files[k] <- file.path(workdir, sprintf("render_%07d.jpg", offset + k))
    grDevices::jpeg(files[k], width = W, height = H, quality = quality)
    graphics::par(mar = c(0, 0, 0, 0))
    open_frame(W, H, img)
    draw_rois(rois, col = "white", lwd = 1.5 * s, lty = 2)
    if (!is.null(entrance)) draw_rois(list(entrance = entrance), col = "white", lwd = 2.5 * s)
    if (!is.null(lines)) {
      ll <- if (is.list(lines)) lines else list(lines)
      for (l in ll) graphics::segments(l[1], l[2], l[3], l[4], col = "#FF3B30", lwd = 2.5 * s)
    }
    t <- sel$time[k]
    w <- which(d$time > t - tail - 1e-9 & d$time <= t + 1e-9)
    cur <- by_frame[[as.character(sel$frame[k])]]
    if (length(w)) {
      for (r in split(w, d$track[w])) {
        if (length(r) > 1) {
          graphics::lines(d$x[r], d$y[r], col = "black", lwd = lwd * s + 2)
          graphics::lines(d$x[r], d$y[r], col = tcol[r[1]], lwd = lwd * s)
        }
      }
    }
    if (length(cur)) {
      if (!is.null(bx)) graphics::rect(bx[cur, 1], bx[cur, 2], bx[cur, 3], bx[cur, 4], border = pcol[cur], lwd = 1.5 * s)
      graphics::points(d$x[cur], d$y[cur], pch = 21, bg = pcol[cur], col = "black", cex = 1.3 * s, lwd = s)
      off <- abs(graphics::strheight("M", cex = id_cex * s)) * 1.25
      ly <- ifelse(d$y[cur] - off < off, d$y[cur] + off, d$y[cur] - off)
      halo_text(d$x[cur], ly, lab[cur], col = pcol[cur], cex = id_cex * s)
    }
    if (counter) {
      txt <- sprintf("t = %.1f s   insects in view: %d", t, length(cur))
      if (!is.null(events) && nrow(events)) {
        txt <- sprintf("%s   in: %d   out: %d", txt, sum(events$direction == "in" & events$time <= t),
                       sum(events$direction == "out" & events$time <= t))
      }
      cs <- 1.1 * s
      graphics::rect(0, 0, graphics::strwidth(txt, cex = cs) + W * 0.02, H * 0.06,
                     col = grDevices::rgb(0, 0, 0, 0.6), border = NA)
      graphics::text(W * 0.01, H * 0.03, txt, adj = c(0, 0.5), col = "white", cex = cs)
    }
    grDevices::dev.off()
    if (!file.exists(files[k]))
      stop("the graphics device wrote no file ", files[k], "; set options(bitmapType = \"cairo\") and try again",
           call. = FALSE)
  }
  files
}
