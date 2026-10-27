background_raster <- function(background, desaturate = FALSE) {
  if (is.character(background)) background <- it_read_frame(background, gray = FALSE)
  if (length(dim(background)) == 2) {
    m <- background
    if (max(m, na.rm = TRUE) > 1) m <- m / 255
    return(grDevices::as.raster(pmin(pmax(m, 0), 1)))
  }
  if (desaturate) {
    g <- 0.299 * background[, , 1] + 0.587 * background[, , 2] + 0.114 * background[, , 3]
    return(grDevices::as.raster(g))
  }
  grDevices::as.raster(background)
}

open_frame <- function(width, height, background = NULL, desaturate = FALSE, fade = 0) {
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, width), ylim = c(height, 0), xaxs = "i", yaxs = "i", asp = 1)
  if (!is.null(background)) {
    graphics::rasterImage(background_raster(background, desaturate), 0, height, width, 0)
    if (fade > 0) graphics::rect(0, height, width, 0, col = grDevices::rgb(1, 1, 1, fade), border = NA)
  }
}

draw_rois <- function(rois, col = "white", lwd = 1.5, lty = 1, labels = FALSE) {
  if (is.null(rois)) return(invisible())
  rois <- as_roi_list(rois)
  for (nm in names(rois)) {
    p <- rois[[nm]]
    graphics::polygon(p$x, p$y, border = col, lwd = lwd, lty = lty)
    if (labels) graphics::text(mean(p$x), mean(p$y), nm, col = col, cex = 0.7)
  }
}

#' Plot tracks over a frame
#'
#' @param tr Tracks from [it_track()].
#' @param width,height Frame size (pixels).
#' @param background Optional frame (matrix, array or image file).
#' @param col Track colours (recycled); default one colour per track id
#'   ([it_track_colours()]).
#' @param lwd Line width.
#' @param rois,lines Optional regions (polygon or list) and counting lines
#'   (`c(x1, y1, x2, y2)` or a list of them) to draw.
#' @param main Title.
#' @return `NULL`, invisibly.
#' @export
it_plot_tracks <- function(tr, width, height, background = NULL, col = NULL, lwd = 1.2,
                           rois = NULL, lines = NULL, main = NULL) {
  d <- tr[!is.na(tr$track), , drop = FALSE]
  d <- d[order(d$track, d$time), , drop = FALSE]
  op <- graphics::par(mar = c(0.2, 0.2, if (is.null(main)) 0.2 else 1.6, 0.2))
  on.exit(graphics::par(op))
  open_frame(width, height, background, fade = 0.25)
  ids <- unique(d$track)
  col <- if (is.null(col)) it_track_colours(ids) else rep(col, length.out = length(ids))
  for (k in seq_along(ids)) {
    r <- d$track == ids[k]
    graphics::lines(d$x[r], d$y[r], col = col[k], lwd = lwd)
  }
  draw_rois(rois)
  if (!is.null(lines)) {
    if (!is.list(lines)) lines <- list(lines)
    for (l in lines) graphics::segments(l[1], l[2], l[3], l[4], col = "red", lwd = 2, lty = 2)
  }
  if (!is.null(main)) graphics::title(main, cex.main = 0.9, line = 0.3)
  invisible(NULL)
}

#' Plot a heat map over a frame
#'
#' @param hm Heat map from [it_heatmap()] or [it_hotspots()].
#' @param background Optional frame shown in grey under the heat map.
#' @param palette Colours for increasing values.
#' @param alpha Opacity of the heat map.
#' @param log Show `log1p` of the values.
#' @param hot Outline hot-spot cells when `hm` comes from [it_hotspots()].
#' @param rois Optional regions to draw.
#' @param main Title.
#' @param legend_title Title of the colour bar.
#' @param min_frac Cells below this fraction of the maximum are not drawn.
#' @return `NULL`, invisibly.
#' @export
it_plot_heatmap <- function(hm, background = NULL, palette = NULL, alpha = 0.75, log = TRUE,
                            hot = TRUE, rois = NULL, main = NULL, legend_title = NULL,
                            min_frac = 0.02) {
  if (is.null(palette)) palette <- grDevices::hcl.colors(100, "YlOrRd", rev = TRUE)
  z <- if (log) log1p(hm$z) else hm$z
  zmax <- max(z)
  op <- graphics::par(mar = c(0.2, 0.2, if (is.null(main)) 0.2 else 1.6, 0.2))
  on.exit(graphics::par(op))
  open_frame(hm$width, hm$height, background, desaturate = TRUE, fade = 0.15)
  if (zmax > 0) {
    idx <- pmax(1, ceiling(z / zmax * length(palette)))
    cols <- matrix(grDevices::adjustcolor(palette[idx], alpha.f = alpha), nrow(z))
    cols[hm$z <= min_frac * max(hm$z)] <- NA
    nx <- ncol(z); ny <- nrow(z)
    graphics::rasterImage(grDevices::as.raster(cols), 0, ny * hm$cell, nx * hm$cell, 0,
                          interpolate = FALSE)
  }
  if (hot && !is.null(hm$hot) && any(hm$hot)) {
    graphics::contour(hm$x, hm$y, t(hm$hot * 1), levels = 0.5, add = TRUE,
                      drawlabels = FALSE, col = "#0B3D91", lwd = 1.6)
  }
  draw_rois(rois, col = "white", lwd = 1)
  if (zmax > 0) {
    w <- hm$width; h <- hm$height
    bx0 <- w * 0.70; bx1 <- w * 0.97; by0 <- h * 0.94; by1 <- h * 0.97
    graphics::rect(bx0 - w * 0.01, by0 - h * 0.055, bx1 + w * 0.01, by1 + h * 0.012,
                   col = grDevices::rgb(1, 1, 1, 0.85), border = NA)
    xs <- seq(bx0, bx1, length.out = length(palette) + 1)
    graphics::rect(xs[-length(xs)], by0, xs[-1], by1, col = palette, border = NA)
    vmax <- max(hm$z)
    graphics::text(c(bx0, bx1), by0 - h * 0.012, c("0", formatC(vmax, digits = 3, format = "g")),
                   cex = 0.65, adj = c(0.5, 1))
    if (!is.null(legend_title)) graphics::text((bx0 + bx1) / 2, by0 - h * 0.012, legend_title,
                                               cex = 0.65, adj = c(0.5, 1))
  }
  if (!is.null(main)) graphics::title(main, cex.main = 0.9, line = 0.3)
  invisible(NULL)
}

#' Plot an activity time series
#'
#' @param act Activity table from [it_activity()].
#' @param what Columns to plot: any of `"mean_count"`, `"max_count"`,
#'   `"n_tracks"`, `"n_new_tracks"`.
#' @param main Title.
#' @param col Line colours.
#' @return `NULL`, invisibly.
#' @export
it_plot_activity <- function(act, what = c("mean_count", "max_count"), main = NULL,
                             col = c("#1f4e9c", "#d95f02", "#2a9d8f", "#7f7f7f")) {
  lab <- c(mean_count = "Mean insects per frame", max_count = "Maximum insects per frame (MaxN)",
           n_tracks = "Tracks present", n_new_tracks = "New tracks")
  y <- as.matrix(act[, what, drop = FALSE])
  x <- act$bin_start
  op <- graphics::par(mar = c(4.2, 4.4, if (is.null(main)) 2.2 else 3.6, 1))
  on.exit(graphics::par(op))
  graphics::matplot(x, y, type = "l", lty = 1, lwd = 2, col = col[seq_along(what)], las = 1,
                    xlab = "Time (s)", ylab = "Insects", ylim = c(0, max(1, max(y, na.rm = TRUE))))
  usr <- graphics::par("usr")
  graphics::legend(mean(usr[1:2]), usr[4], legend = lab[what], col = col[seq_along(what)], lwd = 2, bty = "n",
                   cex = 0.85, horiz = TRUE, xjust = 0.5, yjust = 0, xpd = TRUE)
  if (!is.null(main)) graphics::title(main, cex.main = 1, line = 2)
  invisible(NULL)
}

#' Plot entrance traffic
#'
#' Bars of insects entering (above zero) and leaving (below zero) per time
#' bin, with the cumulative net flow.
#'
#' @param ec Result of [it_entrance_counts()].
#' @param main Title.
#' @return `NULL`, invisibly.
#' @export
it_plot_entrance <- function(ec, main = NULL) {
  cn <- ec$counts
  op <- graphics::par(mar = c(4.2, 4.4, if (is.null(main)) 2.2 else 3.6, 4.4))
  on.exit(graphics::par(op))
  mx <- max(1, cn$n_in, cn$n_out)
  mid <- (cn$bin_start + cn$bin_end) / 2
  wd <- if (nrow(cn)) min(cn$bin_end - cn$bin_start) * 0.8 else 1
  graphics::plot(NA, xlim = range(c(cn$bin_start, cn$bin_end)), ylim = c(-mx, mx) * 1.1, las = 1,
                 xlab = "Time (s)", ylab = "Insects per bin")
  graphics::abline(h = 0, col = "grey50")
  graphics::rect(mid - wd / 2, 0, mid + wd / 2, cn$n_in, col = "#2a9d8f", border = NA)
  graphics::rect(mid - wd / 2, -cn$n_out, mid + wd / 2, 0, col = "#e76f51", border = NA)
  net <- cumsum(cn$net)
  sc <- if (max(abs(net)) > 0) mx / max(abs(net)) else 1
  graphics::lines(c(cn$bin_start[1], cn$bin_end), c(0, net) * sc, type = "s", lwd = 2, col = "black")
  ax <- pretty(c(-max(abs(net)), max(abs(net))))
  graphics::axis(4, at = ax * sc, labels = ax, las = 1)
  graphics::mtext("Cumulative net flow (in - out)", side = 4, line = 2.8, cex = 0.9)
  usr <- graphics::par("usr")
  graphics::legend(mean(usr[1:2]), usr[4], c("In", "Out", "Cumulative net"), fill = c("#2a9d8f", "#e76f51", NA),
                   border = NA, lwd = c(NA, NA, 2), col = c(NA, NA, "black"), bty = "n", cex = 0.85,
                   horiz = TRUE, xjust = 0.5, yjust = 0, xpd = TRUE)
  if (!is.null(main)) graphics::title(main, cex.main = 1, line = 2)
  invisible(NULL)
}
