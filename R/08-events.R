#' Regions of interest
#'
#' Regions are polygons given as data frames with columns `x` and `y`
#' (pixels). `it_roi_circle()` and `it_roi_ellipse()` build polygons that
#' approximate circles and ellipses; `it_read_rois_json()` reads polygons
#' stored as a JSON object of named lists of `[x, y]` points.
#'
#' @param x,y Centre (pixels).
#' @param r,rx,ry Radius or semi-axes (pixels).
#' @param n Number of vertices.
#' @param file JSON file.
#' @param scale Factor applied to the coordinates.
#' @return A data frame (`x`, `y`) or a named list of them.
#' @name rois
NULL

#' @rdname rois
#' @export
it_roi_circle <- function(x, y, r, n = 64) it_roi_ellipse(x, y, r, r, n)

#' @rdname rois
#' @export
it_roi_ellipse <- function(x, y, rx, ry, n = 64) {
  a <- seq(0, 2 * pi, length.out = n + 1)[-1]
  data.frame(x = x + rx * cos(a), y = y + ry * sin(a))
}

#' @rdname rois
#' @export
it_read_rois_json <- function(file, scale = 1) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("package 'jsonlite' is required")
  j <- jsonlite::fromJSON(file, simplifyVector = TRUE)
  lapply(j, function(m) {
    m <- matrix(unlist(m), ncol = 2, byrow = is.list(m))
    data.frame(x = m[, 1] * scale, y = m[, 2] * scale)
  })
}

as_roi_list <- function(rois) {
  if (is.data.frame(rois)) rois <- list(roi = rois)
  nm <- names(rois)
  if (is.null(nm)) nm <- rep("", length(rois))
  nm[is.na(nm) | !nzchar(nm)] <- paste0("roi_", which(is.na(nm) | !nzchar(nm)))
  if (anyDuplicated(nm)) stop("region names must be unique")
  names(rois) <- nm
  ok <- vapply(rois, function(p) is.data.frame(p) && nrow(p) >= 3 && !anyNA(p$x) && !anyNA(p$y), TRUE)
  if (!all(ok)) stop("regions must be polygons with at least three vertices and no missing coordinates: ",
                     paste(names(rois)[!ok], collapse = ", "))
  rois
}

#' Visits to regions of interest
#'
#' A visit is a run of consecutive detections of one track inside a region
#' (e.g. a flower, a nest entrance, a bait). Runs of the same track separated
#' by at most `merge_gap` seconds outside the region are merged.
#'
#' @param tr Tracks from [it_track()].
#' @param rois A polygon (data frame with `x`, `y`) or a named list of
#'   polygons.
#' @param merge_gap Maximum time (s) outside a region within one visit.
#' @param min_duration Minimum visit duration (s).
#' @return One row per visit: `roi`, `track`, `start`, `end`, `duration` (s,
#'   from the first to the last detection inside), `n` detections and the
#'   track class when available.
#' @export
it_roi_visits <- function(tr, rois, merge_gap = 0, min_duration = 0) {
  rois <- as_roi_list(rois)
  d <- tr[!is.na(tr$track) & !is.na(tr$x) & !is.na(tr$y), , drop = FALSE]
  d <- d[order(d$track, d$time), , drop = FALSE]
  out <- list()
  for (nm in names(rois)) {
    p <- rois[[nm]]
    inside <- it_point_in_polygon(d$x, d$y, p$x, p$y)
    if (!any(inside)) next
    rid <- cumsum(c(TRUE, diff(d$track) != 0 | diff(inside) != 0))
    keep <- inside
    v <- data.frame(run = rid[keep], track = d$track[keep], time = d$time[keep])
    s <- do.call(rbind, lapply(split(v, v$run), function(q)
      data.frame(track = q$track[1], start = min(q$time), end = max(q$time), n = nrow(q))))
    s <- s[order(s$track, s$start), , drop = FALSE]
    if (merge_gap > 0 && nrow(s) > 1) {
      g <- cumsum(c(TRUE, diff(s$track) != 0 | (s$start[-1] - s$end[-nrow(s)]) > merge_gap))
      s <- do.call(rbind, lapply(split(s, g), function(q)
        data.frame(track = q$track[1], start = min(q$start), end = max(q$end), n = sum(q$n))))
    }
    s$roi <- nm
    out[[nm]] <- s
  }
  res <- do.call(rbind, out)
  if (is.null(res)) {
    return(data.frame(roi = character(), track = integer(), start = numeric(),
                      end = numeric(), duration = numeric(), n = integer()))
  }
  res$duration <- res$end - res$start
  res <- res[res$duration >= min_duration, c("roi", "track", "start", "end", "duration", "n")]
  if ("class" %in% names(d)) {
    cl <- tapply(d$class, d$track, function(z) names(which.max(table(z))))
    res$class <- unname(cl[as.character(res$track)])
  }
  rownames(res) <- NULL
  res
}

#' Movements into and out of a zone
#'
#' Classifies each track by where it starts and ends relative to a zone such
#' as a nest or hive entrance: `entered` (starts outside, ends inside),
#' `exited` (starts inside, ends outside), `visited` (starts and ends outside
#' but enters), `inside` and `outside`. Insects that disappear into a nest
#' typically end their track inside the entrance zone.
#'
#' @param tr Tracks from [it_track()].
#' @param zone Polygon (data frame with `x`, `y`).
#' @return One row per track with the classification and the times of the
#'   first and last detections.
#' @export
it_zone_transitions <- function(tr, zone) {
  d <- tr[!is.na(tr$track) & !is.na(tr$x) & !is.na(tr$y), , drop = FALSE]
  d <- d[order(d$track, d$time), , drop = FALSE]
  inside <- it_point_in_polygon(d$x, d$y, zone$x, zone$y)
  sp <- split(seq_len(nrow(d)), d$track)
  res <- do.call(rbind, lapply(sp, function(r) {
    i <- inside[r]
    type <- if (!any(i)) "outside" else if (i[1] && i[length(i)]) "inside" else
      if (!i[1] && i[length(i)]) "entered" else if (i[1] && !i[length(i)]) "exited" else "visited"
    data.frame(track = d$track[r[1]], start = d$time[r[1]], end = d$time[r[length(r)]],
               type = type, share_inside = mean(i))
  }))
  rownames(res) <- NULL
  res
}

#' Crossings of a counting line
#'
#' Counts crossings of the segment from (`x1`, `y1`) to (`x2`, `y2`) between
#' consecutive detections of each track. The side of a point is the sign of
#' `(x2 - x1) * (y - y1) - (y2 - y1) * (x - x1)` in image coordinates; for a
#' line drawn from left to right the positive side is below the line.
#'
#' @param tr Tracks from [it_track()].
#' @param line Numeric vector `c(x1, y1, x2, y2)`.
#' @param labels Names of the two directions: crossing from the negative to
#'   the positive side, and from the positive to the negative side.
#' @return One row per crossing: `track`, `time` (interpolated), `x`, `y` and
#'   `direction`.
#' @export
it_line_crossings <- function(tr, line, labels = c("neg_to_pos", "pos_to_neg")) {
  d <- tr[!is.na(tr$track) & !is.na(tr$x) & !is.na(tr$y), , drop = FALSE]
  d <- d[order(d$track, d$time), , drop = FALSE]
  x1 <- line[1]; y1 <- line[2]; x2 <- line[3]; y2 <- line[4]
  side <- (x2 - x1) * (d$y - y1) - (y2 - y1) * (d$x - x1)
  n <- nrow(d)
  if (n < 2) return(data.frame())
  a <- seq_len(n - 1); b <- a + 1
  same <- d$track[a] == d$track[b]
  cross <- same & (side[a] * side[b] < 0)
  if (!any(cross)) {
    return(data.frame(track = integer(), time = numeric(), x = numeric(), y = numeric(),
                      direction = character()))
  }
  a <- a[cross]; b <- b[cross]
  f <- side[a] / (side[a] - side[b])
  xi <- d$x[a] + f * (d$x[b] - d$x[a]); yi <- d$y[a] + f * (d$y[b] - d$y[a])
  u <- ((xi - x1) * (x2 - x1) + (yi - y1) * (y2 - y1)) / ((x2 - x1)^2 + (y2 - y1)^2)
  on <- u >= 0 & u <= 1
  res <- data.frame(track = d$track[a], time = d$time[a] + f * (d$time[b] - d$time[a]),
                    x = xi, y = yi, direction = ifelse(side[a] < 0, labels[1], labels[2]))
  res[on, , drop = FALSE]
}

#' Proximity interactions between tracks
#'
#' Two tracks interact when their detections in the same frame are at most
#' `max_dist` pixels apart. Consecutive frames of a pair form one event.
#'
#' @param tr Tracks from [it_track()].
#' @param max_dist Distance threshold (pixels).
#' @param min_duration Minimum event duration (s).
#' @param max_gap Maximum time (s) without contact within one event; default
#'   1.5 times the median frame interval.
#' @return One row per event: the two tracks, start, end, duration, number of
#'   frames and minimum distance, and the classes when available.
#' @export
it_interactions <- function(tr, max_dist, min_duration = 0, max_gap = NULL) {
  d <- tr[!is.na(tr$track) & !is.na(tr$x) & !is.na(tr$y), , drop = FALSE]
  grp <- split(seq_len(nrow(d)), d$frame)
  grp <- grp[lengths(grp) > 1]
  rec <- lapply(grp, function(r) {
    D <- as.matrix(stats::dist(cbind(d$x[r], d$y[r])))
    w <- which(D <= max_dist & upper.tri(D), arr.ind = TRUE)
    if (!nrow(w)) return(NULL)
    ta <- d$track[r[w[, 1]]]; tb <- d$track[r[w[, 2]]]
    data.frame(a = pmin(ta, tb), b = pmax(ta, tb), time = d$time[r[1]], dist = D[w])
  })
  rec <- do.call(rbind, rec)
  if (is.null(rec)) {
    return(data.frame(a = integer(), b = integer(), start = numeric(), end = numeric(),
                      duration = numeric(), n = integer(), min_dist = numeric()))
  }
  if (is.null(max_gap)) max_gap <- 1.5 * stats::median(diff(sort(unique(d$time))))
  rec <- rec[order(rec$a, rec$b, rec$time), , drop = FALSE]
  ev <- cumsum(c(TRUE, diff(rec$a) != 0 | diff(rec$b) != 0 | diff(rec$time) > max_gap))
  res <- do.call(rbind, lapply(split(rec, ev), function(q)
    data.frame(a = q$a[1], b = q$b[1], start = min(q$time), end = max(q$time), n = nrow(q),
               min_dist = min(q$dist))))
  res$duration <- res$end - res$start
  res <- res[res$duration >= min_duration, c("a", "b", "start", "end", "duration", "n", "min_dist")]
  if ("class" %in% names(d)) {
    cl <- tapply(d$class, d$track, function(z) names(which.max(table(z))))
    res$class_a <- unname(cl[as.character(res$a)])
    res$class_b <- unname(cl[as.character(res$b)])
  }
  rownames(res) <- NULL
  res
}

#' Insects entering and leaving a nest or hive
#'
#' Counts the tracks that start outside the entrance zone and end inside it
#' (`in`: the insect entered the nest and disappeared) and the tracks that
#' start inside and end outside (`out`: the insect appeared at the entrance
#' and left), as classified by [it_zone_transitions()]. An entry is timed by
#' the last detection of its track and an exit by the first. Tracks that
#' start and end on the same side are not counted. Counts are only as good as
#' the tracks: a track broken while the insect is outside the zone does not
#' change the counts, but a broken track inside the zone can turn one entry
#' into an entry and an exit.
#'
#' @param tr Tracks.
#' @param zone Entrance polygon (data frame `x`, `y`), for example from
#'   [it_find_entrance()].
#' @param bin Bin width (s); default one bin for the whole record.
#' @param frames Optional analysed frames (`frame`, `time`), so that the bins
#'   cover the whole record.
#' @return A list with `events` (one row per counted track: `track`,
#'   `direction`, `time`) and `counts` (one row per bin: `bin_start`,
#'   `bin_end`, `n_in`, `n_out`, `net` = in minus out).
#' @examples
#' zone <- it_roi_circle(100, 100, 20)
#' tr <- data.frame(track = rep(1:2, each = 5), time = c(0:4, 10:14), frame = c(1:5, 11:15),
#'                  x = c(seq(200, 100, length.out = 5), seq(100, 250, length.out = 5)), y = 100)
#' it_entrance_counts(tr, zone)$events
#' @export
it_entrance_counts <- function(tr, zone, bin = NULL, frames = NULL) {
  zt <- it_zone_transitions(tr, zone)
  ev <- if (is.null(zt) || !nrow(zt)) data.frame(track = integer(), type = character(), start = numeric(),
                                                  end = numeric()) else zt
  ev <- ev[ev$type %in% c("entered", "exited"), , drop = FALSE]
  events <- data.frame(track = as.integer(ev$track), direction = as.character(ifelse(ev$type == "entered", "in", "out")),
                       time = as.numeric(ifelse(ev$type == "entered", ev$end, ev$start)), stringsAsFactors = FALSE)
  events <- events[order(events$time), , drop = FALSE]
  tt <- tr$time[is.finite(tr$time)]
  rng <- if (!is.null(frames)) range(frames$time) else if (length(tt)) range(tt) else c(0, 0)
  if (is.null(bin)) bin <- max(diff(rng), 1e-9)
  br <- seq(rng[1], rng[2] + bin, by = bin)
  if (length(br) < 2) br <- c(rng[1], rng[1] + bin)
  b_in <- cut(events$time[events$direction == "in"], br, right = FALSE, include.lowest = TRUE)
  b_out <- cut(events$time[events$direction == "out"], br, right = FALSE, include.lowest = TRUE)
  counts <- data.frame(bin_start = br[-length(br)], bin_end = br[-1],
                       n_in = as.integer(table(b_in)), n_out = as.integer(table(b_out)))
  counts <- counts[counts$bin_start <= rng[2], , drop = FALSE]
  counts$net <- counts$n_in - counts$n_out
  rownames(events) <- NULL; rownames(counts) <- NULL
  list(events = events, counts = counts)
}

#' Locate a nest or hive entrance from track end points
#'
#' Insects that enter a nest disappear at the entrance and insects that leave
#' appear there, so the first and last positions of tracks cluster at the
#' entrance. End points near the frame border (where insects leave the field
#' of view) are ignored; the remaining ones are binned, smoothed, and a circle
#' is centred on the densest cell. Check the result on a frame: when the
#' entrance is not the densest cluster of track ends (for example when
#' insects rest elsewhere in view), draw the zone by hand instead.
#'
#' @param tr Tracks.
#' @param width,height Frame size (pixels).
#' @param cell Cell size (pixels).
#' @param edge End points closer than this to the frame border are ignored.
#' @param min_length Minimum number of detections of a track.
#' @param radius Circle radius (pixels); by default the `radius_quantile`
#'   quantile of the distances from the centre of the end points within
#'   `search` pixels, and at least `min_radius`.
#' @param radius_quantile,min_radius,search See `radius`.
#' @param smooth Standard deviation (cells) of the smoothing kernel.
#' @return A polygon (data frame `x`, `y`) with attributes `centre`,
#'   `radius` and `n_endpoints`, or `NULL` when fewer than five end points
#'   are available.
#' @export
it_find_entrance <- function(tr, width, height, cell = 10, edge = 20, min_length = 3, radius = NULL,
                             radius_quantile = 0.75, min_radius = 25, search = 5 * cell, smooth = 1) {
  d <- tr[!is.na(tr$track) & !is.na(tr$x) & !is.na(tr$y), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d <- d[order(d$track, d$time), , drop = FALSE]
  sp <- split(seq_len(nrow(d)), d$track)
  sp <- sp[lengths(sp) >= min_length]
  if (!length(sp)) return(NULL)
  idx <- c(vapply(sp, function(r) r[1], 1L), vapply(sp, function(r) r[length(r)], 1L))
  ex <- d$x[idx]; ey <- d$y[idx]
  keep <- ex > edge & ex < width - edge & ey > edge & ey < height - edge
  ex <- ex[keep]; ey <- ey[keep]
  if (length(ex) < 5) return(NULL)
  nx <- ceiling(width / cell); ny <- ceiling(height / cell)
  ix <- pmin(pmax(floor(ex / cell) + 1, 1), nx); iy <- pmin(pmax(floor(ey / cell) + 1, 1), ny)
  z <- matrix(0, ny, nx)
  tab <- table(factor(iy, levels = seq_len(ny)), factor(ix, levels = seq_len(nx)))
  z[] <- as.numeric(tab)
  if (smooth > 0) z <- gauss_smooth(z, smooth)
  w <- which(z == max(z), arr.ind = TRUE)[1, ]
  cx <- (w[2] - 0.5) * cell; cy <- (w[1] - 0.5) * cell
  dd <- sqrt((ex - cx)^2 + (ey - cy)^2)
  near <- dd <= search
  if (any(near)) { cx <- mean(ex[near]); cy <- mean(ey[near]); dd <- sqrt((ex - cx)^2 + (ey - cy)^2); near <- dd <= search }
  if (is.null(radius)) radius <- max(min_radius, if (any(near)) stats::quantile(dd[near], radius_quantile, names = FALSE) else 0)
  zone <- it_roi_circle(cx, cy, radius)
  attr(zone, "centre") <- c(x = cx, y = cy)
  attr(zone, "radius") <- radius
  attr(zone, "n_endpoints") <- sum(near)
  zone
}
