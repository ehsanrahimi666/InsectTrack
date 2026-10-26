#' Merge detections that belong to one insect
#'
#' Background subtraction can split one insect into several blobs (wings,
#' body, shadow). Detections of the same frame closer than `dist` pixels are
#' merged (single-linkage clustering); the merged position is the mean
#' weighted by blob area (or detection confidence when there is no area) and
#' the box is the union of the boxes.
#'
#' @param det Detections from [it_detect_motion()].
#' @param dist Merge distance in pixels.
#' @return Detections with merged rows and a column `n_merged`.
#' @export
it_merge_detections <- function(det, dist) {
  if (!nrow(det) || dist <= 0) { det$n_merged <- rep(1L, nrow(det)); return(det) }
  grp <- split(seq_len(nrow(det)), det$frame)
  out <- lapply(grp, function(r) {
    d <- det[r, , drop = FALSE]
    d$n_merged <- 1L
    if (nrow(d) < 2) return(d)
    cl <- stats::cutree(stats::hclust(stats::dist(cbind(d$x, d$y)), method = "single"), h = dist)
    if (max(cl) == nrow(d)) return(d)
    do.call(rbind, lapply(split(seq_len(nrow(d)), cl), function(k) {
      if (length(k) == 1) return(d[k, , drop = FALSE])
      a <- if ("area" %in% names(d)) d$area[k] else if ("conf" %in% names(d)) d$conf[k] else rep(1, length(k))
      m <- d[k[which.max(a)], , drop = FALSE]
      m$x <- sum(d$x[k] * a) / sum(a); m$y <- sum(d$y[k] * a) / sum(a)
      if ("area" %in% names(d)) m$area <- sum(d$area[k])
      for (v in c("xmin", "ymin", "x0", "y0")) if (v %in% names(d)) m[[v]] <- min(d[[v]][k])
      for (v in c("xmax", "ymax", "x1", "y1")) if (v %in% names(d)) m[[v]] <- max(d[[v]][k])
      m$n_merged <- length(k)
      m
    }))
  })
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

#' Link detections into tracks
#'
#' Dispatches to one of the trackers of the package: `"hungarian"`
#' ([it_track_hungarian()], point-based assignment with motion prediction),
#' `"sort"` ([it_track_sort()]), `"bytetrack"` ([it_track_bytetrack()]), or a
#' tracker of the Python package 'BoxMOT' ([it_track_boxmot()]) given as
#' `"ocsort"`, `"botsort"`, `"boosttrack"`, `"sfsort"`, `"hybridsort"`,
#' `"occluboost"`, `"strongsort"`, `"deepocsort"` or `"boxmot_bytetrack"`.
#'
#' A track is a sequence of detections attributed to one insect while it is
#' in view; an insect that leaves the frame and returns gets a new track, so
#' tracks are not individuals.
#'
#' @param det Detections with columns `frame`, `time`, `x`, `y` (and boxes for
#'   the box-based trackers).
#' @param method Tracker.
#' @param ... Arguments of the selected tracker.
#' @return The detections with an integer column `track` (`NA` for detections
#'   not assigned to a track).
#' @examples
#' det <- data.frame(frame = rep(1:10, 2), time = rep((1:10 - 1) / 10, 2),
#'                   x = c(10 + 5 * 1:10, 200 - 5 * 1:10), y = 50, conf = 0.9)
#' det$x0 <- det$x - 8; det$x1 <- det$x + 8; det$y0 <- det$y - 8; det$y1 <- det$y + 8
#' table(it_track(det, method = "bytetrack")$track)
#' table(it_track(det, method = "hungarian", gate = 20)$track)
#' @export
it_track <- function(det, method = "hungarian", ...) {
  m <- tolower(method)
  switch(m,
    hungarian = it_track_hungarian(det, ...),
    sort = it_track_sort(det, ...),
    bytetrack = it_track_bytetrack(det, ...),
    it_track_boxmot(det, method = sub("^boxmot_", "", m), ...))
}

#' Link detections into tracks by optimal assignment
#'
#' Detections are processed frame by frame. The position of each active track
#' is predicted from its smoothed velocity, and detections are assigned to
#' tracks by solving the linear assignment problem (Hungarian algorithm,
#' 'clue::solve_LSAP') on the distance between predicted (or last) positions
#' and detections. Pairs farther apart than the gate are not linked. A track
#' that receives no detection for more than `max_gap` seconds is closed;
#' unassigned detections start new tracks. Tracks with fewer than
#' `min_length` detections are discarded (track `NA`).
#'
#' @param det Detections with columns `frame`, `time`, `x`, `y`.
#' @param gate Maximum distance (pixels) between a predicted position and a
#'   detection for a link.
#' @param max_speed Speed (pixels/s) added to the gate per second elapsed
#'   since the last detection of the track.
#' @param max_gap Time (s) a track survives without detections.
#' @param min_length Minimum number of detections of a track.
#' @param velocity_weight Weight (0-1) of the newest displacement in the
#'   exponentially smoothed velocity.
#' @param use_last Use the smaller of the distances to the predicted and to
#'   the last position, which helps when insects stop or turn.
#' @return The detections with an integer column `track` (consecutive ids).
#' @export
it_track_hungarian <- function(det, gate = 40, max_speed = 0, max_gap = 0.5, min_length = 3,
                     velocity_weight = 0.6, use_last = TRUE) {
  need <- c("frame", "time", "x", "y")
  if (!all(need %in% names(det))) stop("detections need columns ", paste(need, collapse = ", "))
  det <- det[order(det$frame, det$time), , drop = FALSE]
  rownames(det) <- NULL
  n <- nrow(det)
  track <- rep(NA_integer_, n)
  if (n == 0) { det$track <- integer(); return(det) }
  grp <- split(seq_len(n), det$frame)
  grp <- grp[order(as.numeric(names(grp)))]
  tid <- integer(); tx <- numeric(); ty <- numeric(); tvx <- numeric(); tvy <- numeric()
  tt <- numeric(); tn <- integer()
  next_id <- 1L
  BIG <- 1e12
  for (rows in grp) {
    t <- det$time[rows[1]]
    if (length(tid)) {
      alive <- (t - tt) <= max_gap + 1e-9
      if (!all(alive)) {
        tid <- tid[alive]; tx <- tx[alive]; ty <- ty[alive]; tvx <- tvx[alive]
        tvy <- tvy[alive]; tt <- tt[alive]; tn <- tn[alive]
      }
    }
    dx <- det$x[rows]; dy <- det$y[rows]
    nT <- length(tid); nD <- length(rows)
    matched <- integer(nD)
    if (nT > 0) {
      dt <- t - tt
      px <- tx + tvx * dt; py <- ty + tvy * dt
      cost <- sqrt(outer(px, dx, "-")^2 + outer(py, dy, "-")^2)
      if (use_last) cost <- pmin(cost, sqrt(outer(tx, dx, "-")^2 + outer(ty, dy, "-")^2))
      g <- gate + max_speed * dt
      ok <- cost <= g
      if (any(ok)) {
        m <- nT + nD
        M <- matrix(BIG, m, m)
        C <- cost; C[!ok] <- BIG
        M[seq_len(nT), seq_len(nD)] <- C
        M[cbind(seq_len(nT), nD + seq_len(nT))] <- g
        M[cbind(nT + seq_len(nD), seq_len(nD))] <- gate
        M[nT + seq_len(nD), nD + seq_len(nT)] <- 0
        sol <- as.integer(clue::solve_LSAP(M))
        for (i in seq_len(nT)) {
          j <- sol[i]
          if (j <= nD && ok[i, j]) matched[j] <- i
        }
      }
    }
    for (j in which(matched > 0)) {
      i <- matched[j]
      dti <- t - tt[i]
      if (dti > 0) {
        vx <- (dx[j] - tx[i]) / dti; vy <- (dy[j] - ty[i]) / dti
        if (tn[i] == 1L) {
          tvx[i] <- vx; tvy[i] <- vy
        } else {
          tvx[i] <- velocity_weight * vx + (1 - velocity_weight) * tvx[i]
          tvy[i] <- velocity_weight * vy + (1 - velocity_weight) * tvy[i]
        }
      }
      tx[i] <- dx[j]; ty[i] <- dy[j]; tt[i] <- t; tn[i] <- tn[i] + 1L
      track[rows[j]] <- tid[i]
    }
    new <- which(matched == 0L)
    if (length(new)) {
      ids <- next_id + seq_along(new) - 1L
      tid <- c(tid, ids); tx <- c(tx, dx[new]); ty <- c(ty, dy[new])
      tvx <- c(tvx, rep(0, length(new))); tvy <- c(tvy, rep(0, length(new)))
      tt <- c(tt, rep(t, length(new))); tn <- c(tn, rep(1L, length(new)))
      track[rows[new]] <- ids
      next_id <- next_id + length(new)
    }
  }
  cnt <- table(track)
  short <- as.integer(names(cnt)[cnt < min_length])
  track[track %in% short] <- NA_integer_
  det$track <- match(track, sort(unique(track[!is.na(track)])))
  attr(det, "tracking") <- list(method = "hungarian", gate = gate, max_speed = max_speed, max_gap = max_gap,
                                min_length = min_length, velocity_weight = velocity_weight,
                                use_last = use_last)
  det
}

#' Join track fragments
#'
#' A track that ends and another that starts shortly afterwards close to the
#' predicted position of the first are joined. Candidate pairs are resolved
#' one-to-one by optimal assignment, so chains of fragments can be joined.
#'
#' @param tr Tracks from [it_track()].
#' @param max_gap Maximum time (s) between the end of one fragment and the
#'   start of the next.
#' @param max_dist Maximum distance (pixels) between the predicted end and
#'   the next start, plus `max_speed` times the gap.
#' @param max_speed Speed (pixels/s) added to `max_dist` per second of gap.
#' @return Tracks with joined ids and the number of joins as attribute
#'   `n_joins`.
#' @export
it_stitch <- function(tr, max_gap = 1, max_dist = 50, max_speed = 0) {
  d <- tr[!is.na(tr$track), , drop = FALSE]
  attr(tr, "n_joins") <- 0L
  if (!nrow(d)) return(tr)
  d <- d[order(d$track, d$time), , drop = FALSE]
  sp <- split(seq_len(nrow(d)), d$track)
  ids <- as.integer(names(sp))
  nt <- length(sp)
  first <- vapply(sp, function(r) r[1], 1L)
  last <- vapply(sp, function(r) r[length(r)], 1L)
  prev <- vapply(sp, function(r) if (length(r) > 1) r[length(r) - 1] else r[1], 1L)
  t0 <- d$time[first]; t1 <- d$time[last]
  dtt <- d$time[last] - d$time[prev]
  vx <- ifelse(dtt > 0, (d$x[last] - d$x[prev]) / dtt, 0)
  vy <- ifelse(dtt > 0, (d$y[last] - d$y[prev]) / dtt, 0)
  # candidate pairs: fragment j starts within (end of i, end of i + max_gap]
  o <- order(t0); t0s <- t0[o]
  lo <- findInterval(t1, t0s) + 1L
  hi <- findInterval(t1 + max_gap, t0s)
  n_c <- pmax(0L, hi - lo + 1L)
  if (!any(n_c > 0)) return(tr)
  I <- rep(seq_len(nt), n_c)
  J <- o[unlist(lapply(which(n_c > 0), function(i) lo[i]:hi[i]))]
  gap <- t0[J] - t1[I]
  ex <- d$x[last][I] + vx[I] * gap; ey <- d$y[last][I] + vy[I] * gap
  sx <- d$x[first][J]; sy <- d$y[first][J]
  cost <- pmin(sqrt((ex - sx)^2 + (ey - sy)^2),
               sqrt((d$x[last][I] - sx)^2 + (d$y[last][I] - sy)^2))
  ok <- gap > 0 & cost <= max_dist + max_speed * gap & I != J
  if (!any(ok)) return(tr)
  I <- I[ok]; J <- J[ok]; cost <- cost[ok]
  # connected groups of ends (nodes 1..nt) and starts (nodes nt + 1..2nt)
  par_ <- seq_len(2L * nt)
  fnd <- function(a) { while (par_[a] != a) { par_[a] <<- par_[par_[a]]; a <- par_[a] }; a }
  for (k in seq_along(I)) {
    a <- fnd(I[k]); b <- fnd(nt + J[k])
    if (a != b) par_[max(a, b)] <- min(a, b)
  }
  grp <- vapply(I, fnd, 1L)
  links <- list()
  thr <- max_dist + max_speed * max_gap
  BIG <- 1e12
  for (g in unique(grp)) {
    w <- which(grp == g)
    ri <- unique(I[w]); ci <- unique(J[w])
    nr <- length(ri); nc <- length(ci)
    if (nr * nc > 250000) {
      ord <- w[order(cost[w])]; ue <- integer(); us <- integer()
      for (k in ord) if (!(I[k] %in% ue) && !(J[k] %in% us)) {
        links[[length(links) + 1]] <- c(I[k], J[k]); ue <- c(ue, I[k]); us <- c(us, J[k])
      }
      next
    }
    C <- matrix(BIG, nr, nc)
    C[cbind(match(I[w], ri), match(J[w], ci))] <- cost[w]
    m <- nr + nc
    M <- matrix(BIG, m, m)
    M[seq_len(nr), seq_len(nc)] <- C
    M[cbind(seq_len(nr), nc + seq_len(nr))] <- thr
    M[cbind(nr + seq_len(nc), seq_len(nc))] <- thr
    M[nr + seq_len(nc), nc + seq_len(nr)] <- 0
    sol <- as.integer(clue::solve_LSAP(M))
    for (a in seq_len(nr)) {
      b <- sol[a]
      if (b <= nc && C[a, b] < BIG) links[[length(links) + 1]] <- c(ri[a], ci[b])
    }
  }
  parent <- seq_len(nt)
  find <- function(a) { while (parent[a] != a) a <- parent[a]; a }
  for (l in links) parent[find(l[2])] <- find(l[1])
  root <- vapply(seq_len(nt), find, 1L)
  newid <- ids[root]
  tr$track <- newid[match(tr$track, ids)]
  tr$track <- match(tr$track, sort(unique(tr$track[!is.na(tr$track)])))
  attr(tr, "n_joins") <- length(links)
  tr
}
