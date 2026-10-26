match_points <- function(D, max_dist) {
  ok <- D <= max_dist
  if (!length(D) || !any(ok)) return(matrix(integer(), 0, 2))
  ng <- nrow(D); np <- ncol(D); m <- ng + np
  BIG <- 1e12
  C <- D; C[!ok] <- BIG
  M <- matrix(BIG, m, m)
  M[seq_len(ng), seq_len(np)] <- C
  M[cbind(seq_len(ng), np + seq_len(ng))] <- max_dist
  M[cbind(ng + seq_len(np), seq_len(np))] <- max_dist
  M[ng + seq_len(np), np + seq_len(ng)] <- 0
  s <- as.integer(clue::solve_LSAP(M))
  pr <- cbind(seq_len(ng), s[seq_len(ng)])
  pr <- pr[pr[, 2] <= np, , drop = FALSE]
  pr[ok[pr], , drop = FALSE]
}

#' Evaluate detections against annotations
#'
#' In each frame, predicted and annotated objects are matched one-to-one
#' (optimal assignment): by centre distance when they are at most `max_dist`
#' pixels apart, or, when `iou` is given, by intersection over union of their
#' boxes when it is at least `iou`.
#'
#' @param pred Detections (`frame`, `x`, `y`; boxes for `iou`, see
#'   [it_det_boxes()]).
#' @param truth Annotations (`frame`, `x`, `y`; boxes for `iou`).
#' @param max_dist Matching distance (pixels); ignored when `iou` is given.
#' @param frames Frames to evaluate; default all frames in `truth`.
#' @param iou Minimum IoU for a match; `NULL` for distance matching.
#' @return A list with a per-frame table and a summary (true and false
#'   positives, false negatives, precision, recall, F1, mean absolute count
#'   error, mean count bias and, for IoU matching, the mean IoU of matches).
#' @export
it_eval_detections <- function(pred, truth, max_dist = NULL, frames = NULL, iou = NULL) {
  if (is.null(max_dist) && is.null(iou)) stop("give 'max_dist' or 'iou'")
  if (is.null(frames)) frames <- sort(unique(truth$frame))
  pg <- split(seq_len(nrow(pred)), pred$frame)
  tg <- split(seq_len(nrow(truth)), truth$frame)
  if (!is.null(iou)) { pb <- it_det_boxes(pred); tb <- it_det_boxes(truth) }
  per <- do.call(rbind, lapply(frames, function(f) {
    p <- pg[[as.character(f)]]; g <- tg[[as.character(f)]]
    np <- length(p); ng <- length(g); tp <- 0L; md <- NA_real_; mi <- NA_real_
    if (np && ng) {
      D <- sqrt(outer(truth$x[g], pred$x[p], "-")^2 + outer(truth$y[g], pred$y[p], "-")^2)
      if (is.null(iou)) {
        m <- match_points(D, max_dist)
      } else {
        S <- it_box_iou(tb[g, , drop = FALSE], pb[p, , drop = FALSE])
        m <- match_points(1 - S, 1 - iou)
        if (nrow(m)) mi <- mean(S[m])
      }
      tp <- nrow(m)
      if (tp) md <- mean(D[m])
    }
    data.frame(frame = f, n_true = ng, n_pred = np, tp = tp, fp = np - tp, fn = ng - tp,
               mean_dist = md, mean_iou = mi)
  }))
  TP <- sum(per$tp); FP <- sum(per$fp); FN <- sum(per$fn)
  list(per_frame = per,
       summary = data.frame(frames = nrow(per), n_true = sum(per$n_true), n_pred = sum(per$n_pred),
                            tp = TP, fp = FP, fn = FN,
                            precision = if (TP + FP > 0) TP / (TP + FP) else NA_real_,
                            recall = if (TP + FN > 0) TP / (TP + FN) else NA_real_,
                            f1 = if (TP + FP + FN > 0) 2 * TP / (2 * TP + FP + FN) else NA_real_,
                            count_mae = mean(abs(per$n_pred - per$n_true)),
                            count_bias = mean(per$n_pred - per$n_true),
                            mean_iou = if (TP) sum(per$mean_iou * per$tp, na.rm = TRUE) / TP else NA_real_))
}

#' Evaluate tracks against annotated tracks
#'
#' Computes the CLEAR MOT metrics (MOTA, MOTP, identity switches; Bernardin &
#' Stiefelhagen 2008) and the identity metrics IDF1, IDP and IDR (Ristani et
#' al. 2016) with matching by centre distance. A predicted and an annotated
#' object can be matched in a frame when they are at most `max_dist` pixels
#' apart; correspondences of the previous frame are kept while valid.
#'
#' @param pred Tracks (`frame`, `x`, `y` and the id column `pred_id`).
#' @param truth Annotated tracks (`frame`, `x`, `y` and `truth_id`).
#' @param max_dist Matching distance (pixels).
#' @param pred_id,truth_id Names of the id columns.
#' @return A one-row data frame of metrics.
#' @export
it_eval_tracks <- function(pred, truth, max_dist, pred_id = "track", truth_id = "id") {
  pred <- pred[!is.na(pred[[pred_id]]), , drop = FALSE]
  pid <- as.character(pred[[pred_id]]); gid <- as.character(truth[[truth_id]])
  frames <- sort(unique(c(pred$frame, truth$frame)))
  pg <- split(seq_len(nrow(pred)), pred$frame)
  tg <- split(seq_len(nrow(truth)), truth$frame)
  last <- character(); prev <- character()
  FN <- 0; FP <- 0; IDSW <- 0; GT <- 0; sd <- 0; nm <- 0
  matched_frames <- stats::setNames(numeric(length(unique(gid))), unique(gid))
  ug <- unique(gid); up <- unique(pid)
  idc <- matrix(0, length(ug), length(up), dimnames = list(ug, up))
  for (f in frames) {
    p <- pg[[as.character(f)]]; g <- tg[[as.character(f)]]
    GT <- GT + length(g)
    if (!length(g)) { FP <- FP + length(p); prev <- character(); next }
    if (!length(p)) { FN <- FN + length(g); prev <- character(); next }
    D <- sqrt(outer(truth$x[g], pred$x[p], "-")^2 + outer(truth$y[g], pred$y[p], "-")^2)
    okD <- D <= max_dist
    if (any(okD)) {
      w <- which(okD, arr.ind = TRUE)
      cnt <- table(factor(gid[g[w[, 1]]], levels = ug), factor(pid[p[w[, 2]]], levels = up))
      idc <- idc + unclass(cnt)
    }
    keep <- matrix(integer(), 0, 2)
    if (length(prev)) {
      for (i in seq_along(g)) {
        h <- prev[gid[g[i]]]
        if (!is.na(h)) {
          j <- which(pid[p] == h)
          if (length(j) == 1 && okD[i, j]) keep <- rbind(keep, c(i, j))
        }
      }
    }
    ri <- setdiff(seq_along(g), keep[, 1]); ci <- setdiff(seq_along(p), keep[, 2])
    newp <- if (length(ri) && length(ci)) {
      m <- match_points(D[ri, ci, drop = FALSE], max_dist)
      cbind(ri[m[, 1]], ci[m[, 2]])
    } else matrix(integer(), 0, 2)
    for (k in seq_len(nrow(newp))) {
      gi <- gid[g[newp[k, 1]]]; hj <- pid[p[newp[k, 2]]]
      if (!is.na(last[gi]) && last[gi] != hj) IDSW <- IDSW + 1
    }
    pairs <- rbind(keep, newp)
    np <- nrow(pairs)
    if (np) {
      gi <- gid[g[pairs[, 1]]]; hj <- pid[p[pairs[, 2]]]
      last[gi] <- hj
      prev <- stats::setNames(hj, gi)
      matched_frames[gi] <- matched_frames[gi] + 1
      sd <- sd + sum(D[pairs])
    } else prev <- character()
    nm <- nm + np
    FN <- FN + length(g) - np
    FP <- FP + length(p) - np
  }
  sq <- max(dim(idc))
  P <- matrix(0, sq, sq)
  P[seq_len(nrow(idc)), seq_len(ncol(idc))] <- idc
  s <- as.integer(clue::solve_LSAP(P, maximum = TRUE))
  IDTP <- sum(P[cbind(seq_len(sq), s)])
  nP <- nrow(pred)
  gt_len <- table(gid)
  cov <- matched_frames[names(gt_len)] / as.numeric(gt_len)
  data.frame(gt_objects = GT, pred_objects = nP, gt_tracks = length(ug), pred_tracks = length(up),
             tp = nm, fp = FP, fn = FN, id_switches = IDSW,
             mota = 1 - (FN + FP + IDSW) / GT, motp = if (nm) sd / nm else NA_real_,
             idf1 = 2 * IDTP / (GT + nP), idp = IDTP / nP, idr = IDTP / GT,
             mostly_tracked = sum(cov >= 0.8), mostly_lost = sum(cov < 0.2),
             recall = nm / GT, precision = nm / nP)
}

#' Higher Order Tracking Accuracy (HOTA)
#'
#' Computes HOTA and its components (Luiten et al. 2021) following the
#' reference implementation of TrackEval: detection accuracy (DetA, DetRe,
#' DetPr), association accuracy (AssA, AssRe, AssPr) and localisation
#' accuracy (LocA), averaged over localisation thresholds `alphas`. The
#' similarity of a predicted and an annotated object is the IoU of their boxes
#' or, for points, `max(0, 1 - d / max_dist)` with `d` the centre distance.
#'
#' @param pred Tracks (`frame`, `x`, `y`, boxes for IoU, and `pred_id`).
#' @param truth Annotated tracks (`frame`, `x`, `y`, boxes for IoU, and
#'   `truth_id`).
#' @param similarity `"iou"` or `"distance"`.
#' @param max_dist Distance (pixels) at which the similarity reaches 0.
#' @param pred_id,truth_id Names of the id columns.
#' @param alphas Localisation thresholds.
#' @return A one-row data frame: `hota`, `deta`, `assa`, `deta_re`,
#'   `deta_pr`, `assa_re`, `assa_pr`, `loca` (means over `alphas`) and `hota_0`
#'   (HOTA at the lowest threshold).
#' @examples
#' g <- data.frame(frame = 1:20, x = 1:20 * 3, y = 40, id = 1, width = 8, height = 8)
#' p <- transform(g, track = ifelse(frame <= 10, 1, 2))
#' it_eval_hota(p, g)[, c("hota", "deta", "assa")]
#' @export
it_eval_hota <- function(pred, truth, similarity = c("iou", "distance"), max_dist = NULL,
                         pred_id = "track", truth_id = "id", alphas = seq(0.05, 0.95, by = 0.05)) {
  similarity <- match.arg(similarity)
  if (similarity == "distance" && is.null(max_dist)) stop("'max_dist' is needed for distance similarity")
  pred <- pred[!is.na(pred[[pred_id]]), , drop = FALSE]
  gid <- match(as.character(truth[[truth_id]]), unique(as.character(truth[[truth_id]])))
  pid <- match(as.character(pred[[pred_id]]), unique(as.character(pred[[pred_id]])))
  G <- max(c(0, gid)); Tn <- max(c(0, pid))
  if (similarity == "iou") { tb <- it_det_boxes(truth); pb <- it_det_boxes(pred) }
  frames <- sort(unique(c(truth$frame, pred$frame)))
  tg <- split(seq_len(nrow(truth)), truth$frame); pg <- split(seq_len(nrow(pred)), pred$frame)
  sim <- function(g, p) {
    if (similarity == "iou") return(it_box_iou(tb[g, , drop = FALSE], pb[p, , drop = FALSE]))
    S <- 1 - sqrt(outer(truth$x[g], pred$x[p], "-")^2 + outer(truth$y[g], pred$y[p], "-")^2) / max_dist
    S[S < 0] <- 0
    S
  }
  eps <- .Machine$double.eps
  pot <- matrix(0, G, Tn); gcnt <- numeric(G); pcnt <- numeric(Tn)
  S_list <- vector("list", length(frames))
  for (k in seq_along(frames)) {
    g <- tg[[as.character(frames[k])]]; p <- pg[[as.character(frames[k])]]
    if (length(g)) gcnt[gid[g]] <- gcnt[gid[g]] + 1
    if (length(p)) pcnt[pid[p]] <- pcnt[pid[p]] + 1
    if (!length(g) || !length(p)) next
    S <- sim(g, p)
    S_list[[k]] <- S
    den <- outer(rowSums(S), colSums(S), "+") - S
    siou <- ifelse(den > eps, S / den, 0)
    pot[gid[g], pid[p]] <- pot[gid[g], pid[p]] + siou
  }
  gas <- pot / (outer(gcnt, pcnt, "+") - pot)
  gas[!is.finite(gas)] <- 0
  na <- length(alphas)
  TP <- numeric(na); FN <- numeric(na); FP <- numeric(na); loc <- numeric(na)
  mc <- replicate(na, matrix(0, G, Tn), simplify = FALSE)
  for (k in seq_along(frames)) {
    g <- tg[[as.character(frames[k])]]; p <- pg[[as.character(frames[k])]]
    if (!length(g)) { FP <- FP + length(p); next }
    if (!length(p)) { FN <- FN + length(g); next }
    S <- S_list[[k]]
    sc <- gas[gid[g], pid[p], drop = FALSE] * S
    n <- max(dim(sc)); M <- matrix(0, n, n); M[seq_len(nrow(sc)), seq_len(ncol(sc))] <- sc
    sol <- as.integer(clue::solve_LSAP(M, maximum = TRUE))
    pr <- cbind(seq_len(n), sol)
    pr <- pr[pr[, 1] <= nrow(sc) & pr[, 2] <= ncol(sc), , drop = FALSE]
    for (a in seq_len(na)) {
      ok <- S[pr] >= alphas[a] - eps
      nm <- sum(ok)
      TP[a] <- TP[a] + nm; FN[a] <- FN[a] + length(g) - nm; FP[a] <- FP[a] + length(p) - nm
      if (nm) {
        loc[a] <- loc[a] + sum(S[pr[ok, , drop = FALSE]])
        ij <- cbind(gid[g[pr[ok, 1]]], pid[p[pr[ok, 2]]])
        mc[[a]][ij] <- mc[[a]][ij] + 1
      }
    }
  }
  res <- t(vapply(seq_len(na), function(a) {
    m <- mc[[a]]
    den <- pmax(1, outer(gcnt, pcnt, "+") - m)
    assa <- sum(m * (m / den)) / max(1, TP[a])
    assre <- sum(m * sweep(m, 1, pmax(1, gcnt), "/")) / max(1, TP[a])
    asspr <- sum(m * sweep(m, 2, pmax(1, pcnt), "/")) / max(1, TP[a])
    deta <- TP[a] / max(1, TP[a] + FN[a] + FP[a])
    c(hota = sqrt(deta * assa), deta = deta, assa = assa, deta_re = TP[a] / max(1, TP[a] + FN[a]),
      deta_pr = TP[a] / max(1, TP[a] + FP[a]), assa_re = assre, assa_pr = asspr,
      loca = max(eps, loc[a]) / max(eps, TP[a]))
  }, numeric(8)))
  out <- as.data.frame(t(colMeans(res)))
  out$hota_0 <- res[1, "hota"]
  attr(out, "per_alpha") <- data.frame(alpha = alphas, tp = TP, fn = FN, fp = FP, res, row.names = NULL)
  out
}

#' Combine HOTA over sequences
#'
#' Pools the results of [it_eval_hota()] for several sequences as TrackEval
#' does: true positives, false negatives and false positives are summed and
#' the association and localisation scores are averaged with true-positive
#' weights at each threshold before HOTA is computed.
#'
#' @param results List of results of [it_eval_hota()].
#' @return A one-row data frame like [it_eval_hota()].
#' @export
it_combine_hota <- function(results) {
  pa <- lapply(results, attr, "per_alpha")
  tp <- Reduce(`+`, lapply(pa, `[[`, "tp")); fn <- Reduce(`+`, lapply(pa, `[[`, "fn"))
  fp <- Reduce(`+`, lapply(pa, `[[`, "fp"))
  wavg <- function(v) Reduce(`+`, lapply(pa, function(z) z[[v]] * z$tp)) / pmax(1, tp)
  deta <- tp / pmax(1, tp + fn + fp); assa <- wavg("assa")
  hota <- sqrt(deta * assa)
  data.frame(hota = mean(hota), deta = mean(deta), assa = mean(assa), deta_re = mean(tp / pmax(1, tp + fn)),
             deta_pr = mean(tp / pmax(1, tp + fp)), assa_re = mean(wavg("assa_re")), assa_pr = mean(wavg("assa_pr")),
             loca = mean(wavg("loca")), hota_0 = hota[1])
}
