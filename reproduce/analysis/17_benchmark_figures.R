# Figures of the tracking benchmark: accuracy of each tracker with annotated
# boxes, trained detectors and zero-shot detections as input, and hive-entrance
# in/out counts against the counts from the annotated tracks.

source(here::here("analysis", "10_common.R"))
od <- bout("tracking")
p <- utils::read.csv(file.path(od, "metrics_pooled.csv"))
bs <- utils::read.csv(file.path(od, "metrics_by_sequence.csv"))
trk <- c("native_hungarian", "native_sort", "native_bytetrack", "native_bytetrack_stitch", "boxmot_bytetrack",
         "boxmot_ocsort", "boxmot_botsort", "boxmot_boosttrack", "boxmot_sfsort", "boxmot_hybridsort")
tlab <- c("Hungarian (R)", "SORT (R)", "ByteTrack (R)", "ByteTrack + joining (R)", "ByteTrack (BoxMOT)",
          "OC-SORT", "BoT-SORT", "BoostTrack", "SFSORT", "HybridSort")
inputs <- c("ground_truth", "yolo_melipona", "yolo_bee", "gdino_tiny_512")
ilab <- c(ground_truth = "Annotated boxes", yolo_melipona = "YOLO26n trained on Melipona",
          yolo_bee = "YOLO26n trained on Delosperma bees", gdino_tiny_512 = "Grounding DINO tiny (zero-shot)")
icol <- c(ground_truth = "#d95f02", yolo_melipona = "#1f4e9c", yolo_bee = "#2a9d8f", gdino_tiny_512 = "#7f7f7f")
panels <- list(c("melipona", "120fps"), c("melipona", "30fps"), c("native_bee", "all"))
plab <- c("Melipona, 120 frames per second", "Melipona, 30 frames per second", "Native bees")
dev <- function(f, w, h) grDevices::png(file.path(od, f), width = w, height = h, res = 230,
                                        type = if (isTRUE(capabilities("cairo"))) "cairo" else NULL)

dev("tracking_benchmark.png", 3600, 2600)
graphics::layout(matrix(c(1:9, rep(10, 3)), 4, byrow = TRUE), heights = c(1, 1, 1, 0.22))
for (metric in c("hota", "idf1", "mota")) {
  for (k in seq_along(panels)) {
    pn <- panels[[k]]
    z <- p[p$dataset == pn[1] & p$fps_group == pn[2], ]
    graphics::par(mar = c(if (metric == "mota") 8.5 else 1.2, 4.5, 2.4, 0.5))
    lo <- if (metric == "mota") max(-1, min(c(0, z$mota), na.rm = TRUE)) else 0
    graphics::plot(NA, xlim = c(0.5, length(trk) + 0.5), ylim = c(lo, 1), xaxt = "n", las = 1,
                   ylab = c(hota = "HOTA", idf1 = "IDF1", mota = "MOTA")[[metric]], xlab = "")
    graphics::abline(h = c(hota = bcfg$criteria$tracking_hota, idf1 = bcfg$criteria$tracking_idf1,
                           mota = bcfg$criteria$tracking_mota)[[metric]], lty = 3, col = "grey50")
    if (metric == "mota") graphics::axis(1, at = seq_along(trk), labels = tlab, las = 2, cex.axis = 0.8)
    else graphics::axis(1, at = seq_along(trk), labels = FALSE)
    for (j in seq_along(inputs)) {
      zz <- z[z$input == inputs[j], ]
      if (!nrow(zz)) next
      x <- match(zz$tracker, trk) + (j - 2.5) * 0.17
      v <- pmax(zz[[metric]], lo)
      graphics::points(x, v, pch = 19, col = icol[[inputs[j]]], cex = 1.1)
    }
    if (metric == "hota") graphics::title(sprintf("(%s) %s", letters[k], plab[k]), adj = 0, line = 0.8)
  }
}
graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
graphics::legend("center", legend = ilab[inputs], col = icol[inputs], pch = 19, ncol = 4, bty = "n", cex = 1.05)
grDevices::dev.off()

# in/out counts per sequence: predicted against counts from the annotated tracks
best <- c("native_bytetrack", "boxmot_ocsort", "native_hungarian")
dev("inout_counts.png", 3300, 1300)
graphics::par(mfrow = c(1, 3), mar = c(4.5, 4.5, 2.6, 0.8))
for (k in seq_along(best)) {
  z <- bs[bs$tracker == best[k] & bs$input %in% c("ground_truth", "yolo_melipona", "yolo_bee"), ]
  mx <- max(1, z$gt_in + z$gt_out, z$n_in + z$n_out)
  graphics::plot(NA, xlim = c(0, mx), ylim = c(0, mx), las = 1, xlab = "Movements in annotated tracks (in + out)",
                 ylab = "Movements counted (in + out)")
  graphics::abline(0, 1, lty = 2, col = "grey50")
  for (inp in unique(z$input)) {
    q <- z[z$input == inp, ]
    graphics::points(q$gt_in + q$gt_out, q$n_in + q$n_out, cex = 1.2, lwd = 1.5, col = icol[[inp]],
                     pch = ifelse(q$dataset == "native_bee", 17, ifelse(q$fps == 120, 19, 1)))
  }
  graphics::title(sprintf("(%s) %s", letters[k], tlab[match(best[k], trk)]), adj = 0, line = 0.8)
  if (k == 1) graphics::legend("topleft", c(ilab[c("ground_truth", "yolo_melipona", "yolo_bee")],
                                            "Melipona, 120 frames per second", "Melipona, 30 frames per second",
                                            "Native bees"),
                               col = c(icol[c("ground_truth", "yolo_melipona", "yolo_bee")], rep("black", 3)),
                               pch = c(15, 15, 15, 19, 1, 17), bty = "n", cex = 0.85)
}
grDevices::dev.off()
log_msg("benchmark figures written")
