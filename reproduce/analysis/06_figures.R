# Summary figures drawn from the saved outputs.

source(here::here("analysis", "00_common.R"))
od <- out_dir("figures")

# Figure 1: trajectories and activity heat maps of each scenario -------------
panels <- list(
  c("V01", "Honeybee hive entrance"), c("V02", "Apiary, three entrances"),
  c("V03", "Wasp ground nest"), c("V04", "Hornet founding nest"),
  c("V05", "Wasps at a honey bait"), c("V08", "Harvester ant nest"))
files <- lapply(panels, function(p) c(
  file.path(root, cfg$paths$output, "web_videos", p[1], paste0(p[1], "_trajectories.png")),
  file.path(root, cfg$paths$output, "web_videos", p[1], paste0(p[1], "_heatmap.png")), p[2]))
files[[length(files) + 1]] <- c(
  file.path(root, cfg$paths$output, "delosperma", "delosperma_trajectories_clip_window.png"),
  file.path(root, cfg$paths$output, "delosperma", "delosperma_heatmap_full_video.png"),
  "Honeybees on Delosperma (heat map: 22 min)")
save_png(file.path(od, "fig1_scenarios.png"), 3200, 2350, res = 200, {
  graphics::par(mfrow = c(4, 2), mar = c(0.3, 0.3, 1.9, 0.3), oma = c(0, 0, 0, 0))
  for (k in seq_along(files)) {
    a <- png::readPNG(files[[k]][1]); b <- png::readPNG(files[[k]][2])
    h <- 0.5625
    graphics::plot.new(); graphics::plot.window(c(0, 2.02), c(0, h), xaxs = "i", yaxs = "i")
    ha <- dim(a)[1] / dim(a)[2]; ha <- min(ha, h); wa <- ha * dim(a)[2] / dim(a)[1]
    graphics::rasterImage(a, 0.5 - wa / 2, (h - ha) / 2, 0.5 + wa / 2, (h + ha) / 2)
    graphics::rasterImage(b, 1.52 - wa / 2, (h - ha) / 2, 1.52 + wa / 2, (h + ha) / 2)
    graphics::title(sprintf("(%s) %s", letters[k], files[[k]][3]), adj = 0, cex.main = 1.25, line = 0.6)
  }
})

# Figure 2: accuracy at the Melipona hive entrance -------------------------------
m <- utils::read.csv(file.path(root, cfg$paths$output, "melipona", "melipona_metrics_by_sequence.csv"))
m <- m[m$stitched, ]
y <- utils::read.csv(file.path(root, cfg$paths$output, "melipona_yolo", "melipona_yolo_metrics_by_sequence.csv"))
y$input <- ifelse(y$tracker == "InsectTrack", "YOLO + InsectTrack tracker", "YOLO + ByteTrack")
cols_keep <- c("sequence", "fps", "input", "det_f1", "mota", "idf1")
d <- rbind(m[, cols_keep], y[, cols_keep])
cond <- c("07h00", "08h00v", "10h00", "12h00", "15h00", "17h00", "18h00")
lab <- c("07:00", "08:00*", "10:00", "12:00", "15:00", "17:00", "18:00")
inputs <- c("motion detections", "motion detections, pixel threshold", "YOLO + InsectTrack tracker",
            "YOLO + ByteTrack", "annotated positions")
legend_lab <- c("Background subtraction", "Background subtraction, pixel threshold",
                "YOLO26n + InsectTrack tracker", "YOLO26n + ByteTrack", "Annotated positions + InsectTrack tracker")
cols <- c("#7f7f7f", "#b07aa1", "#1f4e9c", "#2a9d8f", "#d95f02")
save_png(file.path(od, "fig2_melipona_accuracy.png"), 3300, 1250, res = 220, {
  graphics::layout(matrix(c(1, 2, 3, 4, 4, 4), 2, byrow = TRUE), heights = c(1, 0.22))
  graphics::par(mar = c(4.8, 4.6, 2.4, 0.8))
  for (metric in c("det_f1", "mota", "idf1")) {
    ylab <- c(det_f1 = "Detection F1", mota = "MOTA", idf1 = "IDF1")[[metric]]
    lo <- if (metric == "mota") -1 else 0
    graphics::plot(NA, xlim = c(0.5, length(cond) + 0.5), ylim = c(lo, 1), xaxt = "n", xlab = "Time of day",
                   ylab = ylab, las = 1)
    graphics::abline(h = c(det_f1 = cfg$criteria$detection_f1, mota = cfg$criteria$tracking_mota,
                           idf1 = cfg$criteria$tracking_idf1)[[metric]], lty = 3, col = "grey40")
    graphics::axis(1, at = seq_along(cond), labels = lab, cex.axis = 0.85)
    for (j in seq_along(inputs)) {
      if (metric == "det_f1" && inputs[j] %in% c("annotated positions", "YOLO + ByteTrack")) next
      for (fps in c(120, 30)) {
        dd <- d[d$input == inputs[j] & d$fps == fps, ]
        x <- match(dd$sequence, cond) + (j - 3) * 0.14
        v <- pmax(dd[[metric]], lo)
        graphics::points(x, v, pch = if (fps == 120) 19 else 1, col = cols[j], cex = 1.3, lwd = 1.5)
        graphics::points(x[dd[[metric]] < lo], rep(lo, sum(dd[[metric]] < lo)), pch = 6, col = cols[j], cex = 1.3)
      }
    }
    graphics::title(sprintf("(%s) %s", letters[match(metric, c("det_f1", "mota", "idf1"))], ylab), adj = 0, line = 0.7)
  }
  graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
  graphics::legend("center", horiz = FALSE, ncol = 4, bty = "n", cex = 0.95,
                   legend = c(legend_lab, "120 frames per second", "30 frames per second", "value below axis"),
                   col = c(cols, "black", "black", "black"), pch = c(rep(15, 5), 19, 1, 6))
})
log_msg("figures written")
