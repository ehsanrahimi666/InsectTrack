# Example data of the package: detections of the example clip by the YOLO26n
# bee model (inst/extdata/example_detections.csv), so that tracking, summaries
# and plots can be run without Python, and the output figure of the README
# (man/figures/README-outputs.jpg) from the saved results of the honeybee
# flight demonstration video.

library(InsectTrack)
root <- here::here()
pkg <- file.path(root, "package", "InsectTrack")
ext <- file.path(pkg, "inst", "extdata")

# Detections of the example clip ---------------------------------------------------
clip <- file.path(ext, "honeybees_delosperma.mp4")
fr <- it_extract_frames(clip, dir = file.path(tempdir(), "example_clip"), gray = FALSE)
det <- it_detect_yolo(fr, model = file.path(root, "data", "derived", "models", "flower333_yolo26n_deploy.pt"),
                      imgsz = 640, conf = 0.1, verbose = FALSE)
num <- c("time", "x", "y", "x0", "y0", "x1", "y1", "conf")
det[num] <- lapply(det[num], round, 3)
utils::write.csv(det[, c("frame", "time", num[-1], "class")], file.path(ext, "example_detections.csv"),
                 row.names = FALSE)
message(sprintf("example detections: %d boxes in %d of %d frames", nrow(det), length(unique(det$frame)), nrow(fr)))

# README figure: outputs of the honeybee flight demonstration video -----------------------
od <- file.path(root, "output", "demo", "hive_flight")
st <- jsonlite::read_json(file.path(od, "settings.json"))
tr <- utils::read.csv(file.path(od, "tracks.csv"))
bg <- png::readPNG(file.path(od, "frame_middle.png"))
still <- png::readPNG(file.path(od, "still.png"))
W <- st$width; H <- st$height
# heat map and hot spots as saved by it_track_video()
hmt <- utils::read.csv(file.path(od, "heatmap.csv"))
xs <- sort(unique(hmt$x)); ys <- sort(unique(hmt$y))
idx <- cbind(match(hmt$y, ys), match(hmt$x, xs))
z <- matrix(0, length(ys), length(xs)); z[idx] <- hmt$insect_seconds
hot <- matrix(FALSE, length(ys), length(xs)); hot[idx] <- as.logical(hmt$hot)
hm <- list(z = z, x = xs, y = ys, cell = st$heatmap_cell, width = W, height = H, hot = hot)
act <- utils::read.csv(file.path(od, "activity.csv"))
fig <- file.path(pkg, "man", "figures")
dir.create(fig, showWarnings = FALSE, recursive = TRUE)
grDevices::jpeg(file.path(fig, "README-outputs.jpg"), width = 2400, height = 1460, res = 200, quality = 88,
               type = if (isTRUE(capabilities("cairo"))) "cairo" else NULL)
graphics::layout(matrix(1:8, 4, byrow = TRUE), heights = c(0.09, 1, 0.09, 1))
strip <- function(label) {
  graphics::par(mar = c(0, 0.4, 0, 0))
  graphics::plot.new()
  graphics::text(0, 0.35, label, adj = c(0, 0.5), font = 2, cex = 1.15)
}
strip("a  Tracked video: one colour and identity per bee")
strip("b  Trajectories")
graphics::par(mar = c(0.2, 0.2, 0.2, 0.2))
graphics::plot.new(); graphics::plot.window(c(0, W), c(H, 0), xaxs = "i", yaxs = "i", asp = 1)
graphics::rasterImage(still, 0, H, W, 0)
it_plot_tracks(tr, W, H, background = bg)
strip("c  Time spent (insect-seconds) and hot spots")
strip("d  Bees in view")
it_plot_heatmap(hm, background = bg)
it_plot_activity(act)
grDevices::dev.off()
