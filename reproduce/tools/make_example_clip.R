# Example data of the package: a 10-s, 640 x 360 crop of the Delosperma
# smartphone video around the flowers with the most honeybee detections in
# its first 30 s, and the real-time 10 frames-per-second version of the
# Delosperma analysis clip used by the demonstration videos.

library(InsectTrack)
root <- here::here()
map <- utils::read.csv(file.path(root, "data", "derived", "clips", "delosperma_clip_frames.csv"))
fr <- it_extract_frames(file.path(root, "data", "derived", "clips", "delosperma_clip.mp4"),
                        dir = file.path(tempdir(), "delosperma_clip"), gray = FALSE, format = "png")
stopifnot(nrow(fr) == nrow(map))
regular <- (map$original_frame - min(map$original_frame)) %% 3 == 0
clip10 <- file.path(root, "data", "derived", "clips", "delosperma_clip_10fps.mp4")
av::av_encode_video(fr$file[regular], clip10, framerate = 10, codec = "libx264", vfilter = "format=yuv420p",
                    verbose = FALSE)

d <- utils::read.csv(file.path(root, "data", "raw", "delosperma", "yolo26n_detections.csv"))
d <- d[d$frame >= 14700 & d$frame <= 14700 + 300, ]
xc <- d$cx * 2 / 3; yc <- d$cy * 2 / 3
best <- c(0, 0); bs <- -1
for (x0 in seq(0, 640, 32)) for (y0 in seq(0, 360, 24)) {
  n <- sum(xc >= x0 & xc < x0 + 640 & yc >= y0 & yc < y0 + 360)
  if (n > bs) { bs <- n; best <- c(x0, y0) }
}
av::av_encode_video(clip10, file.path(root, "package", "InsectTrack", "inst", "extdata", "honeybees_delosperma.mp4"),
                    framerate = 10, codec = "libx264",
                    vfilter = sprintf("trim=duration=10,crop=640:360:%d:%d,format=yuv420p", best[1], best[2]),
                    verbose = FALSE)
message(sprintf("example crop at x = %d, y = %d (%d detections)", best[1], best[2], bs))
