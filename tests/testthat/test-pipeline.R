test_that("it_track_video runs the whole analysis on a synthetic video", {
  skip_on_cran()
  dir <- tempfile("synthetic_"); dir.create(dir)
  n <- 30
  for (k in seq_len(n)) {
    m <- array(0.9, c(120, 160, 3))
    x1 <- 20 + 3 * k; y1 <- 40
    x2 <- 140 - 2 * k; y2 <- 80
    m[y1 + 0:5, x1 + 0:5, ] <- 0.1
    m[y2 + 0:5, x2 + 0:5, ] <- 0.1
    png::writePNG(m, file.path(dir, sprintf("img_%03d.png", k)))
  }
  video <- file.path(dir, "synthetic.mp4")
  av::av_encode_video(list.files(dir, "\\.png$", full.names = TRUE), video, framerate = 10, verbose = FALSE)
  out <- file.path(dir, "out")
  res <- it_track_video(video, output_dir = out, detector = "motion", fps = 10, tracker = "bytetrack",
                        detector_args = list(min_area = 10, bg_frames = 15), entrance = it_roi_circle(150, 40, 15),
                        verbose = FALSE)
  expect_s3_class(res, "it_video_result")
  expect_equal(res$summary$tracks, 2)
  for (f in c("tracks.csv", "track_summary.csv", "activity.csv", "heatmap.csv", "heatmap.png", "activity.png",
              "trajectories.png", "entrance_counts.csv", "report.html", "settings.json", "synthetic_tracked.mp4")) {
    expect_true(file.exists(file.path(out, f)), info = f)
  }
  expect_equal(nrow(res$frames), n)
  expect_gte(it_video_info(file.path(out, "synthetic_tracked.mp4"))$duration_s, 2.9)
})

test_that("image folders are read in frame order", {
  dir <- tempfile("imgs_"); dir.create(dir)
  for (k in c(3, 1, 2)) png::writePNG(array(0.5, c(10, 10, 3)), file.path(dir, sprintf("cam_frame_%02d.png", k)))
  fr <- it_frames_from_images(dir, fps = 5, frame_regex = "frame_(\\d+)")
  expect_equal(fr$frame, 1:3)
  expect_equal(fr$time, c(0, 0.2, 0.4))
})
