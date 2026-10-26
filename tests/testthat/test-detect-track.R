write_moving_square <- function(dir, n = 15) {
  dir.create(dir, showWarnings = FALSE)
  files <- character(n)
  for (k in seq_len(n)) {
    img <- matrix(0.4, 60, 80)
    x0 <- 5 + 4 * k
    img[20:25, x0:(x0 + 5)] <- 0.05
    files[k] <- file.path(dir, sprintf("frame_%06d.png", k))
    png::writePNG(img, files[k])
  }
  data.frame(frame = seq_len(n), time = seq_len(n) / 10, file = files)
}

test_that("motion detection finds a moving dark square at its centre", {
  fr <- write_moving_square(tempfile())
  det <- it_detect_motion(fr, min_area = 5, verbose = FALSE)
  expect_equal(nrow(det), 15)
  expect_equal(det$x, 7 + 4 * seq_len(15), tolerance = 1e-8)
  expect_equal(det$y, rep(22, 15), tolerance = 1e-8)
})

test_that("crossing insects keep their identities", {
  t <- (0:20) / 10
  det <- rbind(data.frame(frame = 0:20, time = t, x = 10 * (0:20), y = 10 * (0:20)),
               data.frame(frame = 0:20, time = t, x = 200 - 10 * (0:20), y = 10 * (0:20)))
  tr <- it_track(det, gate = 30, max_gap = 0.5)
  expect_equal(length(unique(tr$track)), 2)
  a <- tr$track[tr$frame == 0 & tr$x == 0]
  expect_equal(tr$x[tr$track == a & tr$frame == 20], 200)
})

test_that("gaps close tracks and stitching joins the fragments", {
  f <- setdiff(0:20, 5:7)
  det <- data.frame(frame = f, time = f / 10, x = 5 * f, y = 50)
  expect_equal(length(unique(it_track(det, gate = 20, max_gap = 0.5)$track)), 1)
  tr <- it_track(det, gate = 20, max_gap = 0.2)
  expect_equal(length(unique(tr$track)), 2)
  st <- it_stitch(tr, max_gap = 1, max_dist = 30)
  expect_equal(length(unique(st$track)), 1)
  expect_equal(attr(st, "n_joins"), 1L)
})

test_that("stitching joins a chain of fragments and ignores a distant one", {
  mk <- function(f, x0, id) data.frame(frame = f, time = f / 10, x = x0 + 5 * (f - f[1]), y = 50, track = id)
  tr <- rbind(mk(0:4, 0, 1L), mk(7:11, 35, 2L), mk(14:18, 70, 3L), mk(8:12, 400, 4L))
  st <- it_stitch(tr, max_gap = 0.5, max_dist = 30)
  expect_equal(attr(st, "n_joins"), 2L)
  expect_equal(length(unique(st$track[st$x < 200])), 1)
  expect_false(st$track[st$x == 400][1] %in% st$track[st$x < 200])
})

test_that("the per-pixel variability threshold suppresses a flickering patch", {
  dir <- tempfile(); dir.create(dir)
  files <- character(21)
  for (k in seq_len(21)) {
    img <- matrix(0.4, 60, 80)
    img[40:50, 5:15] <- if (k %% 2) 0.15 else 0.65
    x0 <- 20 + 2 * k
    img[10:15, x0:(x0 + 5)] <- 0.05
    files[k] <- file.path(dir, sprintf("frame_%06d.png", k))
    png::writePNG(img, files[k])
  }
  fr <- data.frame(frame = seq_len(21), time = seq_len(21) / 10, file = files)
  d0 <- it_detect_motion(fr, min_area = 5, verbose = FALSE)
  d1 <- it_detect_motion(fr, min_area = 5, k_pixel = 4, verbose = FALSE)
  expect_true(any(d0$y > 30))
  expect_false(any(d1$y > 30))
  expect_equal(sum(d1$y < 30), 21)
})
