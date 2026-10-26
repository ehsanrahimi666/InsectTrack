test_that("blobs are labelled with area and centroid in pixel-centre coordinates", {
  m <- matrix(FALSE, 20, 30)
  m[5:7, 10:13] <- TRUE
  m[15, 25] <- TRUE
  b <- it_label_blobs(m, matrix(1, 20, 30), 1L, Inf)
  expect_equal(nrow(b), 2)
  big <- b[which.max(b$area), ]
  expect_equal(big$area, 12)
  expect_equal(big$x, 11)
  expect_equal(big$y, 5.5)
  expect_equal(c(big$xmin, big$xmax, big$ymin, big$ymax), c(9, 13, 4, 7))
})

test_that("diagonal pixels form one blob (8-connectivity) and area limits apply", {
  m <- matrix(FALSE, 5, 5)
  m[1, 1] <- m[2, 2] <- m[3, 3] <- TRUE
  expect_equal(nrow(it_label_blobs(m, matrix(1, 5, 5), 1L, Inf)), 1)
  expect_equal(nrow(it_label_blobs(m, matrix(1, 5, 5), 4L, Inf)), 0)
})

test_that("box mean, morphology and median stack are correct", {
  x <- matrix(1:12, 3, 4) * 1
  bm <- it_box_mean(x, 1)
  expect_equal(bm[2, 2], mean(x[1:3, 1:3]))
  expect_equal(bm[1, 1], mean(x[1:2, 1:2]))
  m <- matrix(FALSE, 7, 7); m[4, 4] <- TRUE
  expect_equal(sum(it_morph(m, 1, TRUE)), 9)
  expect_equal(sum(it_morph(it_morph(m, 1, TRUE), 1, FALSE)), 1)
  a <- array(c(1, 2, 9, 4, 3, 8), c(2, 1, 3))
  expect_equal(it_median_stack(a, 2L, 3L), c(3, 4))
})

test_that("point in polygon follows the even-odd rule", {
  sq <- data.frame(x = c(0, 10, 10, 0), y = c(0, 0, 10, 10))
  expect_equal(it_point_in_polygon(c(5, 15, NA), c(5, 5, 1), sq$x, sq$y), c(TRUE, FALSE, NA))
})

test_that("rotation metadata is read only from MP4 and QuickTime files", {
  f <- tempfile(fileext = ".ogv")
  writeBin(c(charToRaw("OggS"), as.raw(c(0, 2, 0, 0)), as.raw(0:63)), f)
  expect_identical(InsectTrack:::video_rotation(f), 0)
  unlink(f)
})
