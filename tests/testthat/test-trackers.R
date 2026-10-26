two_insects <- function(n = 40, noise = 0, drop = integer(), conf = 0.9) {
  mk <- function(id, x0, y0, vx, vy) data.frame(frame = seq_len(n), time = (seq_len(n) - 1) / 10,
                                                x = x0 + vx * (seq_len(n) - 1), y = y0 + vy * (seq_len(n) - 1), id = id)
  g <- rbind(mk(1, 100, 100, 5, 2), mk(2, 300, 100, -5, 2.2))
  g$width <- 20; g$height <- 20
  d <- g
  if (noise > 0) { d$x <- d$x + stats::rnorm(nrow(d), 0, noise); d$y <- d$y + stats::rnorm(nrow(d), 0, noise) }
  d$x0 <- d$x - 10; d$x1 <- d$x + 10; d$y0 <- d$y - 10; d$y1 <- d$y + 10
  d$conf <- conf
  if (length(drop)) d <- d[-drop, ]
  list(gt = g, det = d)
}

test_that("box helpers convert and compare boxes", {
  b <- it_det_boxes(data.frame(x = 10, y = 20, width = 4, height = 6))
  expect_equal(unname(b[1, ]), c(8, 17, 12, 23))
  b2 <- it_det_boxes(data.frame(x = 10, y = 20, w = 4, h = 6))
  expect_equal(b, b2)
  expect_equal(unname(it_det_boxes(data.frame(x = 5, y = 5), box_size = 2)[1, ]), c(4, 4, 6, 6))
  a <- rbind(c(0, 0, 10, 10)); z <- rbind(c(0, 0, 10, 10), c(5, 0, 15, 10), c(20, 20, 30, 30))
  expect_equal(as.vector(it_box_iou(a, z)), c(1, 50 / 150, 0))
})

test_that("SORT and ByteTrack keep two crossing insects apart", {
  set.seed(1)
  x <- two_insects(noise = 0.5)
  for (m in c("sort", "bytetrack", "hungarian")) {
    tr <- it_track(x$det, method = m)
    expect_equal(length(unique(stats::na.omit(tr$track))), 2, info = m)
    e <- it_eval_tracks(tr, x$gt, 5)
    expect_gt(e$idf1, 0.9)
  }
})

test_that("ByteTrack bridges missed frames and recovers low-score detections", {
  x <- two_insects(drop = c(10:13))
  tr <- it_track_bytetrack(x$det, track_buffer = 30, frame_rate = 10)
  expect_equal(length(unique(stats::na.omit(tr$track))), 2)
  y <- two_insects()
  y$det$conf[y$det$id == 1 & y$det$frame %in% 15:18] <- 0.2
  tr2 <- it_track_bytetrack(y$det, track_thresh = 0.5, low_thresh = 0.1)
  expect_true(all(!is.na(tr2$track[tr2$id == 1 & tr2$frame %in% 15:18])))
  expect_equal(length(unique(tr2$track[tr2$id == 1])), 1)
})

test_that("SORT confirms tracks only after min_hits matches", {
  d <- data.frame(frame = c(1, 5), time = c(0, 0.4), x = c(50, 300), y = c(50, 300))
  tr <- it_track_sort(d, min_hits = 3, box_size = 10)
  expect_true(all(is.na(tr$track)))
})

test_that("the dispatcher passes arguments to the Hungarian tracker", {
  x <- two_insects()
  a <- it_track(x$det, gate = 30, max_gap = 0.5)
  b <- it_track_hungarian(x$det, gate = 30, max_gap = 0.5)
  expect_identical(a$track, b$track)
  expect_identical(attr(a, "tracking")$method, "hungarian")
})
