test_that("entrance counts classify entries and exits", {
  zone <- it_roi_circle(100, 100, 20)
  mk <- function(track, xs, ys, t0) data.frame(track = track, frame = seq_along(xs) + t0, time = (seq_along(xs) + t0) / 10,
                                               x = xs, y = ys)
  tr <- rbind(mk(1, seq(200, 100, length.out = 6), rep(100, 6), 0),
              mk(2, seq(100, 250, length.out = 6), rep(100, 6), 20),
              mk(3, seq(0, 300, length.out = 6), rep(100, 6), 40),
              mk(4, rep(300, 6), seq(0, 50, length.out = 6), 60))
  ec <- it_entrance_counts(tr, zone, bin = 2)
  expect_equal(sort(ec$events$direction), c("in", "out"))
  expect_equal(sum(ec$counts$n_in), 1); expect_equal(sum(ec$counts$n_out), 1)
  expect_equal(ec$counts$net, ec$counts$n_in - ec$counts$n_out)
  empty <- it_entrance_counts(tr[tr$track == 4, ], zone)
  expect_equal(nrow(empty$events), 0)
})

test_that("the entrance is found at the cluster of track end points", {
  set.seed(2)
  tr <- do.call(rbind, lapply(1:30, function(i) {
    a <- stats::runif(1, 0, 2 * pi)
    xs <- 400 + cos(a) * seq(150, 5, length.out = 8) + stats::rnorm(8, 0, 1)
    ys <- 300 + sin(a) * seq(150, 5, length.out = 8) + stats::rnorm(8, 0, 1)
    data.frame(track = i, frame = 1:8 + i * 10, time = (1:8 + i * 10) / 10, x = xs, y = ys)
  }))
  z <- it_find_entrance(tr, 800, 600, min_radius = 15)
  expect_lt(sqrt(sum((attr(z, "centre") - c(400, 300))^2)), 15)
  expect_null(it_find_entrance(tr[tr$track == 1, ], 800, 600))
})

test_that("track colours are distinct and stable", {
  cl <- it_track_colours(1:20)
  expect_length(unique(cl), 20)
  expect_identical(it_track_colours(7), cl[7])
  expect_identical(it_track_colours(1:3, palette = c("red", "blue")), c("red", "blue", "red"))
})
