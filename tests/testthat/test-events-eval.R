test_that("line crossings, visits, zone transitions and interactions", {
  tr <- data.frame(frame = 0:10, time = (0:10) / 10, x = 50, y = 10 * (0:10), track = 1L)
  cr <- it_line_crossings(tr, c(0, 45, 100, 45), labels = c("down", "up"))
  expect_equal(nrow(cr), 1)
  expect_equal(cr$direction, "down")
  expect_equal(cr$time, 0.45, tolerance = 1e-8)
  roi <- data.frame(x = c(40, 60, 60, 40), y = c(25, 25, 65, 65))
  v <- it_roi_visits(tr, list(flower = roi))
  expect_equal(nrow(v), 1)
  expect_equal(c(v$start, v$end, v$n), c(0.3, 0.6, 4))
  expect_equal(it_zone_transitions(tr, data.frame(x = c(0, 100, 100, 0), y = c(80, 80, 120, 120)))$type,
               "entered")
  two <- rbind(tr, transform(tr, x = 55, track = 2L))
  ev <- it_interactions(two[two$frame <= 4, ], max_dist = 10)
  expect_equal(nrow(ev), 1)
  expect_equal(ev$duration, 0.4, tolerance = 1e-8)
})

test_that("heat map time weights sum to the observed time", {
  tr <- data.frame(frame = 1:10, time = (1:10) / 10, x = 5, y = 5, track = 1L)
  hm <- it_heatmap(tr, 20, 20, cell = 10, frames = data.frame(frame = 1:10, time = (1:10) / 10))
  expect_equal(sum(hm$z), 1, tolerance = 1e-8)
  expect_equal(it_heatmap(tr, 20, 20, cell = 10, weight = "tracks")$z[1, 1], 1)
})

test_that("tracking metrics are 1 for perfect tracks and count identity switches", {
  gt <- rbind(data.frame(frame = 1:10, x = 0, y = 0, id = "A"),
              data.frame(frame = 1:10, x = 100, y = 0, id = "B"))
  pr <- transform(gt, track = ifelse(id == "A", 1L, 2L))
  m <- it_eval_tracks(pr, gt, max_dist = 5)
  expect_equal(c(m$mota, m$idf1, m$id_switches), c(1, 1, 0))
  sw <- pr
  sw$track[sw$frame > 5] <- 3L - sw$track[sw$frame > 5]
  m2 <- it_eval_tracks(sw, gt, max_dist = 5)
  expect_equal(m2$id_switches, 2)
  expect_equal(m2$mota, 0.9)
  expect_equal(m2$idf1, 0.5)
  d <- it_eval_detections(pr, gt, max_dist = 5)$summary
  expect_equal(c(d$precision, d$recall), c(1, 1))
})
