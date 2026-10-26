test_that("HOTA is 1 for perfect tracking", {
  g <- data.frame(frame = rep(1:10, 2), x = c(1:10 * 5, 200 - 1:10 * 5), y = 50, id = rep(1:2, each = 10),
                  width = 10, height = 10)
  p <- transform(g, track = id + 10)
  h <- it_eval_hota(p, g, "iou")
  expect_equal(h$hota, 1); expect_equal(h$deta, 1); expect_equal(h$assa, 1); expect_equal(h$loca, 1)
})

test_that("HOTA matches the analytical value for a track split in two", {
  # one object tracked perfectly but split into two predicted tracks of equal
  # length: DetA = 1 and AssA = 1/2, so HOTA = sqrt(1/2); IDF1 = 1/2
  g <- data.frame(frame = 1:20, x = 1:20 * 3, y = 40, id = 1, width = 8, height = 8)
  p <- transform(g, track = ifelse(frame <= 10, 1, 2))
  h <- it_eval_hota(p, g, "iou")
  expect_equal(h$deta, 1)
  expect_equal(h$assa, 0.5)
  expect_equal(h$hota, sqrt(0.5))
  e <- it_eval_tracks(p, g, 5)
  expect_equal(e$idf1, 0.5)
  expect_equal(e$id_switches, 1)
})

test_that("HOTA counts misses and false positives in DetA", {
  g <- data.frame(frame = 1:10, x = 50, y = 50, id = 1, width = 10, height = 10)
  p <- rbind(transform(g[1:5, ], track = 1), data.frame(frame = 1:10, x = 200, y = 200, id = NA, width = 10,
                                                         height = 10, track = 2))
  h <- it_eval_hota(p, g, "iou")
  expect_equal(h$deta, 5 / (5 + 5 + 10))
  hd <- it_eval_hota(p, g, "distance", max_dist = 20)
  expect_equal(hd$deta, 5 / 20)
})

test_that("HOTA combined over sequences equals HOTA of the concatenated sequences", {
  g1 <- data.frame(frame = 1:10, x = 50, y = 50, id = 1, width = 10, height = 10)
  p1 <- transform(g1, track = ifelse(frame <= 4, 1, 2))
  g2 <- data.frame(frame = 1:10, x = 80, y = 50, id = 1, width = 10, height = 10)
  p2 <- transform(g2[1:7, ], track = 1)
  comb <- it_combine_hota(list(it_eval_hota(p1, g1), it_eval_hota(p2, g2)))
  g <- rbind(g1, transform(g2, frame = frame + 100, id = 2))
  p <- rbind(p1, transform(p2, frame = frame + 100, track = track + 10))
  expect_equal(comb$deta, it_eval_hota(p, g)$deta)
  expect_equal(comb$hota, it_eval_hota(p, g)$hota, tolerance = 1e-12)
})
