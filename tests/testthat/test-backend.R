has_backend <- function(mods) {
  requireNamespace("reticulate", quietly = TRUE) &&
    all(vapply(mods, function(m) reticulate::py_module_available(m), TRUE))
}

test_that("BoxMOT trackers link synthetic detections", {
  skip_on_cran()
  skip_if_not(has_backend(c("boxmot", "torch")), "Python modules boxmot and torch are not available")
  n <- 20
  d <- data.frame(frame = rep(1:n, 2), time = rep((1:n - 1) / 10, 2), x = c(100 + 4 * 1:n, 400 - 4 * 1:n), y = 200,
                  conf = 0.9)
  d$x0 <- d$x - 12; d$x1 <- d$x + 12; d$y0 <- d$y - 12; d$y1 <- d$y + 12
  for (m in c("bytetrack", "ocsort")) {
    tr <- it_track(d, method = paste0("boxmot_", m), frame_size = c(480, 640),
                   params = if (m == "ocsort") list(min_hits = 1L) else list())
    expect_gte(length(unique(stats::na.omit(tr$track))), 2)
  }
})
