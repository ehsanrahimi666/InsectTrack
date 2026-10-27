# Cross-check of HOTA: tracks of three trackers on three Melipona sequences
# (annotated boxes and detections as input) are written with their ground
# truth; analysis/15_xcheck_hota.py recomputes HOTA independently.

source(here::here("analysis", "10_common.R"))
od <- bout("xcheck_hota")
seqs <- c("10h00_30fps", "15h00_120fps", "17h00_30fps")
rows <- list()
for (sq in seqs) {
  fps <- as.numeric(sub(".*_(\\d+)fps$", "\\1", sq))
  g <- utils::read.csv(file.path(root, "output", "melipona", paste0(sq, "_gt.csv")))
  d <- utils::read.csv(file.path(bout("detections", "melipona"), paste0(sq, "__yolo_melipona.csv")))
  det <- data.frame(frame = d$frame, time = (d$frame - 1) / fps, x = (d$x1 + d$x2) / 2, y = (d$y1 + d$y2) / 2,
                    x0 = d$x1, y0 = d$y1, x1 = d$x2, y1 = d$y2, conf = d$score)
  gb <- it_det_boxes(g)
  write_csv(data.frame(frame = g$frame, id = g$id, x0 = gb[, 1], y0 = gb[, 2], x1 = gb[, 3], y1 = gb[, 4]),
            file.path(od, paste0(sq, "__gt.csv")))
  for (m in c("hungarian", "sort", "bytetrack")) {
    dd <- if (m == "bytetrack") det[det$conf >= 0.1, ] else det[det$conf >= 0.25, ]
    tr <- it_track(dd, method = m)
    tr <- tr[!is.na(tr$track), ]
    write_csv(tr[, c("frame", "track", "x0", "y0", "x1", "y1")], file.path(od, paste0(sq, "__", m, ".csv")))
    h <- it_eval_hota(tr, g, "iou")
    rows[[length(rows) + 1]] <- data.frame(sequence = sq, tracker = m, hota_r = h$hota, deta_r = h$deta,
                                           assa_r = h$assa, loca_r = h$loca)
  }
}
write_csv(do.call(rbind, rows), file.path(od, "hota_r.csv"))
