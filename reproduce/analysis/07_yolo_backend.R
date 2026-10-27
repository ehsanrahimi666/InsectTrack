# Check of the deep-learning back end: the YOLO26n model fine-tuned on the
# Delosperma frames is run from R (reticulate, Ultralytics) on frames of the
# clip and compared with the detections of the original run on the same frames.

source(here::here("analysis", "00_common.R"))
od <- out_dir("yolo_backend")
p <- cfg$delosperma
map <- utils::read.csv(file.path(root, cfg$paths$clips, p$clip_frames))
clip <- file.path(root, cfg$paths$clips, p$clip)
frc <- it_extract_frames(clip, dir = file.path(tempdir(), "yolo_col"), gray = FALSE, format = "jpg",
                         times = map$pts_sec)
frc$frame <- map$original_frame
sel <- frc[frc$frame %% 3 == 0, ][1:60, ]
model <- file.path(root, "data", "derived", "models", "flower333_yolo26n_deploy.pt")
tm <- timed(it_detect_yolo(sel, model, imgsz = 1280, conf = 0.3, batch = 10, device = "cpu", verbose = FALSE))
ours <- tm$value
yd <- utils::read.csv(file.path(root, cfg$paths$delosperma, "yolo26n_detections.csv"))
orig <- data.frame(frame = yd$frame, x = yd$cx * p$scale_to_clip, y = yd$cy * p$scale_to_clip)
ev <- it_eval_detections(ours, orig[orig$frame %in% sel$frame, ], max_dist = 3, frames = sel$frame)
write_csv(ours, file.path(od, "yolo_detections_from_R.csv"))
write_csv(cbind(ev$summary, seconds_per_frame = tm$seconds / nrow(sel),
                ultralytics = reticulate::py_to_r(reticulate::import("ultralytics")$`__version__`)),
          file.path(od, "agreement_with_original_run.csv"))
log_msg("YOLO back end: %d frames, %d detections (original run %d); agreement precision %.3f recall %.3f; %.2f s/frame",
        nrow(sel), nrow(ours), sum(orig$frame %in% sel$frame), ev$summary$precision, ev$summary$recall,
        tm$seconds / nrow(sel))
unlink(file.path(tempdir(), "yolo_col"), recursive = TRUE)

# Same model on the original 1920 x 1080 frames 14700-14759 (stream copy, no
# re-encoding), to separate the effect of the reduced and re-encoded clip.
full <- file.path(root, cfg$paths$clips, "delosperma_fullres_f14700-14759.mp4")
ff <- it_extract_frames(full, dir = file.path(tempdir(), "yolo_full"), gray = FALSE, format = "png")
ff$frame <- 14699L + ff$frame
ff <- ff[ff$frame %% 3 == 0, ]
tf <- timed(it_detect_yolo(ff, model, imgsz = 1280, conf = 0.3, batch = 10, device = "cpu", verbose = FALSE))
o2 <- data.frame(frame = yd$frame, x = yd$cx, y = yd$cy)
ev2 <- it_eval_detections(tf$value, o2[o2$frame %in% ff$frame, ], max_dist = 3, frames = ff$frame)
oursc <- ours[ours$frame %in% ff$frame, ]
ev3 <- it_eval_detections(oursc, orig[orig$frame %in% ff$frame, ], max_dist = 3, frames = ff$frame)
write_csv(rbind(cbind(input = "original frames 1920 x 1080", ev2$summary),
                cbind(input = "analysis clip 1280 x 720 (re-encoded)", ev3$summary)),
          file.path(od, "agreement_by_input_frames.csv"))
log_msg("original frames: %d frames, %d detections vs %d in original run (recall %.3f); clip frames: recall %.3f",
        nrow(ff), nrow(tf$value), sum(o2$frame %in% ff$frame), ev2$summary$recall, ev3$summary$recall)
unlink(file.path(tempdir(), "yolo_full"), recursive = TRUE)
