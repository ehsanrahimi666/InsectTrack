# Honeybees on Delosperma (smartphone video): (A) background-subtraction
# detection against manual annotations, (B) tracking of the YOLO detections of
# the whole video, flower visits, heat map, hot spots and encounters,
# (C) annotated video.

source(here::here("analysis", "00_common.R"))

p <- cfg$delosperma
od <- out_dir("delosperma")
s <- p$scale_to_clip
ft <- utils::read.csv(file.path(root, "data", "derived", "delosperma", "honeybee_frame_times.csv"))
map <- utils::read.csv(file.path(root, cfg$paths$clips, p$clip_frames))
clip <- file.path(root, cfg$paths$clips, p$clip)
info <- it_video_info(clip)
log_msg("Delosperma clip: %d frames, %d x %d", nrow(map), info$width, info$height)

# A. Detection by background subtraction --------------------------------------
fr <- it_extract_frames(clip, dir = file.path(tempdir(), "delo_gray"), gray = TRUE, times = map$pts_sec)
stopifnot(nrow(fr) == nrow(map))
fr$frame <- map$original_frame
dt <- timed(run_detection(fr, p$detect))
det <- dt$value
write_csv(det, file.path(od, "motion_detections_clip.csv"))

lab <- list.files(file.path(root, cfg$paths$delosperma, "annotations"), pattern = "\\.txt$",
                  full.names = TRUE)
gt <- it_read_yolo_labels(lab, info$width, info$height, frame_regex = "frame_(\\d+)_")
ann <- sort(unique(map$original_frame[map$annotated == 1]))
stopifnot(setequal(ann, unique(gt$frame)) || all(unique(gt$frame) %in% ann))
yd <- utils::read.csv(file.path(root, cfg$paths$delosperma, "yolo26n_detections.csv"))
yd <- yd[yd$conf >= p$yolo_conf_min, ]
ydc <- data.frame(frame = yd$frame, x = yd$cx * s, y = yd$cy * s)
ann3 <- ann[ann %% 3 == 0]

ev <- list(
  motion_all = it_eval_detections(det, gt, p$eval_max_dist, frames = ann),
  motion_stride3 = it_eval_detections(det, gt, p$eval_max_dist, frames = ann3),
  yolo_stride3 = it_eval_detections(ydc[ydc$frame %in% ann3, ], gt, p$eval_max_dist, frames = ann3))
evs <- do.call(rbind, lapply(names(ev), function(k) cbind(method = k, ev[[k]]$summary)))
write_csv(evs, file.path(od, "detection_evaluation.csv"))
write_csv(cbind(method = "motion", ev$motion_all$per_frame), file.path(od, "detection_evaluation_per_frame.csv"))

# planned sensitivity of the detector
sens <- list(k3 = list(k = 3), k7 = list(k = 7), area_half = list(min_area = p$detect$min_area * 0.5),
             area_double = list(min_area = p$detect$min_area * 2),
             pixel_threshold = list(k_pixel = cfg$variant_k_pixel))
bg <- it_background(fr, n = p$detect$bg_frames)
sres <- do.call(rbind, lapply(names(sens), function(nm) {
  pp <- utils::modifyList(p$detect, sens[[nm]])
  fa <- fr[fr$frame %in% ann, ]
  d2 <- run_detection(fa, pp, background = bg)
  cbind(variant = nm, it_eval_detections(d2, gt, p$eval_max_dist, frames = ann)$summary)
}))
write_csv(sres, file.path(od, "detection_sensitivity.csv"))
log_msg("detection evaluated: F1 motion = %.3f, YOLO (stride frames) = %.3f",
        ev$motion_all$summary$f1, ev$yolo_stride3$summary$f1)

# B. Tracking the YOLO detections of the whole video ---------------------------
yd$time <- ft$pts_sec[match(yd$frame, ft$frame)]
stopifnot(!anyNA(yd$time))
ydt <- data.frame(frame = yd$frame, time = yd$time, x = yd$cx, y = yd$cy, conf = yd$conf,
                  x0 = yd$x0, y0 = yd$y0, x1 = yd$x1, y1 = yd$y1)
tk <- timed(run_tracking(ydt, p$yolo_track, p$stitch))
yt <- tk$value
write_csv(yt, file.path(od, "yolo_tracks_full_video.csv"))
summ <- it_track_summary(yt, 1920, 1080)
write_csv(summ, file.path(od, "yolo_track_summary.csv"))

zones <- it_read_rois_json(file.path(root, cfg$paths$delosperma, "flower_zones.json"))
uv <- utils::read.csv(file.path(root, cfg$paths$delosperma, "user_visit_tables", "tables", "visits.csv"))
gap_used <- jsonlite::fromJSON(file.path(root, cfg$paths$delosperma, "user_visit_tables", "tables",
                                         "statistics.json"))
vt <- it_roi_visits(yt, zones, merge_gap = gap_used$max_gap_sec_used, min_duration = gap_used$min_dwell_sec)
write_csv(vt, file.path(od, "track_based_flower_visits.csv"))
fl <- names(zones)
cmp <- data.frame(flower = fl,
                  visits_tracks = as.integer(table(factor(vt$roi, levels = fl))),
                  visits_flower_detections = as.integer(table(factor(uv$flower_id, levels = fl))),
                  dwell_tracks = as.numeric(tapply(c(vt$duration, rep(0, length(fl))),
                                                   factor(c(vt$roi, fl), levels = fl), sum)),
                  dwell_flower_detections = as.numeric(tapply(c(uv$duration_sec, rep(0, length(fl))),
                                                              factor(c(uv$flower_id, fl), levels = fl), sum)))
write_csv(cmp, file.path(od, "visits_per_flower_comparison.csv"))
rho_v <- stats::cor.test(cmp$visits_tracks, cmp$visits_flower_detections, method = "spearman", exact = FALSE)
rho_d <- stats::cor.test(cmp$dwell_tracks, cmp$dwell_flower_detections, method = "spearman", exact = FALSE)

frames_all <- data.frame(frame = ft$frame[ft$frame %% 3 == 0])
frames_all$time <- ft$pts_sec[match(frames_all$frame, ft$frame)]
hm <- it_hotspots(it_heatmap(yt, 1920, 1080, cell = p$heatmap$cell, weight = "time", frames = frames_all,
                             smooth = p$heatmap$smooth), radius = p$hotspot_radius)
hmt <- it_heatmap(yt, 1920, 1080, cell = p$heatmap$cell, weight = "tracks")
saveRDS(list(hm = hm, hm_tracks = hmt), file.path(od, "heatmaps_full_video.rds"))
act <- it_activity(yt, frames_all, bin = 30)
act_det <- it_activity(transform(ydt, track = 1L), frames_all, bin = 30, tracked_only = FALSE)
write_csv(act, file.path(od, "activity_30s.csv"))
inter <- it_interactions(yt, max_dist = 30, min_duration = 0.2)
write_csv(inter, file.path(od, "bee_encounters.csv"))
ftr <- if (nrow(vt)) tapply(vt$roi, vt$track, function(z) length(unique(z))) else integer()

res <- data.frame(
  detections = nrow(ydt), frames_analysed = nrow(frames_all),
  tracks_before_stitch = attr(yt, "n_tracks_before_stitch"), joins = attr(yt, "n_joins"),
  tracks = length(unique(stats::na.omit(yt$track))), tracked_share = mean(!is.na(yt$track)),
  median_track_s = stats::median(summ$duration), p90_track_s = stats::quantile(summ$duration, 0.9),
  track_based_visits = nrow(vt), flower_detection_visits = nrow(uv),
  flowers_visited_tracks = sum(cmp$visits_tracks > 0),
  flowers_visited_flower_detections = sum(cmp$visits_flower_detections > 0),
  spearman_visits = unname(rho_v$estimate), spearman_visits_p = rho_v$p.value,
  spearman_dwell = unname(rho_d$estimate), spearman_dwell_p = rho_d$p.value,
  median_dwell_tracks = stats::median(vt$duration), median_dwell_flower_detections = stats::median(uv$duration_sec),
  tracks_visiting_2plus_flowers = sum(ftr >= 2), tracks_with_visits = length(ftr),
  mean_bees_per_frame_tracked = sum(act$mean_count * act$n_frames) / sum(act$n_frames),
  mean_detections_per_frame = sum(act_det$mean_count * act_det$n_frames) / sum(act_det$n_frames),
  encounters = nrow(inter), hot_cells = sum(hm$hot), tracking_s = tk$seconds)
write_csv(res, file.path(od, "delosperma_summary.csv"))
log_msg("tracks: %d (before stitching %d); visits tracks %d vs flower detections %d; rho = %.2f",
        res$tracks, res$tracks_before_stitch, res$track_based_visits, res$flower_detection_visits,
        res$spearman_visits)

# C. Figures and annotated video ---------------------------------------------------
frc <- it_extract_frames(clip, dir = file.path(tempdir(), "delo_col"), gray = FALSE, format = "jpg",
                         times = map$pts_sec)
frc$frame <- map$original_frame
bgc <- it_read_frame(frc$file[1], gray = FALSE)
save_png(file.path(od, "delosperma_heatmap_full_video.png"), 1800, 1013,
         it_plot_heatmap(hm, background = bgc, rois = zones, legend_title = "bee-seconds"))
save_png(file.path(od, "delosperma_trajectories_clip_window.png"), 1800, 1013,
         it_plot_tracks(yt[yt$time >= min(map$pts_sec) & yt$time <= max(map$pts_sec), ], 1920, 1080,
                        background = bgc, rois = zones))
save_png(file.path(od, "delosperma_visits_comparison.png"), 1300, 1200, {
  graphics::par(mar = c(4.5, 4.8, 1, 1))
  mx <- max(cmp$visits_tracks, cmp$visits_flower_detections)
  graphics::plot(jitter(cmp$visits_flower_detections, 0.4), jitter(cmp$visits_tracks, 0.4), pch = 21,
                 bg = grDevices::adjustcolor("#1f4e9c", 0.6), col = "white", xlim = c(0, mx), ylim = c(0, mx),
                 xlab = "Visits per flower, flower-centred detections", ylab = "Visits per flower, tracks", las = 1)
  graphics::abline(0, 1, lty = 2, col = "grey40")
  graphics::legend("topleft", bty = "n", legend = sprintf("Spearman rho = %.2f (n = %d flowers)",
                                                          rho_v$estimate, nrow(cmp)))
})
sel <- frc[frc$frame %% 3 == 0 & frc$time <= min(frc$time) + p$render$to, ]
ytc <- yt; ytc$x <- ytc$x * s; ytc$y <- ytc$y * s
zc <- lapply(zones, function(z) z * s)
it_render_video(sel, ytc, file.path(od, "delosperma_annotated.mp4"), tail = p$render$tail, rois = zc)

# example frame: annotations (green), background-subtraction detections (red)
fx <- ann[which.max(table(factor(gt$frame, levels = ann)))]
img <- it_read_frame(frc$file[frc$frame == fx], gray = FALSE)
save_png(file.path(od, "delosperma_detection_example.png"), 1800, 1013, {
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new(); graphics::plot.window(c(0, 1280), c(720, 0), xaxs = "i", yaxs = "i", asp = 1)
  graphics::rasterImage(img, 0, 720, 1280, 0)
  g <- gt[gt$frame == fx, ]
  graphics::rect(g$x - g$w / 2, g$y - g$h / 2, g$x + g$w / 2, g$y + g$h / 2, border = "#00E676", lwd = 2)
  d <- det[det$frame == fx, ]
  graphics::points(d$x, d$y, pch = 3, col = "#FF1744", cex = 1.6, lwd = 2.5)
  y3 <- ydc[ydc$frame == fx, ]
  if (nrow(y3)) graphics::points(y3$x, y3$y, pch = 4, col = "#2979FF", cex = 1.6, lwd = 2.5)
})
unlink(file.path(tempdir(), c("delo_gray", "delo_col")), recursive = TRUE)
log_msg("Delosperma finished")
