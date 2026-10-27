# Detection, tracking, activity maps and events for the example videos, and
# camera-stability check of all example clips.

source(here::here("analysis", "00_common.R"))

od_all <- out_dir("web_videos")
rows <- list()

ids <- strsplit(Sys.getenv("IT_VIDEOS", paste(names(cfg$web_videos), collapse = ",")), ",")[[1]]
for (id in ids) {
  v <- cfg$web_videos[[id]]
  clip <- file.path(root, cfg$paths$clips, v$clip)
  info <- it_video_info(clip)
  od <- out_dir("web_videos", id)
  wd <- file.path(tempdir(), id)
  log_msg("%s: %s (%d x %d, %.1f fps, %.1f s)", id, v$scenario, info$width, info$height,
          info$fps, info$duration_s)

  ex <- timed(it_extract_frames(clip, dir = file.path(wd, "gray"), gray = TRUE, format = "png"))
  fr <- ex$value
  dt <- timed(run_detection(fr, v$detect))
  det <- dt$value
  tk <- timed(run_tracking(det, v$track, v$stitch))
  tr <- tk$value
  detB <- run_detection(fr, utils::modifyList(v$detect, list(k_pixel = cfg$variant_k_pixel)))
  trB <- run_tracking(detB, v$track, v$stitch)
  write_csv(trB, file.path(od, paste0(id, "_tracks_variant_kpixel.csv")))
  zones <- lapply(v$zones, zone_poly)

  summ <- it_track_summary(tr, info$width, info$height)
  act <- it_activity(tr, fr, bin = 1)
  hm <- it_hotspots(it_heatmap(tr, info$width, info$height, cell = v$heatmap$cell, weight = "time",
                               frames = fr, smooth = v$heatmap$smooth), radius = 2)
  hmt <- it_heatmap(tr, info$width, info$height, cell = v$heatmap$cell, weight = "tracks")
  visits <- it_roi_visits(tr, zones)
  trans <- do.call(rbind, lapply(names(zones), function(z) {
    x <- it_zone_transitions(tr, zones[[z]])
    if (nrow(x)) cbind(zone = z, x) else NULL
  }))
  inter <- if (!is.null(v$interaction_dist)) it_interactions(tr, v$interaction_dist) else NULL

  write_csv(det, file.path(od, paste0(id, "_detections.csv")))
  write_csv(tr, file.path(od, paste0(id, "_tracks.csv")))
  write_csv(summ, file.path(od, paste0(id, "_track_summary.csv")))
  write_csv(act, file.path(od, paste0(id, "_activity_1s.csv")))
  write_csv(visits, file.path(od, paste0(id, "_zone_visits.csv")))
  if (!is.null(trans)) write_csv(trans, file.path(od, paste0(id, "_zone_transitions.csv")))
  if (!is.null(inter)) write_csv(inter, file.path(od, paste0(id, "_interactions.csv")))
  saveRDS(list(hm = hm, hm_tracks = hmt), file.path(od, paste0(id, "_heatmaps.rds")))

  # colour frames for figures and the annotated video
  frc <- it_extract_frames(clip, dir = file.path(wd, "colour"), gray = FALSE, format = "jpg",
                           start = v$render$from, duration = v$render$to - v$render$from)
  frc$frame <- as.integer(round(frc$time * info$fps)) + 1L
  bg <- it_read_frame(frc$file[1], gray = FALSE)
  fw <- 1600; fh <- round(fw * info$height / info$width)
  save_png(file.path(od, paste0(id, "_trajectories.png")), fw, fh,
           it_plot_tracks(tr, info$width, info$height, background = bg, rois = zones))
  save_png(file.path(od, paste0(id, "_trajectories_variant_kpixel.png")), fw, fh,
           it_plot_tracks(trB, info$width, info$height, background = bg, rois = zones))
  save_png(file.path(od, paste0(id, "_heatmap.png")), fw, fh,
           it_plot_heatmap(hm, background = bg, rois = zones, legend_title = "insect-seconds"))
  save_png(file.path(od, paste0(id, "_activity.png")), 1600, 700, {
    graphics::par(mar = c(4.2, 4.5, 1, 4.5))
    graphics::plot(act$bin_start, act$mean_count, type = "s", lwd = 2, col = "#1f4e9c",
                   xlab = "Time (s)", ylab = "Insects per frame (mean)", las = 1,
                   ylim = c(0, max(1, act$max_count)))
    graphics::lines(act$bin_start, act$max_count, type = "s", lty = 2, col = "#1f4e9c")
    graphics::par(new = TRUE)
    graphics::plot(act$bin_start, act$n_new_tracks, type = "h", lwd = 3, col = "#d95f02",
                   axes = FALSE, xlab = "", ylab = "", ylim = c(0, max(1, act$n_new_tracks) * 1.2))
    graphics::axis(4, las = 1, col.axis = "#d95f02")
    graphics::mtext("New tracks per second", side = 4, line = 3, col = "#d95f02")
    graphics::legend("topleft", c("mean count", "maximum count", "new tracks"), bty = "n",
                     lty = c(1, 2, 1), lwd = c(2, 1, 3), col = c("#1f4e9c", "#1f4e9c", "#d95f02"))
  })
  rd <- timed(it_render_video(frc, tr, file.path(od, paste0(id, "_annotated.mp4")),
                              tail = v$render$tail, rois = zones))
  it_render_video(frc, trB, file.path(od, paste0(id, "_annotated_variant_kpixel.mp4")),
                  tail = v$render$tail, rois = zones)

  tt <- table(factor(if (!is.null(trans)) trans$type else character(),
                     levels = c("entered", "exited", "visited", "inside", "outside")))
  rows[[id]] <- data.frame(
    video = id, scenario = v$scenario, width = info$width, height = info$height,
    fps = info$fps, duration_s = round(info$duration_s, 1), frames = nrow(fr),
    detections = nrow(det), tracked_share = round(mean(!is.na(tr$track)), 3),
    tracks = length(unique(stats::na.omit(tr$track))),
    tracks_before_stitch = attr(tr, "n_tracks_before_stitch"), joins = attr(tr, "n_joins"),
    median_track_s = round(stats::median(summ$duration), 2),
    mean_insects_per_frame = round(mean(act$mean_count * act$n_frames) / mean(act$n_frames), 2),
    max_insects_per_frame = max(act$max_count),
    zone_visits = nrow(visits), entered = tt[["entered"]], exited = tt[["exited"]],
    visited = tt[["visited"]], interactions = if (is.null(inter)) NA_integer_ else nrow(inter),
    hot_cells = sum(hm$hot),
    detections_variant_kpixel = nrow(detB),
    tracks_variant_kpixel = length(unique(stats::na.omit(trB$track))),
    mean_insects_per_frame_variant_kpixel = round(sum(!is.na(trB$track)) / nrow(fr), 2),
    s_per_frame_extract = round(ex$seconds / nrow(fr), 4),
    s_per_frame_detect = round(dt$seconds / nrow(fr), 4),
    s_per_frame_track = round(tk$seconds / nrow(fr), 4),
    s_per_frame_render = round(rd$seconds / nrow(frc), 4))
  unlink(wd, recursive = TRUE)
  log_msg("%s done: %d detections, %d tracks", id, nrow(det), rows[[id]]$tracks)
}
write_csv(do.call(rbind, rows), file.path(od_all, "web_video_summary.csv"))

# Camera stability of all example clips
if (Sys.getenv("IT_STABILITY", "1") == "1") {
st <- do.call(rbind, lapply(cfg$stability$clips, function(cl) {
  f <- file.path(root, cfg$paths$clips, cl)
  info <- it_video_info(f)
  fr <- it_extract_frames(f, dir = file.path(tempdir(), paste0("stab_", cl)), fps = 10, width = 640)
  s <- it_check_stability(fr, n = cfg$stability$pairs, lag = round(cfg$stability$lag_s * 10),
                          tolerance = cfg$stability$tolerance_px * 640 / info$width)
  unlink(file.path(tempdir(), paste0("stab_", cl)), recursive = TRUE)
  data.frame(clip = cl, median_shift_px = round(s$median_shift * info$width / 640, 2),
             max_shift_px = round(s$max_shift * info$width / 640, 2),
             share_pairs_moved = s$share_moved, stable = s$stable)
}))
write_csv(st, file.path(od_all, "camera_stability.csv"))
}
log_msg("web videos finished")
