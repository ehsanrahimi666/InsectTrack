`%||%` <- function(a, b) if (is.null(a)) b else a

#' Track insects in a video in one call
#'
#' Reads a video (any format FFmpeg reads, such as MP4, MOV, AVI, MKV, WebM,
#' OGV or MTS, including smartphone videos with a rotation flag and a
#' variable frame rate) or a folder of images, detects the insects in every
#' analysed frame, links the detections into tracks, and writes the results
#' to `output_dir`:
#'
#' * `<name>_tracked.mp4`: the video with a coloured trajectory and an id
#'   label for every insect, running counts and, when given, the entrance
#'   zone, regions and counting lines;
#' * `detections.csv`, `tracks.csv` (one row per detection with its track),
#'   `track_summary.csv` (one row per track: duration, path length, speed,
#'   straightness, entry and exit edges, body size with `segment = TRUE`);
#' * `activity.csv` and `activity.png`: insects per frame (mean and MaxN),
#'   tracks and new tracks per time bin;
#' * `heatmap.csv`, `heatmap.png` and `hotspots.png`: time spent per cell
#'   (insect-seconds) and Getis-Ord Gi* hot spots; `trajectories.png`;
#' * with `entrance`: `entrance_events.csv`, `entrance_counts.csv` and
#'   `entrance.png` (insects in, out and net flow per bin);
#' * with `rois`: `visits.csv` and `visits_per_roi.csv`; with `lines`:
#'   `line_crossings.csv`; with `interaction_dist`: `interactions.csv`;
#' * `settings.json` (all settings, video information, software versions and
#'   run times) and `report.html`, a summary page with the figures.
#'
#' Frames are processed in chunks of `chunk` seconds so that long videos do
#' not fill the disk; the frames are deleted afterwards.
#'
#' @param video Video file or folder of images.
#' @param output_dir Output folder; default a folder named after the video
#'   inside `insecttrack_output` next to the video.
#' @param detector `"yolo"` (a trained model, `model`), `"gdino"` (Grounding
#'   DINO with `prompt`), `"owl"` (OWLv2 or OWL-ViT with `prompt`), `"motion"`
#'   (background subtraction, for a fixed camera and a plain background) or
#'   `"auto"` (`"yolo"` when `model` is given, otherwise `"gdino"`).
#' @param model YOLO model file, or a Hugging Face model id for `"gdino"`
#'   and `"owl"`.
#' @param prompt Text prompt of the open-vocabulary detectors, for example
#'   `"bee"`, `"wasp, hornet"` or `"insect"`. With several prompts (a
#'   character vector), each is tried on `screen_frames` frames spread over
#'   the segment and the one with the highest median of the per-frame maximum
#'   score is used (`prompt_screening.csv`).
#' @param conf Minimum detection score kept; default 0.1 (YOLO, OWL) or
#'   0.15 (Grounding DINO).
#' @param tracker Tracker (see [it_track()]); default `"bytetrack"`.
#' @param tracker_args Named list of tracker arguments.
#' @param fps Analysis frame rate; default the video frame rate up to 30, or
#'   5 for the open-vocabulary detectors, which are slow without a GPU.
#' @param start,duration Segment to analyse (s); default the whole video.
#' @param width Analysis width (pixels); frames are scaled down when wider.
#' @param image_fps Frame rate of an image folder.
#' @param rois Regions of interest (polygon, named list of polygons or a JSON
#'   file read by [it_read_rois_json()]) in pixels of the analysed frames.
#' @param entrance Entrance zone: a polygon, `"auto"` ([it_find_entrance()])
#'   or `NULL`.
#' @param lines Counting lines, `c(x1, y1, x2, y2)` or a list of them.
#' @param interaction_dist Distance (pixels) for proximity interactions.
#' @param segment Measure body size with SAM 2.1 ([it_segment_sam2()]).
#' @param segment_every Time (s) between segmented frames.
#' @param sam2_model SAM 2.1 model id.
#' @param stitch Join track fragments ([it_stitch()]).
#' @param bin Time bin (s) of activity and entrance counts; default from the
#'   analysed duration (1 s up to 2 min, 10 s up to 20 min, else 60 s).
#' @param heatmap_cell Heat map cell (pixels); default `width / 64`.
#' @param render Write the annotated video.
#' @param tail,id_cex,boxes,show_class Passed to [it_render_video()].
#' @param chunk Chunk length (s) for frame extraction.
#' @param detector_args Further arguments of the detector function.
#' @param screen_frames Frames used to choose among several prompts.
#' @param device,threads Passed to the Python detectors.
#' @param verbose Print progress.
#' @return Invisibly, a list of class `it_video_result` with the tables and
#'   settings; the files are written to `output_dir`.
#' @examples
#' \donttest{
#' clip <- system.file("extdata", "honeybees_delosperma.mp4", package = "InsectTrack")
#' res <- it_track_video(clip, detector = "motion", fps = 5, duration = 2,
#'                       output_dir = file.path(tempdir(), "example"), verbose = FALSE)
#' list.files(res$output_dir)
#' }
#' \dontrun{
#' # a trained YOLO model and an entrance found from the tracks
#' res <- it_track_video("hive.mp4", detector = "yolo", model = "bees.pt",
#'                       entrance = "auto", output_dir = "hive_results")
#' # Grounding DINO with candidate prompts
#' res <- it_track_video("wasps.mp4", detector = "gdino", prompt = c("wasp", "hornet"),
#'                       fps = 5, output_dir = "wasp_results")
#' }
#' @export
it_track_video <- function(video, output_dir = NULL, detector = c("auto", "yolo", "gdino", "owl", "owlv2", "motion"),
                           model = NULL, prompt = "insect", conf = NULL, tracker = "bytetrack",
                           tracker_args = list(), fps = NULL, start = NULL, duration = NULL, width = 1280,
                           image_fps = 30, rois = NULL, entrance = NULL, lines = NULL, interaction_dist = NULL,
                           segment = FALSE, segment_every = 1, sam2_model = "facebook/sam2.1-hiera-tiny",
                           stitch = TRUE, bin = NULL, heatmap_cell = NULL, render = TRUE, tail = 2,
                           id_cex = 1.6, boxes = FALSE, show_class = TRUE, chunk = 60,
                           detector_args = list(), screen_frames = 5, device = "auto", threads = NULL,
                           verbose = TRUE) {
  detector <- match.arg(detector)
  if (detector == "owlv2") detector <- "owl"
  if (detector == "auto") detector <- if (!is.null(model)) "yolo" else "gdino"
  if (detector == "yolo" && is.null(model)) stop("'model' (a YOLO model file) is needed for detector = 'yolo'")
  t_run <- Sys.time()
  timing <- list()
  tick <- function(step, t0) timing[[step]] <<- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  say <- function(...) if (verbose) message(sprintf(...))
  is_dir <- dir.exists(video)
  name <- if (is_dir) basename(normalizePath(video)) else tools::file_path_sans_ext(basename(video))
  if (is.null(output_dir)) output_dir <- file.path(dirname(normalizePath(video)), "insecttrack_output", name)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out <- function(f) file.path(output_dir, f)

  # video and analysis grid -------------------------------------------------
  if (is_dir) {
    all_frames <- it_frames_from_images(video, fps = image_fps)
    info <- data.frame(file = name, duration_s = max(all_frames$time) + 1 / image_fps, fps = image_fps,
                       rotation = 0, stringsAsFactors = FALSE)
    fps <- image_fps
  } else {
    info <- it_video_info(video)
    if (is.null(fps)) fps <- if (detector %in% c("gdino", "owl")) 5 else min(30, round(info$fps))
  }
  t0v <- start %||% 0
  t1v <- min(info$duration_s, t0v + (duration %||% Inf))
  if (t1v <= t0v) stop("empty time range")
  if (is.null(bin)) bin <- if (t1v - t0v <= 120) 1 else if (t1v - t0v <= 1200) 10 else 60
  if (is.null(conf)) conf <- c(yolo = 0.1, gdino = 0.15, owl = 0.1, motion = 0.1)[[detector]]
  hi <- c(yolo = 0.25, gdino = 0.25, owl = 0.15, motion = 0.5)[[detector]]
  say("InsectTrack: %s, %.1f-%.1f s at %g frames per second, detector %s, tracker %s",
      name, t0v, t1v, fps, detector, tracker)

  read_chunk <- function(cs, ce, dir) {
    if (is_dir) {
      fr <- all_frames[all_frames$time >= cs & all_frames$time < ce, , drop = FALSE]
      return(fr)
    }
    w <- if (!is.null(width) && info$display_width > width) width else NULL
    it_extract_frames(video, dir = dir, fps = fps, start = cs, duration = ce - cs, width = w, gray = FALSE,
                      format = "png", rotate = TRUE)
  }
  screening <- NULL
  if (length(prompt) > 1 && detector %in% c("gdino", "owl")) {
    tmp <- tempfile("it_screen_")
    fr <- if (is_dir) all_frames[unique(round(seq(1, nrow(all_frames), length.out = min(screen_frames, nrow(all_frames))))), ]
          else it_extract_frames(video, dir = tmp, fps = screen_frames / (t1v - t0v), start = t0v, duration = t1v - t0v,
                                 width = if (!is.null(width) && info$display_width > width) width else NULL,
                                 gray = FALSE, format = "png")
    screening <- do.call(rbind, lapply(prompt, function(pr) {
      d <- if (detector == "gdino") {
        do.call(it_detect_gdino, c(list(fr, prompt = pr, box_threshold = conf, text_threshold = conf, device = device,
                                        threads = threads, verbose = FALSE),
                                   if (!is.null(model)) list(model = model), detector_args))
      } else {
        do.call(it_detect_owl, c(list(fr, prompt = pr, threshold = conf, device = device, threads = threads,
                                      verbose = FALSE), if (!is.null(model)) list(model = model), detector_args))
      }
      mx <- vapply(fr$frame, function(f) max(c(0, d$conf[d$frame == f])), 0)
      data.frame(prompt = pr, frames = nrow(fr), median_max_score = stats::median(mx), mean_max_score = mean(mx))
    }))
    prompt <- screening$prompt[which.max(screening$median_max_score)]
    unlink(tmp, recursive = TRUE)
    say("  prompt screening: '%s' chosen (median of the highest score per frame %.2f)", prompt,
        max(screening$median_max_score))
  } else prompt <- prompt[1]
  cuts <- seq(t0v, t1v, by = chunk)
  if (cuts[length(cuts)] < t1v - 1e-9) cuts <- c(cuts, t1v)
  if (length(cuts) < 2) cuts <- c(t0v, t1v)

  # detection -----------------------------------------------------------------
  t0 <- Sys.time()
  dets <- list(); frames_all <- list(); offset <- 0L; W <- H <- NA; bg_file <- out("frame_middle.png")
  seg_t <- -Inf
  for (k in seq_len(length(cuts) - 1)) {
    tmp <- tempfile("it_frames_")
    fr <- read_chunk(cuts[k], cuts[k + 1], tmp)
    if (!nrow(fr)) { unlink(tmp, recursive = TRUE); next }
    if (!is_dir) { fr$frame <- fr$frame + offset; offset <- offset + nrow(fr) }
    if (is.na(W)) { im <- it_read_frame(fr$file[1], gray = FALSE); H <- dim(im)[1]; W <- dim(im)[2] }
    if (k == ceiling((length(cuts) - 1) / 2)) file.copy(fr$file[ceiling(nrow(fr) / 2)], bg_file, overwrite = TRUE)
    say("  chunk %d/%d: %d frames", k, length(cuts) - 1, nrow(fr))
    d <- switch(detector,
      yolo = do.call(it_detect_yolo, c(list(fr, model = model, conf = conf, device = device, threads = threads,
                                            verbose = verbose), detector_args)),
      gdino = do.call(it_detect_gdino, c(list(fr, prompt = prompt, box_threshold = conf, text_threshold = conf,
                                              device = device, threads = threads, verbose = verbose),
                                         if (!is.null(model)) list(model = model), detector_args)),
      owl = do.call(it_detect_owl, c(list(fr, prompt = prompt, threshold = conf, device = device,
                                              threads = threads, verbose = verbose),
                                         if (!is.null(model)) list(model = model), detector_args)),
      motion = {
        z <- do.call(it_detect_motion, c(list(fr, verbose = verbose), detector_args))
        if (nrow(z)) { z$conf <- 1; z$x0 <- z$xmin; z$y0 <- z$ymin; z$x1 <- z$xmax + 1; z$y1 <- z$ymax + 1 }
        z
      })
    if (segment && nrow(d)) {
      ft <- fr$time[fr$frame %in% d$frame]
      fsel <- fr$frame[fr$frame %in% d$frame][c(TRUE, diff(floor((ft - t0v) / segment_every)) > 0)]
      d <- it_segment_sam2(fr, d, model = sam2_model, frames_sel = fsel, device = device, threads = threads,
                           verbose = verbose)
    }
    dets[[k]] <- d
    frames_all[[k]] <- fr[, c("frame", "time")]
    if (!is_dir) unlink(tmp, recursive = TRUE)
  }
  det <- do.call(rbind, dets); frames <- do.call(rbind, frames_all)
  if (is.null(det)) det <- data.frame(frame = integer(), time = numeric(), x = numeric(), y = numeric())
  if (!file.exists(bg_file) && is_dir) file.copy(all_frames$file[ceiling(nrow(all_frames) / 2)], bg_file)
  tick("detection", t0)
  write_csv_out(det, out("detections.csv"))
  if (!is.null(screening)) write_csv_out(screening, out("prompt_screening.csv"))
  say("  %d detections in %d frames", nrow(det), nrow(frames))

  # tracking --------------------------------------------------------------------
  t0 <- Sys.time()
  targs <- tracker_args
  m <- tolower(tracker)
  if (m == "bytetrack") {
    targs <- utils::modifyList(list(track_thresh = hi, low_thresh = conf, new_track_thresh = hi + 0.05,
                                    frame_rate = fps, frames = frames$frame), targs)
  } else if (m %in% c("sort", "hungarian")) {
    det <- det[if ("conf" %in% names(det)) det$conf >= hi else TRUE, , drop = FALSE]
    if (m == "sort") targs <- utils::modifyList(list(frames = frames$frame), targs)
  } else {
    targs <- utils::modifyList(list(frame_size = c(H, W), frames = frames$frame,
                                    params = boxmot_thresholds(m, hi, conf)), targs)
  }
  tr <- if (nrow(det)) do.call(it_track, c(list(det, method = tracker), targs)) else cbind(det, track = integer())
  n_before <- length(unique(stats::na.omit(tr$track)))
  if (stitch && nrow(tr)) tr <- it_stitch(tr, max_gap = 1, max_dist = 0.05 * W)
  tick("tracking", t0)
  say("  %d tracks (%d before joining fragments)", length(unique(stats::na.omit(tr$track))), n_before)

  # analyses ------------------------------------------------------------------------
  t0 <- Sys.time()
  res <- list(video = video, output_dir = output_dir, info = info, frames = frames, detections = det, tracks = tr)
  trk <- tr[!is.na(tr$track), , drop = FALSE]
  write_csv_out(trk, out("tracks.csv"))
  summ <- it_track_summary(tr, width = W, height = H)
  if (segment && "body_length" %in% names(tr) && nrow(summ)) {
    bs <- stats::aggregate(cbind(body_length = trk$body_length, body_width = trk$body_width, mask_area = trk$mask_area),
                           list(track = trk$track), stats::median, na.rm = TRUE, na.action = stats::na.pass)
    summ <- merge(summ, bs, by = "track", all.x = TRUE)
  }
  write_csv_out(summ, out("track_summary.csv"))
  act <- it_activity(tr, frames, bin = bin)
  write_csv_out(act, out("activity.csv"))
  cell <- heatmap_cell %||% max(4, round(W / 64))
  hm <- it_hotspots(it_heatmap(tr, W, H, cell = cell, weight = "time", frames = frames, smooth = 1))
  hm_tab <- data.frame(x = rep(hm$x, each = length(hm$y)), y = rep(hm$y, times = length(hm$x)),
                       insect_seconds = as.vector(hm$z), gi = as.vector(hm$gi), hot = as.vector(hm$hot))
  write_csv_out(hm_tab, out("heatmap.csv"))
  res$track_summary <- summ; res$activity <- act; res$heatmap <- hm
  if (is.character(rois) && length(rois) == 1) rois <- it_read_rois_json(rois)
  if (!is.null(rois)) {
    v <- it_roi_visits(tr, rois, merge_gap = 0.5)
    write_csv_out(v, out("visits.csv"))
    rl <- as_roi_list(rois)
    pr <- data.frame(roi = names(rl), visits = as.integer(table(factor(v$roi, levels = names(rl)))),
                     total_duration = vapply(names(rl), function(r) sum(v$duration[v$roi == r]), 0))
    write_csv_out(pr, out("visits_per_roi.csv"))
    res$visits <- v; res$visits_per_roi <- pr
  }
  zone <- NULL
  if (identical(entrance, "auto")) {
    zone <- it_find_entrance(tr, W, H)
    if (is.null(zone)) say("  no entrance found: too few track end points")
  } else if (!is.null(entrance)) zone <- entrance
  if (!is.null(zone)) {
    ec <- it_entrance_counts(tr, zone, bin = bin, frames = frames)
    write_csv_out(ec$events, out("entrance_events.csv"))
    write_csv_out(ec$counts, out("entrance_counts.csv"))
    write_csv_out(data.frame(x = zone$x, y = zone$y), out("entrance_zone.csv"))
    res$entrance_zone <- zone; res$entrance <- ec
  }
  if (!is.null(lines)) {
    ll <- if (is.list(lines)) lines else list(lines)
    lc <- do.call(rbind, lapply(seq_along(ll), function(i) {
      z <- it_line_crossings(tr, ll[[i]])
      if (nrow(z)) z$line <- i
      z
    }))
    write_csv_out(lc, out("line_crossings.csv"))
    res$line_crossings <- lc
  }
  if (!is.null(interaction_dist)) {
    ia <- it_interactions(tr, max_dist = interaction_dist)
    write_csv_out(ia, out("interactions.csv"))
    res$interactions <- ia
  }
  tick("analysis", t0)

  # figures ------------------------------------------------------------------------
  t0 <- Sys.time()
  bg <- if (file.exists(bg_file)) bg_file else NULL
  fig <- function(file, w, h, expr) {
    grDevices::png(out(file), width = w, height = h, res = 150)
    on.exit(grDevices::dev.off())
    expr
  }
  ar <- H / W
  fig("trajectories.png", 1600, round(1600 * ar), it_plot_tracks(tr, W, H, background = bg, lwd = 1.6, rois = rois,
                                                                 lines = lines))
  fig("heatmap.png", 1600, round(1600 * ar), it_plot_heatmap(hm, background = bg, hot = FALSE, rois = rois,
                                                             legend_title = "insect-seconds"))
  fig("hotspots.png", 1600, round(1600 * ar), it_plot_heatmap(hm, background = bg, hot = TRUE, rois = rois,
                                                              legend_title = "insect-seconds"))
  fig("activity.png", 1600, 800, it_plot_activity(act))
  if (!is.null(zone)) fig("entrance.png", 1600, 800, it_plot_entrance(res$entrance))
  tick("figures", t0)

  # annotated video ------------------------------------------------------------------
  if (render && nrow(frames)) {
    t0 <- Sys.time()
    wd <- tempfile("it_render_"); dir.create(wd)
    files <- character(); offset <- 0L
    ev <- if (!is.null(zone)) res$entrance$events else NULL
    for (k in seq_len(length(cuts) - 1)) {
      tmp <- tempfile("it_frames_")
      fr <- read_chunk(cuts[k], cuts[k + 1], tmp)
      if (!nrow(fr)) next
      if (!is_dir) fr$frame <- fr$frame + offset
      files <- c(files, render_frames(fr, tr, wd, offset = offset, tail = tail, id_cex = id_cex,
                                      show_class = show_class, rois = rois, lines = lines, entrance = zone,
                                      events = ev, boxes = boxes))
      offset <- offset + nrow(fr)
      if (!is_dir) unlink(tmp, recursive = TRUE)
    }
    vout <- out(paste0(name, "_tracked.mp4"))
    av::av_encode_video(files, output = vout, framerate = fps, codec = "libx264", vfilter = "format=yuv420p",
                        verbose = FALSE)
    unlink(wd, recursive = TRUE)
    res$annotated_video <- vout
    tick("rendering", t0)
  }

  # settings, summary and report -------------------------------------------------------
  settings <- list(video = basename(video), analysed = c(start = t0v, end = t1v), fps = fps, width = W, height = H,
                   detector = detector, model = if (is.null(model)) NULL else basename(model),
                   prompt = if (detector %in% c("gdino", "owl")) prompt else NULL, conf = conf, tracker = tracker,
                   tracker_args = tracker_args, stitch = stitch, bin = bin, heatmap_cell = cell,
                   entrance = if (is.null(entrance)) NULL else if (identical(entrance, "auto")) "auto" else "given",
                   segment = segment, video_info = as.list(info),
                   software = list(R = R.version.string, InsectTrack = as.character(utils::packageVersion("InsectTrack"))),
                   run_time_s = timing)
  if (detector %in% c("yolo", "gdino", "owl") || segment || !m %in% c("hungarian", "sort", "bytetrack")) {
    settings$software$python <- tryCatch(it_backend_status(), error = function(e) NULL)
  }
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(settings, out("settings.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null")
  }
  res$summary <- video_summary(res, t0v, t1v)
  write_csv_out(res$summary, out("summary.csv"))
  res$settings <- settings
  write_report(res, out("report.html"))
  say("  done in %.0f s; results in %s", as.numeric(difftime(Sys.time(), t_run, units = "secs")), output_dir)
  class(res) <- "it_video_result"
  invisible(res)
}

boxmot_thresholds <- function(method, hi, lo) {
  m <- sub("^boxmot_", "", method)
  switch(m,
    bytetrack = list(track_thresh = hi, min_conf = lo),
    botsort = list(track_high_thresh = hi, track_low_thresh = lo, new_track_thresh = hi + 0.05),
    sfsort = list(high_th = hi, low_th = lo, new_track_th = hi + 0.05),
    hybridsort = list(track_thresh = hi, low_thresh = lo),
    ocsort = list(min_conf = lo, det_thresh = hi),
    list(det_thresh = hi))
}

write_csv_out <- function(x, file) {
  if (is.null(x)) return(invisible())
  utils::write.csv(x, file, row.names = FALSE, na = "")
}

video_summary <- function(res, t0, t1) {
  tr <- res$tracks; trk <- tr[!is.na(tr$track), , drop = FALSE]
  per_frame <- table(factor(trk$frame, levels = res$frames$frame))
  s <- data.frame(video = basename(res$video), start_s = t0, end_s = t1, frames = nrow(res$frames),
                  detections = nrow(res$detections), tracks = length(unique(trk$track)),
                  mean_insects_per_frame = if (length(per_frame)) mean(per_frame) else 0,
                  max_insects_per_frame = if (length(per_frame)) max(per_frame) else 0,
                  median_track_s = if (!is.null(res$track_summary) && nrow(res$track_summary))
                    stats::median(res$track_summary$duration) else NA_real_,
                  hot_cells = sum(res$heatmap$hot))
  if (!is.null(res$entrance)) {
    s$n_in <- sum(res$entrance$events$direction == "in")
    s$n_out <- sum(res$entrance$events$direction == "out")
  }
  if (!is.null(res$visits)) s$visits <- nrow(res$visits)
  if (!is.null(res$interactions)) s$interactions <- nrow(res$interactions)
  s
}

#' @export
print.it_video_result <- function(x, ...) {
  s <- x$summary
  cat("InsectTrack results for", s$video, "\n")
  cat(sprintf("  analysed %.1f-%.1f s, %d frames; %d detections, %d tracks\n", s$start_s, s$end_s, s$frames,
              s$detections, s$tracks))
  cat(sprintf("  insects per frame: mean %.2f, maximum %d\n", s$mean_insects_per_frame, s$max_insects_per_frame))
  if (!is.null(s$n_in)) cat(sprintf("  entrance: %d in, %d out\n", s$n_in, s$n_out))
  if (!is.null(s$visits)) cat(sprintf("  visits to regions: %d\n", s$visits))
  cat("  output folder:", x$output_dir, "\n")
  invisible(x)
}

#' Track several videos
#'
#' Runs [it_track_video()] on each video with the same settings, writing the
#' results of each video to its own subfolder of `output_dir`, and returns
#' (and writes `summary_all.csv`) one summary row per video.
#'
#' @param videos Video files or image folders.
#' @param output_dir Output folder.
#' @param ... Arguments of [it_track_video()].
#' @return A data frame with one summary row per video.
#' @export
it_track_videos <- function(videos, output_dir, ...) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  rows <- lapply(videos, function(v) {
    nm <- if (dir.exists(v)) basename(normalizePath(v)) else tools::file_path_sans_ext(basename(v))
    r <- tryCatch(it_track_video(v, output_dir = file.path(output_dir, nm), ...),
                  error = function(e) { message("  failed: ", nm, ": ", conditionMessage(e)); NULL })
    if (is.null(r)) data.frame(video = basename(v), error = TRUE) else cbind(r$summary, error = FALSE)
  })
  all_cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(r) { for (cc in setdiff(all_cols, names(r))) r[[cc]] <- NA; r[, all_cols] })
  s <- do.call(rbind, rows)
  write_csv_out(s, file.path(output_dir, "summary_all.csv"))
  s
}
