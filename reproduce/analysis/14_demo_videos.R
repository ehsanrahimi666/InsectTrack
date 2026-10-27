# Demonstration videos: one call of it_track_video() per video with the
# settings of config/benchmark.yaml (demo). Results in output/demo/<video>;
# videos already processed are skipped.

source(here::here("analysis", "10_common.R"))
dm <- bcfg$demo
ids <- names(dm$videos)
only <- Sys.getenv("IT_ONLY", "")
if (nzchar(only)) ids <- intersect(ids, strsplit(only, ",")[[1]])
dir.create(file.path(root, "output", "demo"), showWarnings = FALSE, recursive = TRUE)
for (id in ids) {
  v <- dm$videos[[id]]
  odir <- file.path(root, "output", "demo", id)
  if (file.exists(file.path(odir, "summary.csv"))) next
  args <- c(list(video = file.path(root, v$file), output_dir = odir, detector = v$detector, fps = v$fps,
                 start = v$start, duration = v$duration, threads = bcfg$threads), dm$defaults)
  if (v$detector == "gdino") {
    args$prompt <- unlist(v$prompt); args$detector_args <- dm$gdino_args; args$model <- dm$gdino_model
  }
  if (v$detector == "yolo") {
    args$model <- file.path(root, bcfg$models[[v$model]])
    args$detector_args <- list(imgsz = v$imgsz)
  }
  if (v$detector == "motion") args$detector_args <- dm$motion_args
  if (!is.null(v$detector_args)) args$detector_args <- utils::modifyList(args$detector_args %||% list(), v$detector_args)
  if (!is.null(v$entrance)) args$entrance <- v$entrance
  if (!is.null(v$interaction_dist)) args$interaction_dist <- v$interaction_dist
  if (isTRUE(v$segment)) args$segment <- TRUE
  if (!is.null(v$rois)) args$rois <- it_read_rois_json(file.path(root, v$rois), scale = v$rois_scale %||% 1)
  log_msg("demo %s", id)
  res <- tryCatch(do.call(it_track_video, args), error = function(e) { log_msg("  %s failed: %s", id, conditionMessage(e)); NULL })
}
sm <- lapply(names(dm$videos), function(id) {
  f <- file.path(root, "output", "demo", id, "summary.csv")
  if (!file.exists(f)) return(NULL)
  s <- utils::read.csv(f)
  st <- jsonlite::read_json(file.path(root, "output", "demo", id, "settings.json"))
  cbind(demo = id, detector = st$detector, prompt = st$prompt %||% NA, fps = st$fps,
        seconds = sum(unlist(st$run_time_s)), s)
})
cols <- unique(unlist(lapply(sm, names)))
sm <- do.call(rbind, lapply(sm[!vapply(sm, is.null, TRUE)], function(z) { for (k in setdiff(cols, names(z))) z[[k]] <- NA; z[, cols] }))
write_csv(sm, file.path(root, "output", "demo", "demo_summary.csv"))
log_msg("demo videos: %d done", nrow(sm))
