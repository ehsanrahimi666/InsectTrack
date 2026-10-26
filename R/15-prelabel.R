#' Pre-label frames for annotation
#'
#' Samples frames evenly from a video, detects insects with an
#' open-vocabulary detector (Grounding DINO by default, or OWLv2/OWL-ViT, or a YOLO
#' model) and, with `sam = TRUE`, outlines each detection with SAM 2.1 and
#' replaces its box by the tight box of the mask. The frames are written
#' with labels in YOLO format (`labels/`, and polygons in `labels_seg/` with
#' `sam = TRUE`) and in COCO format (`annotations_coco.json`), which
#' annotation tools such as CVAT, Label Studio and Roboflow import. The
#' annotator then corrects the pre-labels instead of drawing every box, and
#' the corrected labels can train a YOLO model for [it_track_video()].
#'
#' @param video Video file or image folder.
#' @param out_dir Output folder.
#' @param n Number of frames.
#' @param start,duration Segment (s) to sample; default the whole video.
#' @param width Frame width (pixels); frames are scaled down when wider.
#' @param detector `"gdino"`, `"owl"` (OWLv2 or OWL-ViT) or `"yolo"`.
#' @param prompt Text prompt (one or several phrases separated by commas);
#'   the phrases become the classes.
#' @param threshold Score threshold.
#' @param model Model file (YOLO) or Hugging Face model id.
#' @param sam Refine boxes and write outlines with SAM 2.1.
#' @param sam2_model SAM 2.1 model id.
#' @param image_fps Frame rate of an image folder.
#' @param device,threads See [it_detect_gdino()].
#' @param verbose Print progress.
#' @return Invisibly, the pre-labels (one row per box).
#' @export
it_prelabel <- function(video, out_dir, n = 50, start = NULL, duration = NULL, width = 1280,
                        detector = c("gdino", "owl", "yolo"), prompt = "insect", threshold = 0.3,
                        model = NULL, sam = TRUE, sam2_model = "facebook/sam2.1-hiera-tiny", image_fps = 30,
                        device = "auto", threads = NULL, verbose = TRUE) {
  detector <- match.arg(detector)
  for (d in c("images", "labels", if (sam) "labels_seg")) dir.create(file.path(out_dir, d), recursive = TRUE,
                                                                   showWarnings = FALSE)
  if (dir.exists(video)) {
    fr <- it_frames_from_images(video, fps = image_fps)
    if (!is.null(start)) fr <- fr[fr$time >= start, , drop = FALSE]
    if (!is.null(duration)) fr <- fr[fr$time < (start %||% 0) + duration, , drop = FALSE]
    fr <- fr[unique(round(seq(1, nrow(fr), length.out = min(n, nrow(fr))))), , drop = FALSE]
    name <- basename(normalizePath(video))
  } else {
    info <- it_video_info(video)
    s0 <- start %||% 0
    dur <- min(info$duration_s - s0, duration %||% Inf)
    w <- if (!is.null(width) && info$display_width > width) width else NULL
    fr <- it_extract_frames(video, dir = tempfile("it_prelabel_"), fps = n / dur, start = s0, duration = dur,
                            width = w, gray = FALSE, format = "png")
    name <- tools::file_path_sans_ext(basename(video))
  }
  ps <- unlist(strsplit(prompt, "[,.;]")); ps <- trimws(tolower(ps)); ps <- ps[nzchar(ps)]
  det <- switch(detector,
    gdino = it_detect_gdino(fr, prompt = prompt, box_threshold = threshold, text_threshold = threshold,
                            model = model %||% "IDEA-Research/grounding-dino-tiny", device = device, threads = threads,
                            verbose = verbose),
    owl = it_detect_owl(fr, prompt = prompt, threshold = threshold,
                            model = model %||% "google/owlv2-base-patch16-ensemble", device = device,
                            threads = threads, verbose = verbose),
    yolo = it_detect_yolo(fr, model = model, conf = threshold, device = device, threads = threads, verbose = verbose))
  if (detector == "yolo") ps <- sort(unique(det$class))
  be <- if (sam && nrow(det)) it_backend() else NULL
  images <- list(); annots <- list(); ann_id <- 0L
  for (k in seq_len(nrow(fr))) {
    img_name <- sprintf("%s_%06d.png", name, fr$frame[k])
    file.copy(fr$file[k], file.path(out_dir, "images", img_name), overwrite = TRUE)
    im <- it_read_frame(fr$file[k], gray = FALSE); H <- dim(im)[1]; W <- dim(im)[2]
    images[[k]] <- list(id = k, file_name = img_name, width = W, height = H)
    r <- which(det$frame == fr$frame[k])
    polys <- vector("list", length(r))
    if (length(r) && !is.null(be)) {
      o <- reticulate::py_to_r(be$segment_sam2(fr$file[k], it_det_boxes(det[r, ]), model_id = sam2_model,
                                               device = device, polygons = TRUE))
      p <- matrix(as.numeric(o$props), ncol = 10)
      ok <- p[, 1] > 0
      det$x0[r[ok]] <- p[ok, 7]; det$y0[r[ok]] <- p[ok, 8]; det$x1[r[ok]] <- p[ok, 9]; det$y1[r[ok]] <- p[ok, 10]
      polys <- lapply(o$polygons, function(z) matrix(as.numeric(z), ncol = 2))
    }
    cls <- match(det$class[r], ps) - 1L
    cls[is.na(cls)] <- 0L
    bw <- det$x1[r] - det$x0[r]; bh <- det$y1[r] - det$y0[r]
    lab <- sprintf("%d %.6f %.6f %.6f %.6f", cls, (det$x0[r] + bw / 2) / W, (det$y0[r] + bh / 2) / H, bw / W, bh / H)
    writeLines(if (length(r)) lab else character(), file.path(out_dir, "labels", sub("\\.png$", ".txt", img_name)))
    if (sam) {
      seg <- vapply(seq_along(r), function(i) {
        pp <- polys[[i]]
        if (is.null(pp) || nrow(pp) < 3) return(NA_character_)
        paste(cls[i], paste(sprintf("%.6f %.6f", pp[, 1] / W, pp[, 2] / H), collapse = " "))
      }, "")
      writeLines(stats::na.omit(seg), file.path(out_dir, "labels_seg", sub("\\.png$", ".txt", img_name)))
    }
    for (i in seq_along(r)) {
      ann_id <- ann_id + 1L
      a <- list(id = ann_id, image_id = k, category_id = cls[i] + 1L,
                bbox = round(c(det$x0[r[i]], det$y0[r[i]], bw[i], bh[i]), 2), area = round(bw[i] * bh[i], 1),
                iscrowd = 0L, score = round(det$conf[r[i]], 4))
      pp <- polys[[i]]
      if (!is.null(pp) && nrow(pp) >= 3) a$segmentation <- list(round(as.vector(t(pp)), 1))
      annots[[ann_id]] <- a
    }
  }
  writeLines(ps, file.path(out_dir, "classes.txt"))
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    coco <- list(images = images, annotations = annots,
                 categories = lapply(seq_along(ps), function(i) list(id = i, name = ps[i])))
    jsonlite::write_json(coco, file.path(out_dir, "annotations_coco.json"), auto_unbox = TRUE, digits = NA)
  }
  if (!dir.exists(video)) unlink(dirname(fr$file[1]), recursive = TRUE)
  if (verbose) message(sprintf("  %d frames and %d pre-labels written to %s", nrow(fr), nrow(det), out_dir))
  invisible(det)
}
