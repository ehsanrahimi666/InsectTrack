#' Python back end
#'
#' Deep-learning detectors (YOLO, Grounding DINO, OWLv2), SAM 2.1
#' segmentation and the BoxMOT trackers run in Python through 'reticulate'.
#' `it_install_backend()` creates a Python environment with the required
#' modules; `it_backend_status()` reports the modules found and the device
#' (`"cuda"`, `"mps"` or `"cpu"`) that will be used. Select a Python
#' environment with `reticulate::use_virtualenv()`, `reticulate::use_condaenv()`
#' or the `RETICULATE_PYTHON` environment variable before the first call.
#'
#' @param envname Name of the virtual environment to create.
#' @param gpu Install the CUDA build of PyTorch (Linux and Windows).
#' @param packages Additional Python packages.
#' @return `it_install_backend()`: the environment name, invisibly.
#'   `it_backend_status()`: a named list of module versions and the device.
#' @name backend
NULL

#' @rdname backend
#' @export
it_install_backend <- function(envname = "insecttrack", gpu = FALSE, packages = NULL) {
  if (!requireNamespace("reticulate", quietly = TRUE)) stop("package 'reticulate' is required")
  if (!envname %in% reticulate::virtualenv_list()) reticulate::virtualenv_create(envname)
  torch <- c("torch", "torchvision")
  if (gpu) reticulate::virtualenv_install(envname, torch, pip_options = "--index-url https://download.pytorch.org/whl/cu124")
  else reticulate::virtualenv_install(envname, torch)
  reticulate::virtualenv_install(envname, c("transformers>=4.56", "ultralytics", "boxmot", "opencv-python",
                                             "pillow", "huggingface_hub", packages))
  message("Python back end installed in virtual environment '", envname,
          "'. Use reticulate::use_virtualenv('", envname, "') before loading models.")
  invisible(envname)
}

#' @rdname backend
#' @export
it_backend_status <- function() {
  be <- it_backend()
  v <- reticulate::py_to_r(be$versions())
  v$device <- reticulate::py_to_r(be$select_device())
  v
}

it_backend <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) stop("package 'reticulate' is required")
  path <- system.file("python", package = "InsectTrack")
  if (!nzchar(path)) stop("the Python files of InsectTrack were not found")
  reticulate::import_from_path("insecttrack_backend", path = path, delay_load = FALSE)
}

# Convert a list of N x 6 arrays (x0, y0, x1, y1, score, class) to detections.
dets_to_df <- function(res, frames, class_names = NULL, model = NA_character_) {
  out <- vector("list", length(res))
  for (k in seq_along(res)) {
    m <- res[[k]]
    if (!length(m) || !nrow(m)) next
    cls <- as.integer(m[, 6])
    cn <- if (is.null(class_names)) as.character(cls) else {
      z <- class_names[cls + 1L]; z[is.na(z)] <- as.character(cls[is.na(z)]); z
    }
    out[[k]] <- data.frame(frame = frames$frame[k], time = frames$time[k],
                           x = (m[, 1] + m[, 3]) / 2, y = (m[, 2] + m[, 4]) / 2,
                           x0 = m[, 1], y0 = m[, 2], x1 = m[, 3], y1 = m[, 4],
                           conf = m[, 5], class = cn, stringsAsFactors = FALSE)
  }
  det <- do.call(rbind, out)
  if (is.null(det)) {
    det <- data.frame(frame = integer(), time = numeric(), x = numeric(), y = numeric(), x0 = numeric(),
                      y0 = numeric(), x1 = numeric(), y1 = numeric(), conf = numeric(), class = character())
  }
  rownames(det) <- NULL
  attr(det, "detector") <- model
  det
}

run_chunks <- function(frames, chunk, fun, verbose, label) {
  idx <- split(seq_len(nrow(frames)), ceiling(seq_len(nrow(frames)) / chunk))
  res <- vector("list", nrow(frames))
  t0 <- Sys.time()
  for (ci in seq_along(idx)) {
    i <- idx[[ci]]
    r <- fun(as.list(frames$file[i]))
    res[i] <- lapply(r, function(z) matrix(as.numeric(z), ncol = 6))
    if (verbose && (ci %% 10 == 0 || ci == length(idx))) {
      el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      message(sprintf("  %s: %d/%d frames (%.2f s per frame)", label, max(i), nrow(frames), el / max(i)))
    }
  }
  res
}

#' Detect insects with deep-learning models
#'
#' `it_detect_yolo()` runs an 'Ultralytics' model (any format it reads, e.g.
#' `.pt` or `.onnx`), typically trained on annotated frames of the study
#' system. `it_detect_gdino()` (Grounding DINO; Liu et al. 2024) and
#' `it_detect_owl()` (OWLv2, Minderer et al. 2023, or OWL-ViT, Minderer et
#' al. 2022, chosen by `model`) are open-vocabulary detectors that find objects described by a text prompt such as `"bee"` or
#' `"wasp, hornet"` without training; they are slower and less accurate for
#' small insects than a trained model, and are mainly useful to pre-label
#' frames for annotation (see [it_prelabel()]).
#'
#' @param frames Data frame (`frame`, `time`, `file`) of colour frames.
#' @param model YOLO model file; for the open-vocabulary detectors a model
#'   identifier on the Hugging Face Hub.
#' @param prompt Text prompt: one or several phrases separated by commas.
#' @param conf,threshold,box_threshold,text_threshold Score thresholds.
#' @param imgsz YOLO inference size (pixels).
#' @param shortest_edge,longest_edge Grounding DINO input size (pixels).
#' @param tile Optional tile size (pixels) for Grounding DINO: the frame is
#'   split into overlapping tiles processed at full resolution, which finds
#'   smaller insects at a proportional cost in time.
#' @param bf16 Run in bfloat16 (faster on recent CPUs and GPUs).
#' @param max_box_frac Boxes covering more than this share of the frame are
#'   discarded (open-vocabulary detectors sometimes return the whole scene).
#' @param device `"auto"`, `"cpu"`, `"cuda"` or `"mps"`.
#' @param batch,chunk Frames per call.
#' @param threads CPU threads used by PyTorch.
#' @param verbose Print progress.
#' @param ... Arguments of `it_detect_owl()`.
#' @return Detections: `frame`, `time`, box centre `x`, `y`, corners `x0`,
#'   `y0`, `x1`, `y1`, score `conf` and `class`.
#' @name detect_dl
NULL

#' @rdname detect_dl
#' @export
it_detect_yolo <- function(frames, model, imgsz = 640, conf = 0.25, device = "auto", batch = 8,
                           threads = NULL, verbose = TRUE) {
  be <- it_backend()
  if (!is.null(threads)) be$set_threads(as.integer(threads))
  ul <- reticulate::import("ultralytics", convert = FALSE)
  names_map <- unlist(reticulate::py_to_r(ul$YOLO(model, task = "detect")$names))
  cn <- unname(names_map[order(as.integer(names(names_map)))])
  res <- run_chunks(frames, batch, function(f) reticulate::py_to_r(
    be$detect_yolo(f, model, conf = conf, imgsz = as.integer(imgsz), device = device, batch = as.integer(batch))),
    verbose, "YOLO")
  dets_to_df(res, frames, cn, model = basename(model))
}

#' @rdname detect_dl
#' @export
it_detect_gdino <- function(frames, prompt = "insect", box_threshold = 0.3, text_threshold = 0.25,
                            model = "IDEA-Research/grounding-dino-tiny", shortest_edge = 800,
                            longest_edge = 1333, tile = NULL, bf16 = FALSE, max_box_frac = 0.5,
                            device = "auto", chunk = 4, threads = NULL, verbose = TRUE) {
  be <- it_backend()
  if (!is.null(threads)) be$set_threads(as.integer(threads))
  ps <- unlist(reticulate::py_to_r(be$phrases(prompt)))
  res <- run_chunks(frames, chunk, function(f) reticulate::py_to_r(
    be$detect_gdino(f, prompt = prompt, box_threshold = box_threshold, text_threshold = text_threshold,
                    model_id = model, device = device, shortest_edge = as.integer(shortest_edge),
                    longest_edge = as.integer(longest_edge), tile = if (is.null(tile)) NULL else as.integer(tile),
                    bf16 = bf16, max_box_frac = max_box_frac)), verbose, "Grounding DINO")
  dets_to_df(res, frames, ps, model = model)
}

#' @rdname detect_dl
#' @export
it_detect_owl <- function(frames, prompt = "insect", threshold = 0.15,
                          model = "google/owlv2-base-patch16-ensemble", max_box_frac = 0.5,
                          device = "auto", chunk = 4, threads = NULL, verbose = TRUE) {
  be <- it_backend()
  if (!is.null(threads)) be$set_threads(as.integer(threads))
  ps <- unlist(reticulate::py_to_r(be$phrases(prompt)))
  res <- run_chunks(frames, chunk, function(f) reticulate::py_to_r(
    be$detect_owl(f, prompt = prompt, threshold = threshold, model_id = model, device = device,
                  max_box_frac = max_box_frac)), verbose, "OWL")
  dets_to_df(res, frames, ps, model = model)
}

#' @rdname detect_dl
#' @export
it_detect_owlv2 <- function(frames, ...) it_detect_owl(frames, ...)

#' Segment detected insects with SAM 2.1
#'
#' Each detection box is segmented with Segment Anything 2.1 (Ravi et al.
#' 2024), giving the outline of the insect. From the mask the function
#' derives its area, centroid, length and width (major and minor axes of the
#' equivalent ellipse), orientation and a tight bounding box, which can be
#' used to measure body size, separate taxa or castes by size, or refine
#' boxes for annotation.
#'
#' @param frames Frames (`frame`, `file`).
#' @param det Detections with boxes.
#' @param model Model identifier on the Hugging Face Hub.
#' @param frames_sel Optional frame numbers to segment (default all frames
#'   with detections).
#' @param device,threads See [it_detect_gdino()].
#' @param verbose Print progress.
#' @return `det` with columns `mask_area`, `mask_x`, `mask_y`,
#'   `body_length`, `body_width`, `body_angle` and the mask box `mx0`, `my0`,
#'   `mx1`, `my1` (`NA` for detections not segmented).
#' @export
it_segment_sam2 <- function(frames, det, model = "facebook/sam2.1-hiera-tiny", frames_sel = NULL,
                            device = "auto", threads = NULL, verbose = TRUE) {
  be <- it_backend()
  if (!is.null(threads)) be$set_threads(as.integer(threads))
  cols <- c("mask_area", "mask_x", "mask_y", "body_length", "body_width", "body_angle", "mx0", "my0", "mx1", "my1")
  for (v in cols) det[[v]] <- NA_real_
  B <- it_det_boxes(det)
  fs <- sort(unique(det$frame))
  if (!is.null(frames_sel)) fs <- intersect(fs, frames_sel)
  t0 <- Sys.time()
  for (k in seq_along(fs)) {
    r <- which(det$frame == fs[k])
    file <- frames$file[match(fs[k], frames$frame)]
    if (is.na(file)) next
    out <- reticulate::py_to_r(be$segment_sam2(file, B[r, , drop = FALSE], model_id = model, device = device))
    p <- matrix(as.numeric(out$props), ncol = 10)
    if (nrow(p) == length(r)) det[r, cols] <- p
    if (verbose && (k %% 20 == 0 || k == length(fs))) {
      el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      message(sprintf("  SAM 2.1: %d/%d frames (%.2f s per frame)", k, length(fs), el / k))
    }
  }
  det
}
