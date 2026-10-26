#' InsectTrack: detection, tracking and activity mapping of insects in videos
#'
#' The package turns videos of insects into tracks and activity measures.
#' [it_track_video()] runs the whole analysis on one video and writes an
#' annotated video, tables, figures and a report; [it_track_videos()] does
#' so for several videos. The steps can also be run one by one:
#'
#' 1. frames: [it_video_info()], [it_extract_frames()],
#'    [it_frames_from_images()], [it_check_stability()];
#' 2. detection: [it_detect_yolo()] (trained YOLO models),
#'    [it_detect_gdino()] and [it_detect_owl()] (open-vocabulary detectors
#'    with a text prompt), [it_detect_motion()] (background subtraction), or
#'    [it_read_detections()] and [it_read_yolo_labels()]; body size with
#'    [it_segment_sam2()]; pre-labels for annotation with [it_prelabel()];
#' 3. tracking: [it_track()] with the Hungarian, SORT and ByteTrack trackers
#'    of the package or the BoxMOT trackers, and [it_stitch()];
#' 4. activity: [it_track_summary()], [it_activity()], [it_heatmap()],
#'    [it_hotspots()];
#' 5. events: [it_roi_visits()], [it_find_entrance()],
#'    [it_entrance_counts()], [it_zone_transitions()],
#'    [it_line_crossings()], [it_interactions()];
#' 6. output and evaluation: [it_render_video()], [it_plot_tracks()],
#'    [it_plot_heatmap()], [it_plot_activity()], [it_plot_entrance()],
#'    [it_eval_detections()], [it_eval_tracks()], [it_eval_hota()].
#'
#' The deep-learning detectors, SAM 2.1 and the BoxMOT trackers run in Python
#' through 'reticulate' (see [it_install_backend()]); everything else runs in
#' R and C++.
#'
#' Image coordinates are in pixels with the origin at the top-left corner of
#' the frame, x to the right and y downwards. Times are in seconds.
#'
#' @keywords internal
#' @useDynLib InsectTrack, .registration = TRUE
#' @importFrom Rcpp sourceCpp
"_PACKAGE"
