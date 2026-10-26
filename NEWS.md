# InsectTrack 0.1.0

* `it_track_video()` analyses a whole video in one call and writes an
  annotated video, tables, figures, settings and a report; `it_track_videos()`
  processes several videos.
* Detectors: trained YOLO models (`it_detect_yolo()`), open-vocabulary
  Grounding DINO (`it_detect_gdino()`) and OWLv2/OWL-ViT (`it_detect_owl()`)
  with text prompts, and background subtraction (`it_detect_motion()`).
* SAM 2.1 outlines detected insects (`it_segment_sam2()`) for body size, and
  `it_prelabel()` writes pre-labelled frames for annotation tools.
* Trackers: SORT (`it_track_sort()`) and ByteTrack (`it_track_bytetrack()`)
  implemented in R, the BoxMOT trackers (`it_track_boxmot()`), and the
  Hungarian tracker (`it_track_hungarian()`); `it_track()` dispatches.
* Hive and nest entrances: `it_find_entrance()` and `it_entrance_counts()`.
* Every track has its own colour (`it_track_colours()`) and larger,
  outlined identity labels in rendered videos.
* Videos with a rotation flag are rotated; image folders can be used instead
  of videos (`it_frames_from_images()`).
* HOTA (`it_eval_hota()`, `it_combine_hota()`), and box-overlap matching in
  `it_eval_detections()`.
