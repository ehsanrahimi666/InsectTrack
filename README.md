# InsectTrack

Detection, tracking and activity mapping of insects in videos, in R.

InsectTrack reads a video of insects (any format FFmpeg reads, smartphone
videos included) and returns where every insect went and what it did: an
annotated video in which each insect has its own colour and identity number,
counts and activity over time, heat maps and hot spots of activity, visits to
flowers or other regions, insects entering and leaving nests and hives, and
proximity interactions. One function runs the whole analysis and writes
tables, figures and a report.

## Installation

```r
# install.packages("remotes")
remotes::install_github("ehsanrahimi666/InsectTrack")
```

Deep-learning detectors (YOLO, Grounding DINO, OWLv2, OWL-ViT), SAM 2.1 and
the BoxMOT trackers run in Python through
[reticulate](https://rstudio.github.io/reticulate/). Install them once:

```r
library(InsectTrack)
it_install_backend()            # it_install_backend(gpu = TRUE) for an NVIDIA GPU
reticulate::use_virtualenv("insecttrack")
it_backend_status()             # module versions and the device used
```

Background subtraction, the trackers implemented in R (Hungarian assignment,
SORT, ByteTrack), all summaries, figures, rendered videos and evaluation
metrics need no Python.

## Quick start

```r
library(InsectTrack)

# a trained YOLO model, flower outlines as regions of interest
res <- it_track_video("flowers.mp4", detector = "yolo", model = "bees.pt",
                      rois = it_read_rois_json("flowers.json"), output_dir = "results/flowers")

# no trained model: Grounding DINO finds the insects from a text prompt, and the
# hive entrance is located from where tracks start and end
res <- it_track_video("hive.mp4", detector = "gdino", prompt = c("bee", "honeybee"),
                      fps = 5, entrance = "auto", output_dir = "results/hive")
res$entrance$counts             # insects in and out per time bin

# a laboratory arena with a plain background
res <- it_track_video("arena.mp4", detector = "motion", fps = 25, interaction_dist = 60,
                      output_dir = "results/arena")

# several videos with the same settings
it_track_videos(c("a.mp4", "b.mov"), output_dir = "results", detector = "gdino", prompt = "wasp")
```

Each analysis writes to its output folder:

| File | Content |
|---|---|
| `<video>_tracked.mp4` | video with a coloured trajectory and identity label per insect, running counts, entrance zone and regions |
| `tracks.csv`, `detections.csv` | every detection with its track, box, score and class |
| `track_summary.csv` | per track: duration, path length, speed, straightness, entry and exit edges, body size (SAM 2.1) |
| `activity.csv`, `activity.png` | insects per frame (mean and MaxN), tracks and new tracks per time bin |
| `heatmap.csv`, `heatmap.png`, `hotspots.png` | time spent per cell (insect-seconds) and Getis-Ord Gi* hot spots |
| `entrance_events.csv`, `entrance_counts.csv`, `entrance.png` | insects entering and leaving, per bin, with net flow |
| `visits.csv`, `visits_per_roi.csv` | visits to regions (number, duration, sequences between regions) |
| `line_crossings.csv`, `interactions.csv` | crossings of counting lines; proximity events between insects |
| `settings.json`, `report.html` | all settings, software versions and run times; summary page |

## Detectors and trackers

| Step | Options |
|---|---|
| Detection | trained YOLO models (`it_detect_yolo()`), Grounding DINO (`it_detect_gdino()`), OWLv2 and OWL-ViT (`it_detect_owl()`), background subtraction (`it_detect_motion()`), imported detections (`it_read_detections()`, `it_read_yolo_labels()`) |
| Segmentation | SAM 2.1 outlines and body size (`it_segment_sam2()`) |
| Annotation | pre-labelled frames for CVAT, Label Studio or Roboflow (`it_prelabel()`) |
| Tracking | Hungarian assignment, SORT and ByteTrack in R; OC-SORT, BoT-SORT, BoostTrack, SFSORT, HybridSort, OccluBoost and ByteTrack from BoxMOT, and StrongSORT and Deep OC-SORT with re-identification weights (`it_track()`); fragment joining (`it_stitch()`) |
| Events | regions of interest (`it_roi_visits()`), entrances (`it_find_entrance()`, `it_entrance_counts()`), counting lines (`it_line_crossings()`), interactions (`it_interactions()`) |
| Evaluation | detection precision, recall and F1 (`it_eval_detections()`), CLEAR MOT and IDF1 (`it_eval_tracks()`), HOTA (`it_eval_hota()`, `it_combine_hota()`) |

A trained YOLO model is the most accurate and fastest detector when one
exists for the species and the camera set-up. Open-vocabulary detectors need
no training but are slow without a GPU and miss many small insects; they are
most useful to pre-label frames for training a YOLO model. Background
subtraction works with a fixed camera and a plain background.

A track is the record of one insect while it stays in view: an insect that
leaves and returns starts a new track, so tracks are not individuals.

## License

GPL (>= 3)
