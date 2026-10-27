# Code and results of the InsectTrack evaluation

This folder holds the code, the settings and the results of the evaluation of
the R package [InsectTrack](https://github.com/ehsanrahimi666/InsectTrack)
0.1.0: the tracking benchmark on annotated hive-entrance sequences of
*Melipona capixaba* and *Tetragonula carbonaria*, the hive-entrance counts,
the frame-rate analysis, the zero-shot pre-labelling benchmark, the independent
check of the HOTA implementation and the demonstration videos shown in this
repository. The folder layout is that of the project, so the scripts run from
this folder.

| Folder or file | Content |
|---|---|
| `config/benchmark.yaml` | All settings of the benchmarks and demonstration videos: data paths, models, score thresholds, tracker settings, metrics, entrance-zone rule, sensitivity variants, acceptance criteria and the settings of each video |
| `config/feasibility.yaml` | Settings of the data-preparation scripts `01`-`08` |
| `analysis/run_all.R` | Runs the data-preparation scripts: part `A`, `01`-`02` (example videos, *Delosperma* video); part `B`, `03`-`05` (validation data sets); `03_melipona.R` writes the annotation tables of the *M. capixaba* sequences used by the benchmarks |
| `analysis/run_benchmarks.sh` | Runs the benchmark steps in order; each step skips finished work |
| `analysis/10_detect.py` | Detections of every annotated frame (YOLO26n models, Grounding DINO) |
| `analysis/11_prelabel.py`, `13_prelabel_eval.R` | Zero-shot pre-labelling and its evaluation |
| `analysis/10_tracking_common.R`, `12_tracking_benchmark.R` | Tracking benchmark: ten trackers, four inputs, CLEAR MOT, IDF1, HOTA, entrance counts and the ByteTrack sensitivity analysis |
| `analysis/18_inout_diagnostic.R` | Movements into and out of the entrance zone matched between each tracker and the annotations: reproduced, missed and spurious movements |
| `analysis/19_confirmation_check.R` | OC-SORT, Hybrid-SORT and BoostTrack run again with one match required to report a track |
| `analysis/20_frame_rate.R` | Five *M. capixaba* sequences annotated at 30, 40, 50, 60, 100 and 120 frames per second, and the Hungarian tracker with its gate scaled to the frame interval |
| `analysis/15_xcheck_hota.R`, `15_xcheck_hota.py` | Independent Python implementation of HOTA compared with the package |
| `analysis/14_demo_videos.R`, `16_demo_gallery.R` | Demonstration videos and the gallery page |
| `analysis/17_benchmark_figures.R`, `21_paper_figures.py`, `22_paper_numbers.py` | Figures of the benchmarks and of the paper; every number reported in the paper with its source file (`output/paper/numbers.csv`) |
| `data/raw` | Download scripts of the validation data and test videos, with source lists and SHA-256 checksums |
| `output/benchmark` | Results: `tracking/`, `frame_rate/`, `prelabel/`, `xcheck_hota/` and the detections of every annotated frame (`detections/`) |
| `environment` | R session information and Python package versions of the runs |

## Running the analyses

1. Install R (≥ 4.1) with the packages `av`, `clue`, `jpeg`, `png`, `Rcpp`,
   `jsonlite`, `yaml`, `here` and `reticulate`, and Python 3 with `torch`,
   `torchvision`, `transformers`, `ultralytics`, `onnxruntime`, `boxmot`,
   `opencv-python`, `pillow`, `scipy`, `pyyaml`, `pandas`, `numpy` and
   `matplotlib` (versions in `environment/`).
2. Clone the package into this folder and install it:
   `git clone https://github.com/ehsanrahimi666/InsectTrack package/InsectTrack`
   and `R CMD INSTALL package/InsectTrack`.
3. Download the validation data with `bash data/raw/download_validation.sh`
   (the files are checked against `data/raw/validation/SHA256SUMS.txt`).
   Unpack `melipona_2026_03_06_MOTA.zip` to `data/derived/melipona` (and each
   `*_fps/partes/*.zip` into a folder named after the zip with the suffix
   `_gt`) and `native_bee_tracking.zip` to `data/derived/native_bee`. The
   YOLO26n model of the *M. capixaba* data set is part of
   <https://doi.org/10.5281/zenodo.20276311>; `data/raw/validation/melipona_yolo26n_SOURCE.txt`
   gives the file and its checksum.
4. Download the test videos listed in `data/raw/web_videos/video_sources.csv`
   (`bash data/raw/download_web_videos.sh`) and
   `data/raw/web_videos_2/video_sources_2.csv` (`download_web_videos_2.cmd` on
   Windows, or the URLs in the list).
5. Create a `logs` folder, run `Rscript analysis/run_all.R B`, then
   `bash analysis/run_benchmarks.sh`, which ends with the figures of the paper and
   `output/paper/numbers.csv`.

On two CPU threads the benchmark steps took about ten hours, mostly for
Grounding DINO, OWLv2 and SAM 2.1. The smartphone video of honeybees on
*Delosperma* flowers, its annotations and the YOLO26n model trained on bees on
flowers (`data/raw/delosperma`, `data/derived/models`) are not included in this
repository; the steps that use them (detections with this model, the
*Delosperma* pre-labelling frames and the *Delosperma* demonstration video)
need these files.

## Licences

The code is distributed under the GNU General Public Licence (version 3 or
later), like the package. The result tables are shared under the Creative
Commons Attribution 4.0 International licence (CC BY 4.0). The validation data
are available from their sources under CC BY 4.0: *M. capixaba*, de Sá
Barbalho et al. (2026), <https://doi.org/10.5281/zenodo.22235293>;
*T. carbonaria*, Asaduzzaman et al. (2026),
<https://doi.org/10.26180/32315067.v1>.
