#!/usr/bin/env bash
# All benchmark steps in order. Every step skips work already saved, so the
# script can be run again after an interruption and continues where it stopped.
set -u
cd "$(dirname "$0")/.."
export RETICULATE_PYTHON=${RETICULATE_PYTHON:-$(command -v python3)}
# file names of some test videos contain non-ASCII characters
export LC_ALL=C.UTF-8 LANG=C.UTF-8
rlib='.libPaths(c(Sys.getenv("IT_RLIB", .libPaths()[1]), .libPaths()))'
# detections of every annotated sequence
python3 analysis/10_detect.py --dataset melipona --input yolo_melipona
python3 analysis/10_detect.py --dataset melipona --input yolo_bee
python3 analysis/10_detect.py --dataset native_bee --input yolo_bee
python3 analysis/10_detect.py --dataset melipona --input gdino_tiny_512 --sequences 30fps
# zero-shot pre-labelling of annotated frames
python3 analysis/11_prelabel.py
# trackers, metrics, in/out counts and sensitivity
Rscript analysis/run_tracking_benchmark.R
Rscript -e "$rlib; source(here::here('analysis', '18_inout_diagnostic.R'))"
# exploratory: the same Melipona sequences annotated at six frame rates
Rscript -e "$rlib; source(here::here('analysis', '20_frame_rate.R'))"
Rscript -e "$rlib; source(here::here('analysis', '13_prelabel_eval.R'))"
# independent HOTA implementation
Rscript -e "$rlib; source(here::here('analysis', '15_xcheck_hota.R'))"
python3 analysis/15_xcheck_hota.py
# demonstration videos
Rscript -e "$rlib; source(here::here('analysis', '14_demo_videos.R'))"
Rscript -e "$rlib; source(here::here('analysis', '16_demo_gallery.R'))"
Rscript -e "$rlib; source(here::here('analysis', '17_benchmark_figures.R'))"
# figures of the paper
python3 analysis/21_paper_figures.py
echo "BENCHMARKS COMPLETE"
