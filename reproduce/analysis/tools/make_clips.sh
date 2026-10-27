#!/usr/bin/env bash
# Builds the analysis clips in data/derived/clips from the original videos.
# Usage: make_clips.sh <folder with the original web videos> <path to Honeybee.mp4>
# Requires FFmpeg (libx264) and Python 3. The Delosperma clip keeps every third
# original frame (14700, 14703, ...) plus the annotated frames of the window
# 14700-18173; delosperma_clip_frames.csv maps clip frames to original frames.
set -euo pipefail
WEB="$1"; HB="$2"
OUT="$(cd "$(dirname "$0")/../.." && pwd)/data/derived/clips"
mkdir -p "$OUT"
enc() { ffmpeg -hide_banner -loglevel error -y $1 -i "$2" -t "$3" -an -vf "$4" -c:v libx264 -crf "$5" -preset veryfast -pix_fmt yuv420p "$OUT/$6"; }
enc "-ss 0"  "$WEB/V01_hive_entrance_flight.webm"  30 "scale=1280:720,fps=25" 18 V01_clip.mp4
enc "-ss 0"  "$WEB/V02_apiary_multi_hive.ogv"      30 "fps=25"                23 V02_clip.mp4
enc "-ss 0"  "$WEB/V03_wasp_ground_nest.ogv"       45 "scale=960:720,fps=25"  18 V03_clip.mp4
enc "-ss 60" "$WEB/V04_hornet_founding_nest.webm"  60 "fps=10"                23 V04_clip.mp4
enc "-ss 0"  "$WEB/V05_wasps_at_bait.webm"         30 "scale=960:540,fps=10"  23 V05_clip.mp4
enc "-ss 0"  "$WEB/V06_flower_patch.webm"          30 "scale=960:-2,fps=10"   23 V06_clip.mp4
enc "-ss 0"  "$WEB/V07_flowering_shrub.webm"       30 "scale=960:-2,fps=10"   23 V07_clip.mp4
enc "-ss 60" "$WEB/V08_ant_nest_entrance.ogv"      60 "fps=10"                23 V08_clip.mp4

# Delosperma: copy frames 14700-18173 (keyframe 14700 at 510.2274 s), then select frames
TMP="$(mktemp -d)"
ffmpeg -hide_banner -loglevel error -y -ss 510.2420 -i "$HB" -frames:v 3474 -c copy -an -avoid_negative_ts make_zero "$TMP/hb_copy.mp4"
SEL=$(python3 "$(dirname "$0")/delosperma_select.py" "$OUT/delosperma_clip_frames.csv")
ffmpeg -hide_banner -loglevel error -y -i "$TMP/hb_copy.mp4" -an -vf "select='$SEL',scale=1280:720" -vsync 0 -c:v libx264 -crf 18 -preset veryfast -pix_fmt yuv420p "$OUT/delosperma_clip.mp4"
rm -rf "$TMP"
