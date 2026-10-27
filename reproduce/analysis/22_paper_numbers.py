"""Numbers reported in the InsectTrack paper, computed from the saved outputs of
the benchmarks and demonstration videos. Writes output/paper/numbers.csv with
one row per number: key, value as printed, source file and definition.

Usage: python3 analysis/22_paper_numbers.py
"""

import json
import re
import subprocess
from decimal import ROUND_HALF_UP, Decimal
from pathlib import Path

import numpy as np
import pandas as pd
import yaml

ROOT = Path(__file__).resolve().parents[1]
BENCH = ROOT / "output" / "benchmark"
DEMO = ROOT / "output" / "demo"
PKG = ROOT / "package" / "InsectTrack"
OUT = ROOT / "output" / "paper"
OUT.mkdir(parents=True, exist_ok=True)
CFG = yaml.safe_load(open(ROOT / "config" / "benchmark.yaml"))

rows = []


def put(key, value, source, definition):
    if any(r["key"] == key for r in rows):
        raise ValueError(f"duplicate key {key}")
    rows.append({"key": key, "value": value, "source": source, "definition": definition})


def rhu(x, nd):
    """x rounded half up (away from zero) to nd decimals, as printed."""
    v = str(Decimal(repr(float(x))).quantize(Decimal(1).scaleb(-nd), rounding=ROUND_HALF_UP))
    return v[1:] if v.startswith("-") and float(v) == 0 else v


def f2(x):
    return rhu(x, 2).replace("-", "\u2212")


def f1d(x):
    return rhu(x, 1).replace("-", "\u2212")


def rng(values, fmt=f2):
    """Range of values as printed: 'lo–hi', or 'lo to hi' when a bound is negative."""
    values = np.asarray(values, float)
    lo, hi = fmt(values.min()), fmt(values.max())
    if lo == hi:
        return lo
    return f"{lo} to {hi}" if "\u2212" in lo + hi else f"{lo}–{hi}"


def pct(x):
    return rhu(100 * x, 0)


def thousands(n):
    return f"{int(n):,}"


# Package -----------------------------------------------------------------------------------------
ns = (PKG / "NAMESPACE").read_text()
put("n_exported", str(len(re.findall(r"^export\(", ns, re.M))), "package/InsectTrack/NAMESPACE", "exported functions")
be = (PKG / "inst" / "python" / "insecttrack_backend.py").read_text()
boxmot = re.search(r"BOXMOT_TRACKERS = \(([^)]*)\)", be, re.S).group(1)
n_boxmot = len(re.findall(r'"(\w+)"', boxmot))
put("n_boxmot_trackers", str(n_boxmot), "inst/python/insecttrack_backend.py", "BoxMOT trackers callable from R")
put("n_trackers", str(n_boxmot + 3), "inst/python/insecttrack_backend.py; R/05-track.R", "trackers (3 in R + BoxMOT)")
tst = (ROOT / "logs" / "testthat_results.txt").read_text()
m = re.search(r"\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]", tst)
put("n_test_pass", m.group(4), "logs/testthat_results.txt", "passing test expectations")
put("n_test_fail", m.group(1), "logs/testthat_results.txt", "failing test expectations")
put("n_test_skip", m.group(3), "logs/testthat_results.txt", "skipped test expectations (Python back end absent)")
put("n_test_files", str(len(list((PKG / "tests" / "testthat").glob("test-*.R")))), "package/InsectTrack/tests/testthat",
    "test files")

# Validation data ---------------------------------------------------------------------------------
p = pd.read_csv(BENCH / "tracking" / "metrics_pooled.csv")
s = pd.read_csv(BENCH / "tracking" / "metrics_by_sequence.csv")
gtp = p[(p.input == "ground_truth") & (p.tracker == "native_hungarian")].set_index(["dataset", "fps_group"])
for (ds, fg), lab in ((("melipona", "120fps"), "mel120"), (("melipona", "30fps"), "mel30"), (("native_bee", "all"), "nat")):
    r = gtp.loc[(ds, fg)]
    put(f"{lab}_seq", str(int(r.sequences)), "output/benchmark/tracking/metrics_pooled.csv", f"sequences, {ds} {fg}")
    put(f"{lab}_boxes", thousands(r.gt_objects), "output/benchmark/tracking/metrics_pooled.csv", f"annotated boxes, {ds} {fg}")
    put(f"{lab}_tracks", thousands(r.gt_tracks), "output/benchmark/tracking/metrics_pooled.csv", f"annotated tracks, {ds} {fg}")
mel_frames = {120: 1200, 30: 300}
put("mel120_frames", thousands(int(gtp.loc[("melipona", "120fps")].sequences) * mel_frames[120]),
    "analysis/10_tracking_common.R (seq_frames)", "frames, Melipona 120 fps (10-s sequences)")
put("mel30_frames", thousands(int(gtp.loc[("melipona", "30fps")].sequences) * mel_frames[30]),
    "analysis/10_tracking_common.R (seq_frames)", "frames, Melipona 30 fps (10-s sequences)")
nat_dir = ROOT / CFG["paths"]["native_bee"]
nat_frames = sum(len(list((d / "images").glob("*.jpg"))) for d in nat_dir.iterdir() if d.is_dir())
put("nat_frames", thousands(nat_frames), CFG["paths"]["native_bee"], "annotated native-bee frames")
put("nat_fps", str(CFG["tracking"]["frame_rate"]["native_bee"]), "config/benchmark.yaml", "native-bee frame rate")
fr = pd.read_csv(BENCH / "frame_rate" / "frame_rate_by_sequence.csv")
put("fr_seq", str(fr.sequence.nunique()), "output/benchmark/frame_rate/frame_rate_by_sequence.csv",
    "sequences annotated at all six frame rates")
rates_ = [str(x) for x in sorted(fr.fps.unique())]
put("fr_rates", ", ".join(rates_[:-1]) + " and " + rates_[-1], "output/benchmark/frame_rate/frame_rate_by_sequence.csv",
    "frame rates")
frb = fr[fr.tracker == fr.tracker.iloc[0]].groupby("fps").gt_objects.sum()
put("fr_boxes_range", f"{thousands(frb.min())}–{thousands(frb.max())}", "output/benchmark/frame_rate/frame_rate_by_sequence.csv",
    "annotated boxes per frame rate (lowest and highest rate)")
put("fr_frames_range", f"{thousands(fr.sequence.nunique() * 10 * min(fr.fps))}–{thousands(fr.sequence.nunique() * 10 * max(fr.fps))}",
    "analysis/20_frame_rate.R", "frames per frame rate (10-s sequences)")
frames = pd.read_csv(BENCH / "prelabel" / "frames.csv")
gt = pd.read_csv(BENCH / "prelabel" / "gt.csv").merge(frames[["image_id", "dataset"]], on="image_id")
put("pl_frames", str(len(frames)), "output/benchmark/prelabel/frames.csv", "pre-labelling frames")
put("pl_boxes", str(len(gt)), "output/benchmark/prelabel/gt.csv", "annotated boxes in pre-labelling frames")
for ds, lab in (("delosperma", "del"), ("melipona", "mel"), ("native_bee", "nat")):
    put(f"pl_{lab}_frames", str(int((frames.dataset == ds).sum())), "output/benchmark/prelabel/frames.csv", f"frames, {ds}")
    put(f"pl_{lab}_boxes", str(int((gt.dataset == ds).sum())), "output/benchmark/prelabel/gt.csv", f"boxes, {ds}")
gw = (gt.x2 - gt.x1)
for ds, lab in (("delosperma", "del"), ("melipona", "mel"), ("native_bee", "nat")):
    w = (gt[gt.dataset == ds].x2 - gt[gt.dataset == ds].x1) / frames[frames.dataset == ds].width.median() * 1280
    put(f"pl_{lab}_boxw", rhu(w.median(), 0), "output/benchmark/prelabel/gt.csv, frames.csv",
        f"median box width (px at 1280 px frame width), {ds}")

# Demonstration videos ----------------------------------------------------------------------------
g = pd.read_csv(DEMO / "demo_gallery.csv")
variants = g.demo.str.endswith(("_tiled", "_gdino"))
prim = g[~variants]
put("demo_videos", str(len(prim)), "output/demo/demo_gallery.csv", "demonstration videos (primary runs)")
put("demo_runs", str(len(g)), "output/demo/demo_gallery.csv", "runs including variants")
put("demo_variants", str(int(variants.sum())), "output/demo/demo_gallery.csv", "additional runs")
for det in ("gdino", "motion", "yolo"):
    put(f"demo_{det}", str(int((prim.detector == det).sum())), "output/demo/demo_gallery.csv", f"primary runs with {det}")
put("demo_frames", thousands(g.frames.sum()), "output/demo/demo_gallery.csv", "frames analysed, all runs")
put("demo_tracks_range", rng(prim.tracks, lambda v: rhu(v, 0)), "output/demo/demo_gallery.csv", "tracks per video (primary)")
put("demo_gdino_found", str(int(((prim.detector == "gdino") & (prim.tracks > 0)).sum())), "output/demo/demo_gallery.csv",
    "primary Grounding DINO runs with at least one track")
gd = g[g.detector == "gdino"]
put("demo_gdino_s_per_frame", rng(gd.run_s / gd.frames, lambda v: f1d(v)), "output/demo/demo_gallery.csv",
    "run time per analysed frame, Grounding DINO base (whole pipeline, CPU)")
put("demo_gdino_s_per_frame_median", f1d(np.median(gd.run_s / gd.frames)), "output/demo/demo_gallery.csv",
    "median run time per analysed frame, Grounding DINO base")
tracks = g.set_index("demo").tracks
for k in ("bee_counter", "bee_counter_tiled", "ant_trail", "leafcutter_ants", "leafcutter_ants_gdino", "drosophila_arena",
          "drosophila_arena_gdino", "hive_fanning", "hive_entrance", "hive_flight", "delosperma", "melipona"):
    put(f"tracks_{k}", thousands(tracks[k]), "output/demo/demo_gallery.csv", f"tracks, {k}")
ds_ = pd.read_csv(DEMO / "demo_summary.csv").set_index("demo")
for k in ("hive_entrance", "hive_flight", "melipona", "bee_counter_tiled"):
    put(f"in_{k}", str(int(ds_.loc[k, "n_in"])), "output/demo/demo_summary.csv", f"insects in, {k}")
    put(f"out_{k}", str(int(ds_.loc[k, "n_out"])), "output/demo/demo_summary.csv", f"insects out, {k}")
put("mean_insects_hive_fanning", rhu(ds_.loc['hive_fanning', 'mean_insects_per_frame'], 0), "output/demo/demo_summary.csv",
    "mean bees per frame, hive_fanning")
put("max_insects_hive_fanning", rhu(ds_.loc['hive_fanning', 'max_insects_per_frame'], 0), "output/demo/demo_summary.csv",
    "maximum bees per frame, hive_fanning")
for k in ("ant_trail", "leafcutter_ants", "drosophila_arena"):
    put(f"median_track_{k}", f1d(ds_.loc[k, "median_track_s"]), "output/demo/demo_summary.csv", f"median track duration (s), {k}")
    put(f"det_per_frame_{k}", rhu(ds_.loc[k, "detections"] / ds_.loc[k, "frames"], 0), "output/demo/demo_summary.csv",
        f"mean detections per analysed frame, {k}")
    put(f"mean_insects_{k}", f1d(ds_.loc[k, "mean_insects_per_frame"]), "output/demo/demo_summary.csv",
        f"mean tracked objects per frame, {k}")
bc = pd.read_csv(DEMO / "bee_counter" / "detections.csv")
put("bee_counter_boxes", str(len(bc)), "output/demo/bee_counter/detections.csv", "boxes returned on the full frames, bee counter")
pipe = (PKG / "R" / "14-pipeline.R").read_text()
hi_gd = float(re.search(r"hi <- c\(yolo = [\d.]+, gdino = ([\d.]+)", pipe).group(1))
new_off = float(re.search(r"new_track_thresh = hi \+ ([\d.]+)", pipe).group(1))
put("demo_gdino_new_thr", f2(hi_gd + new_off), "package/InsectTrack/R/14-pipeline.R",
    "score needed to start a track with Grounding DINO in it_track_video() (ByteTrack new-track threshold)")
put("bee_counter_max_below_new", "yes" if bc.conf.max() < hi_gd + new_off else "no", "output/demo/bee_counter/detections.csv",
    "all boxes of the bee counter below the new-track threshold")
widths = {}
for k in prim.demo:
    sj = json.load(open(DEMO / k / "settings.json"))
    widths[k] = int(sj["width"])
small = sorted(v for v in widths.values() if v < 1280)
put("demo_n_small", str(len(small)), "output/demo/*/settings.json", "primary runs analysed below 1280 px (original width)")
put("demo_small_widths", f"{small[0]}–{small[-1]}", "output/demo/*/settings.json", "analysis widths of the smaller videos (px)")
for k in ("yellowjacket_honeybee", "leafcutter_bees_sunflower", "drosophila_arena_gdino"):
    put(f"interactions_{k}", str(int(ds_.loc[k, "interactions"])), "output/demo/demo_summary.csv", f"interaction events, {k}")

# Worked example: honeybees on Delosperma ----------------------------------------------------------
d = DEMO / "delosperma"
st = json.load(open(d / "settings.json"))
tr = pd.read_csv(d / "tracks.csv")
vis = pd.read_csv(d / "visits.csv")
vpr = pd.read_csv(d / "visits_per_roi.csv")
act = pd.read_csv(d / "activity.csv")
hm = pd.read_csv(d / "heatmap.csv")
put("del_seconds", str(int(st["analysed"][1] - st["analysed"][0])), "output/demo/delosperma/settings.json", "seconds analysed")
put("del_fps", str(int(st["fps"])), "output/demo/delosperma/settings.json", "analysis frame rate")
put("del_tracks", str(tr.track.nunique()), "output/demo/delosperma/tracks.csv", "tracks")
put("del_visits", str(int(vpr.visits.sum())), "output/demo/delosperma/visits_per_roi.csv", "visits")
put("del_flowers_visited", str(int((vpr.visits > 0).sum())), "output/demo/delosperma/visits_per_roi.csv", "flowers visited")
put("del_flowers", str(len(vpr)), "output/demo/delosperma/visits_per_roi.csv", "flowers outlined")
put("del_visit_median", f1d(vis.duration.median()), "output/demo/delosperma/visits.csv", "median visit duration (s)")
put("del_visit_max", f1d(vis.duration.max()), "output/demo/delosperma/visits.csv", "longest visit (s)")
put("del_mean_bees", f1d(act.mean_count.mean()), "output/demo/delosperma/activity.csv", "mean bees in view per frame")
put("del_maxn", str(int(act.max_count.max())), "output/demo/delosperma/activity.csv", "largest MaxN in a 1-s bin")
hot_col = [c for c in hm.columns if c.lower() in ("hot", "hotspot", "is_hot", "hot_spot")]
if hot_col:
    put("del_hot_cells", str(int(hm[hot_col[0]].astype(bool).sum())), "output/demo/delosperma/heatmap.csv", "hot-spot cells")
put("del_conf", f2(st["conf"]), "output/demo/delosperma/settings.json", "detection score threshold")
vid =ROOT / "data" / "raw" / "user_videos" / "SupplementaryVideo_honeybee_visitation (1).mp4"
num, den = subprocess.run(["ffprobe", "-v", "error", "-select_streams", "v:0", "-show_entries", "stream=r_frame_rate",
                           "-of", "csv=p=0", str(vid)], capture_output=True, text=True, check=True).stdout.strip().split("/")
put("del_video_fps", f1d(int(num) / int(den)), "data/raw/user_videos (ffprobe)", "frame rate of the Delosperma video")

# Tracking benchmark ------------------------------------------------------------------------------
GRP = {"mel120": ("melipona", "120fps"), "mel30": ("melipona", "30fps"), "nat": ("native_bee", "all")}
INP = {"gt": "ground_truth", "ym": "yolo_melipona", "gd": "gdino_tiny_512", "yb": "yolo_bee"}
for gk, (ds, fg) in GRP.items():
    for ik, inp in INP.items():
        z = p[(p.dataset == ds) & (p.fps_group == fg) & (p.input == inp)]
        if not len(z):
            continue
        for met in ("hota", "deta", "assa", "idf1", "mota"):
            put(f"{met}_{gk}_{ik}", rng(z[met]), "output/benchmark/tracking/metrics_pooled.csv",
                f"{met} range over trackers, {ds} {fg}, {inp}")
        best = z.sort_values("hota").iloc[-1]
        put(f"best_{gk}_{ik}", best.tracker, "output/benchmark/tracking/metrics_pooled.csv", f"tracker with highest HOTA, {ds} {fg}, {inp}")
        put(f"besthota_{gk}_{ik}", f2(best.hota), "output/benchmark/tracking/metrics_pooled.csv", f"highest HOTA, {ds} {fg}, {inp}")
# native bees, annotated boxes: trackers that report a track only after three consecutive matches (BoxMOT
# default min_hits = 3), SFSORT, and the other six trackers
CONF3 = ["boxmot_ocsort", "boxmot_hybridsort", "boxmot_boosttrack"]
z = p[(p.dataset == "native_bee") & (p.input == "ground_truth")]
zi = z.set_index("tracker")
other = z[~z.tracker.isin(CONF3 + ["boxmot_sfsort"])]
sb = s[(s.dataset == "native_bee") & (s.input == "ground_truth")].groupby("tracker")[["pred_objects", "gt_objects"]].sum()
sb["share"] = sb.pred_objects / sb.gt_objects
SRC_P = "output/benchmark/tracking/metrics_pooled.csv"
put("hota_nat_gt_other", rng(other.hota), SRC_P, "HOTA, native bees, annotated boxes, six trackers other than OC-SORT, "
    "Hybrid-SORT, BoostTrack and SFSORT")
put("assa_nat_gt_other", rng(other.assa), SRC_P, "AssA, native bees, annotated boxes, the six other trackers")
put("deta_nat_gt_other", rng(other.deta), SRC_P, "DetA, native bees, annotated boxes, the six other trackers")
for met in ("hota", "deta", "assa"):
    put(f"{met}_nat_gt_conf3", rng(zi.loc[CONF3, met]), SRC_P, f"{met}, native bees, annotated boxes, OC-SORT, Hybrid-SORT, BoostTrack")
    put(f"{met}_nat_gt_sfsort", f2(zi.loc["boxmot_sfsort", met]), SRC_P, f"{met}, native bees, annotated boxes, SFSORT")
put("boxes_out_nat_gt_conf3", rng(sb.loc[CONF3, "share"], pct), "output/benchmark/tracking/metrics_by_sequence.csv",
    "percentage of annotated boxes returned in tracks, native bees, OC-SORT, Hybrid-SORT, BoostTrack")
put("boxes_out_nat_gt_other", rng(sb.loc[other.tracker, "share"], pct), "output/benchmark/tracking/metrics_by_sequence.csv",
    "percentage of annotated boxes returned in tracks, native bees, the six other trackers")
put("hota_nat_gt_spread", f2(z.hota.max() - z.hota.min()), SRC_P, "difference between highest and lowest HOTA over trackers, "
    "native bees, annotated boxes")
put("idsw_nat_sfsort", thousands(z.set_index("tracker").loc["boxmot_sfsort", "id_switches"]),
    "output/benchmark/tracking/metrics_pooled.csv", "identity switches, native bees, annotated boxes, SFSORT")
put("idsw_nat_other_max", str(int(z[z.tracker != "boxmot_sfsort"].id_switches.max())),
    "output/benchmark/tracking/metrics_pooled.csv", "largest identity switches of other trackers, native bees")
z = p[(p.dataset == "melipona") & (p.input == "ground_truth") & (p.fps_group == "120fps")].set_index("tracker")
put("hota_mel120_gt_stitch", f2(z.loc["native_bytetrack_stitch", "hota"]), "output/benchmark/tracking/metrics_pooled.csv",
    "HOTA, Melipona 120 fps, annotated boxes, ByteTrack + joining")
put("hota_mel120_gt_bytetrack", f2(z.loc["native_bytetrack", "hota"]), "output/benchmark/tracking/metrics_pooled.csv",
    "HOTA, Melipona 120 fps, annotated boxes, ByteTrack (R)")
# detection quality of the inputs at their tracking thresholds
q = pd.read_csv(BENCH / "tracking" / "detection_quality_by_sequence.csv")
q["fps_group"] = np.where(q.dataset == "melipona", q.sequence.str.extract(r"_(\d+)fps$")[0] + "fps", "all")
for gk, (ds, fg) in GRP.items():
    for ik, inp in INP.items():
        z = q[(q.dataset == ds) & (q.fps_group == fg) & (q.input == inp)]
        if not len(z):
            continue
        tp, fp, fn = z.tp.sum(), z.fp.sum(), z.fn.sum()
        put(f"prec_{gk}_{ik}", f2(tp / max(1, tp + fp)), "output/benchmark/tracking/detection_quality_by_sequence.csv",
            f"precision (IoU >= 0.5), {ds} {fg}, {inp}")
        put(f"rec_{gk}_{ik}", f2(tp / max(1, tp + fn)), "output/benchmark/tracking/detection_quality_by_sequence.csv",
            f"recall (IoU >= 0.5), {ds} {fg}, {inp}")
        put(f"f1_{gk}_{ik}", f2(2 * tp / max(1, 2 * tp + fp + fn)), "output/benchmark/tracking/detection_quality_by_sequence.csv",
            f"F1 (IoU >= 0.5), {ds} {fg}, {inp}")
        put(f"npred_{gk}_{ik}", thousands(z.n_pred.sum()), "output/benchmark/tracking/detection_quality_by_sequence.csv",
            f"detections above the threshold, {ds} {fg}, {inp}")
# speed of the trackers (whole benchmark, CPU): frames per second of tracking alone
frames_seq = s.groupby(["dataset", "sequence", "fps"]).size().reset_index()[["dataset", "sequence", "fps"]]


def seq_frames(r):
    if r.dataset == "melipona":
        return mel_frames[int(r.fps)]
    return len(list((nat_dir / r.sequence / "images").glob("*.jpg")))


sp = s[s.input == "ground_truth"].copy()
sp["frames"] = sp.apply(seq_frames, axis=1)
spd = sp.groupby("tracker").agg(frames=("frames", "sum"), seconds=("seconds", "sum"))
spd["fps"] = spd.frames / spd.seconds
put("speed_range", rng(spd.fps, lambda v: thousands(round(v))), "output/benchmark/tracking/metrics_by_sequence.csv",
    "frames per second of tracking (annotated boxes, all sequences), range over trackers")
for t in ("native_hungarian", "native_sort", "native_bytetrack", "boxmot_bytetrack", "boxmot_ocsort"):
    put(f"speed_{t}", thousands(round(spd.loc[t, "fps"])), "output/benchmark/tracking/metrics_by_sequence.csv", f"frames per second, {t}")
# HOTA cross-check
x = pd.read_csv(BENCH / "xcheck_hota" / "hota_xcheck.csv")
diff = max((x[f"{m}_r"] - x[f"{m}_py"]).abs().max() for m in ("hota", "deta", "assa", "loca"))
put("xcheck_n", str(len(x)), "output/benchmark/xcheck_hota/hota_xcheck.csv", "tracker-sequence results cross-checked")
put("xcheck_maxdiff", f"{diff:.1e}" if diff > 0 else "0", "output/benchmark/xcheck_hota/hota_xcheck.csv",
    "largest absolute difference of HOTA, DetA, AssA, LocA between implementations")
txt = (BENCH / "xcheck_hota" / "hota_xcheck.csv").read_text().splitlines()[1:]
dec = min(len(v.split(".")[1]) for line in txt for v in line.split(",")[2:] if "." in v)
put("xcheck_decimals", str(dec), "output/benchmark/xcheck_hota/hota_xcheck.csv", "decimal places stored in the cross-check")
# acceptance criteria fixed before the analysis
crit = CFG["criteria"]
put("crit_hota", f2(crit["tracking_hota"]), "config/benchmark.yaml", "HOTA criterion")
put("crit_mota", f2(crit["tracking_mota"]), "config/benchmark.yaml", "MOTA criterion")
put("crit_idf1", f2(crit["tracking_idf1"]), "config/benchmark.yaml", "IDF1 criterion")
put("crit_f1", f2(crit["detection_f1"]), "config/benchmark.yaml", "detection F1 criterion")
put("crit_inout", f2(CFG["in_out"]["criterion_relative_error"]), "config/benchmark.yaml", "in/out relative error criterion")
fq = q.groupby(["dataset", "fps_group", "input"])[["tp", "fp", "fn"]].sum()
fq["f1"] = 2 * fq.tp / (2 * fq.tp + fq.fp + fq.fn)
put("det_f1_max", f2(fq.f1.max()), "output/benchmark/tracking/detection_quality_by_sequence.csv",
    "highest detection F1 of any detector input of the tracking benchmark (all frames)")
put("det_f1_n_crit", str(int((fq.f1 >= crit["detection_f1"]).sum())), "output/benchmark/tracking/detection_quality_by_sequence.csv",
    "detector inputs meeting the detection F1 criterion")
ok = p[(p.hota >= crit["tracking_hota"]) & (p.idf1 >= crit["tracking_idf1"]) & (p.mota >= crit["tracking_mota"])]
put("crit_met_inputs", ", ".join(sorted(ok.input.unique())), "output/benchmark/tracking/metrics_pooled.csv",
    "inputs for which at least one tracker met all tracking criteria")
for gk, (ds, fg) in GRP.items():
    z = p[(p.dataset == ds) & (p.fps_group == fg) & (p.input == "ground_truth")]
    met = (z.hota >= crit["tracking_hota"]) & (z.idf1 >= crit["tracking_idf1"]) & (z.mota >= crit["tracking_mota"])
    put(f"crit_n_{gk}_gt", str(int(met.sum())), "output/benchmark/tracking/metrics_pooled.csv",
        f"trackers meeting all tracking criteria with annotated boxes, {ds} {fg}")
ym = p[p.input == "yolo_melipona"]
put("crit_met_yolo", str(int(((ym.hota >= crit["tracking_hota"]) & (ym.idf1 >= crit["tracking_idf1"]) &
                              (ym.mota >= crit["tracking_mota"])).sum())), "output/benchmark/tracking/metrics_pooled.csv",
    "tracker-frame-rate combinations meeting the criteria with YOLO26n Melipona detections")

# ByteTrack sensitivity
se = pd.read_csv(BENCH / "tracking" / "sensitivity_bytetrack.csv")
b = s[(s.tracker == "native_bytetrack") & (s.dataset == "melipona") & (s.input == "yolo_melipona")].copy()
b["variant"] = "primary"
zz = pd.concat([se[se.dataset == "melipona"][["fps", "variant", "hota", "idf1", "mota"]], b[["fps", "variant", "hota", "idf1", "mota"]]])
mh = zz.groupby(["fps", "variant"]).hota.mean().unstack(0)
dlt = mh.sub(mh.loc["primary"], axis=1)
put("sens_thr06_drop", rng(-dlt.loc["track_thresh=0.6"]), "output/benchmark/tracking/sensitivity_bytetrack.csv",
    "decrease of mean HOTA with detection threshold 0.6 (range over 120 and 30 fps)")
put("sens_thr04_change", rng(dlt.loc["track_thresh=0.4"]), "output/benchmark/tracking/sensitivity_bytetrack.csv",
    "change of mean HOTA with detection threshold 0.4 (range over 120 and 30 fps)")
oth = dlt.drop(index=["primary", "track_thresh=0.6", "track_thresh=0.4"])
put("sens_other_maxabs", f2(oth.abs().max().max()), "output/benchmark/tracking/sensitivity_bytetrack.csv",
    "largest absolute change of mean HOTA with track buffer or matching IoU")
nse = se[se.dataset == "native_bee"]
put("sens_nat_max_hota", f2(nse.hota.max()), "output/benchmark/tracking/sensitivity_bytetrack.csv",
    "largest HOTA of any native-bee sensitivity run (detections of the bees-on-flowers model)")

# Hive-entrance counts ----------------------------------------------------------------------------
zn = pd.read_csv(BENCH / "tracking" / "entrance_zones.csv").set_index("dataset")
put("zone_mel_radius", rhu(zn.loc['melipona', 'radius'], 0), "output/benchmark/tracking/entrance_zones.csv", "entrance zone radius, Melipona (px)")
put("zone_nat_radius", rhu(zn.loc['native_bee', 'radius'], 0), "output/benchmark/tracking/entrance_zones.csv", "entrance zone radius, native bees (px)")
for gk, (ds, fg) in GRP.items():
    gz = p[(p.dataset == ds) & (p.fps_group == fg) & (p.input == "ground_truth")].iloc[0]
    put(f"gtin_{gk}", str(int(gz.gt_in)), "output/benchmark/tracking/metrics_pooled.csv", f"annotated movements in, {ds} {fg}")
    put(f"gtout_{gk}", str(int(gz.gt_out)), "output/benchmark/tracking/metrics_pooled.csv", f"annotated movements out, {ds} {fg}")
    for ik, inp in INP.items():
        z = p[(p.dataset == ds) & (p.fps_group == fg) & (p.input == inp)]
        if len(z):
            put(f"ioerr_{gk}_{ik}", rng(z.inout_rel_error), "output/benchmark/tracking/metrics_pooled.csv",
                f"relative error of in + out counts, range over trackers, {ds} {fg}, {inp}")
            put(f"ioerr_seq_{gk}_{ik}", rng(z.inout_rel_error_seq), "output/benchmark/tracking/metrics_pooled.csv",
                f"relative error of in + out counts with the errors of single sequences added, range over trackers, {ds} {fg}, {inp}")
            put(f"ioerr_seq_ncrit_{gk}_{ik}", str(int((z.inout_rel_error_seq <= CFG["in_out"]["criterion_relative_error"]).sum())),
                "output/benchmark/tracking/metrics_pooled.csv", f"trackers meeting the count criterion with per-sequence errors, {ds} {fg}, {inp}")
            put(f"ioerr_ncrit_{gk}_{ik}", str(int((z.inout_rel_error <= CFG["in_out"]["criterion_relative_error"]).sum())),
                "output/benchmark/tracking/metrics_pooled.csv", f"trackers meeting the count criterion, {ds} {fg}, {inp}")
            put(f"ioerr_med_{gk}_{ik}", f2(z.inout_rel_error.median()), "output/benchmark/tracking/metrics_pooled.csv",
                f"median relative error of in + out counts over trackers, {ds} {fg}, {inp}")
z = p[(p.dataset == "melipona") & (p.fps_group == "120fps") & (p.input == "ground_truth")]
zt = p[p.dataset == "melipona"].set_index(["fps_group", "input", "tracker"])
for t in ("boxmot_boosttrack", "boxmot_sfsort", "native_hungarian"):
    for fg in ("120fps", "30fps"):
        for ik, inp in (("gt", "ground_truth"), ("ym", "yolo_melipona")):
            put(f"ioerr_{fg}_{ik}_{t}", f2(zt.loc[(fg, inp, t), "inout_rel_error"]), "output/benchmark/tracking/metrics_pooled.csv",
                f"relative error of in + out counts, Melipona {fg}, {inp}, {t}")
            put(f"ioerr_seq_{fg}_{ik}_{t}", f2(zt.loc[(fg, inp, t), "inout_rel_error_seq"]), "output/benchmark/tracking/metrics_pooled.csv",
                f"relative error of in + out counts with per-sequence errors added, Melipona {fg}, {inp}, {t}")
put("ioerr_mel120_gt_ncrit", str(int((z.inout_rel_error <= CFG["in_out"]["criterion_relative_error"]).sum())),
    "output/benchmark/tracking/metrics_pooled.csv", "trackers meeting the in/out criterion, Melipona 120 fps, annotated boxes")
st_ = pd.read_csv(BENCH / "tracking" / "inout_diagnostic_steps.csv").set_index(["dataset", "fps_group"])
for gk, key in (("mel120", ("melipona", "120fps")), ("mel30", ("melipona", "30fps"))):
    put(f"step_{gk}", f1d(st_.loc[key, 'median_step_per_box']), "output/benchmark/tracking/inout_diagnostic_steps.csv",
        f"median largest step between frames in box sizes (square root of box area), {key}")
    put(f"step_over1_{gk}", pct(st_.loc[key, "share_over_one_box"]), "output/benchmark/tracking/inout_diagnostic_steps.csv",
        f"percentage of movements with a step longer than one box size, {key}")
    put(f"moves_{gk}", str(int(st_.loc[key, "movements"])), "output/benchmark/tracking/inout_diagnostic_steps.csv",
        f"annotated movements, {key}")
dg = pd.read_csv(BENCH / "tracking" / "inout_diagnostic_summary.csv")
SRC_D = "output/benchmark/tracking/inout_diagnostic_summary.csv"
for gk, (ds, fg) in GRP.items():
    for ik, inp in (("gt", "ground_truth"), ("ym", "yolo_melipona")):
        z = dg[(dg.dataset == ds) & (dg.fps_group == fg) & (dg.input == inp)]
        if not len(z):
            continue
        put(f"rep_{gk}_{ik}", rng(z.recall, pct), SRC_D, f"percentage of annotated movements reproduced, range over trackers, {ds} {fg}, {inp}")
        put(f"rep_med_{gk}_{ik}", pct(z.recall.median()), SRC_D, f"median percentage of annotated movements reproduced, {ds} {fg}, {inp}")
        put(f"missrate_{gk}_{ik}", rng(1 - z.recall, pct), SRC_D, f"percentage of annotated movements missed, range over trackers, {ds} {fg}, {inp}")
        put(f"spur_{gk}_{ik}", rng(z.spurious, lambda v: rhu(v, 0)), SRC_D, f"spurious movements, range over trackers, {ds} {fg}, {inp}")
        put(f"miss_{gk}_{ik}", rng(z.missed, lambda v: rhu(v, 0)), SRC_D, f"missed movements, range over trackers, {ds} {fg}, {inp}")
        put(f"moves_{gk}_{ik}", str(int(z.movements.iloc[0])), SRC_D, f"annotated movements, {ds} {fg}")
        canc = z[(z.missed > 0) & (z.spurious > 0)]
        put(f"ncanc_{gk}_{ik}", str(len(canc)), SRC_D, f"trackers with both missed and spurious movements, {ds} {fg}, {inp}")
        tot = z.missed.sum()
        for col, lab in (("missed_track_broken_at_the_zone_edge", "edge"), ("missed_no_tracked_box_inside_the_zone", "noin"),
                         ("missed_no_tracked_box_outside_the_zone", "noout"), ("missed_not_tracked", "none")):
            put(f"fail_{lab}_{gk}_{ik}", pct(z[col].sum() / tot) if tot else "0", SRC_D,
                f"percentage of missed movements (all trackers) of type {col}, {ds} {fg}, {inp}")
        nsp = z.spurious.sum()
        for col, lab in (("spurious_duplicate_of_an_annotated_movement", "dup"), ("spurious_annotated_bee_without_this_movement", "other"),
                         ("spurious_false_detections", "fd")):
            put(f"spurtype_{lab}_{gk}_{ik}", pct(z[col].sum() / nsp) if nsp else "0", SRC_D,
                f"percentage of spurious movements (all trackers) of type {col}, {ds} {fg}, {inp}")
zd = dg.set_index(["dataset", "fps_group", "input", "tracker"])
for t in ("native_hungarian", "boxmot_sfsort", "boxmot_boosttrack", "boxmot_bytetrack"):
    for fg in ("120fps", "30fps"):
        for ik, inp in (("gt", "ground_truth"), ("ym", "yolo_melipona")):
            r = zd.loc[("melipona", fg, inp, t)]
            put(f"rep_{fg}_{ik}_{t}", pct(r.recall), SRC_D, f"percentage of movements reproduced, Melipona {fg}, {inp}, {t}")
            put(f"spur_{fg}_{ik}_{t}", str(int(r.spurious)), SRC_D, f"spurious movements, Melipona {fg}, {inp}, {t}")
            put(f"miss_{fg}_{ik}_{t}", str(int(r.missed)), SRC_D, f"missed movements, Melipona {fg}, {inp}, {t}")
sc_ = pd.read_csv(BENCH / "tracking" / "inout_diagnostic_step_classes.csv")
for fg in ("120fps", "30fps"):
    z = sc_[(sc_.dataset == "melipona") & (sc_.fps_group == fg) & (sc_.input == "ground_truth")]
    g_ = z.groupby("step_class")[["movements", "reproduced"]].sum()
    for cl, lab in (("up to one box size", "small"), ("more than one box size", "large")):
        if cl in g_.index:
            put(f"rep_step_{lab}_{fg}", pct(g_.loc[cl, "reproduced"] / g_.loc[cl, "movements"]), "output/benchmark/tracking/inout_diagnostic_step_classes.csv",
                f"percentage of movement-tracker pairs reproduced, steps {cl}, Melipona {fg}, annotated boxes (all trackers)")
oc_ = pd.read_csv(BENCH / "tracking" / "inout_diagnostic_outside_classes.csv")
for fg in ("120fps", "30fps"):
    for ik, inp in (("gt", "ground_truth"), ("ym", "yolo_melipona")):
        z = oc_[(oc_.dataset == "melipona") & (oc_.fps_group == fg) & (oc_.input == inp)]
        g_ = z.groupby("outside_class")[["movements", "reproduced"]].sum()
        for cl, lab in (("fewer than three boxes outside", "few"), ("three or more boxes outside", "many")):
            if cl in g_.index:
                put(f"rep_out_{lab}_{fg}_{ik}", pct(g_.loc[cl, "reproduced"] / g_.loc[cl, "movements"]),
                    "output/benchmark/tracking/inout_diagnostic_outside_classes.csv",
                    f"percentage of movement-tracker pairs reproduced, movements with {cl} the zone, Melipona {fg}, {inp}")
for gk, key in (("mel120", ("melipona", "120fps")), ("mel30", ("melipona", "30fps")), ("nat", ("native_bee", "all"))):
    put(f"out_med_{gk}", rhu(st_.loc[key, "median_boxes_outside"], 0), "output/benchmark/tracking/inout_diagnostic_steps.csv",
        f"median number of annotated boxes outside the zone per movement, {key}")
    put(f"out_lt3_{gk}", pct(st_.loc[key, "share_under_three_outside"]), "output/benchmark/tracking/inout_diagnostic_steps.csv",
        f"percentage of movements with fewer than three annotated boxes outside the zone, {key}")
# post hoc track-confirmation check (min_hits = 1 instead of the BoxMOT default 3)
cc = pd.read_csv(BENCH / "tracking" / "confirmation_check.csv")
for gk, (ds, fg) in GRP.items():
    for mh in (1, 3):
        z = cc[(cc.dataset == ds) & (cc.fps_group == fg) & (cc.min_hits == mh)]
        put(f"cc_boxes_{gk}_{mh}", rng(z.boxes_returned, pct), "output/benchmark/tracking/confirmation_check.csv",
            f"percentage of annotated boxes returned, OC-SORT, Hybrid-SORT, BoostTrack, min_hits = {mh}, {ds} {fg}")
        put(f"cc_hota_{gk}_{mh}", rng(z.hota), "output/benchmark/tracking/confirmation_check.csv",
            f"HOTA, OC-SORT, Hybrid-SORT, BoostTrack, min_hits = {mh}, {ds} {fg}")
ru = pd.read_csv(BENCH / "tracking" / "annotated_track_runs.csv").set_index(["dataset", "fps_group"])
for gk, (ds, fg) in GRP.items():
    put(f"runs_{gk}", thousands(ru.loc[(ds, fg), "runs"]), "output/benchmark/tracking/annotated_track_runs.csv",
        f"runs of consecutive frames in the annotated tracks, {ds} {fg}")
    put(f"runtracks_{gk}", thousands(ru.loc[(ds, fg), "tracks"]), "output/benchmark/tracking/annotated_track_runs.csv",
        f"annotated tracks, {ds} {fg}")
put("missing_frames_nat", thousands(ru.loc[("native_bee", "all"), "missing_frames"]), "output/benchmark/tracking/annotated_track_runs.csv",
    "frame numbers without an image file, native-bee sequences")
dmm = dg[(dg.dataset == "melipona") & (dg.fps_group == "30fps") & (dg.input == "ground_truth")]
put("missed_pairs_mel30_gt", str(int(dmm.missed.sum())), SRC_D, "missed tracker-movement pairs, Melipona 30 fps, annotated boxes")
put("fail_otherall_mel30_gt", pct((dmm.missed_other.sum() + dmm.missed_not_tracked.sum() + dmm.missed_no_tracked_box_inside_the_zone.sum())
                                  / dmm.missed.sum()), SRC_D, "percentage of missed pairs of other types, Melipona 30 fps, annotated boxes")
# exploratory frame-rate analysis
fp_ = pd.read_csv(BENCH / "frame_rate" / "frame_rate_pooled.csv")
med = fp_.groupby("fps").agg(err=("inout_rel_error", "median"), hota=("hota", "median"))
for r in sorted(fp_.fps.unique()):
    put(f"fr_err_{r}", f2(med.loc[r, "err"]), "output/benchmark/frame_rate/frame_rate_pooled.csv",
        f"median relative error of in + out counts over trackers at {r} fps")
    put(f"fr_hota_{r}", f2(med.loc[r, "hota"]), "output/benchmark/frame_rate/frame_rate_pooled.csv", f"median HOTA over trackers at {r} fps")
put("fr_hota_30_60", rng(med.loc[[r for r in med.index if r <= 60], "hota"]), "output/benchmark/frame_rate/frame_rate_pooled.csv",
    "range of the median HOTA over trackers at 30-60 fps")
put("fr_err_30_60", rng(med.loc[[r for r in med.index if r <= 60], "err"]), "output/benchmark/frame_rate/frame_rate_pooled.csv",
    "range of the median relative count error over trackers at 30-60 fps")
put("fr_err_range_30_60", rng(fp_[fp_.fps <= 60].inout_rel_error), "output/benchmark/frame_rate/frame_rate_pooled.csv",
    "range of the relative count error over trackers and frame rates of 30-60 fps")
for r in sorted(fp_.fps.unique()):
    put(f"fr_err_range_{r}", rng(fp_[fp_.fps == r].inout_rel_error), "output/benchmark/frame_rate/frame_rate_pooled.csv",
        f"range of the relative count error over trackers at {r} fps")
    put(f"fr_err_seq_range_{r}", rng(fp_[fp_.fps == r].inout_rel_error_seq), "output/benchmark/frame_rate/frame_rate_pooled.csv",
        f"range over trackers of the relative count error with per-sequence errors added, {r} fps")
crit_io = CFG["in_out"]["criterion_relative_error"]
put("fr_n_over_crit_120", str(int((fp_[fp_.fps == 120].inout_rel_error > crit_io).sum())), "output/benchmark/frame_rate/frame_rate_pooled.csv",
    "trackers above the count criterion at 120 fps on the five frame-rate sequences")
put("fr_n_meet_crit_120", str(int((fp_[fp_.fps == 120].inout_rel_error <= crit_io).sum())), "output/benchmark/frame_rate/frame_rate_pooled.csv",
    "trackers meeting the count criterion at 120 fps on the five frame-rate sequences")
put("fr_extra_moves", str(int(p[(p.dataset == "melipona") & (p.fps_group == "120fps") & (p.input == "ground_truth")].iloc[0][["gt_in", "gt_out"]].sum()
                              - fr[(fr.tracker == fr.tracker.iloc[0]) & (fr.fps == 120)][["gt_in", "gt_out"]].sum().sum())),
    "metrics_pooled.csv; frame_rate_by_sequence.csv", "annotated movements in the two 120-fps sequences outside the frame-rate set")
put("fr_err_over_crit_120", rng(fp_[(fp_.fps == 120) & (fp_.inout_rel_error > crit_io)].inout_rel_error),
    "output/benchmark/frame_rate/frame_rate_pooled.csv", "count errors of those trackers")
sgp = pd.read_csv(BENCH / "frame_rate" / "frame_rate_scaled_gate.csv").set_index("fps")
sgs = pd.read_csv(BENCH / "frame_rate" / "frame_rate_scaled_gate_by_sequence.csv")
hun = fp_[fp_.tracker == "native_hungarian"].set_index("fps")
for r in sorted(sgp.index):
    put(f"fr_scaled_err_{r}", f2(sgp.loc[r, "inout_rel_error"]), "output/benchmark/frame_rate/frame_rate_scaled_gate.csv",
        f"relative count error, Hungarian tracker with gate scaled to the frame interval, {r} fps")
    put(f"fr_scaled_hota_{r}", f2(sgp.loc[r, "hota"]), "output/benchmark/frame_rate/frame_rate_scaled_gate.csv",
        f"HOTA, Hungarian tracker with scaled gate, {r} fps")
    put(f"fr_scaled_gate_{r}", rhu(sgs[sgs.fps == r].gate.iloc[0], 0), "output/benchmark/frame_rate/frame_rate_scaled_gate_by_sequence.csv",
        f"scaled gate (px), {r} fps")
    put(f"fr_hung_err_{r}", f2(hun.loc[r, "inout_rel_error"]), "output/benchmark/frame_rate/frame_rate_pooled.csv",
        f"relative count error, Hungarian tracker with the fixed gate, {r} fps")
    put(f"fr_hung_hota_{r}", f2(hun.loc[r, "hota"]), "output/benchmark/frame_rate/frame_rate_pooled.csv",
        f"HOTA, Hungarian tracker with the fixed gate, {r} fps")
mv = pd.read_csv(BENCH / "frame_rate" / "frame_rate_movements.csv")
ms = mv.groupby("fps").step_per_box.median()
for r in sorted(mv.fps.unique()):
    put(f"fr_step_{r}", f1d(ms.loc[r]), "output/benchmark/frame_rate/frame_rate_movements.csv",
        f"median largest step in box sizes at {r} fps")
put("fr_moves", str(int(mv.groupby("fps").size().iloc[0])), "output/benchmark/frame_rate/frame_rate_movements.csv",
    "annotated movements per frame rate")
put("fr_gtin", str(int(fr[(fr.tracker == fr.tracker.iloc[0]) & (fr.fps == 120)].gt_in.sum())),
    "output/benchmark/frame_rate/frame_rate_by_sequence.csv", "annotated movements in, frame-rate sequences")
put("fr_gtout", str(int(fr[(fr.tracker == fr.tracker.iloc[0]) & (fr.fps == 120)].gt_out.sum())),
    "output/benchmark/frame_rate/frame_rate_by_sequence.csv", "annotated movements out, frame-rate sequences")

# Pre-labelling -----------------------------------------------------------------------------------
mt = pd.read_csv(BENCH / "prelabel" / "prelabel_metrics_by_threshold.csv")
prim_thr = {c["name"]: CFG["detection"]["primary_threshold"]["owlv2" if c["detector"] == "owl" else c["detector"]]
            for c in CFG["prelabel"]["configurations"]}
mt = mt[mt.apply(lambda r: abs(r.threshold - prim_thr[r.configuration]) < 1e-9, axis=1)]
f1 = mt.set_index(["configuration", "dataset"]).f1
ZS = ["gdino_tiny_800", "gdino_tiny_512", "gdino_base_800", "gdino_tiny_800_insect", "owlv2_base"]
for ds, lab in (("delosperma", "del"), ("melipona", "mel"), ("native_bee", "nat")):
    put(f"plf1_{lab}_zs", rng([f1[(c, ds)] for c in ZS]), "output/benchmark/prelabel/prelabel_metrics_by_threshold.csv",
        f"F1 at default thresholds, Grounding DINO and OWLv2 configurations, {ds}")
    put(f"plf1_{lab}_zs_max", f2(max(f1[(c, ds)] for c in ZS)), "output/benchmark/prelabel/prelabel_metrics_by_threshold.csv",
        f"highest F1 at default thresholds, Grounding DINO and OWLv2 configurations, {ds}")
    put(f"plf1_{lab}_gd", rng([f1[(c, ds)] for c in ZS if c.startswith("gdino")]),
        "output/benchmark/prelabel/prelabel_metrics_by_threshold.csv", f"F1, Grounding DINO configurations, {ds}")
    put(f"plf1_{lab}_owlvit", f2(f1[("owlvit_b32", ds)]), "output/benchmark/prelabel/prelabel_metrics_by_threshold.csv",
        f"F1, OWL-ViT, {ds}")
    put(f"plf1_{lab}_owlv2", f2(f1[("owlv2_base", ds)]), "output/benchmark/prelabel/prelabel_metrics_by_threshold.csv",
        f"F1, OWLv2, {ds}")
    put(f"plf1_{lab}_yolo", f2(f1[("yolo_bee_1280", ds)]), "output/benchmark/prelabel/prelabel_metrics_by_threshold.csv",
        f"F1, YOLO26n bees on flowers, {ds}")
ap = pd.read_csv(BENCH / "prelabel" / "prelabel_ap50.csv").set_index(["configuration", "dataset"]).ap50
put("plap_nat_zs", rng([ap[(c, "native_bee")] for c in ZS]), "output/benchmark/prelabel/prelabel_ap50.csv", "AP50, zero-shot, native bees")
put("plap_mel_zs", rng([ap[(c, "melipona")] for c in ZS]), "output/benchmark/prelabel/prelabel_ap50.csv", "AP50, zero-shot, Melipona")
put("plap_del_yolo", f2(ap[("yolo_bee_1280", "delosperma")]), "output/benchmark/prelabel/prelabel_ap50.csv", "AP50, YOLO, Delosperma")
wl = pd.read_csv(BENCH / "prelabel" / "prelabel_workload.csv")
wz = wl[(wl.dataset == "all") & wl.configuration.isin(ZS)]
put("pl_correct_share", rng(wz.share_true_correct, pct), "output/benchmark/prelabel/prelabel_workload.csv",
    "percentage of annotated boxes matched by a pre-label that needs no change (IoU >= 0.75), zero-shot, all data")
wn = wl[(wl.dataset == "native_bee") & wl.configuration.isin(ZS)]
put("pl_correct_share_nat", rng(wn.share_true_correct, pct), "output/benchmark/prelabel/prelabel_workload.csv",
    "percentage of boxes needing no change, zero-shot, native bees")
put("pl_adjust_nat", rng(wn.boxes_to_adjust / wn.n_true, pct), "output/benchmark/prelabel/prelabel_workload.csv",
    "percentage of boxes found but needing adjustment, zero-shot, native bees")
put("pl_add_nat", rng(wn.boxes_to_add / wn.n_true, pct), "output/benchmark/prelabel/prelabel_workload.csv",
    "percentage of boxes to add (missed), zero-shot, native bees")
found = wn.boxes_correct + wn.boxes_to_adjust
put("pl_found_nat", rng(found / wn.n_true, pct), "output/benchmark/prelabel/prelabel_workload.csv",
    "percentage of annotated boxes found (matched at IoU >= 0.5), zero-shot, native bees")
put("pl_adjust_of_found_nat", rng(wn.boxes_to_adjust / found, pct), "output/benchmark/prelabel/prelabel_workload.csv",
    "percentage of the boxes found that need adjusting (IoU < 0.75), zero-shot, native bees")
wo = wl[(wl.dataset == "native_bee") & (wl.configuration == "owlvit_b32")].iloc[0]
put("pl_found_owlvit_nat", pct((wo.boxes_correct + wo.boxes_to_adjust) / wo.n_true), "output/benchmark/prelabel/prelabel_workload.csv",
    "percentage of annotated boxes found, OWL-ViT, native bees")
put("pl_add_owlvit_nat", pct(wo.boxes_to_add / wo.n_true), "output/benchmark/prelabel/prelabel_workload.csv",
    "percentage of boxes to add, OWL-ViT, native bees")
mg_ = pd.read_csv(BENCH / "prelabel" / "prelabel_metrics_by_threshold.csv")
mg_ = mg_[mg_.dataset != "all"] if "all" in set(mg_.dataset) else mg_
best_ = mg_.sort_values("f1").iloc[-1]
put("pl_f1_grid_max", f2(best_.f1), "output/benchmark/prelabel/prelabel_metrics_by_threshold.csv",
    "highest F1 over the threshold grid, any configuration and data set")
put("pl_f1_grid_thr", f2(best_.threshold), "output/benchmark/prelabel/prelabel_metrics_by_threshold.csv",
    f"threshold of that F1 ({best_.configuration}, {best_.dataset})")
put("pl_f1_grid_conf", f"{best_.configuration}, {best_.dataset}", "output/benchmark/prelabel/prelabel_metrics_by_threshold.csv",
    "configuration and data set of the highest F1 over the threshold grid")
put("pl_f1_max", f2(f1.max()), "output/benchmark/prelabel/prelabel_metrics_by_threshold.csv",
    "highest F1 at the default threshold of any configuration and data set")
put("pl_f1_max_zs", f2(max(f1[(c, d)] for c in ZS for d in ("delosperma", "melipona", "native_bee"))),
    "output/benchmark/prelabel/prelabel_metrics_by_threshold.csv", "highest zero-shot F1 at the default threshold")
put("pl_f1_n_crit", str(int((f1 >= CFG["criteria"]["detection_f1"]).sum())), "output/benchmark/prelabel/prelabel_metrics_by_threshold.csv",
    "configuration-data set pairs meeting the detection F1 criterion")
sm = pd.read_csv(BENCH / "prelabel" / "sam2_refinement.csv").set_index("dataset")
for ds, lab in (("native_bee", "nat"), ("melipona", "mel"), ("all", "all")):
    put(f"sam_iou_before_{lab}", f2(sm.loc[ds, "mean_iou_before"]), "output/benchmark/prelabel/sam2_refinement.csv", f"mean IoU before SAM 2.1, {ds}")
    put(f"sam_iou_after_{lab}", f2(sm.loc[ds, "mean_iou_after"]), "output/benchmark/prelabel/sam2_refinement.csv", f"mean IoU after SAM 2.1, {ds}")
    put(f"sam_tp_before_{lab}", str(int(sm.loc[ds, "tp_before"])), "output/benchmark/prelabel/sam2_refinement.csv", f"matched boxes before, {ds}")
    put(f"sam_tp_after_{lab}", str(int(sm.loc[ds, "tp_after"])), "output/benchmark/prelabel/sam2_refinement.csv", f"matched boxes after, {ds}")
put("sam_bodylen_nat", rhu(sm.loc['native_bee', 'median_body_length_px'], 0), "output/benchmark/prelabel/sam2_refinement.csv",
    "median SAM 2.1 body length, native bees (px)")
tm = pd.read_csv(BENCH / "prelabel" / "timing.csv").set_index("configuration").median_s_per_frame
for c in tm.index:
    put(f"time_{c}", f2(tm[c]) if tm[c] < 1 else f1d(tm[c]), "output/benchmark/prelabel/timing.csv",
        f"median CPU time per frame (2 threads), {c}")
put("time_gdino_range", rng([tm[c] for c in ZS if c.startswith("gdino")], lambda v: f1d(v)),
    "output/benchmark/prelabel/timing.csv", "CPU time per frame, Grounding DINO configurations")
put("threads", str(CFG["threads"]), "config/benchmark.yaml", "CPU threads")
fb = pd.read_csv(BENCH / "prelabel" / "fp32_vs_bf16.csv")
put("bf16_frames", str(len(fb)), "output/benchmark/prelabel/fp32_vs_bf16.csv", "frames in the bfloat16 check")
put("bf16_matched", f"{fb.matched.sum()} of {fb.n_fp32.sum()}", "output/benchmark/prelabel/fp32_vs_bf16.csv",
    "float32 boxes matched by bfloat16 boxes")
put("bf16_iou", f2((fb.mean_iou * fb.matched).sum() / fb.matched.sum()), "output/benchmark/prelabel/fp32_vs_bf16.csv",
    "mean IoU of matched boxes, bfloat16 vs float32")
put("bf16_scorediff", f2((fb.mean_abs_score_diff * fb.matched).sum() / fb.matched.sum()), "output/benchmark/prelabel/fp32_vs_bf16.csv",
    "mean absolute score difference, bfloat16 vs float32")

out = pd.DataFrame(rows)
out.to_csv(OUT / "numbers.csv", index=False)
print(f"{len(out)} numbers written to output/paper/numbers.csv")
