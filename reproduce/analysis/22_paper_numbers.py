"""Numbers reported in the InsectTrack paper, computed from the saved outputs of
the benchmarks and demonstration videos. Writes output/paper/numbers.csv with
one row per number: key, value as printed, source file and definition.

Usage: python3 analysis/22_paper_numbers.py
"""

import json
import re
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


def f2(x):
    v = f"{x:.2f}"
    return "0.00" if v == "-0.00" else v.replace("-", "\u2212")


def rng(values, fmt=f2):
    """Range of values as printed: 'lo–hi', or 'lo to hi' when a bound is negative."""
    values = np.asarray(values, float)
    lo, hi = fmt(values.min()), fmt(values.max())
    if lo == hi:
        return lo
    return f"{lo} to {hi}" if "\u2212" in lo + hi else f"{lo}–{hi}"


def pct(x):
    return f"{100 * x:.0f}"


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
put("fr_rates", ", ".join(str(x) for x in sorted(fr.fps.unique())), "output/benchmark/frame_rate/frame_rate_by_sequence.csv",
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
    put(f"pl_{lab}_boxw", f"{w.median():.0f}", "output/benchmark/prelabel/gt.csv, frames.csv",
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
put("demo_tracks_range", rng(prim.tracks, lambda v: f"{v:.0f}"), "output/demo/demo_gallery.csv", "tracks per video (primary)")
put("demo_gdino_found", str(int(((prim.detector == "gdino") & (prim.tracks > 0)).sum())), "output/demo/demo_gallery.csv",
    "primary Grounding DINO runs with at least one track")
gd = g[g.detector == "gdino"]
put("demo_gdino_s_per_frame", rng(gd.run_s / gd.frames, lambda v: f"{v:.1f}"), "output/demo/demo_gallery.csv",
    "run time per analysed frame, Grounding DINO base (whole pipeline, CPU)")
put("demo_gdino_s_per_frame_median", f"{np.median(gd.run_s / gd.frames):.1f}", "output/demo/demo_gallery.csv",
    "median run time per analysed frame, Grounding DINO base")
tracks = g.set_index("demo").tracks
for k in ("bee_counter", "bee_counter_tiled", "ant_trail", "leafcutter_ants", "leafcutter_ants_gdino", "drosophila_arena",
          "drosophila_arena_gdino", "hive_fanning", "hive_entrance", "hive_flight", "delosperma", "melipona"):
    put(f"tracks_{k}", thousands(tracks[k]), "output/demo/demo_gallery.csv", f"tracks, {k}")
ds_ = pd.read_csv(DEMO / "demo_summary.csv").set_index("demo")
for k in ("hive_entrance", "hive_flight", "melipona", "bee_counter_tiled"):
    put(f"in_{k}", str(int(ds_.loc[k, "n_in"])), "output/demo/demo_summary.csv", f"insects in, {k}")
    put(f"out_{k}", str(int(ds_.loc[k, "n_out"])), "output/demo/demo_summary.csv", f"insects out, {k}")
put("mean_insects_hive_fanning", f"{ds_.loc['hive_fanning', 'mean_insects_per_frame']:.0f}", "output/demo/demo_summary.csv",
    "mean bees per frame, hive_fanning")
put("max_insects_hive_fanning", f"{ds_.loc['hive_fanning', 'max_insects_per_frame']:.0f}", "output/demo/demo_summary.csv",
    "maximum bees per frame, hive_fanning")
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
put("del_visit_median", f"{vis.duration.median():.1f}", "output/demo/delosperma/visits.csv", "median visit duration (s)")
put("del_visit_max", f"{vis.duration.max():.1f}", "output/demo/delosperma/visits.csv", "longest visit (s)")
put("del_mean_bees", f"{act.mean_count.mean():.1f}", "output/demo/delosperma/activity.csv", "mean bees in view per frame")
put("del_maxn", str(int(act.max_count.max())), "output/demo/delosperma/activity.csv", "largest MaxN in a 1-s bin")
hot_col = [c for c in hm.columns if c.lower() in ("hot", "hotspot", "is_hot", "hot_spot")]
if hot_col:
    put("del_hot_cells", str(int(hm[hot_col[0]].astype(bool).sum())), "output/demo/delosperma/heatmap.csv", "hot-spot cells")
put("del_conf", f"{st['conf']:.2f}", "output/demo/delosperma/settings.json", "detection score threshold")

# Tracking benchmark ------------------------------------------------------------------------------
IOU_TRACKERS = ["boxmot_ocsort", "boxmot_hybridsort", "boxmot_boosttrack", "boxmot_sfsort"]
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
z = p[(p.dataset == "native_bee") & (p.input == "ground_truth")]
put("hota_nat_gt_iou", rng(z[z.tracker.isin(IOU_TRACKERS)].hota), "output/benchmark/tracking/metrics_pooled.csv",
    "HOTA, native bees, annotated boxes, OC-SORT, Hybrid-SORT, BoostTrack, SFSORT")
put("hota_nat_gt_other", rng(z[~z.tracker.isin(IOU_TRACKERS)].hota), "output/benchmark/tracking/metrics_pooled.csv",
    "HOTA, native bees, annotated boxes, other trackers")
put("assa_nat_gt_iou", rng(z[z.tracker.isin(IOU_TRACKERS)].assa), "output/benchmark/tracking/metrics_pooled.csv",
    "AssA, native bees, annotated boxes, IoU-only trackers")
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
put("zone_mel_radius", f"{zn.loc['melipona', 'radius']:.0f}", "output/benchmark/tracking/entrance_zones.csv", "entrance zone radius, Melipona (px)")
put("zone_nat_radius", f"{zn.loc['native_bee', 'radius']:.0f}", "output/benchmark/tracking/entrance_zones.csv", "entrance zone radius, native bees (px)")
for gk, (ds, fg) in GRP.items():
    gz = p[(p.dataset == ds) & (p.fps_group == fg) & (p.input == "ground_truth")].iloc[0]
    put(f"gtin_{gk}", str(int(gz.gt_in)), "output/benchmark/tracking/metrics_pooled.csv", f"annotated movements in, {ds} {fg}")
    put(f"gtout_{gk}", str(int(gz.gt_out)), "output/benchmark/tracking/metrics_pooled.csv", f"annotated movements out, {ds} {fg}")
    for ik, inp in INP.items():
        z = p[(p.dataset == ds) & (p.fps_group == fg) & (p.input == inp)]
        if len(z):
            put(f"ioerr_{gk}_{ik}", rng(z.inout_rel_error), "output/benchmark/tracking/metrics_pooled.csv",
                f"relative error of in + out counts, range over trackers, {ds} {fg}, {inp}")
            put(f"ioerr_med_{gk}_{ik}", f2(z.inout_rel_error.median()), "output/benchmark/tracking/metrics_pooled.csv",
                f"median relative error of in + out counts over trackers, {ds} {fg}, {inp}")
z = p[(p.dataset == "melipona") & (p.fps_group == "120fps") & (p.input == "ground_truth")]
zt = p[(p.dataset == "melipona") & (p.input == "ground_truth")].set_index(["fps_group", "tracker"]).inout_rel_error
for t in ("boxmot_boosttrack", "boxmot_sfsort", "native_hungarian"):
    for fg in ("120fps", "30fps"):
        put(f"ioerr_{fg}_gt_{t}", f2(zt.loc[(fg, t)]), "output/benchmark/tracking/metrics_pooled.csv",
            f"relative error of in + out counts, Melipona {fg}, annotated boxes, {t}")
put("ioerr_mel120_gt_ncrit", str(int((z.inout_rel_error <= CFG["in_out"]["criterion_relative_error"]).sum())),
    "output/benchmark/tracking/metrics_pooled.csv", "trackers meeting the in/out criterion, Melipona 120 fps, annotated boxes")
st_ = pd.read_csv(BENCH / "tracking" / "inout_diagnostic_steps.csv").set_index(["dataset", "fps_group"])
for gk, key in (("mel120", ("melipona", "120fps")), ("mel30", ("melipona", "30fps"))):
    put(f"step_{gk}", f"{st_.loc[key, 'median_step_per_box']:.1f}", "output/benchmark/tracking/inout_diagnostic_steps.csv",
        f"median largest step between frames in body lengths, {key}")
    put(f"step_over1_{gk}", pct(st_.loc[key, "share_over_one_box"]), "output/benchmark/tracking/inout_diagnostic_steps.csv",
        f"percentage of movements with a step longer than one body length, {key}")
    put(f"moves_{gk}", str(int(st_.loc[key, "movements"])), "output/benchmark/tracking/inout_diagnostic_steps.csv",
        f"annotated movements, {key}")
ev = pd.read_csv(BENCH / "tracking" / "inout_diagnostic_events.csv")
rec = ev[ev.dataset == "melipona"].groupby(["fps_group", "tracker"]).recovered.mean().unstack(0)
put("recov_mel120", rng(rec["120fps"], pct), "output/benchmark/tracking/inout_diagnostic_events.csv",
    "percentage of annotated movements reproduced, Melipona 120 fps, range over trackers")
put("recov_mel30", rng(rec["30fps"], pct), "output/benchmark/tracking/inout_diagnostic_events.csv",
    "percentage of annotated movements reproduced, Melipona 30 fps, range over trackers")
for t in ("boxmot_sfsort", "native_hungarian", "native_bytetrack_stitch", "boxmot_boosttrack"):
    put(f"recov_mel120_{t}", pct(rec.loc[t, "120fps"]), "output/benchmark/tracking/inout_diagnostic_events.csv",
        f"percentage of movements reproduced, 120 fps, {t}")
    put(f"recov_mel30_{t}", pct(rec.loc[t, "30fps"]), "output/benchmark/tracking/inout_diagnostic_events.csv",
        f"percentage of movements reproduced, 30 fps, {t}")
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
mv = pd.read_csv(BENCH / "frame_rate" / "frame_rate_movements.csv")
ms = mv.groupby("fps").step_per_box.median()
for r in sorted(mv.fps.unique()):
    put(f"fr_step_{r}", f"{ms.loc[r]:.1f}", "output/benchmark/frame_rate/frame_rate_movements.csv",
        f"median largest step in body lengths at {r} fps")
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
sm = pd.read_csv(BENCH / "prelabel" / "sam2_refinement.csv").set_index("dataset")
for ds, lab in (("native_bee", "nat"), ("melipona", "mel"), ("all", "all")):
    put(f"sam_iou_before_{lab}", f2(sm.loc[ds, "mean_iou_before"]), "output/benchmark/prelabel/sam2_refinement.csv", f"mean IoU before SAM 2.1, {ds}")
    put(f"sam_iou_after_{lab}", f2(sm.loc[ds, "mean_iou_after"]), "output/benchmark/prelabel/sam2_refinement.csv", f"mean IoU after SAM 2.1, {ds}")
    put(f"sam_tp_before_{lab}", str(int(sm.loc[ds, "tp_before"])), "output/benchmark/prelabel/sam2_refinement.csv", f"matched boxes before, {ds}")
    put(f"sam_tp_after_{lab}", str(int(sm.loc[ds, "tp_after"])), "output/benchmark/prelabel/sam2_refinement.csv", f"matched boxes after, {ds}")
put("sam_bodylen_nat", f"{sm.loc['native_bee', 'median_body_length_px']:.0f}", "output/benchmark/prelabel/sam2_refinement.csv",
    "median SAM 2.1 body length, native bees (px)")
tm = pd.read_csv(BENCH / "prelabel" / "timing.csv").set_index("configuration").median_s_per_frame
for c in tm.index:
    put(f"time_{c}", f"{tm[c]:.2f}" if tm[c] < 1 else f"{tm[c]:.1f}", "output/benchmark/prelabel/timing.csv",
        f"median CPU time per frame (2 threads), {c}")
put("time_gdino_range", rng([tm[c] for c in ZS if c.startswith("gdino")], lambda v: f"{v:.1f}"),
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
