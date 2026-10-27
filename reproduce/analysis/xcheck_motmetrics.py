"""Independent check of the tracking metrics with py-motmetrics.

Reads the ground truth and the tracks written by 03_melipona.R and
04_native_bee.R, recomputes MOTA, IDF1 and identity switches with
motmetrics (centre distance threshold as in the R analysis) and writes
output/xcheck_motmetrics.csv.
"""
import glob
import os

import motmetrics as mm
import numpy as np
import pandas as pd

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, "output")


def evaluate(gt, pr, max_dist, pred_id):
    gt = gt.drop_duplicates(["frame", "id"]).copy(); pr = pr.copy()
    gt["id"] = pd.factorize(gt["id"].astype(str))[0] + 1           # motmetrics needs numeric ids
    pr[pred_id] = pd.factorize(pr[pred_id].astype(str))[0] + 1
    acc = mm.MOTAccumulator(auto_id=False)
    frames = sorted(set(gt["frame"]).union(pr["frame"]))
    g_by = dict(tuple(gt.groupby("frame")))
    p_by = dict(tuple(pr.groupby("frame")))
    for f in frames:
        g = g_by.get(f, gt.iloc[0:0])
        p = p_by.get(f, pr.iloc[0:0])
        if len(g) and len(p):
            d = np.sqrt((g["x"].values[:, None] - p["x"].values[None, :]) ** 2
                        + (g["y"].values[:, None] - p["y"].values[None, :]) ** 2)
            d[d > max_dist] = np.nan
        else:
            d = np.empty((len(g), len(p)))
        acc.update(g["id"].astype(int).tolist(), p[pred_id].astype(int).tolist(), d, frameid=int(f))
    mh = mm.metrics.create()
    s = mh.compute(acc, metrics=["mota", "idf1", "num_switches", "num_misses", "num_false_positives"], name="x")
    return s.iloc[0]


rows = []
for gtf in sorted(glob.glob(os.path.join(OUT, "melipona", "*_gt.csv"))):
    sid = os.path.basename(gtf)[:-7]
    gt = pd.read_csv(gtf)
    for kind, suffix in [("motion detections", "_tracks.csv"), ("annotated positions", "_tracks_from_annotations.csv")]:
        pr = pd.read_csv(os.path.join(OUT, "melipona", sid + suffix))
        pr = pr[pr["track"].notna()]
        r = evaluate(gt, pr, 20.0, "track")
        rows.append(dict(dataset="melipona", id=sid, input=kind, mota_motmetrics=r["mota"],
                         idf1_motmetrics=r["idf1"], id_switches_motmetrics=int(r["num_switches"])))
for gtf in sorted(glob.glob(os.path.join(OUT, "melipona_yolo", "*_gt.csv"))):
    sid = os.path.basename(gtf)[:-7]
    gt = pd.read_csv(gtf)
    for kind, suffix in [("YOLO + InsectTrack", "_tracks_insecttrack.csv"), ("YOLO + ByteTrack", "_tracks_bytetrack.csv")]:
        pr = pd.read_csv(os.path.join(OUT, "melipona_yolo", sid + suffix))
        pr = pr[pr["track"].notna()]
        r = evaluate(gt, pr, 20.0, "track")
        rows.append(dict(dataset="melipona_yolo", id=sid, input=kind, mota_motmetrics=r["mota"],
                         idf1_motmetrics=r["idf1"], id_switches_motmetrics=int(r["num_switches"])))
for gtf in sorted(glob.glob(os.path.join(OUT, "native_bee", "*_gt.csv"))):
    sid = os.path.basename(gtf)[:-7]
    gt = pd.read_csv(gtf)
    pr = pd.read_csv(os.path.join(OUT, "native_bee", sid + "_tracks_from_annotations.csv"))
    pr = pr[pr["track"].notna()]
    r = evaluate(gt, pr, 25.0, "track")
    rows.append(dict(dataset="native_bee", id=sid, input="annotated positions", mota_motmetrics=r["mota"],
                     idf1_motmetrics=r["idf1"], id_switches_motmetrics=int(r["num_switches"])))
x = pd.DataFrame(rows)

# compare with the values computed in R
def r_values():
    out = []
    m = pd.read_csv(os.path.join(OUT, "melipona", "melipona_metrics_by_sequence.csv"))
    m = m[m["stitched"] & m["input"].isin(["motion detections", "annotated positions"])]
    out.append(pd.DataFrame(dict(dataset="melipona", id=m["id"], input=m["input"], mota_r=m["mota"],
                                 idf1_r=m["idf1"], id_switches_r=m["id_switches"])))
    y = pd.read_csv(os.path.join(OUT, "melipona_yolo", "melipona_yolo_metrics_by_sequence.csv"))
    y["input"] = np.where(y["tracker"] == "InsectTrack", "YOLO + InsectTrack", "YOLO + ByteTrack")
    out.append(pd.DataFrame(dict(dataset="melipona_yolo", id=y["id"], input=y["input"], mota_r=y["mota"],
                                 idf1_r=y["idf1"], id_switches_r=y["id_switches"])))
    n = pd.read_csv(os.path.join(OUT, "native_bee", "native_bee_metrics_by_sequence.csv"))
    n = n[n["stitched"]]
    out.append(pd.DataFrame(dict(dataset="native_bee", id=n["sequence"], input="annotated positions",
                                 mota_r=n["mota"], idf1_r=n["idf1"], id_switches_r=n["id_switches"])))
    return pd.concat(out)

x = x.merge(r_values(), on=["dataset", "id", "input"], how="left")
x["mota_diff"] = (x["mota_motmetrics"] - x["mota_r"]).abs()
x["idf1_diff"] = (x["idf1_motmetrics"] - x["idf1_r"]).abs()
x.to_csv(os.path.join(OUT, "xcheck_motmetrics.csv"), index=False)
print(x.groupby("dataset")[["mota_diff", "idf1_diff"]].max())
print("identity switches equal:", (x["id_switches_motmetrics"] == x["id_switches_r"]).mean())
