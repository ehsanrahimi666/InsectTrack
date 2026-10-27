"""Zero-shot pre-labelling benchmark: open-vocabulary detectors (Grounding
DINO, OWLv2) and a trained YOLO model on annotated frames of three data
sets, SAM 2.1 box refinement, and a bfloat16 versus float32 check.

Outputs (output/benchmark/prelabel): frames.csv, gt.csv,
detections__<configuration>.csv, sam2_refined.csv, fp32_check.csv and
timing.csv. Configurations already processed are skipped.
"""

import csv
import glob
import os
import re
import sys
import time
from pathlib import Path

import numpy as np
import yaml

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "package" / "InsectTrack" / "inst" / "python"))
sys.path.insert(0, str(ROOT / "analysis"))
import insecttrack_backend as be  # noqa: E402

CFG = yaml.safe_load(open(ROOT / "config" / "benchmark.yaml", encoding="utf-8"))
OD = ROOT / CFG["paths"]["output"] / "benchmark" / "prelabel"
FD = OD / "frames"


def write_rows(path, header, rows):
    tmp = Path(str(path) + ".part")
    with open(tmp, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(header)
        w.writerows(rows)
    os.replace(tmp, path)


def read_video_frames(video, wanted):
    """Return {frame number (1-based): RGB array} for the wanted frames."""
    import cv2

    cap = cv2.VideoCapture(str(video))
    out, k, wanted = {}, 0, set(wanted)
    while len(out) < len(wanted):
        ok, bgr = cap.read()
        if not ok:
            break
        k += 1
        if k in wanted:
            out[k] = bgr[..., ::-1].copy()
    cap.release()
    return out


def even(xs, n):
    xs = sorted(xs)
    if len(xs) <= n:
        return xs
    idx = np.unique(np.round(np.linspace(0, len(xs) - 1, n)).astype(int))
    return [xs[i] for i in idx]


def build_frames():
    """Write the selected frames as PNG files and the ground truth boxes."""
    from PIL import Image

    FD.mkdir(parents=True, exist_ok=True)
    frames, gt = [], []

    def save(img, image_id, dataset):
        p = FD / f"{image_id}.png"
        if not p.exists():
            Image.fromarray(img).save(p)
        h, w = img.shape[:2]
        frames.append([image_id, dataset, str(p.relative_to(ROOT)), w, h])
        return w, h

    # Delosperma: annotated frames contained in the analysis clip
    fmap = list(csv.DictReader(open(ROOT / CFG["paths"]["delosperma_clip_frames"])))
    ann = {int(r["original_frame"]): int(r["clip_frame"]) + 1 for r in fmap if r["annotated"] == "1"}
    labels = {int(re.search(r"frame_(\d+)", os.path.basename(f)).group(1)): f
              for f in glob.glob(str(ROOT / CFG["paths"]["delosperma_labels"] / "*.txt"))}
    imgs = read_video_frames(ROOT / CFG["paths"]["delosperma_clip"], ann.values())
    for orig in sorted(labels):
        if orig not in ann:
            continue
        iid = f"delosperma_{orig:06d}"
        w, h = save(imgs[ann[orig]], iid, "delosperma")
        for line in open(labels[orig]):
            v = line.split()
            if len(v) < 5:
                continue
            cx, cy, bw, bh = (float(z) for z in v[1:5])
            gt.append([iid, (cx - bw / 2) * w, (cy - bh / 2) * h, (cx + bw / 2) * w, (cy + bh / 2) * h])

    # Melipona: frames with ground truth, evenly spaced within each sequence
    base = ROOT / CFG["paths"]["melipona"]
    seqs = []
    for fps in (120, 30):
        for v in sorted(glob.glob(str(base / f"{fps}_fps" / "partes" / "*.mp4"))):
            g = Path(v[:-4] + "_gt") / "gt" / "gt.txt"
            if g.exists():
                seqs.append((f"{Path(v).name[:5]}_{fps}fps", v, g))
    seqs.append(("08h00v_120fps", str(base / "validacao_mota" / "2026_03_06_h08_M00_S18.mp4"),
                 base / "validacao_mota" / "gt" / "gt.txt"))
    for sid, video, g in seqs:
        labs = [z.strip().lower() for z in open(Path(g).parent / "labels.txt").read().splitlines() if z.strip()]
        bee = [i + 1 for i, z in enumerate(labs) if z == "bee"]
        rows = [r for r in csv.reader(open(g)) if int(float(r[7])) in bee]
        by_f = {}
        for r in rows:
            by_f.setdefault(int(r[0]), []).append([float(z) for z in r[2:6]])
        sel = even(list(by_f), CFG["prelabel"]["frames"]["melipona"])
        imgs = read_video_frames(video, sel)
        for f in sel:
            iid = f"melipona_{sid}_{f:05d}"
            save(imgs[f], iid, "melipona")
            for left, top, bw, bh in by_f[f]:
                gt.append([iid, left, top, left + bw, top + bh])

    # Native bees: annotated images, evenly spaced within each sequence
    nb = ROOT / CFG["paths"]["native_bee"]
    for sid in sorted(os.listdir(nb)):
        lab = {int(re.search(r"_frame_(\d+)", os.path.basename(f)).group(1)): f
               for f in glob.glob(str(nb / sid / "labels" / "*.txt"))}
        img = {int(re.search(r"_frame_(\d+)", os.path.basename(f)).group(1)): f
               for f in glob.glob(str(nb / sid / "images" / "*"))}
        with_boxes = [k for k, f in lab.items() if os.path.getsize(f) > 0 and k in img]
        for f in even(with_boxes, CFG["prelabel"]["frames"]["native_bee"]):
            iid = f"native_bee_{sid}_{f:05d}"
            w, h = save(be._read_image(img[f]), iid, "native_bee")
            seen = set()
            for line in open(lab[f]):
                v = line.split()
                if len(v) < 5 or v[0] in seen:
                    continue
                seen.add(v[0])
                cx, cy, bw, bh = (float(z) for z in v[1:5])
                gt.append([iid, (cx - bw / 2) * w, (cy - bh / 2) * h, (cx + bw / 2) * w, (cy + bh / 2) * h])
    write_rows(OD / "frames.csv", ["image_id", "dataset", "file", "width", "height"], frames)
    write_rows(OD / "gt.csv", ["image_id", "x1", "y1", "x2", "y2"], [[r[0]] + [round(z, 2) for z in r[1:]] for r in gt])
    return frames


def run_config(c, frames):
    out = OD / f"detections__{c['name']}.csv"
    if out.exists():
        return
    ms = CFG["detection"]["min_score"]
    rows, times = [], []
    for iid, _, f, _, _ in frames:
        path = str(ROOT / f)
        t = time.time()
        if c["detector"] == "gdino":
            se = c["shortest_edge"]
            d = be.detect_gdino([path], prompt=c["prompt"], box_threshold=ms, text_threshold=ms,
                                model_id=CFG["models"][c["model"]], shortest_edge=se,
                                longest_edge=int(se * 1333 / 800), bf16=c.get("bf16", False),
                                nms_iou=CFG["detection"]["nms_iou"], max_box_frac=CFG["detection"]["max_box_frac"])[0]
        elif c["detector"] == "owl":
            d = be.detect_owl([path], prompt=c["prompt"], threshold=ms, model_id=CFG["models"][c["model"]],
                              nms_iou=CFG["detection"]["nms_iou"], max_box_frac=CFG["detection"]["max_box_frac"])[0]
        else:
            d = be.detect_yolo([path], str(ROOT / CFG["models"][c["model"]]), conf=ms, imgsz=c["imgsz"])[0]
        times.append(time.time() - t)
        rows += [[iid] + [round(z, 3) for z in r[:5]] + [int(r[5])] for r in d.tolist()]
    write_rows(out, ["image_id", "x1", "y1", "x2", "y2", "score", "class"], rows)
    new = not (OD / "timing.csv").exists()
    with open(OD / "timing.csv", "a", newline="") as fh:
        w = csv.writer(fh)
        if new:
            w.writerow(["configuration", "frames", "median_s_per_frame", "mean_s_per_frame"])
        # the first frame includes model loading
        w.writerow([c["name"], len(times), round(float(np.median(times)), 3), round(float(np.mean(times[1:] or times)), 3)])
    print(c["name"], f"{np.median(times):.2f} s per frame, {len(rows)} boxes", flush=True)


def sam2_refine(frames):
    out = OD / "sam2_refined.csv"
    if out.exists():
        return
    sc = CFG["prelabel"]["sam2_refine"]
    thr = CFG["detection"]["primary_threshold"]["gdino"]
    det = list(csv.DictReader(open(OD / f"detections__{sc['configuration']}.csv")))
    rows, times = [], []
    for iid, _, f, _, _ in frames:
        d = [r for r in det if r["image_id"] == iid and float(r["score"]) >= thr]
        if not d:
            continue
        boxes = np.array([[float(r[k]) for k in ("x1", "y1", "x2", "y2")] for r in d])
        t = time.time()
        res = be.segment_sam2(str(ROOT / f), boxes, model_id=CFG["models"][sc["model"]])
        times.append(time.time() - t)
        for r, p in zip(d, res["props"]):
            rows.append([iid, r["x1"], r["y1"], r["x2"], r["y2"], r["score"]] +
                        [round(float(z), 3) if np.isfinite(z) else "" for z in p])
    write_rows(out, ["image_id", "x1", "y1", "x2", "y2", "score", "mask_area", "mask_x", "mask_y", "body_length",
                     "body_width", "body_angle", "mx1", "my1", "mx2", "my2"], rows)
    with open(OD / "timing.csv", "a", newline="") as fh:
        csv.writer(fh).writerow(["sam2_refine", len(times), round(float(np.median(times)), 3),
                                 round(float(np.mean(times[1:] or times)), 3)])
    print("sam2", f"{np.median(times):.2f} s per frame", flush=True)


def fp32_check(frames):
    out = OD / "fp32_check.csv"
    if out.exists():
        return
    fc = CFG["prelabel"]["fp32_check"]
    c = [z for z in CFG["prelabel"]["configurations"] if z["name"] == fc["configuration"]][0]
    sel = even(list(range(len(frames))), fc["frames"])
    ms = CFG["detection"]["min_score"]
    rows = []
    for i in sel:
        iid, _, f, _, _ = frames[i]
        for bf in (False, True):
            d = be.detect_gdino([str(ROOT / f)], prompt=c["prompt"], box_threshold=ms, text_threshold=ms,
                                model_id=CFG["models"][c["model"]], shortest_edge=c["shortest_edge"],
                                longest_edge=int(c["shortest_edge"] * 1333 / 800), bf16=bf,
                                nms_iou=CFG["detection"]["nms_iou"], max_box_frac=CFG["detection"]["max_box_frac"])[0]
            rows += [[iid, "bf16" if bf else "fp32"] + [round(z, 3) for z in r[:5]] for r in d.tolist()]
    write_rows(out, ["image_id", "precision", "x1", "y1", "x2", "y2", "score"], rows)
    print("fp32 check done", flush=True)


def main():
    import argparse

    ap = argparse.ArgumentParser()
    ap.add_argument("--configs", help="comma-separated configuration names (default: all, with the SAM 2.1 "
                                      "refinement and the float32 check)")
    ap.add_argument("--extras", action="store_true", help="run the SAM 2.1 refinement and the float32 check")
    args = ap.parse_args()
    names = args.configs.split(",") if args.configs else None
    be.set_threads(CFG["threads"])
    OD.mkdir(parents=True, exist_ok=True)
    if (OD / "frames.csv").exists():
        frames = [[r["image_id"], r["dataset"], r["file"], int(r["width"]), int(r["height"])]
                  for r in csv.DictReader(open(OD / "frames.csv"))]
    else:
        frames = build_frames()
    print(len(frames), "frames", flush=True)
    for c in CFG["prelabel"]["configurations"]:
        if names is None or c["name"] in names:
            run_config(c, frames)
    if names is None or args.extras:
        sam2_refine(frames)
        fp32_check(frames)


if __name__ == "__main__":
    main()
