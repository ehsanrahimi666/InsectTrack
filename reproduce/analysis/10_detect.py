"""Detections for the tracking benchmark: every frame of the Melipona and
native-bee sequences, stored with all scores above detection.min_score.

Usage: python analysis/10_detect.py --dataset melipona --input yolo_bee
Inputs: yolo_melipona, yolo_bee (YOLO models) and gdino_tiny_512 (zero-shot).
Output: output/benchmark/detections/<dataset>/<sequence>__<input>.csv with
frame (1-based), x1, y1, x2, y2, score, class, and a timing table.
Sequences already processed are skipped.
"""

import argparse
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
import insecttrack_backend as be  # noqa: E402

CFG = yaml.safe_load(open(ROOT / "config" / "benchmark.yaml", encoding="utf-8"))


def melipona_sequences():
    base = ROOT / CFG["paths"]["melipona"]
    out = []
    for fps in (120, 30):
        for v in sorted(glob.glob(str(base / f"{fps}_fps" / "partes" / "*.mp4"))):
            gt = Path(v[:-4] + "_gt") / "gt" / "gt.txt"
            if gt.exists():
                out.append({"id": f"{Path(v).name[:5]}_{fps}fps", "fps": fps, "video": v})
    out.append({"id": "08h00v_120fps", "fps": 120, "video": str(base / "validacao_mota" / "2026_03_06_h08_M00_S18.mp4")})
    return out


def native_bee_sequences():
    base = ROOT / CFG["paths"]["native_bee"]
    return [{"id": s, "fps": None, "images": str(base / s / "images")} for s in sorted(os.listdir(base))]


def frames_of(seq):
    """Yield (frame number, RGB array)."""
    if "video" in seq:
        import cv2

        cap = cv2.VideoCapture(seq["video"])
        k = 0
        while True:
            ok, bgr = cap.read()
            if not ok:
                break
            k += 1
            yield k, bgr[..., ::-1].copy()
        cap.release()
    else:
        files = sorted(glob.glob(os.path.join(seq["images"], "*")))
        for f in files:
            m = re.search(r"_frame_(\d+)", os.path.basename(f))
            yield int(m.group(1)), be._read_image(f)


def detector(name):
    det = CFG["detection"]
    if name.startswith("yolo"):
        model = str(ROOT / CFG["models"][name])
        imgsz = CFG["tracking"]["yolo"]["imgsz"][name]
        return lambda imgs: be.detect_yolo(imgs, model, conf=det["min_score"], imgsz=imgsz, batch=len(imgs))
    if name == "gdino_tiny_512":
        g = CFG["tracking"]["gdino"]
        return lambda imgs: be.detect_gdino(imgs, prompt=g["prompt"], box_threshold=det["min_score"],
                                            text_threshold=det["min_score"], model_id=CFG["models"]["gdino_tiny"],
                                            shortest_edge=g["shortest_edge"], longest_edge=int(g["shortest_edge"] * 16 / 9) + 1,
                                            bf16=g["bf16"], nms_iou=det["nms_iou"], max_box_frac=det["max_box_frac"])
    raise ValueError(name)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dataset", required=True, choices=["melipona", "native_bee"])
    ap.add_argument("--input", required=True)
    ap.add_argument("--sequences", default="", help="comma-separated ids or 30fps / 120fps")
    a = ap.parse_args()
    be.set_threads(CFG["threads"])
    seqs = melipona_sequences() if a.dataset == "melipona" else native_bee_sequences()
    if a.sequences:
        sel = a.sequences.split(",")
        seqs = [s for s in seqs if s["id"] in sel or any(s["id"].endswith(x) for x in sel)]
    od = ROOT / CFG["paths"]["output"] / "benchmark" / "detections" / a.dataset
    od.mkdir(parents=True, exist_ok=True)
    fn = detector(a.input)
    batch = 4 if a.input == "yolo_bee" else 1   # the ONNX model has a fixed batch size of 1
    timing = od / f"timing__{a.input}.csv"
    for s in seqs:
        out = od / f"{s['id']}__{a.input}.csv"
        if out.exists():
            continue
        rows, n, t0 = [], 0, time.time()
        buf = []
        for k, img in frames_of(s):
            buf.append((k, img))
            if len(buf) == batch:
                for (kk, _), d in zip(buf, fn([b[1] for b in buf])):
                    rows += [[kk, *r] for r in d.tolist()]
                n += len(buf)
                buf = []
        if buf:
            for (kk, _), d in zip(buf, fn([b[1] for b in buf])):
                rows += [[kk, *r] for r in d.tolist()]
            n += len(buf)
        dt = time.time() - t0
        tmp = out.with_suffix(".part")
        with open(tmp, "w", newline="") as f:
            w = csv.writer(f)
            w.writerow(["frame", "x1", "y1", "x2", "y2", "score", "class"])
            w.writerows([[r[0]] + [round(v, 3) for v in r[1:6]] + [int(r[6])] for r in rows])
        os.replace(tmp, out)
        new = not timing.exists()
        with open(timing, "a", newline="") as f:
            w = csv.writer(f)
            if new:
                w.writerow(["sequence", "frames", "seconds", "seconds_per_frame"])
            w.writerow([s["id"], n, round(dt, 1), round(dt / max(n, 1), 4)])
        print(f"{a.dataset} {s['id']} {a.input}: {n} frames, {len(rows)} detections, {dt / max(n, 1):.3f} s/frame", flush=True)


if __name__ == "__main__":
    main()
