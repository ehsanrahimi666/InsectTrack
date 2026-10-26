"""Python back end of the R package InsectTrack.

Open-vocabulary detection (Grounding DINO, OWLv2), YOLO detection
(Ultralytics), SAM 2.1 segmentation of detected boxes and multi-object
trackers from BoxMOT. Functions take and return plain Python and NumPy
objects so that they can be called from R through reticulate.

Detections are returned per image as float arrays with columns
x1, y1, x2, y2, score, class (pixel coordinates of the input image).
"""

from __future__ import annotations

import re

import numpy as np

_MODELS: dict = {}


def select_device(device=None):
    """Return 'cuda', 'mps' or 'cpu' when device is None or 'auto'."""
    import torch

    if device in (None, "", "auto"):
        if torch.cuda.is_available():
            return "cuda"
        mps = getattr(torch.backends, "mps", None)
        if mps is not None and mps.is_available():
            return "mps"
        return "cpu"
    return str(device)


def set_threads(n=None):
    """Set the number of CPU threads used by torch; return the number in use."""
    import torch

    if n:
        torch.set_num_threads(int(n))
    return torch.get_num_threads()


def versions():
    """Versions of the Python packages used by the back end."""
    import importlib
    import sys

    out = {"python": sys.version.split()[0]}
    for name in ["numpy", "torch", "torchvision", "transformers", "ultralytics", "boxmot", "cv2", "PIL"]:
        try:
            out[name] = getattr(importlib.import_module(name), "__version__", "")
        except Exception:
            out[name] = ""
    return out


def _read_image(x):
    """File path or H x W x 3 array (uint8 or 0-1 float) -> RGB uint8 array."""
    if isinstance(x, str):
        from PIL import Image

        with Image.open(x) as im:
            return np.asarray(im.convert("RGB"))
    a = np.asarray(x)
    if a.ndim == 2:
        a = np.repeat(a[..., None], 3, axis=2)
    if a.shape[2] == 4:
        a = a[..., :3]
    if a.dtype != np.uint8:
        scale = 255.0 if float(np.nanmax(a)) <= 1.0 else 1.0
        a = np.clip(a * scale, 0, 255).astype(np.uint8)
    return np.ascontiguousarray(a)


def phrases(prompt):
    """Split a prompt such as 'bee, wasp' or 'bee. wasp.' into phrases."""
    if isinstance(prompt, (list, tuple)):
        ps = [str(p) for p in prompt]
    else:
        ps = re.split(r"[.,;]", str(prompt))
    return [p.strip().lower() for p in ps if p.strip()]


def _phrase_index(label, ps):
    label = str(label).strip().lower()
    for i, p in enumerate(ps):
        if label == p:
            return i
    for i, p in enumerate(ps):
        if p in label or label in p:
            return i
    return 0


def _nms(det, iou=0.5):
    """Class-agnostic non-maximum suppression of an N x 6 array."""
    if len(det) < 2:
        return det
    import torch
    from torchvision.ops import nms

    keep = nms(torch.as_tensor(det[:, :4], dtype=torch.float32),
               torch.as_tensor(det[:, 4], dtype=torch.float32), float(iou)).numpy()
    return det[np.sort(keep)]


def _filter_boxes(det, w, h, max_box_frac, min_box_px):
    if not len(det):
        return det
    bw = det[:, 2] - det[:, 0]
    bh = det[:, 3] - det[:, 1]
    ok = (bw >= min_box_px) & (bh >= min_box_px)
    if max_box_frac:
        ok &= bw * bh <= max_box_frac * w * h
    return det[ok]


def _tiles(w, h, tile, overlap):
    """Top-left corners of overlapping square tiles covering a w x h image."""
    if not tile or (w <= tile and h <= tile):
        return [(0, 0, w, h)]
    step = max(1, int(tile * (1 - overlap)))
    xs = list(range(0, max(1, w - tile) + 1, step))
    ys = list(range(0, max(1, h - tile) + 1, step))
    if xs[-1] + tile < w:
        xs.append(w - tile)
    if ys[-1] + tile < h:
        ys.append(h - tile)
    return [(x, y, min(tile, w), min(tile, h)) for y in ys for x in xs]


def _load_gdino(model_id, device, cache_dir):
    key = ("gdino", model_id, device)
    if key not in _MODELS:
        from transformers import AutoModelForZeroShotObjectDetection, AutoProcessor

        proc = AutoProcessor.from_pretrained(model_id, cache_dir=cache_dir)
        model = AutoModelForZeroShotObjectDetection.from_pretrained(model_id, cache_dir=cache_dir, use_safetensors=True)
        _MODELS[key] = (proc, model.to(device).eval())
    return _MODELS[key]


def _autocast(dev, bf16):
    import contextlib

    import torch

    if bf16 and dev in ("cpu", "cuda"):
        return torch.autocast(dev, dtype=torch.bfloat16)
    return contextlib.nullcontext()


def detect_gdino(images, prompt="insect", box_threshold=0.3, text_threshold=0.25,
                 model_id="IDEA-Research/grounding-dino-tiny", device=None, shortest_edge=800,
                 longest_edge=1333, tile=None, tile_overlap=0.2, nms_iou=0.5, max_box_frac=0.5,
                 min_box_px=4, bf16=False, cache_dir=None):
    """Grounding DINO detection with a text prompt.

    images: list of file paths or RGB arrays. prompt: phrases separated by
    commas or full stops; the class column is the index of the matched
    phrase (0-based). tile: optional tile size (pixels) for small insects in
    large frames; tiles overlap by tile_overlap and are merged by NMS.
    Boxes larger than max_box_frac of the image are removed. bf16 runs the
    model in bfloat16 (faster on CPUs with AMX or AVX-512 BF16 and on GPUs).
    """
    import torch

    dev = select_device(device)
    proc, model = _load_gdino(model_id, dev, cache_dir)
    ps = phrases(prompt)
    text = ". ".join(ps) + "."
    out = []
    for x in images:
        img = _read_image(x)
        h, w = img.shape[:2]
        parts = []
        for (x0, y0, tw, th) in _tiles(w, h, tile, tile_overlap):
            sub = img[y0:y0 + th, x0:x0 + tw]
            inputs = proc(images=sub, text=text, return_tensors="pt",
                          size={"shortest_edge": int(shortest_edge), "longest_edge": int(longest_edge)}).to(dev)
            with torch.inference_mode(), _autocast(dev, bf16):
                o = model(**inputs)
            o.logits = o.logits.float()
            o.pred_boxes = o.pred_boxes.float()
            r = proc.post_process_grounded_object_detection(
                o, inputs["input_ids"], threshold=float(box_threshold), text_threshold=float(text_threshold),
                target_sizes=[(th, tw)])[0]
            boxes = r["boxes"].detach().cpu().numpy().reshape(-1, 4)
            if not len(boxes):
                continue
            labels = r.get("text_labels", r.get("labels"))
            cls = np.array([_phrase_index(lb, ps) for lb in labels], dtype=float)
            boxes[:, [0, 2]] += x0
            boxes[:, [1, 3]] += y0
            parts.append(np.column_stack([boxes, r["scores"].detach().cpu().numpy(), cls]))
        det = np.vstack(parts) if parts else np.zeros((0, 6))
        det = _filter_boxes(det, w, h, max_box_frac, min_box_px)
        out.append(_nms(det, nms_iou).astype(float))
    return out


def detect_owl(images, prompt="insect", threshold=0.15, model_id="google/owlv2-base-patch16-ensemble",
               device=None, nms_iou=0.5, max_box_frac=0.5, min_box_px=4, cache_dir=None):
    """OWL-ViT or OWLv2 open-vocabulary detection; see detect_gdino for the output.

    OWLv2 models (model ids containing 'owlv2') pad the image to a square;
    OWL-ViT models resize it to a square.
    """
    import torch

    v2 = "owlv2" in str(model_id).lower()
    dev = select_device(device)
    key = ("owl", model_id, dev)
    if key not in _MODELS:
        if v2:
            from transformers import Owlv2ForObjectDetection as M, Owlv2Processor as P
        else:
            from transformers import OwlViTForObjectDetection as M, OwlViTProcessor as P
        _MODELS[key] = (P.from_pretrained(model_id, cache_dir=cache_dir),
                        M.from_pretrained(model_id, cache_dir=cache_dir, use_safetensors=True).to(dev).eval())
    proc, model = _MODELS[key]
    ps = phrases(prompt)
    out = []
    for x in images:
        img = _read_image(x)
        h, w = img.shape[:2]
        inputs = proc(text=[ps], images=img, return_tensors="pt").to(dev)
        with torch.inference_mode():
            o = model(**inputs)
        target = [[max(h, w), max(h, w)]] if v2 else [[h, w]]
        post = getattr(proc, "post_process_object_detection", None) or proc.post_process_grounded_object_detection
        r = post(o, threshold=float(threshold), target_sizes=torch.tensor(target))[0]
        boxes = r["boxes"].detach().cpu().numpy().reshape(-1, 4)
        boxes[:, [0, 2]] = boxes[:, [0, 2]].clip(0, w)
        boxes[:, [1, 3]] = boxes[:, [1, 3]].clip(0, h)
        det = np.column_stack([boxes, r["scores"].detach().cpu().numpy(), r["labels"].detach().cpu().numpy()]) \
            if len(boxes) else np.zeros((0, 6))
        det = _filter_boxes(det, w, h, max_box_frac, min_box_px)
        out.append(_nms(det, nms_iou).astype(float))
    return out


def detect_owlv2(images, prompt="insect", threshold=0.15, model_id="google/owlv2-base-patch16-ensemble", **kw):
    """Alias of detect_owl kept for compatibility."""
    return detect_owl(images, prompt=prompt, threshold=threshold, model_id=model_id, **kw)


def detect_yolo(images, model, conf=0.25, iou=0.7, imgsz=640, device=None, classes=None, batch=8):
    """Detection with an Ultralytics model (.pt, .onnx or any exported format)."""
    from ultralytics import YOLO

    dev = select_device(device)
    key = ("yolo", str(model))
    if key not in _MODELS:
        _MODELS[key] = YOLO(str(model), task="detect")
    m = _MODELS[key]
    out = []
    imgs = [_read_image(x)[..., ::-1] for x in images]  # Ultralytics expects BGR arrays
    for i in range(0, len(imgs), int(batch)):
        res = m.predict(imgs[i:i + int(batch)], conf=float(conf), iou=float(iou), imgsz=int(imgsz),
                        device=None if dev == "cpu" and str(model).endswith(".onnx") else dev,
                        classes=None if classes is None else [int(c) for c in np.atleast_1d(classes)], verbose=False)
        for r in res:
            b = r.boxes
            if b is None or len(b) == 0:
                out.append(np.zeros((0, 6)))
            else:
                out.append(np.column_stack([b.xyxy.cpu().numpy(), b.conf.cpu().numpy(), b.cls.cpu().numpy()]).astype(float))
    return out


def _load_sam2(model_id, device, cache_dir):
    key = ("sam2", model_id, device)
    if key not in _MODELS:
        from transformers import Sam2Model, Sam2Processor

        proc = Sam2Processor.from_pretrained(model_id, cache_dir=cache_dir)
        model = Sam2Model.from_pretrained(model_id, cache_dir=cache_dir, use_safetensors=True).to(device).eval()
        _MODELS[key] = (proc, model)
    return _MODELS[key]


def _mask_properties(m):
    """Area, centroid, axis lengths and orientation of a binary mask."""
    ys, xs = np.nonzero(m)
    n = len(xs)
    if n == 0:
        return [0.0] + [np.nan] * 9
    cx, cy = xs.mean(), ys.mean()
    cov = np.cov(np.vstack([xs - cx, ys - cy])) if n > 2 else np.zeros((2, 2))
    ev, evec = np.linalg.eigh(cov)
    ev = np.clip(ev, 0, None)
    major, minor = 4 * np.sqrt(ev[1]), 4 * np.sqrt(ev[0])
    angle = float(np.degrees(np.arctan2(evec[1, 1], evec[0, 1])))
    return [float(n), float(cx + 0.5), float(cy + 0.5), float(major), float(minor), angle,
            float(xs.min()), float(ys.min()), float(xs.max() + 1), float(ys.max() + 1)]


def _polygon(m, eps=1.0):
    import cv2

    cs, _ = cv2.findContours(m.astype(np.uint8), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    if not cs:
        return np.zeros((0, 2))
    c = max(cs, key=cv2.contourArea)
    c = cv2.approxPolyDP(c, float(eps), True)
    return c.reshape(-1, 2).astype(float)


def segment_sam2(image, boxes, model_id="facebook/sam2.1-hiera-tiny", device=None, polygons=False,
                 cache_dir=None, max_boxes=64):
    """Segment each box in one image with SAM 2.1.

    Returns a dict: 'props' (N x 10: area, cx, cy, major, minor, angle,
    x1, y1, x2, y2 of the mask) and, when polygons is True, a list of
    N x 2 polygon arrays (largest contour of each mask).
    """
    import torch

    boxes = np.asarray(boxes, dtype=float).reshape(-1, 4)
    res = {"props": np.zeros((0, 10)), "polygons": []}
    if not len(boxes):
        return res
    dev = select_device(device)
    proc, model = _load_sam2(model_id, dev, cache_dir)
    img = _read_image(image)
    props, polys = [], []
    for i in range(0, len(boxes), int(max_boxes)):
        bb = boxes[i:i + int(max_boxes)]
        inputs = proc(images=img, input_boxes=[bb.tolist()], return_tensors="pt").to(dev)
        with torch.inference_mode():
            o = model(**inputs, multimask_output=False)
        masks = proc.post_process_masks(o.pred_masks.cpu(), inputs["original_sizes"].cpu())[0]
        masks = masks.numpy().reshape(len(bb), -1, img.shape[0], img.shape[1])[:, 0] > 0
        for m in masks:
            props.append(_mask_properties(m))
            if polygons:
                polys.append(_polygon(m))
    res["props"] = np.asarray(props, dtype=float)
    res["polygons"] = polys
    return res


BOXMOT_TRACKERS = ("bytetrack", "ocsort", "botsort", "boosttrack", "sfsort", "hybridsort", "occluboost",
                   "strongsort", "deepocsort")
_BOXMOT_CLASSES = {"bytetrack": "ByteTrack", "ocsort": "OcSort", "botsort": "BotSort", "boosttrack": "BoostTrack",
                   "sfsort": "SFSORT", "hybridsort": "HybridSort", "occluboost": "OccluBoost",
                   "strongsort": "StrongSort", "deepocsort": "DeepOcSort"}
_NEEDS_REID = ("strongsort", "deepocsort")


def track_boxmot(dets, frames=None, method="bytetrack", frame_size=None, params=None):
    """Track detections with a BoxMOT tracker.

    dets: list (one element per frame) of N x 6 arrays (x1, y1, x2, y2,
    score, class). frames: optional list of RGB frames (or paths) for trackers
    that use image content (camera motion compensation, appearance); when
    absent a blank image of frame_size (height, width) is passed, appearance
    features and camera motion compensation are switched off, which suits
    tracking from a fixed camera. Returns an M x 9 array: frame index
    (0-based), track id, x1, y1, x2, y2, score, class, detection index within
    the frame.
    """
    import logging

    import boxmot

    for name in [n for n in logging.root.manager.loggerDict if n.lower().startswith("boxmot")] + ["boxmot"]:
        logging.getLogger(name).setLevel(logging.WARNING)
    params = dict(params or {})
    method = method.lower()
    if method not in _BOXMOT_CLASSES:
        raise ValueError(f"unknown tracker '{method}'")
    cls = getattr(boxmot, _BOXMOT_CLASSES[method])
    if method in _NEEDS_REID and not params.get("reid_weights"):
        raise ValueError(f"tracker '{method}' needs re-identification weights (params$reid_weights)")
    if method in ("botsort", "boosttrack", "hybridsort", "occluboost"):
        params.setdefault("use_embeddings", False)
    if method == "hybridsort":
        params.setdefault("with_longterm_reid", False)
    if frames is None and method in ("botsort", "boosttrack"):
        params.setdefault("use_cmc", False)
    if frames is None and method == "hybridsort":
        params.setdefault("cmc_method", None)
    if method in ("botsort", "boosttrack", "hybridsort", "strongsort", "deepocsort"):
        params.setdefault("device", "cpu")
        params.setdefault("half", False)
    tracker = cls(**params)
    if frame_size is None and frames is None:
        raise ValueError("frame_size or frames is needed")
    blank = None if frames is not None else np.zeros((int(frame_size[0]), int(frame_size[1]), 3), np.uint8)
    rows = []
    for f, d in enumerate(dets):
        d = np.asarray(d, dtype=float).reshape(-1, 6)
        img = _read_image(frames[f])[..., ::-1].copy() if frames is not None else blank
        tr = tracker.update(d.astype(np.float32), img)
        tr = np.asarray(tr, dtype=float).reshape(-1, tr.shape[1] if np.ndim(tr) == 2 and len(tr) else 8)
        for t in tr:
            rows.append([f, t[4], t[0], t[1], t[2], t[3], t[5], t[6], t[7] if len(t) > 7 else -1])
    return np.asarray(rows, dtype=float).reshape(-1, 9)
