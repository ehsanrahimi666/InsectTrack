"""Independent implementation of HOTA (Luiten et al. 2021, Int. J. Comput.
Vis. 129: 548-578) from its published definition, applied to the tracks
written by analysis/15_xcheck_hota.R, and compared with the R values."""

import csv
from pathlib import Path

import numpy as np
from scipy.optimize import linear_sum_assignment

OD = Path(__file__).resolve().parents[1] / "output" / "benchmark" / "xcheck_hota"


def load(path, id_col):
    rows = list(csv.DictReader(open(path)))
    by_frame = {}
    for r in rows:
        by_frame.setdefault(int(float(r["frame"])), []).append(
            (r[id_col], [float(r[k]) for k in ("x0", "y0", "x1", "y1")]))
    return by_frame


def iou(a, b):
    a = np.asarray(a, float).reshape(-1, 4); b = np.asarray(b, float).reshape(-1, 4)
    x1 = np.maximum(a[:, None, 0], b[None, :, 0]); y1 = np.maximum(a[:, None, 1], b[None, :, 1])
    x2 = np.minimum(a[:, None, 2], b[None, :, 2]); y2 = np.minimum(a[:, None, 3], b[None, :, 3])
    inter = np.clip(x2 - x1, 0, None) * np.clip(y2 - y1, 0, None)
    area_a = (a[:, 2] - a[:, 0]) * (a[:, 3] - a[:, 1]); area_b = (b[:, 2] - b[:, 0]) * (b[:, 3] - b[:, 1])
    union = area_a[:, None] + area_b[None, :] - inter
    return np.where(union > 0, inter / union, 0.0)


def hota(gt, pr, alphas=np.arange(0.05, 0.96, 0.05)):
    gids = sorted({i for f in gt.values() for i, _ in f}); pids = sorted({i for f in pr.values() for i, _ in f})
    gi = {g: k for k, g in enumerate(gids)}; pi = {p: k for k, p in enumerate(pids)}
    frames = sorted(set(gt) | set(pr))
    # global alignment from soft (similarity-normalised) co-occurrence
    pot = np.zeros((len(gids), len(pids))); gc = np.zeros(len(gids)); pc = np.zeros(len(pids))
    sims = {}
    for f in frames:
        g = gt.get(f, []); p = pr.get(f, [])
        for i, _ in g: gc[gi[i]] += 1
        for i, _ in p: pc[pi[i]] += 1
        if not g or not p:
            continue
        S = iou([b for _, b in g], [b for _, b in p]); sims[f] = S
        den = S.sum(1, keepdims=True) + S.sum(0, keepdims=True) - S
        soft = np.divide(S, den, out=np.zeros_like(S), where=den > 0)
        pot[np.ix_([gi[i] for i, _ in g], [pi[i] for i, _ in p])] += soft
    with np.errstate(divide="ignore", invalid="ignore"):
        A = pot / (gc[:, None] + pc[None, :] - pot)
    A = np.nan_to_num(A)
    res = []
    for a in alphas:
        tp = fn = fp = 0; loc = 0.0; M = np.zeros_like(pot)
        for f in frames:
            g = gt.get(f, []); p = pr.get(f, [])
            if not g:
                fp += len(p); continue
            if not p:
                fn += len(g); continue
            S = sims[f]
            gix = [gi[i] for i, _ in g]; pix = [pi[i] for i, _ in p]
            r, c = linear_sum_assignment(-(A[np.ix_(gix, pix)] * S))
            ok = S[r, c] >= a - 1e-12
            n = int(ok.sum()); tp += n; fn += len(g) - n; fp += len(p) - n; loc += S[r[ok], c[ok]].sum()
            for rr, cc in zip(r[ok], c[ok]):
                M[gix[rr], pix[cc]] += 1
        assa = (M * M / np.maximum(1, gc[:, None] + pc[None, :] - M)).sum() / max(1, tp)
        deta = tp / max(1, tp + fn + fp)
        # TrackEval convention: LocA is 1 at thresholds without matches
        res.append((np.sqrt(deta * assa), deta, assa, max(1e-10, loc) / max(1e-10, tp)))
    return np.asarray(res).mean(0)


rows = list(csv.DictReader(open(OD / "hota_r.csv")))
out = []
for r in rows:
    gt = load(OD / f"{r['sequence']}__gt.csv", "id"); pr = load(OD / f"{r['sequence']}__{r['tracker']}.csv", "track")
    h = hota(gt, pr)
    out.append([r["sequence"], r["tracker"], *[round(float(r[k]), 6) for k in ("hota_r", "deta_r", "assa_r", "loca_r")],
                *[round(float(v), 6) for v in h]])
    print(out[-1], flush=True)
with open(OD / "hota_xcheck.csv", "w", newline="") as fh:
    w = csv.writer(fh)
    w.writerow(["sequence", "tracker", "hota_r", "deta_r", "assa_r", "loca_r", "hota_py", "deta_py", "assa_py", "loca_py"])
    w.writerows(out)
print("max |difference| HOTA:", max(abs(o[2] - o[6]) for o in out))
