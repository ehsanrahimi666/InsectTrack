"""Figures of the InsectTrack paper, drawn from the saved outputs of the
benchmarks (output/benchmark) and the demonstration videos (output/demo).
Figures are written to output/paper/figures as PNG (600 dpi) and PDF.

Usage: python3 analysis/21_paper_figures.py [figure names, e.g. fig1 fig4]
"""

import csv
import json
import re
import subprocess
import sys
import tempfile
from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.patches import FancyArrowPatch, FancyBboxPatch, Polygon, Rectangle
from PIL import Image

ROOT = Path(__file__).resolve().parents[1]
DEMO = ROOT / "output" / "demo"
BENCH = ROOT / "output" / "benchmark"
OUT = ROOT / "output" / "paper" / "figures"
OUT.mkdir(parents=True, exist_ok=True)

mpl.rcParams.update({
    "font.family": "Liberation Sans", "font.size": 7.5, "axes.titlesize": 8, "axes.labelsize": 7.5,
    "xtick.labelsize": 7, "ytick.labelsize": 7, "legend.fontsize": 7, "axes.linewidth": 0.6,
    "xtick.major.width": 0.6, "ytick.major.width": 0.6, "xtick.major.size": 2.5, "ytick.major.size": 2.5,
    "axes.spines.top": False, "axes.spines.right": False, "figure.facecolor": "white",
    "savefig.facecolor": "white", "pdf.fonttype": 42, "legend.frameon": False,
    "mathtext.fontset": "custom", "mathtext.rm": "Liberation Sans", "mathtext.it": "Liberation Sans:italic",
    "mathtext.bf": "Liberation Sans:bold", "mathtext.bfit": "Liberation Sans:italic:bold", "mathtext.cal": "Liberation Sans",
})
GENERA = ("Melipona", "Delosperma")


def ital(s, bold=False):
    """Genus names in italics (bold italics in bold text) within a label."""
    cmd = r"\mathbfit" if bold else r"\mathit"
    for sp in ("M. capixaba", "T. carbonaria"):
        s = s.replace(sp, f"${cmd}{{{sp.replace(' ', '\\ ')}}}$")
    for g in GENERA:
        rep = f"${cmd}{{{g}}}$"
        s = re.sub(rf"(?<![{{\w]){g}", lambda m: rep, s)
    return s
MM = 1 / 25.4
FULL = 174 * MM
SCALED = "#d95f02"
GREY = "#6b6b6b"

# one colour per detection input, used in every figure
COL = {
    "ground_truth": "#222222",          # annotated boxes
    "yolo_melipona": "#1f78b4",         # YOLO26n trained on M. capixaba
    "yolo_bee": "#8ec3e6",              # YOLO26n trained on bees on flowers
    "gdino_tiny_800": "#6a3d9a", "gdino_tiny_512": "#9e7cc1", "gdino_base_800": "#3f1d6e",
    "gdino_tiny_800_insect": "#c2a5cf", "owlv2_base": "#e66101", "owlvit_b32": "#fdb863",
    "yolo_bee_1280": "#8ec3e6", "motion": "#8c510a",
}
LAB = {
    "ground_truth": "Annotated boxes", "yolo_melipona": "YOLO26n trained on M. capixaba",
    "yolo_bee": "YOLO26n trained on bees on flowers", "gdino_tiny_512": "Grounding DINO tiny (zero-shot)",
    "gdino_tiny_800": "Grounding DINO tiny, 800 px", "gdino_base_800": "Grounding DINO base, 800 px",
    "gdino_tiny_800_insect": "Grounding DINO tiny, prompt ‘insect’",
    "owlv2_base": "OWLv2 base", "owlvit_b32": "OWL-ViT base/32", "yolo_bee_1280": "YOLO26n trained on bees on flowers",
}
LAB = {k: ital(v) for k, v in LAB.items()}
TRACKERS = ["native_hungarian", "native_sort", "native_bytetrack", "native_bytetrack_stitch", "boxmot_bytetrack",
            "boxmot_ocsort", "boxmot_botsort", "boxmot_boosttrack", "boxmot_sfsort", "boxmot_hybridsort"]
TLAB = {"native_hungarian": "Hungarian (R)", "native_sort": "SORT (R)", "native_bytetrack": "ByteTrack (R)",
        "native_bytetrack_stitch": "ByteTrack + joining (R)", "boxmot_bytetrack": "ByteTrack (BoxMOT)",
        "boxmot_ocsort": "OC-SORT", "boxmot_botsort": "BoT-SORT", "boxmot_boosttrack": "BoostTrack",
        "boxmot_sfsort": "SFSORT", "boxmot_hybridsort": "Hybrid-SORT"}


def save(fig, name):
    """PNG at 600 dpi in RGB (no alpha channel) and vector PDF."""
    fig.savefig(OUT / f"{name}.png", dpi=600)
    Image.open(OUT / f"{name}.png").convert("RGB").save(OUT / f"{name}.png", dpi=(600, 600))
    fig.savefig(OUT / f"{name}.pdf")
    plt.close(fig)
    print("written", name)


def title(ax, letter, text, sub=None, pad=0.02, sub_h=0.09, size=8):
    """Bold panel letter and title above the panel, optional grey subtitle between them and the panel."""
    y = 1 + pad + (sub_h if sub else 0)
    ax.text(0, y, f"{letter}  {ital(text, bold=True)}", transform=ax.transAxes, ha="left", va="bottom", fontsize=size,
            fontweight="bold")
    if sub:
        ax.text(0, 1 + pad, ital(sub), transform=ax.transAxes, ha="left", va="bottom", fontsize=6.5, color=GREY)


def frame_of_video(video, index):
    """Frame number index (0-based) of a video as an RGB array."""
    with tempfile.TemporaryDirectory() as d:
        out = Path(d) / "f.png"
        subprocess.run(["ffmpeg", "-v", "error", "-y", "-i", str(video), "-vf", f"select=eq(n\\,{index})",
                        "-frames:v", "1", str(out)], check=True)
        return np.asarray(Image.open(out).convert("RGB"))


def heat_grid(path):
    h = pd.read_csv(path)
    xs, ys = np.sort(h.x.unique()), np.sort(h.y.unique())
    z = np.zeros((len(ys), len(xs)))
    hot = np.zeros_like(z, dtype=bool)
    iy, ix = np.searchsorted(ys, h.y), np.searchsorted(xs, h.x)
    z[iy, ix] = h.insect_seconds
    hot[iy, ix] = h.hot.astype(str).str.upper() == "TRUE"
    cell = xs[1] - xs[0]
    return z, hot, cell


def show_heat(ax, img, z, hot, cell, vmax=None):
    grey = np.dot(img[..., :3].astype(float), [0.299, 0.587, 0.114])
    ax.imshow(grey, cmap="gray", vmin=0, vmax=255, alpha=0.9)
    H, W = img.shape[:2]
    vmax = vmax or z.max()
    zm = np.ma.masked_where(z <= 0.02 * z.max(), z)
    im = ax.imshow(zm, cmap="YlOrRd", extent=(0, z.shape[1] * cell, z.shape[0] * cell, 0), alpha=0.75,
                   vmin=0, vmax=vmax, interpolation="nearest")
    ax.contour(np.arange(z.shape[1]) * cell + cell / 2, np.arange(z.shape[0]) * cell + cell / 2, hot.astype(float),
               levels=[0.5], colors="#08306b", linewidths=0.7)
    ax.set_xlim(0, W); ax.set_ylim(H, 0); ax.axis("off")
    return im


# Figure 1: workflow -----------------------------------------------------------------------------
def fig1():
    d = DEMO / "hive_flight"
    st = json.load(open(d / "settings.json"))
    k = 50                                             # middle analysed frame, saved as frame_middle.png
    raw = np.asarray(Image.open(d / "frame_middle.png").convert("RGB"))
    det = pd.read_csv(d / "detections.csv")
    det = det[det.frame == k]
    trk = frame_of_video(next(d.glob("*_tracked.mp4")), k - 1)
    z, hot, cell = heat_grid(d / "heatmap.csv")
    cx0, cy0, cw = 400, 40, 820                        # same 16:9 crop in every panel
    ch = cw * 9 / 16
    fig = plt.figure(figsize=(FULL, 104 * MM))
    W, H = FULL, 104 * MM
    w = 0.237; h = w * W / H * 9 / 16; gap = (1 - 4 * w - 0.01) / 3; top = 0.895
    axs = [fig.add_axes([0.005 + i * (w + gap), top - h, w, h]) for i in range(4)]
    for ax, img in zip(axs[:3], (raw, raw, trk)):
        ax.imshow(img); ax.set_xlim(cx0, cx0 + cw); ax.set_ylim(cy0 + ch, cy0); ax.axis("off")
    for _, r in det.iterrows():
        axs[1].add_patch(Rectangle((r.x0, r.y0), r.x1 - r.x0, r.y1 - r.y0, fill=False, lw=0.7, ec="#ffd400"))
    show_heat(axs[3], raw, z, hot, cell)
    axs[3].set_xlim(cx0, cx0 + cw); axs[3].set_ylim(cy0 + ch, cy0)
    title(axs[0], "a", "Video frame", "any FFmpeg format or image folder")
    title(axs[1], "b", "Detections", f"Grounding DINO, prompt \u2018{st['prompt']}\u2019")
    title(axs[2], "c", "Tracks", "own colour and identity per bee")
    title(axs[3], "d", "Activity map", "insect-seconds per cell; hot spots")

    ax = fig.add_axes([0, 0, 1, top - h - 0.06])
    ax.set_xlim(0, 100); ax.set_ylim(0, 50); ax.axis("off")
    stages = [
        ("Input", "#e8f1fa", "#1f78b4", ["Videos in any format: phones,", "camera traps, high-speed cameras",
                                         "Folders of images",
                                         "Rotation, frame rate, time window"]),
        ("Detection", "#f1ebf7", "#6a3d9a", ["Trained YOLO models", "Grounding DINO, OWLv2:",
                                             "  text prompt, tiles", "Background subtraction",
                                             "SAM 2.1 outlines: body size"]),
        ("Tracking", "#fdf0e3", "#e66101", ["Hungarian, SORT, ByteTrack (R)", "BoxMOT: OC-SORT, BoT-SORT,",
                                            "  BoostTrack, SFSORT,", "  Hybrid-SORT, OccluBoost,",
                                            "  StrongSORT, Deep OC-SORT", "Joining of track fragments"]),
        ("Outputs", "#eaf5ec", "#238b45", ["Tracked video and report", "Activity, heat maps, hot spots",
                                           "Visits to flowers or regions", "Entrance in/out counts",
                                           "Line crossings, interactions"]),
    ]
    bw, bh, y0 = 22.6, 37, 6.5
    xs = [1 + i * 24.8 for i in range(4)]
    for (name, fill, edge, lines), x in zip(stages, xs):
        ax.add_patch(FancyBboxPatch((x, y0), bw, bh, boxstyle="round,pad=0.3,rounding_size=1.5", fc=fill, ec=edge,
                                    lw=0.8))
        ax.text(x + bw / 2, y0 + bh - 3.6, name, ha="center", va="center", fontsize=8.5, fontweight="bold", color=edge)
        for j, ln in enumerate(lines):
            ax.text(x + 1.0, y0 + bh - 9.4 - j * 4.9, ln, ha="left", va="center", fontsize=6.7)
    for x in xs[:-1]:
        ax.add_patch(FancyArrowPatch((x + bw + 0.35, y0 + bh / 2), (x + 24.8 - 0.35, y0 + bh / 2),
                                     arrowstyle="-|>", mutation_scale=8, lw=0.9, color="#444444"))
    ax.text(50, 47.3, "it_track_video(): one call from a video to tables, figures and an annotated video",
            ha="center", va="center", fontsize=7.3, fontstyle="italic", color="#333333")
    ax.text(50, 2.2, "Evaluation against annotations: precision, recall, F1, CLEAR MOT, IDF1, HOTA;  "
            "pre-labelled frames for training detectors", ha="center", va="center", fontsize=6.7, color=GREY)
    save(fig, "fig1_workflow")


# Figure 2: demonstration videos -----------------------------------------------------------------
GALLERY = [
    ("hive_entrance", "Apis mellifera", "hive entrance hole"),
    ("hive_fanning", "Apis mellifera", "workers fanning on a landing board"),
    ("hive_flight", "Apis mellifera", "flight at a hive with a mesh guard"),
    ("melipona", "Melipona capixaba", "stingless bees at a hive entrance"),
    ("delosperma", "Apis mellifera", "visits to ice plant flowers (detail)"),
    ("bumblebees_thistle", "Bombus", "bumblebees on a thistle head"),
    ("paper_wasps", "Polistes snelleni", "paper wasps on their nest"),
    ("hornets_shrub", "Vespa ducalis", "hornet foraging on a shrub"),
    ("yellowjacket_honeybee", "Vespula, Apis mellifera", "yellowjacket robbing a honeybee"),
    ("butterfly_marigold", "Aglais urticae", "butterfly feeding on marigolds"),
    ("leafcutter_ants_gdino", "Atta", "leafcutter ants carrying petals"),
    ("drosophila_arena_gdino", "Drosophila", "flies in a courtship arena"),
]
DET_COL = {"gdino": "#6a3d9a", "yolo": "#1f78b4", "motion": "#8c510a"}
DET_LAB = {"gdino": "Grounding DINO", "yolo": "trained YOLO26n", "motion": "background subtraction"}


ROMAN = {"sp.", "spp.", "small", "wasps", "Bees", "bees"}


def taxon_words(name):
    """Words of a taxon label with their style: genus and species names in italics; 'sp.', 'spp.',
    family names and common names in roman. A genus repeated in a list is abbreviated (Vespa x, V. y)."""
    words, seen = [], set()
    for part in name.split(", "):
        w = part.split(" ")
        if len(w) > 1 and w[0] in seen and w[1] not in ROMAN:
            w[0] = w[0][0] + "."
        seen.add(part.split(" ")[0])
        words += w[:-1] + [w[-1] + ","]
    words[-1] = words[-1].rstrip(",")
    return [(x, not (x.rstrip(",") in ROMAN or x.rstrip(",").endswith("idae"))) for x in words]


def species_title(ax, letter, species, sub, size=8.0):
    """Panel letter and taxon name in one line above an image panel, grey subtitle below them; the font is
    reduced when the name is wider than the panel."""
    fig = ax.figure
    renderer = fig.canvas.get_renderer()
    while True:
        arts = [ax.text(0, 1.15, letter, transform=ax.transAxes, ha="left", va="bottom", fontsize=size,
                        fontweight="bold")]
        for k, (w, it) in enumerate(taxon_words(species)):
            arts.append(ax.annotate(("  " if k == 0 else " ") + w, xy=(1, 0), xycoords=arts[-1], ha="left",
                                    va="bottom", fontsize=size, fontweight="bold",
                                    fontstyle="italic" if it else "normal"))
        right = arts[-1].get_window_extent(renderer).x1
        if right <= ax.get_window_extent(renderer).x1 or size <= 6.5:
            break
        for a in arts:
            a.remove()
        size -= 0.25
    ax.text(0, 1.025, sub, transform=ax.transAxes, ha="left", va="bottom", fontsize=6.4, color=GREY)


def crop16x9(img, cx=None, cy=None, width=None):
    H, W = img.shape[:2]
    width = width or W
    height = int(round(width * 9 / 16))
    if height > H:
        height = H; width = int(round(H * 16 / 9))
    cx = W / 2 if cx is None else cx; cy = H / 2 if cy is None else cy
    x0 = int(np.clip(cx - width / 2, 0, W - width)); y0 = int(np.clip(cy - height / 2, 0, H - height))
    return img[y0:y0 + height, x0:x0 + width]


def fig2():
    g = pd.read_csv(DEMO / "demo_gallery.csv")
    nc, nr = 4, 3
    pw = 42.2; ph = pw * 9 / 16; tb = 7.0; gx = (174 - nc * pw) / (nc - 1); leg = 7.0
    Hmm = nr * (tb + ph) + (nr - 1) * 1.5 + leg
    fig = plt.figure(figsize=(FULL, Hmm * MM))
    for i, (demo, species, scene) in enumerate(GALLERY):
        r, c = divmod(i, nc)
        x = c * (pw + gx) / 174
        y = (Hmm - (r + 1) * (tb + ph) - r * 1.5) / Hmm
        ax = fig.add_axes([x, y, pw / 174, ph / Hmm])
        img = np.asarray(Image.open(DEMO / demo / "still.png").convert("RGB"))
        if demo == "delosperma":
            tr = pd.read_csv(DEMO / demo / "tracks.csv")
            n = tr.groupby("frame").size()
            z = tr[tr.frame == n.idxmax()]
            img = crop16x9(img, z.x.median(), z.y.median(), 400)
        else:
            img = crop16x9(img)
        ax.imshow(img)
        ax.set_xticks([]); ax.set_yticks([])
        det = g.loc[g.demo == demo, "detector"].iloc[0]
        for sp in ax.spines.values():
            sp.set_visible(True); sp.set_color(DET_COL[det]); sp.set_linewidth(1.2)
        species_title(ax, "abcdefghijkl"[i], species, scene)
    handles = [Rectangle((0, 0), 1, 1, fc="white", ec=DET_COL[k], lw=1.2) for k in ("yolo", "gdino")]
    fig.legend(handles, ["frame colour: trained YOLO26n", "frame colour: Grounding DINO"], loc="lower center",
               ncol=2, fontsize=7, bbox_to_anchor=(0.5, 0.0), handlelength=1.6, columnspacing=2.5)
    save(fig, "fig2_demonstrations")


# Figure 3: outputs for honeybees visiting Delosperma flowers -------------------------------------------
def track_colour(i):
    import colorsys
    return colorsys.hsv_to_rgb(((i * 137.508) % 360) / 360, 0.78, 0.92)


def fig3():
    d = DEMO / "delosperma"
    st = json.load(open(d / "settings.json"))
    bg = np.asarray(Image.open(d / "frame_middle.png").convert("RGB"))
    tr = pd.read_csv(d / "tracks.csv")
    vis = pd.read_csv(d / "visits.csv")
    vpr = pd.read_csv(d / "visits_per_roi.csv")
    act = pd.read_csv(d / "activity.csv")
    z, hot, cell = heat_grid(d / "heatmap.csv")
    scale = 2 / 3
    rois = {k: np.asarray(v, float) * scale for k, v in json.load(open(ROOT / "data/raw/delosperma/flower_zones.json")).items()}
    H, W = bg.shape[:2]
    grey = np.dot(bg[..., :3].astype(float), [0.299, 0.587, 0.114])

    fig = plt.figure(figsize=(FULL, 100 * MM))
    pw = 56.5; ph = pw * 9 / 16; Hmm = 100
    ys = (Hmm - 7 - ph) / Hmm
    axs = [fig.add_axes([i * (pw + 2.25) / 174, ys, pw / 174, ph / Hmm]) for i in range(3)]
    ax = axs[0]
    ax.imshow(grey, cmap="gray", vmin=0, vmax=255, alpha=0.75); ax.axis("off")
    for k, g in tr.groupby("track"):
        g = g.sort_values("time")
        ax.plot(g.x, g.y, lw=0.9, color=track_colour(k))
        ax.plot(g.x.iloc[-1], g.y.iloc[-1], "o", ms=1.4, color=track_colour(k))
    ax.set_xlim(0, W); ax.set_ylim(H, 0)
    title(ax, "a", "Trajectories", f"{tr.track.nunique()} tracks in {st['analysed'][1] - st['analysed'][0]} s, trained YOLO26n")
    ax = axs[1]
    ax.imshow(grey, cmap="gray", vmin=0, vmax=255); ax.axis("off")
    cmap = mpl.colormaps["YlGn"]
    vmax = max(1, vpr.visits.max())
    for _, r in vpr.iterrows():
        poly = rois[r.roi]
        if r.visits > 0:
            ax.add_patch(Polygon(poly, closed=True, fc=cmap(0.35 + 0.65 * r.visits / vmax), ec="#00441b", lw=0.4, alpha=0.95))
        else:
            ax.add_patch(Polygon(poly, closed=True, fill=False, ec="white", lw=0.35, alpha=0.8))
    ax.set_xlim(0, W); ax.set_ylim(H, 0)
    title(ax, "b", "Visits per flower", f"{int(vpr.visits.sum())} visits to {int((vpr.visits > 0).sum())} of {len(vpr)} flowers")
    for v in range(1, int(vmax) + 1):
        ax.add_patch(Rectangle((W * 0.70 + (v - 1) * W * 0.085, H * 0.90), W * 0.08, H * 0.055,
                               fc=cmap(0.35 + 0.65 * v / vmax), ec="#00441b", lw=0.4))
        ax.text(W * 0.74 + (v - 1) * W * 0.085, H * 0.927, str(v), ha="center", va="center", fontsize=5.5)
    ax.text(W * 0.69, H * 0.927, "visits", ha="right", va="center", fontsize=5.5, color="white")
    im = show_heat(axs[2], bg, z, hot, cell)
    title(axs[2], "c", "Activity map", "insect-seconds per 20-px cell; hot spots outlined")
    cax = fig.add_axes([axs[2].get_position().x0 + 0.62 * pw / 174, ys + 0.03, 0.3 * pw / 174, 0.012])
    cb = fig.colorbar(im, cax=cax, orientation="horizontal")
    cb.ax.tick_params(labelsize=5, length=1.5, pad=1); cb.outline.set_linewidth(0.3)

    # activity over time and visit durations
    ax = fig.add_axes([0.06, 0.11, 0.52, ys - 0.24])
    ax.plot(act.bin_start, act.max_count, color="#e66101", lw=1, label="maximum (MaxN)")
    ax.plot(act.bin_start, act.mean_count, color="#1f78b4", lw=1.2, label="mean")
    ax.set_xlabel("Time (s)"); ax.set_ylabel("Honeybees in view")
    ax.set_ylim(0, None); ax.set_xlim(0, act.bin_start.max() + 1)
    ax.legend(loc="upper left", ncol=2, bbox_to_anchor=(0, 1.0))
    title(ax, "d", "Bees in view per 1-s bin", None, pad=0.06)
    ax = fig.add_axes([0.69, 0.11, 0.29, ys - 0.24])
    dur = vis.duration.values
    bins = np.arange(0, np.ceil(dur.max()) + 1, 1)
    ax.hist(dur, bins=bins, color="#41ab5d", edgecolor="white", lw=0.5)
    ax.set_xlabel("Visit duration (s)"); ax.set_ylabel("Visits")
    ax.yaxis.set_major_locator(mpl.ticker.MaxNLocator(integer=True))
    ax.text(0.98, 0.95, f"n = {len(dur)} visits\nmedian {np.median(dur):.1f} s", transform=ax.transAxes,
            ha="right", va="top", fontsize=6.5)
    title(ax, "e", "Visit durations", None, pad=0.06)
    save(fig, "fig3_example_outputs")


# Figure 4: tracking benchmark -------------------------------------------------------------------------
GROUPS = [("melipona", "120fps", "M. capixaba, 120 fps"), ("melipona", "30fps", "M. capixaba, 30 fps"),
          ("native_bee", "all", "T. carbonaria, 25 fps")]
INPUTS = ["ground_truth", "yolo_melipona", "gdino_tiny_512", "yolo_bee"]


def fig4():
    p = pd.read_csv(BENCH / "tracking" / "metrics_pooled.csv")
    ref = p[(p.dataset == "melipona") & (p.fps_group == "120fps") & (p.input == "ground_truth")]
    order = ref.sort_values("hota").tracker.tolist()            # rows ranked by HOTA with annotated boxes
    fig = plt.figure(figsize=(FULL, 84 * MM))
    w = 0.17; x0 = 0.155; gap = 0.012
    axs = [fig.add_axes([x0 + i * (w + gap), 0.2, w, 0.66]) for i in range(3)]
    off = {"ground_truth": 0.21, "yolo_melipona": 0.07, "gdino_tiny_512": -0.07, "yolo_bee": -0.21}
    for ax, (ds, fg, lab) in zip(axs, GROUPS):
        z = p[(p.dataset == ds) & (p.fps_group == fg)]
        for i, t in enumerate(order):
            ax.axhline(i, color="#eeeeee", lw=0.5, zorder=0)
        for inp in INPUTS:
            q = z[z.input == inp]
            if not len(q):
                continue
            yy = [order.index(t) + off[inp] for t in q.tracker]
            ax.scatter(q.hota, yy, s=13, color=COL[inp], edgecolor="white", linewidth=0.3, zorder=3)
        ax.set_xlim(-0.02, 1.0); ax.set_ylim(-0.6, len(order) - 0.4)
        ax.set_yticks(range(len(order)))
        ax.set_yticklabels([TLAB[t] for t in order] if ax is axs[0] else [])
        ax.set_xticks([0, 0.25, 0.5, 0.75, 1]); ax.set_xticklabels(["0", "0.25", "0.5", "0.75", "1"])
        ax.set_xlabel("HOTA")
        ax.tick_params(axis="y", length=0)
        title(ax, "abc"[GROUPS.index((ds, fg, lab))], lab, None, pad=0.03, size=7.2)
    # DetA against AssA
    ax = fig.add_axes([0.745, 0.2, 0.235, 0.66])
    for inp in INPUTS:
        q = p[p.input == inp]
        mk = {"melipona": "o", "native_bee": "^"}
        for ds in ("melipona", "native_bee"):
            qq = q[q.dataset == ds]
            ax.scatter(qq.deta, qq.assa, s=12, marker=mk[ds], color=COL[inp], edgecolor="white", linewidth=0.3,
                       zorder=3)
    ax.plot([0, 1], [0, 1], ls="--", lw=0.6, color=GREY)
    ax.set_xlim(-0.02, 1.02); ax.set_ylim(-0.02, 1.02)
    ax.set_xlabel("Detection accuracy (DetA)"); ax.set_ylabel("Association accuracy (AssA)")
    ax.set_aspect("equal", adjustable="box")
    title(ax, "d", "Detection limits accuracy", None, pad=0.03)
    hs = [plt.Line2D([], [], ls="", marker="o", ms=4.5, color=COL[k]) for k in INPUTS]
    hs += [plt.Line2D([], [], ls="", marker="o", ms=4, color=GREY), plt.Line2D([], [], ls="", marker="^", ms=4, color=GREY)]
    fig.legend(hs, [LAB[k] for k in INPUTS] + [ital("M. capixaba (d)"), ital("T. carbonaria (d)")], loc="lower center", ncol=3,
               bbox_to_anchor=(0.5, 0.0), fontsize=6.8, columnspacing=1.5, handletextpad=0.3)
    save(fig, "fig4_tracking_benchmark")


# Figure 6: zero-shot detection and pre-labelling --------------------------------------------------------
PRE = ["gdino_tiny_800", "gdino_tiny_512", "gdino_base_800", "gdino_tiny_800_insect", "owlv2_base", "owlvit_b32",
       "yolo_bee_1280"]
PRIMARY = {"gdino": 0.3, "owl": 0.15, "yolo": 0.25}
DSL = {"delosperma": "Delosperma flowers", "melipona": "M. capixaba entrance",
       "native_bee": "T. carbonaria entrance"}


def primary(cfg):
    return PRIMARY["gdino" if cfg.startswith("gdino") else "owl" if cfg.startswith("owl") else "yolo"]


def fig6():
    pl = BENCH / "prelabel"
    m = pd.read_csv(pl / "prelabel_metrics_by_threshold.csv")
    tm = pd.read_csv(pl / "timing.csv").set_index("configuration")
    frames = pd.read_csv(pl / "frames.csv")
    gt = pd.read_csv(pl / "gt.csv")
    fig = plt.figure(figsize=(FULL, 124 * MM))
    Hmm = 124
    w = 0.19; y = 0.58; h = 0.33
    axs = [fig.add_axes([0.055 + i * (w + 0.045), y, w, h]) for i in range(3)]
    for ax, ds in zip(axs, DSL):
        z = m[m.dataset == ds]
        for cfg in PRE:
            q = z[z.configuration == cfg].sort_values("threshold")
            if not len(q):
                continue
            ax.plot(q.recall, q.precision.fillna(0), color=COL[cfg], lw=1.1)
            pt = q[np.isclose(q.threshold, primary(cfg))]
            ax.plot(pt.recall, pt.precision.fillna(0), "o", ms=3.5, color=COL[cfg], mec="white", mew=0.4)
        ax.set_xlim(-0.02, 1.0); ax.set_ylim(-0.02, 1.02)
        ax.set_xlabel("Recall"); ax.set_ylabel("Precision" if ds == "delosperma" else "")
        n = int(z.n_true.iloc[0]); nf = int((frames.dataset == ds).sum())
        title(ax, "abc"[list(DSL).index(ds)], DSL[ds], f"{n} annotated boxes in {nf} frames", pad=0.03)
    # F1 against run time
    ax = fig.add_axes([0.77, y, 0.215, h])
    mk = {"melipona": "o", "native_bee": "^", "delosperma": "s"}
    for cfg in PRE:
        for ds in mk:
            q = m[(m.dataset == ds) & (m.configuration == cfg) & np.isclose(m.threshold, primary(cfg))]
            ax.scatter(tm.loc[cfg, "mean_s_per_frame"], q.f1, marker=mk[ds], s=14, color=COL[cfg], edgecolor="white",
                       linewidth=0.3, zorder=3)
    ax.set_xscale("log"); ax.set_xlim(0.15, 12); ax.set_ylim(-0.03, 1)
    ax.set_xticks([0.2, 0.5, 1, 2, 5, 10]); ax.set_xticklabels(["0.2", "0.5", "1", "2", "5", "10"])
    ax.set_xlabel("CPU time per frame (s)"); ax.set_ylabel("F1 at the default threshold")
    title(ax, "d", "Accuracy and speed", "2 CPU cores", pad=0.03)
    hs = [plt.Line2D([], [], color=COL[c], lw=1.4) for c in PRE]
    hs += [plt.Line2D([], [], ls="", marker=mk[k], ms=4, color=GREY) for k in mk]
    fig.legend(hs, [LAB[c] for c in PRE] + [ital("M. capixaba entrance (d)"), ital("T. carbonaria entrance (d)"), ital("Delosperma (d)")],
               loc="center", ncol=4, bbox_to_anchor=(0.5, 0.445), fontsize=6.4, columnspacing=1.0, handlelength=1.5)
    # examples: frame with the most annotated boxes in each data set
    det = pd.read_csv(pl / "detections__gdino_tiny_800.csv")
    yolo = pd.read_csv(pl / "detections__yolo_bee_1280.csv")
    pw = 0.325
    for i, ds in enumerate(DSL):
        f = frames[frames.dataset == ds].copy()
        f["n"] = f.image_id.map(gt.groupby("image_id").size()).fillna(0)
        r = f.sort_values(["n", "image_id"], ascending=[False, True]).iloc[0]
        img = np.asarray(Image.open(ROOT / r.file).convert("RGB"))
        ax = fig.add_axes([0.005 + i * (pw + 0.01), 0.02, pw, pw * FULL / (Hmm * MM) * 9 / 16])
        ax.imshow(img); ax.set_xticks([]); ax.set_yticks([])
        g = gt[gt.image_id == r.image_id]
        dd = det[(det.image_id == r.image_id) & (det.score >= 0.3)]
        for _, b in g.iterrows():
            ax.add_patch(Rectangle((b.x1, b.y1), b.x2 - b.x1, b.y2 - b.y1, fill=False, ec="white", lw=0.8, ls="--"))
        for _, b in dd.iterrows():
            ax.add_patch(Rectangle((b.x1, b.y1), b.x2 - b.x1, b.y2 - b.y1, fill=False, ec=COL["gdino_tiny_800"], lw=0.8))
        if ds == "delosperma":
            yy = yolo[(yolo.image_id == r.image_id) & (yolo.score >= 0.25)]
            for _, b in yy.iterrows():
                ax.add_patch(Rectangle((b.x1 - 4, b.y1 - 4), b.x2 - b.x1 + 8, b.y2 - b.y1 + 8, fill=False,
                                       ec="#00b4ff", lw=0.8))
            cw = 520; ch = cw * 9 / 16
            gx, gy = (g.x1 + g.x2).values / 2, (g.y1 + g.y2).values / 2
            best = max(range(len(g)), key=lambda j: (np.sum((np.abs(gx - gx[j]) < cw / 2 - 20) &
                                                            (np.abs(gy - gy[j]) < ch / 2 - 20)), -j))
            inw = (np.abs(gx - gx[best]) < cw / 2 - 20) & (np.abs(gy - gy[best]) < ch / 2 - 20)
            cx, cy = gx[inw].mean(), gy[inw].mean()
            x0 = np.clip(cx - cw / 2, 0, img.shape[1] - cw); y0 = np.clip(cy - ch / 2, 0, img.shape[0] - ch)
            ax.set_xlim(x0, x0 + cw); ax.set_ylim(y0 + ch, y0)
        for sp in ax.spines.values():
            sp.set_visible(False)
        sub = (f"detail; {len(g)} annotated bees in the frame" if ds == "delosperma"
               else f"{len(g)} annotated, {len(dd)} Grounding DINO boxes")
        title(ax, "efg"[i], DSL[ds], sub, pad=0.02, sub_h=0.085)
    fig.text(0.5, 0.372, "Boxes: annotated (white, dashed); Grounding DINO tiny, 800 px, score \u2265 0.3 (purple); "
             "YOLO26n trained on bees on flowers (cyan, e only)", ha="center", fontsize=6.5, color=GREY)
    save(fig, "fig6_prelabelling")


# Figure 5: hive-entrance counts and frame rate -----------------------------------------------------------
def fig5():
    fr = pd.read_csv(BENCH / "frame_rate" / "frame_rate_pooled.csv")
    mv = pd.read_csv(BENCH / "frame_rate" / "frame_rate_movements.csv")
    pool = pd.read_csv(BENCH / "tracking" / "metrics_pooled.csv")
    diag = pd.read_csv(BENCH / "tracking" / "inout_diagnostic_summary.csv")
    sg = pd.read_csv(BENCH / "frame_rate" / "frame_rate_scaled_gate.csv").sort_values("fps")
    rates = sorted(fr.fps.unique())
    fig = plt.figure(figsize=(FULL, 112 * MM))
    h1 = 0.33; y1 = 0.6; w1 = 0.255
    axa = fig.add_axes([0.065, y1, w1, h1]); axb = fig.add_axes([0.395, y1, w1, h1]); axc = fig.add_axes([0.725, y1, w1, h1])
    for ax, col, ylab in ((axa, "inout_rel_error", "Relative error of in + out counts"), (axc, "hota", "HOTA")):
        for t in TRACKERS:
            q = fr[fr.tracker == t].sort_values("fps")
            ax.plot(q.fps, q[col], color="#bdbdbd", lw=0.7, zorder=1)
        med = fr.groupby("fps")[col].median()
        ax.plot(med.index, med.values, color="#222222", lw=1.6, marker="o", ms=3, zorder=3, label="median of 10 trackers")
        ax.plot(sg.fps, sg[col], color=SCALED, lw=1.1, ls="--", marker="s", ms=2.6, zorder=4)
        ax.set_xticks(rates); ax.set_xlabel("Frame rate (frames per second)"); ax.set_ylabel(ylab)
    axa.set_ylim(0, 0.9)
    axa.text(0.97, 0.97, "grey: each tracker\nblack: median", transform=axa.transAxes, ha="right", va="top", fontsize=6.3, color=GREY)
    axa.text(0.97, 0.81, "orange: Hungarian,\ngate scaled to frame rate", transform=axa.transAxes, ha="right", va="top",
             fontsize=6.3, color=SCALED)
    title(axa, "a", "Entrance counts", "annotated boxes, 5 sequences, 18 movements", pad=0.03)
    axc.set_ylim(min(0.75, np.floor(20 * min(fr.hota.min(), sg.hota.min())) / 20), 1.0)
    title(axc, "c", "Tracking accuracy", "annotated boxes, whole sequences", pad=0.03)
    # displacement of entering and leaving bees
    data = [mv[mv.fps == r].step_per_box.values for r in rates]
    bp = axb.boxplot(data, positions=range(len(rates)), widths=0.55, patch_artist=True, showfliers=False,
                     medianprops=dict(color="#222222", lw=1.2), boxprops=dict(fc="#d9ecf7", ec="#1f78b4", lw=0.7),
                     whiskerprops=dict(color="#1f78b4", lw=0.7), capprops=dict(color="#1f78b4", lw=0.7))
    rng = np.random.default_rng(1)
    for i, d in enumerate(data):
        axb.scatter(i + rng.uniform(-0.14, 0.14, len(d)), d, s=5, color="#1f78b4", alpha=0.7, lw=0, zorder=3)
    axb.axhline(1, ls="--", lw=0.7, color=GREY)
    axb.text(-0.45, 1.08, "one box size", ha="left", va="bottom", fontsize=6.3, color=GREY)
    axb.set_xticks(range(len(rates))); axb.set_xticklabels(rates)
    axb.set_xlabel("Frame rate (frames per second)"); axb.set_ylabel("Largest step between frames\n(box sizes)")
    axb.set_ylim(0, None)
    title(axb, "b", "Movement between frames", f"bees entering or leaving, {len(data[0])} movements per rate", pad=0.03)
    # counts with detections (primary benchmark)
    axd = fig.add_axes([0.245, 0.09, 0.29, 0.33])
    rows = [("ground_truth", "120fps"), ("yolo_melipona", "120fps"), ("ground_truth", "30fps"),
            ("yolo_melipona", "30fps"), ("gdino_tiny_512", "30fps")]
    for j, (inp, fg) in enumerate(rows):
        q = pool[(pool.dataset == "melipona") & (pool.fps_group == fg) & (pool.input == inp)]
        y = len(rows) - 1 - j
        axd.plot([q.inout_rel_error.min(), q.inout_rel_error.max()], [y, y], color=COL[inp], lw=2.2, alpha=0.45,
                 solid_capstyle="round")
        axd.scatter(q.inout_rel_error, [y] * len(q), s=8, color=COL[inp], zorder=3, lw=0)
        axd.scatter([q.inout_rel_error.median()], [y], s=34, marker="|", color="#000000", zorder=4)
    axd.set_yticks(range(len(rows)))
    short = {"ground_truth": "Annotated boxes", "yolo_melipona": "YOLO26n (M. capixaba)",
             "gdino_tiny_512": "Grounding DINO tiny"}
    axd.set_yticklabels([ital(f"{short[i]}, {f[:-3]} fps") for i, f in rows][::-1])
    axd.set_xlim(-0.02, 1.0); axd.set_xlabel("Relative error of in + out counts")
    axd.tick_params(axis="y", length=0)
    title(axd, "d", "Counts from detections", "M. capixaba; dots: trackers, bar: median", pad=0.03)
    # annotated movements reproduced and spurious movements of each tracker (annotated boxes)
    axe = fig.add_axes([0.665, 0.09, 0.255, 0.33])
    dg = diag[(diag.dataset == "melipona") & (diag.input == "ground_truth")].set_index(["tracker", "fps_group"])
    rec = dg.recall.unstack().loc[TRACKERS].sort_values(["120fps", "30fps"])
    spur = dg.spurious.unstack().loc[rec.index]
    yt = axe.get_yaxis_transform()
    for i, t in enumerate(rec.index):
        axe.plot([rec.loc[t, "30fps"], rec.loc[t, "120fps"]], [i, i], color="#cfcfcf", lw=1.2, zorder=1)
        axe.scatter(rec.loc[t, "120fps"], i, s=16, color="#01665e", zorder=3, lw=0)
        axe.scatter(rec.loc[t, "30fps"], i, s=16, color="#bf812d", zorder=3, lw=0)
        axe.text(1.07, i, f"{int(spur.loc[t, '120fps'])}", transform=yt, ha="center", va="center", fontsize=6.3, color="#01665e")
        axe.text(1.17, i, f"{int(spur.loc[t, '30fps'])}", transform=yt, ha="center", va="center", fontsize=6.3, color="#bf812d")
    axe.text(1.12, len(rec) - 0.35, "spurious", transform=yt, ha="center", va="bottom", fontsize=6.3, color=GREY)
    axe.set_yticks(range(len(rec))); axe.set_yticklabels([TLAB[t] for t in rec.index]); axe.tick_params(axis="y", length=0)
    axe.set_ylim(-0.6, len(rec) + 0.7)
    axe.set_xlim(-0.02, 1.02); axe.set_xlabel("Share of annotated movements reproduced")
    axe.legend([plt.Line2D([], [], ls="", marker="o", ms=4, color="#01665e"),
                plt.Line2D([], [], ls="", marker="o", ms=4, color="#bf812d")], ["120 fps", "30 fps"],
               loc="upper left", ncol=2, fontsize=6.5, handletextpad=0.2, columnspacing=1.0, borderaxespad=0.2)
    n120 = int(dg.movements.unstack().iloc[0]["120fps"]); n30 = int(dg.movements.unstack().iloc[0]["30fps"])
    title(axe, "e", "Movements reproduced", f"annotated boxes, M. capixaba; {n120} and {n30} movements", pad=0.03)
    save(fig, "fig5_entrance_counts")


# Supplementary figures ------------------------------------------------------------------------------------
def figS1():
    g = pd.read_csv(DEMO / "demo_gallery.csv")
    nc = 4; nr = int(np.ceil(len(g) / nc))
    pw = 42.2; ph = pw * 9 / 16; tb = 7.0; gx = (174 - nc * pw) / (nc - 1); leg = 7.0
    Hmm = nr * (tb + ph) + (nr - 1) * 1.5 + leg
    fig = plt.figure(figsize=(FULL, Hmm * MM))
    dl = {"gdino": "Grounding DINO", "yolo": "trained YOLO26n", "motion": "background subtraction"}
    letters = "abcdefghijklmnopqrstuvwxyz"
    for i, r in g.reset_index(drop=True).iterrows():
        rr, c = divmod(i, nc)
        ax = fig.add_axes([c * (pw + gx) / 174, (Hmm - (rr + 1) * (tb + ph) - rr * 1.5) / Hmm, pw / 174, ph / Hmm])
        img = crop16x9(np.asarray(Image.open(DEMO / r.demo / "still.png").convert("RGB")))
        ax.imshow(img); ax.set_xticks([]); ax.set_yticks([])
        for sp in ax.spines.values():
            sp.set_visible(True); sp.set_color(DET_COL[r.detector]); sp.set_linewidth(1.2)
        sub = dl[r.detector] + (" on tiles" if r.demo.endswith("_tiled") else "") + f"; {int(r.tracks)} tracks"
        species_title(ax, letters[i], r.species, sub)
    handles = [Rectangle((0, 0), 1, 1, fc="white", ec=DET_COL[k], lw=1.2) for k in ("yolo", "gdino", "motion")]
    fig.legend(handles, ["frame colour: trained YOLO26n", "frame colour: Grounding DINO", "frame colour: background subtraction"],
               loc="lower center", ncol=3, fontsize=7, bbox_to_anchor=(0.5, 0.0), handlelength=1.6, columnspacing=2)
    save(fig, "figS1_all_demonstrations")


def figS2():
    p = pd.read_csv(BENCH / "tracking" / "metrics_pooled.csv")
    ref = p[(p.dataset == "melipona") & (p.fps_group == "120fps") & (p.input == "ground_truth")]
    order = ref.sort_values("hota").tracker.tolist()
    off = {"ground_truth": 0.21, "yolo_melipona": 0.07, "gdino_tiny_512": -0.07, "yolo_bee": -0.21}
    fig = plt.figure(figsize=(FULL, 160 * MM))
    w = 0.25; x0 = 0.155; gap = 0.03
    for row, (metric, lab, lo) in enumerate((("idf1", "IDF1", 0), ("mota", "MOTA", -1))):
        for i, (ds, fg, glab) in enumerate(GROUPS):
            ax = fig.add_axes([x0 + i * (w + gap), 0.6 - row * 0.44, w, 0.32])
            z = p[(p.dataset == ds) & (p.fps_group == fg)]
            for j in range(len(order)):
                ax.axhline(j, color="#eeeeee", lw=0.5, zorder=0)
            for inp in INPUTS:
                q = z[z.input == inp]
                if not len(q):
                    continue
                yy = np.array([order.index(t) + off[inp] for t in q.tracker])
                v = q[metric].to_numpy()
                inside = v >= lo
                ax.scatter(v[inside], yy[inside], s=13, color=COL[inp], edgecolor="white", linewidth=0.3, zorder=3)
                ax.scatter(np.full((~inside).sum(), lo), yy[~inside], s=16, marker="<", color=COL[inp],
                           edgecolor="white", linewidth=0.3, zorder=3)
            ax.set_xlim(lo - 0.04, 1.03); ax.set_ylim(-0.6, len(order) - 0.4)
            ax.set_yticks(range(len(order))); ax.set_yticklabels([TLAB[t] for t in order] if i == 0 else [])
            ax.tick_params(axis="y", length=0); ax.set_xlabel(lab)
            title(ax, "abcdef"[row * 3 + i], f"{glab}: {lab}", None, pad=0.03)
    hs = [plt.Line2D([], [], ls="", marker="o", ms=4.5, color=COL[k]) for k in INPUTS]
    hs.append(plt.Line2D([], [], ls="", marker="<", ms=4.5, color=GREY))
    fig.legend(hs, [LAB[k] for k in INPUTS] + ["MOTA below −1, drawn at −1"], loc="lower center", ncol=3,
               bbox_to_anchor=(0.5, 0.0), fontsize=6.8, columnspacing=1.6)
    save(fig, "figS2_idf1_mota")


def figS3():
    s = pd.read_csv(BENCH / "tracking" / "sensitivity_bytetrack.csv")
    b = pd.read_csv(BENCH / "tracking" / "metrics_by_sequence.csv")
    b = b[(b.tracker == "native_bytetrack") & (b.dataset == "melipona") & (b.input == "yolo_melipona")].copy()
    b["variant"] = "primary"
    s = s[s.dataset == "melipona"]
    z = pd.concat([s[["fps", "variant", "hota", "idf1", "mota"]], b[["fps", "variant", "hota", "idf1", "mota"]]])
    order = ["track_thresh=0.4", "primary", "track_thresh=0.6", "track_buffer=15", "track_buffer=60",
             "match_iou=0.1", "match_iou=0.3"]
    labels = {"primary": "primary settings", "track_thresh=0.4": "detection threshold 0.4",
              "track_thresh=0.6": "detection threshold 0.6", "track_buffer=15": "track buffer 15 frames",
              "track_buffer=60": "track buffer 60 frames", "match_iou=0.1": "matching IoU 0.1",
              "match_iou=0.3": "matching IoU 0.3"}
    fig = plt.figure(figsize=(FULL, 70 * MM))
    for i, m in enumerate(("hota", "idf1", "mota")):
        ax = fig.add_axes([0.2 + i * 0.27, 0.27, 0.22, 0.6])
        for k, fps in enumerate((120, 30)):
            a = z[z.fps == fps].groupby("variant")[m].agg(["mean", "std", "count"]).loc[order]
            yy = np.arange(len(order))[::-1] + (0.12 if fps == 120 else -0.12)
            ax.errorbar(a["mean"], yy, xerr=a["std"] / np.sqrt(a["count"]), fmt="o", ms=3,
                        color="#01665e" if fps == 120 else "#bf812d", elinewidth=0.7, capsize=0)
        ax.set_yticks(np.arange(len(order))[::-1]); ax.set_yticklabels([labels[o] for o in order] if i == 0 else [])
        ax.tick_params(axis="y", length=0); ax.set_xlabel(f"Mean {m.upper()} over sequences")
        title(ax, "abc"[i], m.upper(), None, pad=0.03)
    fig.legend([plt.Line2D([], [], ls="", marker="o", ms=4, color="#01665e"),
                plt.Line2D([], [], ls="", marker="o", ms=4, color="#bf812d")], ["120 fps (7 sequences)", "30 fps (5 sequences)"],
               loc="lower center", ncol=2, bbox_to_anchor=(0.55, 0.0), fontsize=6.8)
    save(fig, "figS3_bytetrack_sensitivity")


FIGS = {"fig1": fig1, "fig2": fig2, "fig3": fig3, "fig4": fig4, "fig5": fig5, "fig6": fig6, "figS1": figS1, "figS2": figS2, "figS3": figS3}

if __name__ == "__main__":
    for name in (sys.argv[1:] or list(FIGS)):
        FIGS[name]()
