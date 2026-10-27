"""Gallery of the package: one tracked frame of eight demonstration videos
(inst/extdata/gallery, with ATTRIBUTION.csv) and the gallery mosaic of the
README (man/figures/README-gallery.jpg). Only sources under CC0, public
domain or CC BY licences are used."""

import csv
from pathlib import Path

from PIL import Image

ROOT = Path(__file__).resolve().parents[1]
PKG = ROOT / "package" / "InsectTrack"
GAL = PKG / "inst" / "extdata" / "gallery"
DEMOS = ["hive_flight", "melipona", "paper_wasps", "hornets_shrub", "yellowjacket_honeybee",
         "honeybee_oregano", "leafcutter_ants_gdino", "drosophila_arena_gdino"]
LICENCE_URL = {
    "CC0": "https://creativecommons.org/publicdomain/zero/1.0/",
    "Public domain": "https://creativecommons.org/publicdomain/mark/1.0/",
    "CC BY 2.0": "https://creativecommons.org/licenses/by/2.0/",
    "CC BY 2.5": "https://creativecommons.org/licenses/by/2.5/",
    "CC BY 3.0": "https://creativecommons.org/licenses/by/3.0/",
    "CC BY 4.0": "https://creativecommons.org/licenses/by/4.0/",
}
MODIFICATION = ("frame of the video tracked with InsectTrack (trails and identity labels drawn); "
                "resized to 800 px wide and saved as JPEG")


def main():
    GAL.mkdir(parents=True, exist_ok=True)
    meta = {r["demo"]: r for r in csv.DictReader(open(ROOT / "output" / "demo" / "demo_gallery.csv",
                                                      encoding="utf-8"))}
    rows, thumbs = [], []
    for d in DEMOS:
        m = meta[d]
        if m["licence"] not in LICENCE_URL:
            raise ValueError(f"{d}: licence {m['licence']} is not CC0, public domain or CC BY")
        im = Image.open(ROOT / "output" / "demo" / d / "still.png").convert("RGB")
        im = im.resize((800, round(800 * im.height / im.width)), Image.LANCZOS)
        out = GAL / f"{d}.jpg"
        im.save(out, quality=85, optimize=True)
        thumbs.append(im)
        rows.append({"file": out.name, "species": m["species"], "scene": m["scene"], "detector": m["detector"],
                     "author": m["author"], "licence": m["licence"], "licence_url": LICENCE_URL[m["licence"]],
                     "source": m["page"], "modification": MODIFICATION})
    with open(GAL / "ATTRIBUTION.csv", "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    # README mosaic: 4 x 2 tiles of 16:9 crops
    tw, th, gap = 480, 270, 6
    mos = Image.new("RGB", (4 * tw + 3 * gap, 2 * th + gap), "white")
    for i, im in enumerate(thumbs):
        s = max(tw / im.width, th / im.height)
        t = im.resize((round(im.width * s), round(im.height * s)), Image.LANCZOS)
        x0 = (t.width - tw) // 2
        y0 = (t.height - th) // 2
        t = t.crop((x0, y0, x0 + tw, y0 + th))
        mos.paste(t, ((i % 4) * (tw + gap), (i // 4) * (th + gap)))
    (PKG / "man" / "figures").mkdir(parents=True, exist_ok=True)
    mos.save(PKG / "man" / "figures" / "README-gallery.jpg", quality=88, optimize=True)
    print(f"{len(rows)} gallery images")


if __name__ == "__main__":
    main()
