#!/usr/bin/env bash
# Downloads the openly licensed example videos listed in web_videos/video_sources.csv
# and writes SHA-256 checksums. Run from the data/raw directory.
set -euo pipefail
cd "$(dirname "$0")"
UA="InsectTrack-feasibility/0.1 (academic research script)"
python3 - <<'PY'
import csv, subprocess, hashlib, os
rows=list(csv.DictReader(open("web_videos/video_sources.csv",encoding="utf-8")))
with open("web_videos/SHA256SUMS.txt","w") as out:
    for r in rows:
        dest=os.path.join("web_videos",r["file"])
        if not (os.path.exists(dest) and os.path.getsize(dest)==int(r["bytes"])):
            subprocess.run(["curl","-sS","-L","--retry","5","--retry-delay","5","-A","InsectTrack-feasibility/0.1 (academic research script)","-o",dest,r["url"]],check=True)
        h=hashlib.sha256()
        with open(dest,"rb") as f:
            for b in iter(lambda:f.read(1<<20),b""): h.update(b)
        size=os.path.getsize(dest)
        out.write(f"{h.hexdigest()}  {r['file']}\n")
        print(r["file"], size, "OK" if size==int(r["bytes"]) else "SIZE MISMATCH", h.hexdigest()[:16])
PY
