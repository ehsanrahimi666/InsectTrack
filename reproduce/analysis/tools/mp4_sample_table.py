"""Writes the presentation time of every video frame of an MP4 file.

Reads the sample tables (stts, stss) of the video track from the 'moov' box,
so the file is not decoded. Usage: mp4_sample_table.py <video.mp4> <out.csv>
"""
import os
import struct
import sys

fn, out = sys.argv[1], sys.argv[2]
f = open(fn, "rb")
size = os.path.getsize(fn)


def boxes(pos, end):
    while pos < end:
        f.seek(pos)
        h = f.read(8)
        if len(h) < 8:
            break
        sz, typ = struct.unpack(">I4s", h)
        hdr = 8
        if sz == 1:
            sz = struct.unpack(">Q", f.read(8))[0]
            hdr = 16
        elif sz == 0:
            sz = end - pos
        yield typ.decode("latin1"), pos, pos + hdr, pos + sz
        pos += sz


def find(pos, end, path):
    for typ, s, b, e in boxes(pos, end):
        if typ == path[0]:
            if len(path) == 1:
                return (b, e)
            r = find(b, e, path[1:])
            if r:
                return r
    return None


moov = [(b, e) for t, s, b, e in boxes(0, size) if t == "moov"][0]
for t, s, b, e in boxes(*moov):
    if t != "trak":
        continue
    hd = find(b, e, ["mdia", "hdlr"])
    f.seek(hd[0] + 8)
    if f.read(4) != b"vide":
        continue
    md = find(b, e, ["mdia", "mdhd"])
    f.seek(md[0])
    v = f.read(1)[0]
    f.seek(md[0] + (20 if v == 1 else 12))
    ts = struct.unpack(">I", f.read(4))[0]
    stbl = find(b, e, ["mdia", "minf", "stbl"])
    st = find(*stbl, ["stts"])
    f.seek(st[0] + 4)
    n = struct.unpack(">I", f.read(4))[0]
    ent = [struct.unpack(">II", f.read(8)) for _ in range(n)]
    ss = find(*stbl, ["stss"])
    f.seek(ss[0] + 4)
    m = struct.unpack(">I", f.read(4))[0]
    sync = set(struct.unpack(">%dI" % m, f.read(4 * m)))
    if find(*stbl, ["ctts"]) is not None:
        sys.exit("composition offsets (B-frames) present; presentation order differs from decoding order")
    t = 0
    rows = []
    for c, d in ent:
        for _ in range(c):
            rows.append(t)
            t += d
    with open(out, "w") as o:
        o.write("frame,pts_sec,keyframe\n")
        for i, p in enumerate(rows):
            o.write(f"{i},{p / ts:.6f},{int((i + 1) in sync)}\n")
    break
