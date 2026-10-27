"""Prints the FFmpeg select expression for the Delosperma clip from the frame map."""
import csv
import sys

K0 = 14700
rows = list(csv.DictReader(open(sys.argv[1])))
extra = [int(r["original_frame"]) - K0 for r in rows if (int(r["original_frame"]) - K0) % 3]
print("not(mod(n\\,3))" + "".join(f"+eq(n\\,{n})" for n in extra))
