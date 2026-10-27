"""Checks reference DOIs against Crossref or DataCite and writes the records."""
import csv, json, sys, time, urllib.parse
import requests

dois = [l.strip() for l in open(sys.argv[1]) if l.strip() and not l.startswith("#")]
rows = []
for doi in dois:
    rec = {"doi": doi, "agency": "", "ok": False, "authors": "", "year": "", "title": "", "container": "",
           "volume": "", "issue": "", "pages": "", "type": ""}
    r = requests.get("https://api.crossref.org/works/" + urllib.parse.quote(doi), timeout=60,
                     headers={"User-Agent": "reference-check/0.1"})
    if r.status_code == 200:
        m = r.json()["message"]
        rec.update(agency="crossref", ok=True, type=m.get("type", ""),
                   authors="; ".join(f"{a.get('family','')}, {a.get('given','')}".strip(", ") for a in m.get("author", [])),
                   year=(m.get("issued", {}).get("date-parts", [[None]])[0][0]),
                   title=(m.get("title") or [""])[0], container=(m.get("container-title") or [""])[0],
                   volume=m.get("volume", ""), issue=m.get("issue", ""),
                   pages=m.get("page", "") or m.get("article-number", ""))
    else:
        r = requests.get("https://api.datacite.org/dois/" + urllib.parse.quote(doi), timeout=60)
        if r.status_code == 200:
            a = r.json()["data"]["attributes"]
            rec.update(agency="datacite", ok=True, type=a.get("types", {}).get("resourceTypeGeneral", ""),
                       authors="; ".join(c.get("name", "") for c in a.get("creators", [])),
                       year=a.get("publicationYear", ""), title=(a.get("titles") or [{}])[0].get("title", ""),
                       container=a.get("publisher", "") if isinstance(a.get("publisher"), str) else a.get("publisher", {}).get("name", ""))
    rows.append(rec)
    time.sleep(0.3)
with open(sys.argv[2], "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
    w.writeheader(); w.writerows(rows)
for r in rows:
    print(("OK " if r["ok"] else "FAIL ") + f'{r["doi"]} | {str(r["authors"])[:40]} | {r["year"]} | {r["title"][:80]} | {r["container"][:40]}')
