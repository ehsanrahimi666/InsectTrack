#!/usr/bin/env bash
# Downloads the validation data sets (CC BY 4.0) and checks them against
# validation/SHA256SUMS.txt.
set -euo pipefail
cd "$(dirname "$0")/validation"
get() { [ -s "$1" ] || curl -sS -L --retry 5 --retry-delay 10 -A "Mozilla/5.0" -o "$1" "$2"; }
get native_bee_tracking.zip       "https://ndownloader.figshare.com/files/64894986"
get insect_trajectory_dataset.zip "https://ndownloader.figshare.com/files/38169609"
get melipona_2026_03_06_MOTA.zip  "https://zenodo.org/records/22235293/files/2026_03_06_MOTA.zip?download=1"
sha256sum -c SHA256SUMS.txt
