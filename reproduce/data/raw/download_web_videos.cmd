@echo off
rem Downloads the example videos listed in web_videos\video_sources.csv from
rem Wikimedia Commons and checks them against web_videos\SHA256SUMS.txt.
rem Double-click to run; requires Windows 10 or later (curl.exe, PowerShell).
cd /d "%~dp0"
powershell -NoProfile -ExecutionPolicy Bypass -Command ^
  "$rows = Import-Csv 'web_videos\video_sources.csv';" ^
  "$sums = @{}; Get-Content 'web_videos\SHA256SUMS.txt' | ForEach-Object { $p = $_ -split '\s+', 2; $sums[$p[1].Trim()] = $p[0] };" ^
  "foreach ($r in $rows) {" ^
  "  $dest = Join-Path 'web_videos' $r.file;" ^
  "  $ok = (Test-Path $dest) -and ((Get-FileHash $dest -Algorithm SHA256).Hash.ToLower() -eq $sums[$r.file]);" ^
  "  if (-not $ok) { Write-Host ('Downloading ' + $r.file); & curl.exe -sS -L --retry 5 -A 'InsectTrack-feasibility/0.1 (academic research script)' -o $dest $r.url }" ^
  "  $h = (Get-FileHash $dest -Algorithm SHA256).Hash.ToLower();" ^
  "  if ($h -eq $sums[$r.file]) { Write-Host ('OK  ' + $r.file) } else { Write-Host ('CHECKSUM MISMATCH  ' + $r.file) }" ^
  "}"
pause
