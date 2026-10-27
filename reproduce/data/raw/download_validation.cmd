@echo off
rem Downloads the validation data sets (CC BY 4.0) into the validation folder
rem and checks them against validation\SHA256SUMS.txt.
rem Double-click to run; requires Windows 10 or later (curl.exe, PowerShell).
cd /d "%~dp0validation"
call :get native_bee_tracking.zip "https://ndownloader.figshare.com/files/64894986"
call :get insect_trajectory_dataset.zip "https://ndownloader.figshare.com/files/38169609"
call :get melipona_2026_03_06_MOTA.zip "https://zenodo.org/records/22235293/files/2026_03_06_MOTA.zip?download=1"
powershell -NoProfile -Command "Get-Content SHA256SUMS.txt | ForEach-Object { $p = $_ -split '\s+', 2; $f = $p[1].Trim(); if (Test-Path $f) { $h = (Get-FileHash $f -Algorithm SHA256).Hash.ToLower(); if ($h -eq $p[0]) { 'OK  ' + $f } else { 'CHECKSUM MISMATCH  ' + $f } } else { 'MISSING  ' + $f } }"
pause
goto :eof
:get
if exist "%~1" goto :eof
echo Downloading %~1
curl.exe -sS -L --retry 5 -A "Mozilla/5.0" -o "%~1" "%~2"
goto :eof
