@echo off
rem Downloads the test videos listed in download_videos.html into this folder.
rem Files already present are skipped, so the script can be run again after an interruption.
setlocal
cd /d "%~dp0"
set "UA=InsectTrack-research/0.1 (academic research; https://github.com/ehsanrahimi666)"

if exist "N01_honeybee_hive_entrance.webm" (echo Present: N01_honeybee_hive_entrance.webm) else (
  echo Downloading N01_honeybee_hive_entrance.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N01_honeybee_hive_entrance.webm.part" "https://upload.wikimedia.org/wikipedia/commons/e/e6/Honeybee_hive_entrance.webm?download" && move /y "N01_honeybee_hive_entrance.webm.part" "N01_honeybee_hive_entrance.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N02_honeybee_fanning.webm" (echo Present: N02_honeybee_fanning.webm) else (
  echo Downloading N02_honeybee_fanning.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N02_honeybee_fanning.webm.part" "https://upload.wikimedia.org/wikipedia/commons/a/ab/Honey_bees_fanning_%%281%%29.webm?download" && move /y "N02_honeybee_fanning.webm.part" "N02_honeybee_fanning.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N03_honeybee_entrance_flight.webm" (echo Present: N03_honeybee_entrance_flight.webm) else (
  echo Downloading N03_honeybee_entrance_flight.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N03_honeybee_entrance_flight.webm.part" "https://upload.wikimedia.org/wikipedia/commons/5/5e/Bees_flying_in_HD_in_slow_motion.webm?download" && move /y "N03_honeybee_entrance_flight.webm.part" "N03_honeybee_entrance_flight.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N04_honeybee_hive_wall.webm" (echo Present: N04_honeybee_hive_wall.webm) else (
  echo Downloading N04_honeybee_hive_wall.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N04_honeybee_hive_wall.webm.part" "https://upload.wikimedia.org/wikipedia/commons/6/63/20240728_honey_bee_hive_wood_parcel_wm.webm?download" && move /y "N04_honeybee_hive_wall.webm.part" "N04_honeybee_hive_wall.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N05_bee_counter_in_out.7z" (echo Present: N05_bee_counter_in_out.7z) else (
  echo Downloading N05_bee_counter_in_out.7z ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N05_bee_counter_in_out.7z.part" "https://zenodo.org/records/10674964/files/splendidBC_AMBJ_videos.7z?download=1" && move /y "N05_bee_counter_in_out.7z.part" "N05_bee_counter_in_out.7z" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N06_stingless_bee_landing.avi" (echo Present: N06_stingless_bee_landing.avi) else (
  echo Downloading N06_stingless_bee_landing.avi ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N06_stingless_bee_landing.avi.part" "https://zenodo.org/records/5035796/files/M1.avi?download=1" && move /y "N06_stingless_bee_landing.avi.part" "N06_stingless_bee_landing.avi" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N07_paper_wasps_nest.webm" (echo Present: N07_paper_wasps_nest.webm) else (
  echo Downloading N07_paper_wasps_nest.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N07_paper_wasps_nest.webm.part" "https://upload.wikimedia.org/wikipedia/commons/7/7c/Japanese_paper_wasps_%%28Polistes_snelleni%%29_cooling_their_nest.webm?download" && move /y "N07_paper_wasps_nest.webm.part" "N07_paper_wasps_nest.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N08_hornet_nest.webm" (echo Present: N08_hornet_nest.webm) else (
  echo Downloading N08_hornet_nest.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N08_hornet_nest.webm.part" "https://upload.wikimedia.org/wikipedia/commons/c/c5/2022-10-06_VRHNIKA_Ribniki_pri_stari_opekarni_HYMENOPTERA_Vespa_crabro_SR%%C5%%A0EN.webm?download" && move /y "N08_hornet_nest.webm.part" "N08_hornet_nest.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N09_hornets_shrub.webm" (echo Present: N09_hornets_shrub.webm) else (
  echo Downloading N09_hornets_shrub.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N09_hornets_shrub.webm.part" "https://upload.wikimedia.org/wikipedia/commons/9/90/Black-tailed_hornet_%%28Vespa_ducalis_pulchra%%29.webm?download" && move /y "N09_hornets_shrub.webm.part" "N09_hornets_shrub.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N10_giant_hornet_tree.webm" (echo Present: N10_giant_hornet_tree.webm) else (
  echo Downloading N10_giant_hornet_tree.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N10_giant_hornet_tree.webm.part" "https://upload.wikimedia.org/wikipedia/commons/a/ab/Japanese_giant_hornet.webm?download" && move /y "N10_giant_hornet_tree.webm.part" "N10_giant_hornet_tree.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N11_yellowjacket_honeybee.mp4" (echo Present: N11_yellowjacket_honeybee.mp4) else (
  echo Downloading N11_yellowjacket_honeybee.mp4 ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N11_yellowjacket_honeybee.mp4.part" "https://zenodo.org/records/8283415/files/Yellowjacket_1.mp4?download=1" && move /y "N11_yellowjacket_honeybee.mp4.part" "N11_yellowjacket_honeybee.mp4" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N12_bumblebees_thistle.webm" (echo Present: N12_bumblebees_thistle.webm) else (
  echo Downloading N12_bumblebees_thistle.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N12_bumblebees_thistle.webm.part" "https://upload.wikimedia.org/wikipedia/commons/a/a3/Bumble_bees_%%28Bombus%%29_on_thistle.webm?download" && move /y "N12_bumblebees_thistle.webm.part" "N12_bumblebees_thistle.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N13_bees_hibiscus.webm" (echo Present: N13_bees_hibiscus.webm) else (
  echo Downloading N13_bees_hibiscus.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N13_bees_hibiscus.webm.part" "https://upload.wikimedia.org/wikipedia/commons/9/9c/Bees_on_Hibiscus_syriacus.webm?download" && move /y "N13_bees_hibiscus.webm.part" "N13_bees_hibiscus.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N14_morning_glory_visitors.webm" (echo Present: N14_morning_glory_visitors.webm) else (
  echo Downloading N14_morning_glory_visitors.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N14_morning_glory_visitors.webm.part" "https://upload.wikimedia.org/wikipedia/commons/3/32/Bee_morning_glory_visitors.webm?download" && move /y "N14_morning_glory_visitors.webm.part" "N14_morning_glory_visitors.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N15_leafcutter_bees_sunflower.webm" (echo Present: N15_leafcutter_bees_sunflower.webm) else (
  echo Downloading N15_leafcutter_bees_sunflower.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N15_leafcutter_bees_sunflower.webm.part" "https://upload.wikimedia.org/wikipedia/commons/0/0d/Leafcutting_male_bees_avoid_wasps.webm?download" && move /y "N15_leafcutter_bees_sunflower.webm.part" "N15_leafcutter_bees_sunflower.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N16_honeybees_palm_flowers.webm" (echo Present: N16_honeybees_palm_flowers.webm) else (
  echo Downloading N16_honeybees_palm_flowers.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N16_honeybees_palm_flowers.webm.part" "https://upload.wikimedia.org/wikipedia/commons/5/5c/Apis_on_Adonidia_merrillii_flowers.webm?download" && move /y "N16_honeybees_palm_flowers.webm.part" "N16_honeybees_palm_flowers.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N17_honeybee_oregano.webm" (echo Present: N17_honeybee_oregano.webm) else (
  echo Downloading N17_honeybee_oregano.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N17_honeybee_oregano.webm.part" "https://upload.wikimedia.org/wikipedia/commons/b/b4/20210623-FPAC-KLS-0001-Oregano.webm?download" && move /y "N17_honeybee_oregano.webm.part" "N17_honeybee_oregano.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N18_butterfly_marigold.webm" (echo Present: N18_butterfly_marigold.webm) else (
  echo Downloading N18_butterfly_marigold.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N18_butterfly_marigold.webm.part" "https://upload.wikimedia.org/wikipedia/commons/1/11/Small_tortoiseshell_feeding_20220915_163214.webm?download" && move /y "N18_butterfly_marigold.webm.part" "N18_butterfly_marigold.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N19_hoverfly_poppy.webm" (echo Present: N19_hoverfly_poppy.webm) else (
  echo Downloading N19_hoverfly_poppy.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N19_hoverfly_poppy.webm.part" "https://upload.wikimedia.org/wikipedia/commons/2/2a/Hoverfly.webm?download" && move /y "N19_hoverfly_poppy.webm.part" "N19_hoverfly_poppy.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N20_carpenter_bee_abelia.webm" (echo Present: N20_carpenter_bee_abelia.webm) else (
  echo Downloading N20_carpenter_bee_abelia.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N20_carpenter_bee_abelia.webm.part" "https://upload.wikimedia.org/wikipedia/commons/9/9e/Carpenter_bee_on_Abelia_flowers.webm?download" && move /y "N20_carpenter_bee_abelia.webm.part" "N20_carpenter_bee_abelia.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N21_ant_trail_kerb.webm" (echo Present: N21_ant_trail_kerb.webm) else (
  echo Downloading N21_ant_trail_kerb.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N21_ant_trail_kerb.webm.part" "https://upload.wikimedia.org/wikipedia/commons/d/d7/Ant_trail_on_concrete_kerb.webm?download" && move /y "N21_ant_trail_kerb.webm.part" "N21_ant_trail_kerb.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N22_leafcutter_ants_flowers.webm" (echo Present: N22_leafcutter_ants_flowers.webm) else (
  echo Downloading N22_leafcutter_ants_flowers.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N22_leafcutter_ants_flowers.webm.part" "https://upload.wikimedia.org/wikipedia/commons/a/a4/Leafcutters_transporting_yellow_flowers.webm?download" && move /y "N22_leafcutter_ants_flowers.webm.part" "N22_leafcutter_ants_flowers.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N23_drosophila_arena.ogv" (echo Present: N23_drosophila_arena.ogv) else (
  echo Downloading N23_drosophila_arena.ogv ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N23_drosophila_arena.ogv.part" "https://upload.wikimedia.org/wikipedia/commons/b/bd/Evolution-of-Multiple-Additive-Loci-Caused-Divergence-between-Drosophila-yakuba-and-D.-santomea-in-pone.0043888.s004.ogv?download" && move /y "N23_drosophila_arena.ogv.part" "N23_drosophila_arena.ogv" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N24_honeybee_drinking.webm" (echo Present: N24_honeybee_drinking.webm) else (
  echo Downloading N24_honeybee_drinking.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N24_honeybee_drinking.webm.part" "https://upload.wikimedia.org/wikipedia/commons/9/90/Western_honey_bee_drinking%%2C_August_2026.webm?download" && move /y "N24_honeybee_drinking.webm.part" "N24_honeybee_drinking.webm" >nul
  timeout /t 3 /nobreak >nul
)
if exist "N25_ladybird_ants.webm" (echo Present: N25_ladybird_ants.webm) else (
  echo Downloading N25_ladybird_ants.webm ...
  curl.exe -L --fail --retry 3 --retry-delay 10 -A "%UA%" -o "N25_ladybird_ants.webm.part" "https://upload.wikimedia.org/wikipedia/commons/7/7b/Ladybug_among_ants_-_2013-4-18_-_Alberta_Canada.webm?download" && move /y "N25_ladybird_ants.webm.part" "N25_ladybird_ants.webm" >nul
  timeout /t 3 /nobreak >nul
)

echo.
echo Finished. Files that failed keep the .part ending; run this script again to retry them.
pause
