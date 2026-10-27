# InsectTrack demonstration videos

Twenty videos of bees, wasps, hornets, ants, flies and a butterfly analysed with
the R package [InsectTrack](https://github.com/ehsanrahimi666/InsectTrack).
Every insect has its own colour and identity number in the tracked videos.

**Watch them at <https://ehsanrahimi666.github.io/InsectTrack-demos/>.**

Each folder holds the output of one call of `it_track_video()`:

| File | Content |
|---|---|
| `<video>_tracked.mp4` | the video with a coloured trajectory and identity label per insect |
| `tracks.csv`, `detections.csv` | every detection with its track, box, score and class |
| `track_summary.csv` | per track: duration, path length, speed, straightness, entry and exit edges, body size |
| `activity.csv`, `activity.png` | insects per frame (mean and MaxN), tracks and new tracks per time bin |
| `heatmap.csv`, `heatmap.png`, `hotspots.png` | time spent per cell (insect-seconds) and Getis-Ord Gi* hot spots |
| `entrance_*.csv`, `entrance.png` | insects entering and leaving the hive or nest entrance |
| `interactions.csv`, `visits*.csv` | proximity events between insects; visits to regions such as flowers |
| `prompt_screening.csv` | scores of the text prompts tried for Grounding DINO |
| `settings.json`, `summary.csv`, `report.html` | settings, software versions and run times; summary; report page |

`demo_gallery.csv` and `demo_summary.csv` summarise all runs. Three videos were
analysed twice: the bee counter with Grounding DINO on whole frames and on
320-pixel tiles, and the leafcutter ants and the fruit flies with background
subtraction and with Grounding DINO.

The source videos, their authors and licences are listed in
[LICENSE.md](LICENSE.md) and on each card of the gallery.
