# HTML summary page of one video analysed by it_track_video().
write_report <- function(res, file) {
  esc <- function(x) {
    x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  s <- res$summary; st <- res$settings
  fmt <- function(v) if (is.numeric(v)) formatC(v, digits = 3, format = "fg", big.mark = ",") else esc(v)
  rows <- c(
    sprintf("<tr><th>Video</th><td>%s</td></tr>", esc(s$video)),
    sprintf("<tr><th>Analysed</th><td>%s-%s s, %s frames at %s frames per second, %s x %s pixels</td></tr>",
            fmt(s$start_s), fmt(s$end_s), fmt(s$frames), fmt(st$fps), st$width, st$height),
    sprintf("<tr><th>Detector</th><td>%s%s%s</td></tr>", esc(st$detector),
            if (!is.null(st$model)) paste0(" (", esc(st$model), ")") else "",
            if (!is.null(st$prompt)) paste0(", prompt &ldquo;", esc(st$prompt), "&rdquo;") else ""),
    sprintf("<tr><th>Tracker</th><td>%s%s</td></tr>", esc(st$tracker), if (isTRUE(st$stitch)) ", fragments joined" else ""),
    sprintf("<tr><th>Detections</th><td>%s</td></tr>", fmt(s$detections)),
    sprintf("<tr><th>Tracks</th><td>%s (median duration %s s)</td></tr>", fmt(s$tracks), fmt(s$median_track_s)),
    sprintf("<tr><th>Insects per frame</th><td>mean %s, maximum %s</td></tr>", fmt(s$mean_insects_per_frame),
            fmt(s$max_insects_per_frame)),
    if (!is.null(s$n_in)) sprintf("<tr><th>Entrance</th><td>%s in, %s out</td></tr>", fmt(s$n_in), fmt(s$n_out)),
    if (!is.null(s$visits)) sprintf("<tr><th>Visits to regions</th><td>%s</td></tr>", fmt(s$visits)),
    if (!is.null(s$interactions)) sprintf("<tr><th>Proximity interactions</th><td>%s</td></tr>", fmt(s$interactions)),
    sprintf("<tr><th>Hot-spot cells</th><td>%s</td></tr>", fmt(s$hot_cells)))
  figs <- c(trajectories.png = "Trajectories (one colour per track)",
            heatmap.png = "Activity heat map (insect-seconds per cell)",
            hotspots.png = "Hot spots (Getis-Ord Gi*, outlined)",
            activity.png = "Insects per frame over time",
            entrance.png = "Insects entering and leaving per time bin")
  dir <- dirname(file)
  figs <- figs[file.exists(file.path(dir, names(figs)))]
  fig_html <- paste(sprintf('<figure><img src="%s" alt="%s"><figcaption>%s</figcaption></figure>',
                            names(figs), esc(figs), esc(figs)), collapse = "\n")
  vid <- if (!is.null(res$annotated_video)) {
    v <- basename(res$annotated_video)
    sprintf('<h2>Annotated video</h2><video controls preload="metadata" src="%s"></video><p><a href="%s">%s</a></p>',
            v, v, esc(v))
  } else ""
  csvs <- list.files(dir, pattern = "\\.csv$")
  files_html <- paste(sprintf('<li><a href="%s">%s</a></li>', csvs, esc(csvs)), collapse = "\n")
  html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    '<title>', esc(s$video), ' - InsectTrack</title><style>',
    'body{margin:0;background:#fff;color:#1d2329;font:15px/1.45 system-ui,-apple-system,"Segoe UI",sans-serif}',
    'main{max-width:1100px;margin:0 auto;padding:24px 16px 48px}h1{font-size:24px;margin:0 0 12px}',
    'h2{font-size:18px;margin:28px 0 10px}table{border-collapse:collapse;width:100%}',
    'th,td{text-align:left;border-bottom:1px solid #dde2e7;padding:7px 6px;vertical-align:top}th{width:210px;color:#4a545e}',
    'figure{margin:18px 0}img,video{max-width:100%;border:1px solid #dde2e7;border-radius:6px}',
    'figcaption{font-size:13px;color:#4a545e;margin-top:4px}</style></head><body><main>',
    '<h1>', esc(s$video), '</h1><table>', paste(rows, collapse = ""), '</table>', vid,
    '<h2>Figures</h2>', fig_html, '<h2>Tables</h2><ul>', files_html, '</ul>',
    '<p style="font-size:13px;color:#4a545e">Settings and software versions: <a href="settings.json">settings.json</a>.</p>',
    '</main></body></html>')
  writeLines(html, file, useBytes = TRUE)
  invisible(file)
}
