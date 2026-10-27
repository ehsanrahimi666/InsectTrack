# Gallery of the demonstration videos: one annotated frame per video (the
# frame with the most insects in view), a contact-sheet figure and an HTML
# page linking each video, its report and its tables.

source(here::here("analysis", "10_common.R"))
dm <- bcfg$demo
dd <- file.path(root, "output", "demo")
src <- utils::read.csv(file.path(root, "data", "raw", "web_videos_2", "video_sources_2.csv"))
src$key <- tolower(gsub(" ", "_", src$original_file))
info <- function(id, v) {
  k <- match(tolower(basename(v$file)), src$key)
  if (!is.na(k)) return(list(species = src$species[k], scene = src$scene[k], licence = src$licence[k],
                             author = src$author[k], page = src$page[k],
                             title = sub("\\.[A-Za-z0-9]+$", "", src$original_file[k])))
  switch(sub("_tiled$", "", id),
    bee_counter = list(species = "Apis mellifera", scene = "Bee counter with separate entry and exit funnels",
                       licence = "CC BY 4.0", author = "David Atauri Mezquida", page = "https://zenodo.org/records/10674964",
                       title = "Bee-counter videos"),
    delosperma = list(species = "Apis mellifera", scene = "Honeybees visiting Delosperma flowers (smartphone video)",
                      licence = "\u00a9 Ehsan Rahimi", author = "Ehsan Rahimi", page = "",
                      title = "Honeybees on Delosperma flowers"),
    melipona = list(species = "Melipona capixaba", scene = "Stingless bees at a hive entrance",
                    licence = "CC BY 4.0", author = "de S\u00e1 Barbalho et al.", page = "https://doi.org/10.5281/zenodo.22235293",
                    title = "High-frame-rate video data set of Melipona capixaba"),
    list(species = "", scene = "", licence = "", author = "", page = "", title = ""))
}
still <- function(id) {
  od <- file.path(dd, id)
  vid <- list.files(od, pattern = "_tracked\\.mp4$", full.names = TRUE)
  tr <- file.path(od, "tracks.csv")
  if (!length(vid) || !file.exists(tr)) return(NA_character_)
  t <- utils::read.csv(tr)
  n <- if (nrow(t)) table(t$frame) else integer()
  # analysed frame with the most insects in view (frames are numbered from 1)
  k <- if (length(n)) as.integer(names(n)[which.max(n)]) else 1L
  out <- file.path(od, "still.png")
  idx <- max(0L, k - 1L)
  av::av_encode_video(vid[1], output = out, vfilter = sprintf("select=eq(n\\,%d)", idx), codec = "png",
                      verbose = FALSE)
  if (!file.exists(out)) out <- NA_character_
  out
}
rows <- list()
for (id in names(dm$videos)) {
  v <- dm$videos[[id]]
  sf <- file.path(dd, id, "summary.csv")
  if (!file.exists(sf)) next
  s <- utils::read.csv(sf)
  st <- jsonlite::read_json(file.path(dd, id, "settings.json"))
  m <- info(id, v)
  rows[[id]] <- data.frame(demo = id, species = m$species, scene = m$scene, detector = st$detector,
                           prompt = if (is.null(st$prompt)) "" else st$prompt, fps = st$fps,
                           seconds_analysed = s$end_s - s$start_s, frames = s$frames, tracks = s$tracks,
                           mean_insects = round(s$mean_insects_per_frame, 2), max_insects = s$max_insects_per_frame,
                           n_in = if (is.null(s$n_in)) NA else s$n_in, n_out = if (is.null(s$n_out)) NA else s$n_out,
                           run_s = round(sum(unlist(st$run_time_s))), licence = m$licence, author = m$author, page = m$page,
                           source_title = m$title,
                           still = still(id), stringsAsFactors = FALSE)
}
g <- do.call(rbind, rows)
write_csv(g[, setdiff(names(g), "still")], file.path(dd, "demo_gallery.csv"))

# contact sheet ------------------------------------------------------------------
ok <- g[!is.na(g$still), ]
dlab <- c(gdino = "Grounding DINO", yolo = "YOLO", motion = "background subtraction", owl = "OWLv2", owlv2 = "OWLv2")
nc <- 4; nr <- ceiling(nrow(ok) / nc)
grDevices::png(file.path(dd, "demo_frames.png"), width = 4000, height = round(4000 / nc * 0.5625 * nr + 120 * nr),
               res = 220, type = if (isTRUE(capabilities("cairo"))) "cairo" else NULL)
graphics::par(mfrow = c(nr, nc), mar = c(0.2, 0.2, 2.2, 0.2))
for (i in seq_len(nrow(ok))) {
  im <- png::readPNG(ok$still[i])
  graphics::plot.new(); graphics::plot.window(c(0, 1), c(0, 0.5625), xaxs = "i", yaxs = "i")
  h <- dim(im)[1] / dim(im)[2]
  if (h <= 0.5625) graphics::rasterImage(im, 0, (0.5625 - h) / 2, 1, (0.5625 + h) / 2)
  else { w <- 0.5625 / h; graphics::rasterImage(im, (1 - w) / 2, 0, (1 + w) / 2, 0.5625) }
  lab <- sprintf("(%s) %s", letters[i], ok$species[i])
  graphics::title(lab, adj = 0, cex.main = 0.95, font.main = 3, line = 0.9)
  det <- paste0(dlab[[ok$detector[i]]], if (grepl("_tiled$", ok$demo[i])) " on tiles" else "")
  graphics::title(sub("^(.{0,70}).*$", "\\1", paste0(det, ": ", ok$scene[i])), adj = 0, cex.main = 0.75,
                  font.main = 1, line = 0.15)
}
grDevices::dev.off()

# HTML gallery ---------------------------------------------------------------------
esc <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", as.character(x), fixed = TRUE), fixed = TRUE)
lic_url <- c("CC0" = "https://creativecommons.org/publicdomain/zero/1.0/",
             "Public domain" = "https://creativecommons.org/publicdomain/mark/1.0/",
             "CC BY 2.0" = "https://creativecommons.org/licenses/by/2.0/",
             "CC BY 2.5" = "https://creativecommons.org/licenses/by/2.5/",
             "CC BY 3.0" = "https://creativecommons.org/licenses/by/3.0/",
             "CC BY 4.0" = "https://creativecommons.org/licenses/by/4.0/",
             "CC BY-SA 3.0" = "https://creativecommons.org/licenses/by-sa/3.0/",
             "CC BY-SA 4.0" = "https://creativecommons.org/licenses/by-sa/4.0/")
group <- c(hive_entrance = 1, hive_fanning = 1, hive_flight = 1, bee_counter = 1, bee_counter_tiled = 1, melipona = 1,
           paper_wasps = 2, hornet_nest = 2, hornets_shrub = 2, giant_hornets_tree = 2, yellowjacket_honeybee = 2,
           bumblebees_thistle = 3, bees_hibiscus = 3, honeybee_oregano = 3, butterfly_marigold = 3, hoverfly_poppy = 3,
           leafcutter_bees_sunflower = 3, delosperma = 3,
           ant_trail = 4, leafcutter_ants = 4, leafcutter_ants_gdino = 4, drosophila_arena = 4, drosophila_arena_gdino = 4)
gname <- c("Bees at hive entrances", "Wasps and hornets", "Flower visitors", "Ants and flies")
note <- c(
  bee_counter = "No bee reached the detection threshold on the full 640 &times; 480 frames; the next run uses 320-pixel tiles.",
  bee_counter_tiled = "The same video with Grounding DINO applied to 320-pixel tiles.",
  ant_trail = "Background subtraction: real ants are tracked together with many false detections on the textured kerb.",
  leafcutter_ants = "Background subtraction fails here: moving leaves and camera shake give false detections. See the Grounding DINO run.",
  leafcutter_ants_gdino = "The same video with Grounding DINO.",
  drosophila_arena = "Background subtraction fails here: flies at rest merge with the background, and the species caption was detected. See the Grounding DINO run.",
  drosophila_arena_gdino = "The same video with Grounding DINO.")
dlab_html <- c(gdino = "Grounding DINO", yolo = "trained YOLO26n", motion = "background subtraction", owl = "OWLv2")
poster <- function(id) {
  f <- file.path(dd, id, "still.png")
  if (!file.exists(f)) return(NA_character_)
  im <- png::readPNG(f)
  jpeg::writeJPEG(im[, , 1:3], file.path(dd, id, "still.jpg"), quality = 0.85)
  "still.jpg"
}
g$poster <- vapply(g$demo, poster, "")
g$group <- group[g$demo]
card <- function(r) {
  vid <- list.files(file.path(dd, r$demo), pattern = "_tracked\\.mp4$")
  lic <- if (r$licence %in% names(lic_url)) sprintf('<a href="%s">%s</a>', lic_url[[r$licence]], esc(r$licence)) else esc(r$licence)
  src_t <- if (nzchar(r$page)) sprintf('<a href="%s">%s</a>', r$page, esc(r$source_title)) else esc(r$source_title)
  inout <- if (!is.na(r$n_in)) sprintf("; entrance: %d in, %d out", r$n_in, r$n_out) else ""
  prompt <- if (nzchar(r$prompt)) sprintf(", prompt &ldquo;%s&rdquo;", esc(r$prompt)) else ""
  tiles <- if (grepl("_tiled$", r$demo)) " on tiles" else ""
  nt <- if (r$demo %in% names(note)) sprintf('<p class="note">%s</p>', note[[r$demo]]) else ""
  sprintf(paste0(
    '<article class="card" id="%s"><video controls preload="none" poster="%s/%s" src="%s/%s"></video>',
    '<div class="body"><h3><i>%s</i></h3><p class="scene">%s</p>',
    '<p class="meta">%s%s%s; %s frames per second, %s s analysed<br>%d tracks; insects in view: mean %.2f, max %d%s</p>%s',
    '<p class="links"><a href="%s/report.html">report</a> &middot; <a href="%s/tracks.csv">tracks</a> &middot; ',
    '<a href="%s/track_summary.csv">track summary</a> &middot; <a href="%s/activity.png">activity</a> &middot; ',
    '<a href="%s/heatmap.png">heat map</a></p>',
    '<p class="source">Video: %s by %s, %s</p></div></article>'),
    r$demo, r$demo, r$poster, r$demo, vid[1], esc(r$species), esc(r$scene), dlab_html[[r$detector]], tiles, prompt,
    r$fps, format(r$seconds_analysed), r$tracks, r$mean_insects, r$max_insects, inout, nt,
    r$demo, r$demo, r$demo, r$demo, r$demo, src_t, esc(r$author), lic)
}
sections <- vapply(seq_along(gname), function(k) {
  z <- g[g$group == k, ]
  sprintf('<h2>%s</h2><div class="grid">%s</div>', gname[k],
          paste(vapply(seq_len(nrow(z)), function(i) card(z[i, ]), ""), collapse = ""))
}, "")
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width, initial-scale=1"><title>InsectTrack demonstration videos</title><style>',
  ':root{--ink:#1d2329;--muted:#55606b;--line:#dde2e7;--card:#fafbfc;--accent:#1e5bff;--note:#fff4e5;--notel:#e8a33d}',
  'body{margin:0;background:#fff;color:var(--ink);font:15px/1.5 system-ui,-apple-system,"Segoe UI",Roboto,sans-serif}',
  'main{max-width:1280px;margin:0 auto;padding:28px 16px 56px}h1{font-size:26px;margin:0 0 8px}',
  'h2{font-size:19px;margin:34px 0 4px;padding-bottom:6px;border-bottom:1px solid var(--line)}',
  '.lead{max-width:880px;color:var(--muted);margin:0}',
  '.grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(300px,1fr));gap:18px;margin-top:16px}',
  '.card{border:1px solid var(--line);border-radius:10px;overflow:hidden;background:var(--card);display:flex;flex-direction:column}',
  '.card video{width:100%;display:block;aspect-ratio:16/9;object-fit:contain;background:#000}',
  '.body{padding:10px 12px 12px}.body h3{font-size:16px;margin:0 0 2px}.body p{margin:4px 0}',
  '.scene{color:var(--ink)}.meta,.source,.links{font-size:13px;color:var(--muted)}',
  '.note{font-size:13px;background:var(--note);border-left:3px solid var(--notel);padding:6px 8px;border-radius:4px}',
  'a{color:var(--accent);text-decoration:none}a:hover{text-decoration:underline}',
  'footer{margin-top:40px;font-size:13px;color:var(--muted);max-width:880px}</style></head><body><main>',
  '<h1>InsectTrack demonstration videos</h1>',
  '<p class="lead">Twenty videos of bees, wasps, hornets, ants, flies and a butterfly, analysed with the R package ',
  '<a href="https://github.com/ehsanrahimi666/InsectTrack">InsectTrack</a>. Every insect has its own colour and ',
  'identity number. Each card plays the tracked video and links to the report, the tables and the figures of the analysis.</p>',
  paste(sections, collapse = ""),
  '<footer><p>The tracked videos and frames are adaptations of the source videos named on each card and keep their ',
  'licences; adaptations of CC BY-SA videos are shared under the same licence. Tables and figures were produced with ',
  'InsectTrack 0.1.0.</p></footer></main></body></html>')
writeLines(enc2utf8(html), file.path(dd, "index.html"), useBytes = TRUE)
log_msg("gallery: %d demonstration videos", nrow(g))
