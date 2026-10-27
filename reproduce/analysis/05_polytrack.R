# Polytrack trajectories (Ratnayake et al., strawberry farm): flower visits
# computed by InsectTrack from the published trajectories and flower positions,
# compared with the visits published with the data set, and per-species
# activity maps.

source(here::here("analysis", "00_common.R"))

p <- cfg$polytrack
od <- out_dir("polytrack")
zf <- file.path(root, cfg$paths$polytrack_zip)
wd <- file.path(tempdir(), "polytrack")
utils::unzip(zf, exdir = wd)

ts <- utils::read.csv(file.path(wd, "Pollination_Analysis_Summary", "track_summary.csv"))
fv <- utils::read.csv(file.path(wd, "Pollination_Analysis_Summary", "flower_visits.csv"))
files <- list.files(file.path(wd, "Extracted_Tracks", "Insect_Tracks_CSV"), pattern = "\\.csv$", full.names = TRUE)

anomalies <- list()
read_track <- function(f) {
  d <- utils::read.csv(f, check.names = FALSE)
  id <- sub("\\.csv$", "", basename(f))
  col <- function(nm) d[[paste0(nm, "_", id)]]
  nf <- col("nframe")
  if (is.null(nf)) {
    anomalies[[id]] <<- data.frame(track = id, problem = "column nframe missing (malformed header); row order used")
    nf <- seq_len(nrow(d)) - 1
  }
  data.frame(track = as.numeric(id), nframe = nf, absframe = col("absframe"),
             x = col("x0"), y = col("y0"), flower_polytrack = col("flower"))
}
tr <- do.call(rbind, lapply(files, read_track))
na_pos <- tapply(is.na(tr$x) | is.na(tr$y), tr$track, sum)
if (any(na_pos > 0)) anomalies[["na_positions"]] <- data.frame(
  track = paste(sum(na_pos > 0), "tracks"), problem = sprintf("%d rows without coordinates (frames without a recorded position; excluded)", sum(na_pos)))
if (length(anomalies)) write_csv(do.call(rbind, anomalies), file.path(od, "data_anomalies.csv"))
tr$dcp <- floor(tr$track / 1e8)
tr <- merge(tr, ts[, c("track", "species", "track_time")], by = "track")

# frame rate implied by the published track durations
span <- tapply(tr$absframe, tr$track, function(z) max(z) - min(z))
tt <- ts$track_time[match(as.numeric(names(span)), ts$track)]
fps_est <- stats::median(span[tt > 1] / tt[tt > 1])
tr$time <- tr$absframe / fps_est
tr$frame <- tr$absframe

fpos <- list.files(file.path(wd, "Extracted_Tracks", "Flower_Positions_CSV"), full.names = TRUE)
roi_by_dcp <- lapply(fpos, function(f) {
  d <- utils::read.csv(f)
  bad <- is.na(d$flower_num) | is.na(d$x0) | is.na(d$y0) | is.na(d$radius)
  if (any(bad)) anomalies[[basename(f)]] <<- data.frame(track = basename(f),
    problem = sprintf("%d flower rows without number, position or radius (excluded)", sum(bad)))
  d <- d[!bad, ]
  r <- lapply(seq_len(nrow(d)), function(i) it_roi_circle(d$x0[i], d$y0[i], d$radius[i], n = p$flower_polygon_vertices))
  names(r) <- as.character(d$flower_num)
  r
})
names(roi_by_dcp) <- sub("^flowers_(\\d+)\\.csv$", "\\1", basename(fpos))
if (length(anomalies)) write_csv(do.call(rbind, anomalies), file.path(od, "data_anomalies.csv"))

# per-detection agreement of "on a flower" and visits per track
agree <- list(); visits <- list(); visits_merged <- list()
for (dcp in intersect(names(roi_by_dcp), unique(as.character(tr$dcp)))) {
  d <- tr[tr$dcp == as.numeric(dcp), ]
  rois <- roi_by_dcp[[dcp]]
  inside <- Reduce(`|`, lapply(rois, function(r) it_point_in_polygon(d$x, d$y, r$x, r$y)))
  agree[[dcp]] <- data.frame(dcp = dcp, ours = inside, polytrack = !is.na(d$flower_polytrack))
  v <- it_roi_visits(d, rois)
  if (nrow(v)) visits[[dcp]] <- cbind(dcp = dcp, v)
  v2 <- it_roi_visits(d, rois, merge_gap = 2.75, min_duration = 0.5)
  if (nrow(v2)) visits_merged[[dcp]] <- cbind(dcp = dcp, v2)
}
agree <- do.call(rbind, agree)
visits <- do.call(rbind, visits)
visits_merged <- do.call(rbind, visits_merged)
tab <- table(ours = agree$ours, polytrack = agree$polytrack)
po <- sum(diag(tab)) / sum(tab)
pe <- sum(rowSums(tab) * colSums(tab)) / sum(tab)^2
kappa <- (po - pe) / (1 - pe)

pub <- fv[!is.na(fv$flower), ]
cnt <- merge(data.frame(track = as.numeric(names(table(visits$track))), ours = as.integer(table(visits$track))),
             data.frame(track = as.numeric(names(table(pub$track))), polytrack = as.integer(table(pub$track))),
             by = "track", all = TRUE)
cnt[is.na(cnt)] <- 0
cnt <- merge(cnt, ts[, c("track", "species")], by = "track", all.y = TRUE)
cnt$ours[is.na(cnt$ours)] <- 0; cnt$polytrack[is.na(cnt$polytrack)] <- 0
rho <- stats::cor.test(cnt$ours, cnt$polytrack, method = "spearman", exact = FALSE)
# post hoc check: visit definition of the Delosperma study (gaps <= 2.75 s merged, visits >= 0.5 s)
cm <- table(factor(visits_merged$track, levels = cnt$track))
rho_m <- stats::cor.test(as.integer(cm), cnt$polytrack, method = "spearman", exact = FALSE)
by_sp <- stats::aggregate(cbind(ours, polytrack) ~ species, data = cnt, sum)
write_csv(as.data.frame(tab), file.path(od, "on_flower_agreement_table.csv"))
write_csv(cnt, file.path(od, "visits_per_track_comparison.csv"))
write_csv(by_sp, file.path(od, "visits_by_species.csv"))
summ <- data.frame(tracks = length(unique(tr$track)), detections = nrow(tr), recordings = length(unique(tr$dcp)),
                   fps_estimated = fps_est, agreement_on_flower = po, kappa_on_flower = kappa,
                   visits_ours = nrow(visits), visits_polytrack = nrow(pub),
                   spearman_visits_per_track = unname(rho$estimate),
                   exact_same_count_share = mean(cnt$ours == cnt$polytrack),
                   visits_ours_merged_posthoc = nrow(visits_merged),
                   spearman_visits_per_track_merged_posthoc = unname(rho_m$estimate),
                   exact_same_count_share_merged_posthoc = mean(as.integer(cm) == cnt$polytrack))
write_csv(summ, file.path(od, "polytrack_summary.csv"))

# activity map of the data collection point with most tracks (honeybees vs others)
top <- names(sort(table(tr$dcp), decreasing = TRUE))[1]
d <- tr[tr$dcp == as.numeric(top), ]
for (grp in c("honeybee", "other")) {
  dd <- if (grp == "honeybee") d[d$species == "honeybee", ] else d[d$species != "honeybee", ]
  if (!nrow(dd)) next
  hm <- it_heatmap(dd, 1920, 1080, cell = p$heatmap$cell, weight = "tracks")
  save_png(file.path(od, paste0("polytrack_", top, "_", grp, "_visits_map.png")), 1600, 900,
           it_plot_heatmap(hm, rois = roi_by_dcp[[top]], legend_title = "tracks per cell", log = FALSE))
}
unlink(wd, recursive = TRUE)
log_msg("Polytrack finished: kappa %.3f, rho %.3f", kappa, rho$estimate)
