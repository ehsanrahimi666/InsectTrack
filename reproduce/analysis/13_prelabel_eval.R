# Evaluation of the zero-shot pre-labelling benchmark (analysis/11_prelabel.py):
# precision, recall and F1 over score thresholds, average precision at an IoU
# of 0.5, annotation workload at the primary thresholds, SAM 2.1 box
# refinement, and the bfloat16 versus float32 check.

source(here::here("analysis", "10_common.R"))
od <- bout("prelabel")
frames <- utils::read.csv(file.path(od, "frames.csv"))
gt <- utils::read.csv(file.path(od, "gt.csv"))
gt$frame <- match(gt$image_id, frames$image_id)
gt <- transform(gt, x0 = x1, y0 = y1, x1 = x2, y1 = y2)
gt$x <- (gt$x0 + gt$x1) / 2; gt$y <- (gt$y0 + gt$y1) / 2
pc <- bcfg$prelabel
configs <- vapply(pc$configurations, `[[`, "", "name")
det_of <- function(name) {
  f <- file.path(od, paste0("detections__", name, ".csv"))
  if (!file.exists(f)) return(NULL)
  d <- utils::read.csv(f)
  d$frame <- match(d$image_id, frames$image_id)
  d <- transform(d, x0 = x1, y0 = y1, x1 = x2, y1 = y2)
  d$x <- (d$x0 + d$x1) / 2; d$y <- (d$y0 + d$y1) / 2; d$conf <- d$score
  d
}
primary <- function(cfg) {
  det <- cfg$detector
  bcfg$detection$primary_threshold[[if (det == "owl") "owlv2" else det]]
}

# average precision at IoU 0.5 (greedy matching in descending score order)
average_precision <- function(d, g) {
  if (!nrow(d)) return(c(ap50 = 0, max_recall = 0))
  d <- d[order(-d$score), ]
  gb <- it_det_boxes(g); db <- it_det_boxes(d)
  used <- rep(FALSE, nrow(g)); tp <- logical(nrow(d))
  gi <- split(seq_len(nrow(g)), g$frame)
  for (k in seq_len(nrow(d))) {
    cand <- gi[[as.character(d$frame[k])]]
    cand <- cand[!used[cand]]
    if (!length(cand)) next
    io <- it_box_iou(db[k, , drop = FALSE], gb[cand, , drop = FALSE])
    j <- which.max(io)
    if (io[j] >= pc$match_iou) { tp[k] <- TRUE; used[cand[j]] <- TRUE }
  }
  ctp <- cumsum(tp); rec <- ctp / nrow(g); prec <- ctp / seq_along(tp)
  mprec <- rev(cummax(rev(prec)))
  ap <- sum(diff(c(0, rec)) * mprec)
  c(ap50 = ap, max_recall = max(rec))
}

rows <- list(); aps <- list(); work <- list()
for (cfg in pc$configurations) {
  d <- det_of(cfg$name)
  if (is.null(d)) next
  for (ds in c(unique(frames$dataset), "all")) {
    fi <- if (ds == "all") seq_len(nrow(frames)) else which(frames$dataset == ds)
    gg <- gt[gt$frame %in% fi, ]; dd <- d[d$frame %in% fi, ]
    for (th in bcfg$detection$threshold_grid) {
      e <- it_eval_detections(dd[dd$score >= th, ], gg, iou = pc$match_iou, frames = fi)$summary
      rows[[length(rows) + 1]] <- cbind(configuration = cfg$name, dataset = ds, threshold = th,
                                        e[, c("n_true", "n_pred", "tp", "fp", "fn", "precision", "recall", "f1", "mean_iou")])
    }
    aps[[length(aps) + 1]] <- data.frame(configuration = cfg$name, dataset = ds, t(average_precision(dd, gg)))
    th <- primary(cfg)
    ev <- it_eval_detections(dd[dd$score >= th, ], gg, iou = pc$match_iou, frames = fi)
    pf <- ev$per_frame
    # matched boxes with IoU below adjust_iou need adjusting
    db <- dd[dd$score >= th, ]
    adj <- 0
    for (f in fi) {
      gr <- which(gg$frame == f); dr <- which(db$frame == f)
      if (!length(gr) || !length(dr)) next
      S <- it_box_iou(it_det_boxes(gg[gr, ]), it_det_boxes(db[dr, ]))
      m <- InsectTrack:::match_points(1 - S, 1 - pc$match_iou)
      adj <- adj + sum(S[m] < pc$adjust_iou)
    }
    work[[length(work) + 1]] <- data.frame(configuration = cfg$name, dataset = ds, threshold = th,
                                           n_true = sum(pf$n_true), boxes_to_add = sum(pf$fn), boxes_to_delete = sum(pf$fp),
                                           boxes_to_adjust = adj, boxes_correct = sum(pf$tp) - adj,
                                           share_true_correct = (sum(pf$tp) - adj) / sum(pf$n_true))
  }
}
metrics <- do.call(rbind, rows)
write_csv(metrics, file.path(od, "prelabel_metrics_by_threshold.csv"))
write_csv(do.call(rbind, aps), file.path(od, "prelabel_ap50.csv"))
write_csv(do.call(rbind, work), file.path(od, "prelabel_workload.csv"))

# SAM 2.1 refinement: IoU with the annotation before and after replacing the box by the mask box
sf <- file.path(od, "sam2_refined.csv")
if (file.exists(sf)) {
  s <- utils::read.csv(sf)
  s$frame <- match(s$image_id, frames$image_id)
  before <- transform(s, x0 = x1, y0 = y1, x1 = x2, y1 = y2)
  after <- transform(s, x0 = ifelse(is.na(mx1), x1, mx1), y0 = ifelse(is.na(my1), y1, my1),
                     x1 = ifelse(is.na(mx2), x2, mx2), y1 = ifelse(is.na(my2), y2, my2))
  for (z in c("before", "after")) { dz <- get(z); dz$x <- (dz$x0 + dz$x1) / 2; dz$y <- (dz$y0 + dz$y1) / 2; assign(z, dz) }
  ref <- do.call(rbind, lapply(c(unique(frames$dataset), "all"), function(ds) {
    fi <- if (ds == "all") seq_len(nrow(frames)) else which(frames$dataset == ds)
    gg <- gt[gt$frame %in% fi, ]
    eb <- it_eval_detections(before[before$frame %in% fi, ], gg, iou = pc$match_iou, frames = fi)$summary
    ea <- it_eval_detections(after[after$frame %in% fi, ], gg, iou = pc$match_iou, frames = fi)$summary
    data.frame(dataset = ds, boxes = sum(before$frame %in% fi), tp_before = eb$tp, mean_iou_before = eb$mean_iou,
               tp_after = ea$tp, mean_iou_after = ea$mean_iou,
               median_body_length_px = stats::median(s$body_length[s$frame %in% fi], na.rm = TRUE))
  }))
  write_csv(ref, file.path(od, "sam2_refinement.csv"))
}

# bfloat16 versus float32
ff <- file.path(od, "fp32_check.csv")
if (file.exists(ff)) {
  b <- utils::read.csv(ff)
  th <- bcfg$detection$primary_threshold$gdino
  cmp <- do.call(rbind, lapply(split(b, b$image_id), function(z) {
    a <- z[z$precision == "fp32" & z$score >= th, ]; c2 <- z[z$precision == "bf16" & z$score >= th, ]
    io <- if (nrow(a) && nrow(c2)) it_box_iou(as.matrix(a[, c("x1", "y1", "x2", "y2")]), as.matrix(c2[, c("x1", "y1", "x2", "y2")])) else matrix(0, 0, 0)
    m <- if (length(io)) InsectTrack:::match_points(1 - io, 0.5) else matrix(integer(), 0, 2)
    data.frame(image_id = z$image_id[1], n_fp32 = nrow(a), n_bf16 = nrow(c2), matched = nrow(m),
               mean_iou = if (nrow(m)) mean(io[m]) else NA_real_,
               mean_abs_score_diff = if (nrow(m)) mean(abs(a$score[m[, 1]] - c2$score[m[, 2]])) else NA_real_)
  }))
  write_csv(cmp, file.path(od, "fp32_vs_bf16.csv"))
}

# precision-recall figure
m <- metrics[metrics$dataset != "all", ]
cols <- c(gdino_tiny_800 = "#1f4e9c", gdino_tiny_512 = "#6a9bd8", gdino_base_800 = "#0b2a5e",
          gdino_tiny_800_insect = "#7f7f7f", owlv2_base = "#d95f02", owlvit_b32 = "#e6ab02", yolo_bee_1280 = "#2a9d8f")
labs <- c(gdino_tiny_800 = "Grounding DINO tiny, 800 px", gdino_tiny_512 = "Grounding DINO tiny, 512 px",
          gdino_base_800 = "Grounding DINO base, 800 px", gdino_tiny_800_insect = "Grounding DINO tiny, prompt 'insect'",
          owlv2_base = "OWLv2 base", owlvit_b32 = "OWL-ViT base/32", yolo_bee_1280 = "YOLO26n trained on Delosperma bees")
dsl <- c(delosperma = "Honeybees on Delosperma", melipona = "Melipona hive entrance", native_bee = "Native bee hive entrance")
grDevices::png(file.path(od, "prelabel_precision_recall.png"), width = 3300, height = 1350, res = 230,
               type = if (isTRUE(capabilities("cairo"))) "cairo" else NULL)
graphics::layout(matrix(c(1, 2, 3, 4, 4, 4), 2, byrow = TRUE), heights = c(1, 0.28))
graphics::par(mar = c(4.5, 4.5, 2.5, 0.8))
for (ds in names(dsl)) {
  graphics::plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "Recall", ylab = "Precision", las = 1)
  graphics::title(sprintf("(%s) %s", letters[match(ds, names(dsl))], dsl[[ds]]), adj = 0, line = 0.8)
  for (cn in names(cols)) {
    z <- m[m$dataset == ds & m$configuration == cn, ]
    if (!nrow(z)) next
    z <- z[order(z$threshold), ]
    graphics::lines(z$recall, z$precision, col = cols[[cn]], lwd = 2)
    pz <- z[abs(z$threshold - primary(pc$configurations[[match(cn, configs)]])) < 1e-9, ]
    graphics::points(pz$recall, pz$precision, pch = 19, col = cols[[cn]], cex = 1.2)
  }
}
graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
graphics::legend("center", legend = c(labs[names(cols)], "primary score threshold"), ncol = 4, bty = "n",
                 col = c(cols, "black"), lwd = c(rep(2, length(cols)), NA), pch = c(rep(NA, length(cols)), 19), cex = 0.95)
grDevices::dev.off()
log_msg("pre-labelling evaluation written")
