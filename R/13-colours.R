#' Distinct colours for track identities
#'
#' Each track id gets its own colour. Hues are spaced by the golden angle
#' (137.5 degrees), so consecutive ids, which often appear together, differ
#' strongly; lightness alternates between three levels.
#'
#' @param ids Track ids (integers).
#' @param palette Optional vector of colours recycled over ids instead.
#' @return A character vector of colours, one per id.
#' @examples
#' it_track_colours(1:6)
#' @export
it_track_colours <- function(ids, palette = NULL) {
  ids <- as.integer(ids)
  if (!is.null(palette)) return(rep_len(palette, max(c(ids, 1L), na.rm = TRUE))[ids])
  h <- (ids * 137.508) %% 360
  l <- c(70, 58, 80)[(ids %% 3L) + 1L]
  grDevices::hcl(h, c = 90, l = l, fixup = TRUE)
}

# Text with an outline (halo) that stays readable on any background.
halo_text <- function(x, y, labels, col = "white", halo = "black", cex = 1, font = 2,
                      adj = c(0.5, 0.5), width = 0.08) {
  if (!length(x)) return(invisible())
  o <- graphics::strheight("M", cex = cex) * width
  a <- seq(0, 2 * pi, length.out = 13)[-13]
  for (k in seq_along(a)) {
    graphics::text(x + o * cos(a[k]), y + o * sin(a[k]), labels, col = halo, cex = cex, font = font, adj = adj)
  }
  graphics::text(x, y, labels, col = col, cex = cex, font = font, adj = adj)
}
