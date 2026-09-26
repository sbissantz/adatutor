#' Shared settings for the tutorial's figures
#'
#' The size, margins and palette end that every `tutplot_*()` function draws
#' with, kept in one place so the figures match.
#'
#' This list is for reference only. Changing a copy has no effect, because a
#' function's defaults are looked up inside the package. Pass the value you
#' want instead:
#'
#' ```
#' tutplot_gini(pointsize = 12)
#' ```
#'
#' @format A list of nine settings:
#' * `col`: 4.30 x 3.06 inches, one column of the paper's two-column layout.
#' * `full`: 5.55 x 3.95 inches, the full text width. Not used yet.
#' * `pointsize`: 9. Raise it when you enlarge a figure, or the text stays
#'   small.
#' * `mar_plain`, `mar_legend`: Margins. The legend version has more room on
#'   top.
#' * `mgp`, `tcl`, `cex_axis`: Axis placement, tick length and label size.
#' * `viridis_end`: 0.85. Stops before viridis's yellow, which vanishes as a
#'   line on white.
#'
#' @family tutorial plots
#' @export
tutplot_opts <- list(
  col = c(width = 3.44 * 1.25, height = 2.45 * 1.25),
  full = c(width = 6.94 * 0.8, height = 4.94 * 0.8),
  pointsize = 9,
  mar_plain = c(2.9, 2.9, 1.9, 0.6),
  mar_legend = c(2.9, 2.9, 3.0, 0.6),
  mgp = c(1.7, 0.45, 0),
  tcl = -0.25,
  cex_axis = 0.9,
  viridis_end = 0.85
)


# shared par() block; returns the old settings for on.exit(), since one
# session renders every figure; `reserve_legend` makes room in the top margin
set_figure_par <- function(reserve_legend = TRUE) {
  graphics::par(
    mar = if (reserve_legend) {
      tutplot_opts$mar_legend
    } else {
      tutplot_opts$mar_plain
    },
    mgp = tutplot_opts$mgp,
    tcl = tutplot_opts$tcl,
    cex.axis = tutplot_opts$cex_axis
  )
}

# legend centered in the top margin (`xpd = NA`), so it never covers a
# curve; `text.width = NA` stops `horiz` padding entries to the widest
draw_legend <- function(labels, cex = 1, ...) {
  usr <- graphics::par("usr")
  graphics::legend(
    x = (usr[1] + usr[2]) / 2,
    y = usr[4] + 0.055 * (usr[4] - usr[3]),
    xjust = 0.5,
    yjust = 0,
    horiz = TRUE,
    xpd = NA,
    bty = "n",
    text.width = NA,
    x.intersp = 0.7,
    seg.len = 1.2,
    cex = cex,
    legend = labels,
    ...
  )
}

# rounded rectangle; `radius` is in inches, converted per axis so corners stay
# circular, and clamped to half the box so thin blocks become stadiums
draw_roundrect <- function(
  left,
  bottom,
  right,
  top,
  col,
  radius = 0.04,
  border = NA
) {
  usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  radius_x <- min(radius * (usr[2] - usr[1]) / pin[1], (right - left) / 2)
  radius_y <- min(radius * (usr[4] - usr[3]) / pin[2], (top - bottom) / 2)
  angle <- seq(0, pi / 2, length.out = 12)
  graphics::polygon(
    c(
      right - radius_x + radius_x * sin(angle),
      right - radius_x + radius_x * cos(angle),
      left + radius_x - radius_x * sin(angle),
      left + radius_x - radius_x * cos(angle)
    ),
    c(
      bottom + radius_y - radius_y * cos(angle),
      top - radius_y + radius_y * sin(angle),
      top - radius_y + radius_y * cos(angle),
      bottom + radius_y - radius_y * sin(angle)
    ),
    col = col,
    border = border
  )
}
