#' Shared figure settings
#'
#' The sizes, margins and palette end that the `tutplot_*()` functions draw
#' with. Changing a copy of this list has no effect. To change a figure's size
#' or text size, pass `width`, `height` or `pointsize` together with `file`.
#'
#' @format A list of nine settings:
#' * `col`, `full`: Figure sizes in inches, 4.30 by 3.06 and 5.55 by 3.95.
#'   `full` is the default size of [tutplot_logoscheme()] and
#'   [tutplot_boundary()], `col` of the other figures.
#' * `pointsize`: 9.
#' * `mar_plain`, `mar_legend`: Margins. The legend version has more room on
#'   top.
#' * `mgp`, `tcl`, `cex_axis`: Axis placement, tick length and label size.
#' * `viridis_end`: 0.85, which stops the viridis palette before its yellow.
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


# returns the old settings for on.exit(): one session renders every figure
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
draw_legend <- function(labels, cex = 1, ..., pad = 0) {
  usr <- graphics::par("usr")
  middle <- (usr[1] + usr[2]) / 2
  text_width <- NA
  if (pad != 0) {
    char_width <- cex *
      graphics::par("cex") *
      graphics::xyinch(graphics::par("cin"))[1]
    # no key follows the last label, so a pad there would only be blank
    pads <- c(rep(pad, length(labels) - 1L), 0)
    text_width <- graphics::strwidth(labels, units = "user", cex = cex) +
      pads * char_width
    # legend() centers its box, which starts 0.3 character widths before a
    # key of line and dot but ends 0.5 after the last label
    middle <- middle + 0.1 * char_width
  }
  graphics::legend(
    x = middle,
    y = usr[4] + 0.055 * (usr[4] - usr[3]),
    xjust = 0.5,
    yjust = 0,
    horiz = TRUE,
    xpd = NA,
    bty = "n",
    text.width = text_width,
    x.intersp = 0.7,
    seg.len = 1.2,
    cex = cex,
    legend = labels,
    ...
  )
}

# `radius` is in inches, converted per axis so corners stay circular, and
# clamped to half the box so thin blocks become stadiums
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

#' Give each project its color, the same in every figure
#'
#' @return One color per distinct project, in the order given, named by
#'   project.
#' @noRd
color_projects <- function(projects) {
  projects <- unique(as.character(projects))
  # compare in lowercase, so "ML1" in the scheme gets the color of "ml1" in the
  # estimates; radix sorts the same in every locale
  alphabetical <- order(tolower(projects), projects, method = "radix")
  colors <- character(length(projects))
  colors[alphabetical] <- viridisLite::viridis(
    length(projects),
    end = tutplot_opts$viridis_end
  )
  names(colors) <- projects
  colors
}
