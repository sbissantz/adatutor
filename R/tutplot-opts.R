#' @title Shared Settings Behind the Tutorial's Figures
#'
#' @description The geometry, margins and palette anchor that every
#'   \code{\link[adatutor]{tutplot}} function draws with, kept in one place so
#'   the figures cannot drift apart.
#'
#' @details \strong{This is a reference, not a switch.} R resolves a function's
#'   default arguments inside the package, not in your session, so assigning to
#'   a copy of \code{tutplot_opts} changes nothing:
#'
#'   \preformatted{
#'   opts <- tutplot_opts
#'   opts$pointsize <- 12
#'   tutplot_gini()                          # still 9 point
#'   tutplot_gini(pointsize = opts$pointsize) # 12 point
#'   }
#'
#'   Look the values up here, then pass the ones you want to change.
#'
#'   \strong{The values.} Two float widths, measured from the typeset
#'   manuscript, in inches:
#'
#'   \describe{
#'     \item{\code{col}}{4.30 wide by 3.06 high -- one column of the paper's
#'       two-column layout. Every figure extracted so far uses it.}
#'     \item{\code{full}}{5.55 wide by 3.95 high -- the full text width, for the
#'       two decision-boundary plots the manuscript sets as \code{figure*}.
#'       \strong{Unused for now}: those two figures are the ones still to be
#'       extracted.}
#'     \item{\code{pointsize}}{9. Drawing at print size is what keeps the labels
#'       legible; enlarging a figure without raising this leaves the type
#'       behind.}
#'     \item{\code{mar_plain}, \code{mar_legend}}{Margins. They differ only in
#'       the third element, 1.9 against 3.0: a figure whose legend sits above
#'       the panel needs the extra room up top.}
#'     \item{\code{mgp}, \code{tcl}, \code{cex_axis}}{Axis placement, tick
#'       length and axis type size. R's defaults would leave almost no room to
#'       plot in at this size.}
#'     \item{\code{viridis_end}}{0.85. Stops short of viridis's yellow, which is
#'       invisible as a line on white.}
#'   }
#'
#' @format A list of nine settings, described above.
#'
#' @seealso \code{\link[adatutor]{tutplot}}, the functions that use them.
#'
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

# The shared `par()` block. Returns the previous settings, so a caller can
# restore them with `on.exit()` -- the whole document runs in one session and
# these would otherwise leak into every later figure.
#
# `legend = TRUE` reserves the top margin for a legend drawn above the panel.
# That third margin element is the only thing that differs between the figures.
tut_par <- function(legend = TRUE) {
  graphics::par(
    mar = if (legend) tutplot_opts$mar_legend else tutplot_opts$mar_plain,
    mgp = tutplot_opts$mgp,
    tcl = tutplot_opts$tcl,
    cex.axis = tutplot_opts$cex_axis
  )
}

# The legend, centered in the margin above the panel where `main` would go, so
# it never covers a curve. `xpd = NA` is what lets it draw outside the plotting
# region; `tut_par(legend = TRUE)` reserves the room. `text.width = NA` gives
# each entry its own width -- without it `horiz` pads every column out to the
# widest label, which strings short entries across the whole panel.
tut_legend <- function(labels, cex = 1, ...) {
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

# A rounded rectangle, which base graphics has no primitive for. `r` is the
# corner radius in INCHES, converted separately for x and y from the current
# panel, so the corners stay circular whatever the aspect ratio -- a radius
# given in user units goes oval the moment the figure is resized. Clamped to
# half the box, so a very thin block degrades to a stadium rather than folding
# its corners through each other.
tut_roundrect <- function(x0, y0, x1, y1, col, r = 0.04, border = NA) {
  usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  rx <- min(r * (usr[2] - usr[1]) / pin[1], (x1 - x0) / 2)
  ry <- min(r * (usr[4] - usr[3]) / pin[2], (y1 - y0) / 2)
  a <- seq(0, pi / 2, length.out = 12)
  graphics::polygon(
    c(x1 - rx + rx * sin(a), x1 - rx + rx * cos(a),
      x0 + rx - rx * sin(a), x0 + rx - rx * cos(a)),
    c(y0 + ry - ry * cos(a), y1 - ry + ry * sin(a),
      y1 - ry + ry * cos(a), y0 + ry - ry * sin(a)),
    col = col,
    border = border
  )
}

# Ink that the fill can carry. Relative luminance, so a label stays readable at
# both ends of viridis -- white on the dark purple, near-black on the yellow.
tut_ink <- function(col) {
  v <- grDevices::col2rgb(col) / 255
  # `col2rgb()` names its rows, and the names ride through into the result --
  # strip them, or a caller gets a colour called "red" that is not red
  lum <- unname(0.2126 * v[1, ] + 0.7152 * v[2, ] + 0.0722 * v[3, ])
  ifelse(lum > 0.55, "grey15", "white")
}
