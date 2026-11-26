#' Plot a two-feature decision boundary
#'
#' Draws the region each class is assigned to over a grid of two features,
#' with the boundary between them and the data on top. Takes a single rpart
#' tree or an [adaboost()] ensemble. This is the manuscript's Figures 3 and 9:
#' the same call, with a stump in one and a 1000-round ensemble in the other.
#'
#' @section Shading:
#' The boundary is the zero line of a continuous score: the margin for an
#' ensemble, and the fitted probability minus 0.5 for a tree. That gives a
#' smooth line at a modest `resolution`.
#'
#' `shade = "class"` tints each side in a flat color. `shade = "margin"` (the
#' default) lays the viridis scale over the score, centered on zero, so the
#' classes sit at its ends and uncertainty sits in the middle. For a tree, the
#' scale is fixed at -0.5 to 0.5, so a color always means the same
#' probability. For an ensemble, it runs to the largest margin, so colors
#' compare within a plot but not across plots. A short line marks the middle
#' of the color bar, a score of zero, as the boundary; the label "Boundary"
#' sits under the bar.
#'
#' A stump has one split, so it shows two flat blocks; an ensemble shows a
#' surface. That contrast is what boosting buys.
#'
#' @section Points:
#' Each point is filled with the color of its observed class. A point is
#' misclassified when the sign of its score names the other class, the same
#' score the surface is drawn from. Misclassified points are drawn paler and
#' smaller, under the correct ones. The figure has no key for them, and it
#' prints no count; the return value holds it.
#'
#' @param fit An rpart tree or an [adaboost()] fit.
#' @param data A data frame with the two features and the outcome. It sets the
#'   plotting range and supplies the points.
#' @param features The names of the two features. The default, `NULL`, works
#'   when the model has exactly two predictors and stops otherwise.
#' @param shade `"margin"` (the default) or `"class"`. See the section on
#'   shading.
#' @param resolution Grid points per axis. Defaults to 150. The cost grows
#'   with its square.
#' @param palette Two colors, for the negative and the positive class.
#'   Defaults to `viridisLite::viridis(2)`.
#' @param opacity The opacity of the fill. Defaults to 0.18 for `"class"` and
#'   0.65 for `"margin"`.
#' @param show_points Whether to draw the data. Defaults to `TRUE`. See the
#'   section on points.
#' @param legend_pos Where to put the legend. The default, `"top"`, puts it
#'   above the panel; any other [graphics::legend()] keyword puts it inside.
#'   `NULL` leaves it out, along with the color bar. The legend and the color
#'   bar name the classes by the outcome's levels, in title case.
#' @param add_colorbar Whether `shade = "margin"` draws a color bar in place of
#'   the legend. Defaults to `TRUE`.
#' @param main,subtitle An optional title and gray subtitle above the panel.
#' @param xlab,ylab Axis labels. `NULL` (the default) uses the feature names.
#' @param file A path to write a PDF to, or `NULL` to draw on the current
#'   device.
#' @param width,height,pointsize Passed to [grDevices::pdf()]; ignored when
#'   `file` is `NULL`. They default to the full text width.
#' @param ... Passed to [graphics::plot()].
#'
#' @return Invisibly, a list with the grid and the error count: the two axis
#'   sequences `x1` and `x2`, the matrix of scores `z`, the feature names
#'   `features`, the number of misclassified points `n_misclassified`, and
#'   the number of points with an observed class `n_points`. The counts are
#'   taken even with `show_points = FALSE`. They are `NA` when `data` has no
#'   outcome column, or when its outcome is not a two-level factor, 0/1, -1/1,
#'   or logical.
#'
#' @family tutorial plots
#' @seealso [tutplot] for the other figures.
#'
#' @examples
#' data(altmejd_splits)
#' train <- altmejd_splits$train
#'
#' # Figure 3: the stump of Figure 2, drawn as the boundary it makes
#' h <- rpart::rpart(
#'   replicate ~ power.o + n.o,
#'   data = train,
#'   method = "class",
#'   maxdepth = 1
#' )
#' grid <- tutplot_boundary(h, train, resolution = 60)
#' grid$features
#' # the points the stump puts in the wrong class, and all points
#' grid$n_misclassified
#' grid$n_points
#'
#' # flat tints instead of the margin scale
#' tutplot_boundary(h, train, resolution = 60, shade = "class")
#'
#' @keywords internal
#' @export
tutplot_boundary <- function(
  fit,
  data,
  features = NULL,
  shade = "margin",
  resolution = 150,
  palette = NULL,
  opacity = NULL,
  show_points = TRUE,
  legend_pos = "top",
  add_colorbar = TRUE,
  main = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  file = NULL,
  width = tutplot_opts$full[["width"]],
  height = tutplot_opts$full[["height"]],
  pointsize = tutplot_opts$pointsize,
  ...
) {
  # scalar default: a vector would make formals() contradict the docs
  shade <- match.arg(shade, c("class", "margin"))
  check_df(data)
  if (resolution < 2L) {
    stop("`resolution` must be at least 2.", call. = FALSE)
  }

  roles <- read_variables(fit)
  predictors <- roles$predictors
  if (is.null(features)) {
    if (length(predictors) != 2L) {
      stop(
        "`features` must name two columns: this model has ",
        length(predictors),
        " predictors (",
        paste(predictors, collapse = ", "),
        "). Pick the two to plot rather than leaving it to be guessed.",
        call. = FALSE
      )
    }
    features <- predictors
  }
  if (length(features) != 2L) {
    stop("`features` must name exactly two columns.", call. = FALSE)
  }
  missing_columns <- setdiff(features, names(data))
  if (length(missing_columns)) {
    stop(
      "column(s) not found in `data`: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  make_axis <- function(feature) {
    seq(
      min(data[[feature]], na.rm = TRUE),
      max(data[[feature]], na.rm = TRUE),
      length.out = resolution
    )
  }
  x1 <- make_axis(features[1])
  x2 <- make_axis(features[2])

  # expand.grid varies x1 fastest, so nrow = length(x1) gives the layout
  # image() and contour() expect
  grid <- expand.grid(stats::setNames(list(x1, x2), features))
  # other predictors, if `features` was given, sit at their median
  for (predictor in setdiff(predictors, features)) {
    grid[[predictor]] <- stats::median(data[[predictor]], na.rm = TRUE)
  }
  z <- matrix(score_grid(fit, grid), nrow = resolution)

  undivided <- all(z > 0, na.rm = TRUE) || all(z <= 0, na.rm = TRUE)
  if (undivided) {
    warning(
      "the model assigns one class over the whole plotting window, so there ",
      "is no boundary to draw.",
      call. = FALSE
    )
  }

  labels <- read_class_labels(data, roles$outcome)
  if (!is.null(labels)) {
    # title case like every other label the figures draw
    labels <- tools::toTitleCase(labels)
  }

  # counted whenever the outcome is two classes, drawn or not, for the caption
  n_misclassified <- NA_integer_
  n_points <- NA_integer_
  misclassified <- NULL
  if (!is.null(labels)) {
    misclassified <- find_misclassified(fit, data, roles$outcome)
  }
  if (!is.null(misclassified)) {
    n_misclassified <- sum(misclassified, na.rm = TRUE)
    n_points <- sum(!is.na(misclassified))
  }
  if (show_points && !is.null(labels)) {
    # stops for an outcome that is not two classes, as it did before the count
    y <- as_binary(data[[roles$outcome]])
  }

  if (is.null(palette)) {
    # the scale's two ends are the classes; nothing in between is claimed
    palette <- viridisLite::viridis(2)
  }
  if (is.null(opacity)) {
    # a flat tint only has to hint; a ramp has to be read, so it needs weight
    opacity <- if (shade == "class") 0.18 else 0.65
  }

  # size the top margin from what is drawn: legend, sub-title and title
  top_margin <- 1.0 +
    (if (!is.null(legend_pos)) 1.9 else 0) +
    (if (!is.null(subtitle)) 1.2 else 0) +
    (if (!is.null(main)) 1.4 else 0)
  # after the checks and the grid, so a rejected argument writes no file
  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  old_par <- graphics::par(mar = c(4.4, 4.8, top_margin, 1.4) + 0.1)
  # run before the dev.off() above, so par() is restored on the right device
  on.exit(graphics::par(old_par), add = TRUE, after = FALSE)

  # no box, no default axes: they are drawn below in gray
  graphics::plot(
    NULL,
    xlim = range(x1),
    ylim = range(x2),
    axes = FALSE,
    xlab = "",
    ylab = "",
    ...
  )

  # useRaster: one image instead of resolution^2 rectangles, so no white
  # seams and a pdf about 14 times smaller; needs make_axis()'s equal grid
  if (shade == "class") {
    # zlim ties 0 and 1 to the two colors: a window of one class is a constant
    # matrix, which image() would otherwise draw in the first color
    graphics::image(
      x1,
      x2,
      (z > 0) * 1,
      col = grDevices::adjustcolor(palette, alpha.f = opacity),
      zlim = c(0, 1),
      add = TRUE,
      useRaster = TRUE
    )
  } else {
    # the whole viridis scale, centered on zero: the classes sit at its ends,
    # uncertainty in the middle; the ends reach as far as the score can
    reach <- find_reach(fit, z)
    n_colors <- 64L
    breaks <- seq(-reach, reach, length.out = n_colors + 1L)
    ramp <- viridisLite::viridis(n_colors)
    # nudge the extremes inside: a pure leaf sits exactly on an outer break
    nudge <- (2 * reach) / (2 * n_colors)
    clamped <- pmin(pmax(z, -reach + nudge), reach - nudge)
    graphics::image(
      x1,
      x2,
      clamped,
      col = grDevices::adjustcolor(ramp, alpha.f = opacity),
      breaks = breaks,
      add = TRUE,
      useRaster = TRUE
    )
  }

  # no zero line to draw, and contour() warns on a flat surface
  if (!undivided) {
    graphics::contour(
      x1,
      x2,
      z,
      levels = 0,
      add = TRUE,
      drawlabels = FALSE,
      lwd = 1.0,
      col = "grey15"
    )
  }

  graphics::axis(
    1,
    col = NA,
    col.ticks = "grey70",
    col.axis = "grey30",
    cex.axis = 0.85
  )
  graphics::axis(
    2,
    col = NA,
    col.ticks = "grey70",
    col.axis = "grey30",
    cex.axis = 0.85,
    las = 1
  )
  graphics::mtext(
    if (is.null(xlab)) features[1] else xlab,
    side = 1,
    line = 2.7,
    cex = 0.85,
    col = "grey30"
  )
  graphics::mtext(
    if (is.null(ylab)) features[2] else ylab,
    side = 2,
    line = 3.5,
    cex = 0.85,
    col = "grey30"
  )
  if (!is.null(main)) {
    graphics::mtext(
      main,
      3,
      line = top_margin - 1.5,
      adj = 0,
      font = 2,
      cex = 0.95
    )
  }
  if (!is.null(subtitle)) {
    # above the strip that holds the legend or the color bar
    graphics::mtext(
      subtitle,
      3,
      line = if (is.null(legend_pos)) 0.5 else strip_layout$subtitle,
      adj = 0,
      cex = 0.78,
      col = "grey45"
    )
  }

  if (show_points && !is.null(labels)) {
    look <- style_points(y, misclassified, palette)
    # one call, misclassified first, so the correct points are drawn on top
    drawn <- order(!misclassified)
    graphics::points(
      data[[features[1]]][drawn],
      data[[features[2]]][drawn],
      pch = 21,
      cex = look$cex[drawn],
      lwd = look$lwd[drawn],
      col = look$ring[drawn],
      bg = look$fill[drawn]
    )
  }

  if (shade == "class" && !is.null(legend_pos) && !is.null(labels)) {
    legend_args <- list(
      legend = rev(labels),
      pch = 21,
      pt.bg = rev(palette),
      col = rev(choose_ink(palette)),
      pt.lwd = 1.1,
      pt.cex = 1.2,
      bty = "n",
      cex = 0.8,
      text.col = "grey25",
      xpd = NA
    )
    if (identical(legend_pos, "top")) {
      # centered in the margin above the panel, so it never covers a study
      usr <- graphics::par("usr")
      legend_args$x <- (usr[1] + usr[2]) / 2
      # one row, so the box's middle is the text's line
      legend_args$y <- find_strip_y(strip_layout$legend)
      legend_args$xjust <- 0.5
      legend_args$yjust <- 0.5
      legend_args$horiz <- TRUE
      # each key its own width; `horiz` otherwise pads all to the widest label
      legend_args$text.width <- NA
      legend_args$x.intersp <- 0.7
    } else {
      legend_args$x <- legend_pos
    }
    do.call(graphics::legend, legend_args)
  }

  if (shade == "margin" && isTRUE(add_colorbar) && !is.null(legend_pos)) {
    # the ramp's ends are the classes, so the bar is the legend
    draw_colorbar(opacity, labels)
  }

  invisible(list(
    x1 = x1,
    x2 = x2,
    z = z,
    features = features,
    n_misclassified = n_misclassified,
    n_points = n_points
  ))
}

#' Read predictor and outcome names off a fitted model
#' @noRd
read_variables <- function(fit) {
  # the stored formula may be `outcome ~ .`; the fitted trees carry the names
  model_terms <- if (inherits(fit, "rpart")) {
    fit$terms
  } else if (inherits(fit, "adaboost")) {
    fit[[1]]$h$terms
  } else {
    stop(
      "`fit` must be an rpart object or the output of adaboost().",
      call. = FALSE
    )
  }
  if (is.null(model_terms)) {
    stop(
      "`fit` carries no model terms to read feature names from.",
      call. = FALSE
    )
  }
  list(
    predictors = attr(model_terms, "term.labels"),
    outcome = all.vars(model_terms)[1L]
  )
}

#' Reduce a tree or an ensemble to a signed score whose zero is the boundary
#' @noRd
score_grid <- function(fit, newdata) {
  if (inherits(fit, "rpart")) {
    prob <- stats::predict(fit, newdata = newdata, type = "prob")
    # distance from the 0.5 cut, so zero is the boundary
    return(prob[, ncol(prob)] - 0.5)
  }
  stats::predict(
    fit,
    newdata,
    type = "margin",
    verbose = FALSE,
    check_inputs = FALSE
  )
}

#' Flag the points the model assigns to the other class
#'
#' Reads the sign of the score the surface is drawn from; a positive score is
#' the second class. Returns one logical per row of `data`, `NA` where the
#' outcome is missing, or `NULL` when the outcome is not two classes.
#' @noRd
find_misclassified <- function(fit, data, outcome) {
  # only the outcome may fail quietly; a failing predict() still stops
  observed <- tryCatch(as_binary(data[[outcome]]), error = \(e) NULL)
  if (is.null(observed)) {
    return(NULL)
  }
  predicted <- score_grid(fit, data) > 0
  unname(predicted != (observed == 1L))
}

#' Style each point by its class and by whether the model got it right
#'
#' Correct points get the full class color and a ring that contrasts with
#' it; misclassified ones are paler and smaller. Returns a data frame with one
#' row per point, in the order given, and the columns `fill`, `ring`, `cex`
#' and `lwd`.
#' @noRd
style_points <- function(class, misclassified, palette) {
  fill <- palette[class + 1]
  ring <- choose_ink(palette)[class + 1]
  pale <- !is.na(misclassified) & misclassified
  data.frame(
    fill = ifelse(pale, grDevices::adjustcolor(fill, alpha.f = 0.55), fill),
    ring = ifelse(pale, grDevices::adjustcolor(ring, alpha.f = 0.8), ring),
    cex = ifelse(pale, 1.05, 1.2),
    lwd = ifelse(pale, 0.7, 1.1)
  )
}

#' Lay out the strip above the panel
#'
#' Margin lines up from the panel's top edge. A line follows the text size,
#' not the device, so the strip looks the same at any figure size.
#' @noRd
strip_layout <- list(
  # "Boundary", halfway between the panel and the bar's bottom edge
  label = 0.65,
  # the color bar's bottom and top edges
  bar = c(1.3, 1.83),
  # the class legend's row, level with the bar's top edge
  legend = 1.83,
  # at least 4 pt above the strip's highest ink, the class legend's dots, at
  # pointsize 9; 2.4 left 3.9 pt
  subtitle = 2.41
)

#' Turn margin lines above the panel's top edge into a y in user units
#' @noRd
find_strip_y <- function(lines) {
  usr <- graphics::par("usr")
  # inches per line, then user units per inch
  line_height <- graphics::par("mai")[3] / graphics::par("mar")[3]
  usr[4] + lines * line_height * (usr[4] - usr[3]) / graphics::par("pin")[2]
}

#' Find how far the color scale should reach
#' @noRd
find_reach <- function(fit, z) {
  # a tree's score lies in [-0.5, 0.5], so anchor the scale for a fixed
  # reading; a boosted margin depends on T and eta, so use its observed maximum
  if (inherits(fit, "rpart")) {
    return(0.5)
  }
  max(abs(z), na.rm = TRUE)
}

#' Read the class names for the region labels
#' @noRd
read_class_labels <- function(data, outcome) {
  if (is.null(outcome) || !outcome %in% names(data)) {
    return(NULL)
  }
  values <- data[[outcome]]
  if (is.factor(values)) levels(values) else c("0", "1")
}

#' Draw the margin color bar above the panel
#' @noRd
draw_colorbar <- function(opacity, labels) {
  # centered in the margin above the panel like the class legend; its ends
  # carry the class names, so no caption
  usr <- graphics::par("usr")
  width <- usr[2] - usr[1]
  # 7 pt at pointsize 9, the class legend's size
  cex <- 0.8
  low_label <- if (is.null(labels)) "Negative" else labels[1]
  high_label <- if (is.null(labels)) "Positive" else labels[2]
  center <- (usr[1] + usr[2]) / 2
  gap <- 0.015 * width
  # shorter on a narrow device or at a large pointsize, so neither end label
  # leaves the panel; the same room on both sides keeps the bar centered
  label_width <- max(graphics::strwidth(c(low_label, high_label), cex = cex))
  half <- min(0.18 * width, width / 2 - gap - label_width)
  left <- center - half
  right <- center + half
  bottom <- find_strip_y(strip_layout$bar[1])
  top <- find_strip_y(strip_layout$bar[2])

  # one image, not 64 rectangles, so no seams show between the colors; the
  # panel's 64 colors, so the bar matches its shading
  ramp <- grDevices::adjustcolor(viridisLite::viridis(64L), alpha.f = opacity)
  graphics::rasterImage(
    grDevices::as.raster(matrix(ramp, nrow = 1L)),
    left,
    bottom,
    right,
    top,
    interpolate = FALSE,
    xpd = NA
  )
  graphics::rect(
    left,
    bottom,
    right,
    top,
    border = "grey65",
    lwd = 0.6,
    xpd = NA
  )

  # the scale is centered on zero, so the vote tips at the bar's middle; flat
  # ends, so the mark stops at the bar's edges
  middle <- (left + right) / 2
  graphics::segments(
    middle,
    bottom,
    middle,
    top,
    lwd = 1,
    lend = "butt",
    col = "grey15",
    xpd = NA
  )
  # under the tick, halfway between the bar and the panel
  graphics::text(
    middle,
    find_strip_y(strip_layout$label),
    "Boundary",
    cex = cex,
    col = "grey25",
    xpd = NA
  )

  graphics::text(
    left - gap,
    (bottom + top) / 2,
    low_label,
    adj = c(1, 0.5),
    cex = cex,
    col = "grey25",
    xpd = NA
  )
  graphics::text(
    right + gap,
    (bottom + top) / 2,
    high_label,
    adj = c(0, 0.5),
    cex = cex,
    col = "grey25",
    xpd = NA
  )
}
