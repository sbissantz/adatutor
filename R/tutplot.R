#' Redraw the tutorial's manuscript figures
#'
#' Draws a figure from the AMPPS tutorial. The code lives in the package, not
#' in a vignette, so every printed figure is documented and tested.
#'
#' * `tutplot_cstump()`: a classification stump.
#' * `tutplot_gini()`: Gini impurity by class proportion.
#' * `tutplot_updatefactor()`: the factor by which one round multiplies an
#'   observation's weight.
#' * `tutplot_importance()`: the model weight a stump earns, by its
#'   performance.
#' * `tutplot_weightone()`: observation weights before and after one round.
#' * `tutplot_logoscheme()`: the leave-one-group-out cross-validation scheme.
#' * `tutplot_logocv()`: the leave-one-group-out estimates per project, with
#'   or without bootstrap intervals.
#'
#' @section Output:
#' With `file = NULL` (the default), the figure is drawn on the current
#' device. With a path, a PDF is written at the manuscript's size. `width`,
#' `height` and `pointsize` apply only then; raise `pointsize` when you
#' enlarge `width`. A session without a device, such as `Rscript`, writes
#' `Rplots.pdf`, which is ordinary R behavior.
#'
#' To draw a weighted stump, pass the fit that was grown with the weights; the
#' weights travel with the fit.
#'
#' @param fit A classification stump from [rpart::rpart()].
#' @param extra Node labels, passed to `rpart.plot::rpart.plot()`. The
#'   manuscript uses 102, and 100 for the weighted stump.
#' @param file A path to write a PDF to, or `NULL` to draw on the current
#'   device.
#' @param x_seq The class proportions at which to evaluate the Gini curve,
#'   between 0 and 1.
#' @param lwd The line width.
#' @param alpha The model weights, one curve each.
#' @param pch The plotting symbols for the end points, recycled over `alpha`.
#' @param lty The line types, recycled over the curves.
#' @param ylim The vertical range. Defaults to the manuscript's.
#' @param eta The learning rates, one curve each.
#' @param from Where performance starts. The default, 0.5, is chance.
#' @param marked_perf A performance to mark on the figure, so you can find your
#'   own stump. `NULL` draws no mark. Must be at least `from` and below 1.
#' @param marked_eta The curve the mark belongs to. Defaults to the first.
#' @param data The training data for the first boosting round, used when `d1`
#'   and `d2` are `NULL`.
#' @param d1,d2 The observation weights before and after one round. `NULL`
#'   computes them from `data`.
#' @param chi `+1` where the learner was right and `-1` where it was wrong,
#'   like the tutorial's `chi1`. Required when you pass `d1` or `d2`, because
#'   the weights alone cannot show which points were missed.
#' @param n The number of data points to draw. 25 fits a column.
#' @param scaling The bubble size multiplier.
#' @param group One project label per row, as [logo_cv()] takes it.
#' @param proportional Whether to size the rows by project. `TRUE` (the
#'   default) shows the imbalance: one project holds 59 percent of the rows.
#'   `FALSE` draws the equal blocks the manuscript prints.
#' @param levels The project order, top to bottom. `NULL` uses a factor's
#'   levels or the order of first appearance.
#' @param res A result of [logo_cv()].
#' @param bootstrap Whether to draw bootstrap intervals. `FALSE` draws the
#'   estimates only, without resampling. `TRUE` (the default) uses the
#'   intervals attached by [bootstrap()], or resamples when there are none;
#'   set the seed first. These are percentile intervals within each project,
#'   not standard errors.
#' @param n_resample The number of resamples per project, when `bootstrap`
#'   has to resample. Defaults to 1000.
#' @param metric The measure to draw. Defaults to `"auroc"`.
#' @param bands The interval widths, widest last. Defaults to 0.50, 0.80 and
#'   0.95.
#' @param ylab The axis label. Defaults to `metric` in capitals.
#' @param width,height,pointsize Passed to [grDevices::pdf()]; ignored when
#'   `file` is `NULL`.
#'
#' @return Invisibly, the facts the figure's caption states, so tests can
#'   check them. `tutplot_cstump()` returns the fit, the split variable, the
#'   cutpoint and the leaf sizes; `tutplot_gini()` the curve and its peak;
#'   `tutplot_logoscheme()` the project sizes, their count, the order and the
#'   row heights; `tutplot_logocv()` one row per project with its estimate,
#'   the widest interval, and whether too many resamples were rejected.
#'
#' @family tutorial plots
#' @seealso [tutplot_boundary()] for Figures 3 and 9.
#'
#' @examples
#' data(altmejd_splits)
#' train <- altmejd_splits$train
#'
#' # the stump behind Figure 2, on two reviewer metrics so Figure 3 can draw
#' # its boundary in two dimensions
#' h <- rpart::rpart(
#'   replicate ~ power.o + n.o,
#'   data = train,
#'   method = "class",
#'   maxdepth = 1
#' )
#' facts <- tutplot_cstump(h)
#' facts$variable
#' facts$leaves
#'
#' # Figure 4 needs no model: it is arithmetic
#' gini <- tutplot_gini()
#' gini$peak
#'
#' # Figure 1: the LOGO-CV scheme, drawn from the grouping itself
#' data(altmejd)
#' folds <- tutplot_logoscheme(altmejd$pid)
#' folds$n
#'
#' @name tutplot
#' @keywords internal
NULL

#' @rdname tutplot
#' @export
tutplot_cstump <- function(
  fit,
  extra = 102,
  file = NULL,
  width = tutplot_opts$col[["width"]],
  height = tutplot_opts$col[["height"]],
  pointsize = tutplot_opts$pointsize
) {
  if (!inherits(fit, "rpart")) {
    stop("`fit` must be an rpart object.", call. = FALSE)
  }

  # color-blind palette from the fit's own class proportions
  colors <- viridis_tree(fit)

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  rpart.plot::rpart.plot(
    fit,
    extra = extra,
    digits = 4,
    box.col = colors$box,
    col = colors$text
  )

  # what the caption claims, for tests; a stump that never split has no
  # `splits` matrix
  terminal <- fit$frame$var == "<leaf>"
  cutpoint <- if (is.null(fit$splits)) NULL else fit$splits[1, "index"]

  invisible(list(
    fit = fit,
    variable = as.character(fit$frame$var[1]),
    cutpoint = cutpoint,
    leaves = fit$frame$n[terminal]
  ))
}

#' @rdname tutplot
#' @export
tutplot_gini <- function(
  x_seq = seq(0.01, 1, by = 0.01),
  lwd = 2,
  file = NULL,
  width = tutplot_opts$col[["width"]],
  height = tutplot_opts$col[["height"]],
  pointsize = tutplot_opts$pointsize
) {
  # a proportion of one class: the same range check the splitters use
  check_numeric(x_seq)
  check_prop(x_seq)

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  old_par <- set_figure_par(reserve_legend = FALSE)
  on.exit(graphics::par(old_par), add = TRUE)

  # a single curve, so one color rather than a scale
  color <- viridisLite::viridis(1, end = tutplot_opts$viridis_end)

  proportion <- x_seq
  impurity <- 1 - (proportion^2) - (1 - proportion)^2

  # empty frame, grid, then curve, so the grid lies under the curve
  plot(
    NULL,
    xlim = range(proportion),
    ylim = range(impurity),
    xlab = "Proportion of Successes",
    ylab = "Impurity"
  )
  graphics::grid()
  graphics::lines(proportion, impurity, lwd = lwd, col = color)
  graphics::axis(side = 1, at = seq(0, 1, by = 0.1))

  invisible(list(
    x = proportion,
    y = impurity,
    peak = proportion[which.max(impurity)]
  ))
}

#' @rdname tutplot
#' @export
tutplot_updatefactor <- function(
  alpha = c(0.719, 0.619, 0.519),
  pch = c(20, 17, 15),
  lty = 3,
  lwd = 1,
  ylim = c(0.3, 2.2),
  file = NULL,
  width = tutplot_opts$col[["width"]],
  height = tutplot_opts$col[["height"]],
  pointsize = tutplot_opts$pointsize
) {
  check_numeric(alpha)

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  old_par <- set_figure_par(reserve_legend = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  # same ordered palette as the eta curves: model weights are ordered too
  colors <- viridisLite::viridis(length(alpha), end = tutplot_opts$viridis_end)
  pch <- rep_len(pch, length(alpha))

  # chi is +1 where the learner was right, -1 where wrong
  rescale_weight <- function(alpha, chi) exp(-alpha * chi)

  # empty frame first, so the grid lies under every curve
  plot(
    NULL,
    xlim = c(-1, 1),
    ylim = ylim,
    xlab = "Classification",
    ylab = "Weight Update Factor",
    xaxt = "n"
  )
  graphics::grid()

  # one alpha per curve, and its two end points read from that same alpha
  for (i in seq_along(alpha)) {
    # curve() evaluates its expression in `x`
    graphics::curve(
      rescale_weight(alpha[i], x),
      from = -1,
      to = 1,
      col = colors[i],
      lty = lty,
      lwd = lwd,
      add = TRUE
    )
    graphics::points(
      c(1, -1),
      c(rescale_weight(alpha[i], 1), rescale_weight(alpha[i], -1)),
      pch = pch[i],
      cex = 2,
      col = colors[i]
    )
  }

  # a factor of one is break-even: below it a weight shrinks, above it grows
  graphics::abline(h = 1, lty = 2, lwd = 1.5, col = "gray30")

  # built from `alpha`, so a label cannot outlive the curve it names
  draw_legend(
    as.expression(lapply(
      alpha,
      \(value) bquote(a[t] * " = " * .(format(value)))
    )),
    cex = 1,
    col = colors,
    lty = lty,
    lwd = lwd,
    pch = pch
  )
  graphics::axis(1, at = c(-1, 1))

  invisible(list(
    alpha = alpha,
    correct = rescale_weight(alpha, 1),
    wrong = rescale_weight(alpha, -1)
  ))
}

#' @rdname tutplot
#' @export
tutplot_importance <- function(
  eta = c(1, 0.5, 0.1),
  lty = c(1, 2, 4, 5),
  lwd = 2,
  from = 0.5,
  marked_perf = NULL,
  marked_eta = eta[1],
  file = NULL,
  width = tutplot_opts$col[["width"]],
  height = tutplot_opts$col[["height"]],
  pointsize = tutplot_opts$pointsize
) {
  check_numeric(eta)
  if (!is.null(marked_perf)) {
    check_numeric(marked_perf)
    # refuse an invisible mark: below `from` it is off the panel, at 1 infinite
    if (length(marked_perf) != 1L || marked_perf < from || marked_perf >= 1) {
      stop(
        "`marked_perf` must be one value in [",
        from,
        ", 1). Got ",
        paste(format(marked_perf), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  old_par <- set_figure_par(reserve_legend = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  # ordered learning rates, so the palette runs with them
  colors <- viridisLite::viridis(length(eta), end = tutplot_opts$viridis_end)
  lty <- rep_len(lty, length(eta))

  # a stump's importance as a function of its performance
  weigh <- function(accuracy, eta) 1 / 2 * log(accuracy / (1 - accuracy)) * eta

  # stop short of the asymptote at 1: R drops the one infinite value
  performance <- seq(from, 1, length.out = 101)
  importance <- vapply(
    eta,
    \(rate) weigh(performance, rate),
    numeric(length(performance))
  )

  # size the panel from every curve; the first alone clips ascending rates
  plot(
    NULL,
    xlim = c(from, 1),
    ylim = range(importance[is.finite(importance)]),
    xlab = "Performance",
    ylab = "Importance"
  )
  graphics::grid()

  # the reader's own learner, drawn first so the curves stay on top; segments
  # stop at the crosshair instead of crossing every curve
  marked_alpha <- NULL
  if (!is.null(marked_perf)) {
    marked_alpha <- weigh(marked_perf, marked_eta)
    graphics::segments(
      marked_perf,
      0,
      marked_perf,
      marked_alpha,
      lty = 2,
      col = "gray30"
    )
    graphics::segments(
      from,
      marked_alpha,
      marked_perf,
      marked_alpha,
      lty = 2,
      col = "gray30"
    )
    graphics::points(
      marked_perf,
      marked_alpha,
      pch = 19,
      cex = 1.1,
      col = "gray20"
    )
  }

  for (i in seq_along(eta)) {
    graphics::lines(
      performance,
      importance[, i],
      col = colors[i],
      lty = lty[i],
      lwd = lwd
    )
  }

  # legend in the top margin (`xpd = NA`), so it never covers a curve, built
  # from `eta`; `text.width = NA` stops `horiz` padding entries to the widest
  draw_legend(
    as.expression(lapply(
      eta,
      \(rate) bquote(eta * " = " * .(format(rate)))
    )),
    cex = 0.9,
    col = colors,
    lty = lty,
    lwd = lwd
  )

  invisible(list(
    eta = eta,
    performance = performance,
    importance = importance,
    alpha = marked_alpha
  ))
}

# one round of AdaBoost on `data`, repeating Listings 17--22, so
# tutplot_weightone() needs nothing from the reader's session
run_round_one <- function(
  data,
  predictors = c("power.o", "effect_size.o", "n.o", "p_value.o")
) {
  # reviewer metrics only: `eid` identifies every row, so it would give zero
  # error, infinite alpha and NaN weights
  data <- data[, c(predictors, "replicate")]

  y <- data[["replicate"]]
  m <- nrow(data)

  # equal weights to begin with
  d1 <- rep(1, m) / m

  h1 <- rpart::rpart(
    replicate ~ .,
    data = data,
    method = "class",
    maxdepth = 1,
    maxsurrogate = 0,
    weights = d1
  )
  y_retro <- stats::predict(h1, newdata = data, type = "class")

  # the learner's weighted error, and the say it earns
  e1 <- sum(d1 * (y_retro != y))
  alpha1 <- 0.5 * log((1 - e1) / e1)

  # +1 where it was right, -1 where it was wrong
  chi1 <- (y_retro == y) * 1 + (y_retro != y) * -1
  d2_raw <- d1 * exp(-alpha1 * chi1)

  list(
    d1 = d1,
    d2 = d2_raw / sum(d2_raw),
    chi = chi1,
    fit = h1,
    alpha = alpha1
  )
}

#' @rdname tutplot
#' @export
tutplot_weightone <- function(
  data = altmejd_splits$train,
  d1 = NULL,
  d2 = NULL,
  chi = NULL,
  n = 25,
  scaling = 2,
  ylim = NULL,
  file = NULL,
  width = tutplot_opts$col[["width"]],
  height = tutplot_opts$col[["height"]],
  pointsize = tutplot_opts$pointsize
) {
  if (is.null(d1) || is.null(d2) || is.null(chi)) {
    round_one <- run_round_one(data)
    if (is.null(d1)) {
      d1 <- round_one$d1
    }
    if (is.null(d2)) {
      d2 <- round_one$d2
    }
    # only without caller weights: this round's `chi` would not describe theirs
    if (
      is.null(chi) && identical(d1, round_one$d1) && identical(d2, round_one$d2)
    ) {
      chi <- round_one$chi
    }
  }
  check_numeric(d1)
  check_numeric(d2)
  if (length(d1) != length(d2)) {
    stop("`d1` and `d2` must be the same length.", call. = FALSE)
  }
  # weights alone cannot say which points were missed (the grown set is the
  # minority either way), so weights need a `chi`
  if (is.null(chi)) {
    stop(
      "`chi` is required when `d1` or `d2` is supplied. Weights cannot say ",
      "which points were missed: a round grows the misclassified ones only ",
      "while the learner beats chance.",
      call. = FALSE
    )
  }
  check_numeric(chi)
  if (length(chi) != length(d1) || !all(chi %in% c(-1, 1))) {
    stop(
      "`chi` must be +1 or -1, one per weight. Got ",
      length(chi),
      " value(s) for ",
      length(d1),
      " weights.",
      call. = FALSE
    )
  }
  n <- min(n, length(d1))

  missed <- which(chi[seq_len(n)] == -1)

  if (is.null(ylim)) {
    ylim <- c(0, max(c(d1, d2)) * 1.15)
  }

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  old_par <- set_figure_par(reserve_legend = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  # `end = 0.5` keeps d2 teal, not a yellow-green that muddies on the band
  colors <- viridisLite::viridis(2, end = 0.5)
  band <- grDevices::adjustcolor(viridisLite::viridis(1, begin = 1), 0.35)

  # scale by area, not diameter: sqrt keeps a doubled weight at double the ink
  size_bubble <- function(weights) sqrt(weights / mean(d1)) * scaling

  plot(
    c(0.5, n + 0.5),
    ylim,
    type = "n",
    xaxt = "n",
    xlab = "Data Point",
    ylab = "Weight"
  )
  # band behind everything: a miss belongs to the point, not to a weight
  graphics::rect(
    missed - 0.42,
    ylim[1],
    missed + 0.42,
    ylim[2],
    col = band,
    border = NA
  )
  graphics::box()

  # every fifth point: 25 labels do not fit the width of a column
  graphics::axis(1, at = c(1, seq(5, n, 5)))
  graphics::segments(
    seq_len(n),
    d1[seq_len(n)],
    seq_len(n),
    d2[seq_len(n)],
    lty = 3,
    col = "grey45"
  )

  for (i in seq_len(2)) {
    weights <- list(d1, d2)[[i]]
    graphics::points(
      seq_len(n),
      weights[seq_len(n)],
      pch = 21,
      bg = colors[i],
      col = "white",
      cex = size_bubble(weights[seq_len(n)]),
      lwd = 1.3
    )
  }

  draw_legend(
    c(expression(D[1]), expression(D[2]), "misclassified"),
    cex = 0.9,
    pt.bg = c(colors, band),
    col = c("white", "white", "grey60"),
    pch = c(21, 21, 22),
    pt.cex = 1.3
  )

  invisible(list(d1 = d1, d2 = d2, chi = chi, wrong = missed, n = n))
}

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
#' compare within a plot but not across plots.
#'
#' A stump has one split, so it shows two flat blocks; an ensemble shows a
#' surface. That contrast is what boosting buys.
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
#' @param alpha The opacity of the fill. Defaults to 0.18 for `"class"` and
#'   0.65 for `"margin"`.
#' @param show_points Whether to draw the data. Defaults to `TRUE`.
#' @param legend_pos Where to put the legend. The default, `"top"`, puts it
#'   above the panel; any other [graphics::legend()] keyword puts it inside.
#'   `NULL` leaves it out.
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
#' @return Invisibly, the grid: the two axis sequences, the matrix of scores
#'   and the feature names.
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
  alpha = NULL,
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
  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  check_df(data)
  if (resolution < 2L) {
    stop("`resolution` must be at least 2.", call. = FALSE)
  }

  roles <- read_terms(fit)
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

  if (all(z > 0, na.rm = TRUE) || all(z <= 0, na.rm = TRUE)) {
    warning(
      "the model assigns one class over the whole plotting window, so there ",
      "is no boundary to draw.",
      call. = FALSE
    )
  }

  if (is.null(palette)) {
    # the scale's two ends are the classes; nothing in between is claimed
    palette <- viridisLite::viridis(2)
  }
  if (is.null(alpha)) {
    # a flat tint only has to hint; a ramp has to be read, so it needs weight
    alpha <- if (shade == "class") 0.18 else 0.65
  }

  # size the top margin from what is drawn: legend, sub-title and title
  top_margin <- 1.0 +
    (if (!is.null(legend_pos)) 1.9 else 0) +
    (if (!is.null(subtitle)) 1.2 else 0) +
    (if (!is.null(main)) 1.4 else 0)
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
    graphics::image(
      x1,
      x2,
      (z > 0) * 1,
      col = grDevices::adjustcolor(palette, alpha.f = alpha),
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
      col = grDevices::adjustcolor(ramp, alpha.f = alpha),
      breaks = breaks,
      add = TRUE,
      useRaster = TRUE
    )
  }

  graphics::contour(
    x1,
    x2,
    z,
    levels = 0,
    add = TRUE,
    drawlabels = FALSE,
    lwd = 1.6,
    col = "grey15"
  )

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
    graphics::mtext(
      subtitle,
      3,
      line = 0.5,
      adj = 0,
      cex = 0.78,
      col = "grey45"
    )
  }

  labels <- read_class_labels(data, roles$outcome)

  if (show_points && !is.null(labels)) {
    y <- as_binary(data[[roles$outcome]])
    # full-strength, ringed endpoints keep the studies the most saturated marks
    stroke <- choose_ink(palette)
    graphics::points(
      data[[features[1]]],
      data[[features[2]]],
      pch = 21,
      cex = 1.2,
      lwd = 1.1,
      col = stroke[y + 1],
      bg = palette[y + 1]
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
      legend_args$y <- usr[4] + 0.055 * (usr[4] - usr[3])
      legend_args$xjust <- 0.5
      legend_args$yjust <- 0
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
    draw_colorbar(alpha, labels)
  }

  invisible(list(x1 = x1, x2 = x2, z = z, features = features))
}

#' Read predictor and outcome names off a fitted model
#' @noRd
read_terms <- function(fit) {
  # the stored formula may be `outcome ~ .`; the fitted trees carry the names
  model_terms <- if (inherits(fit, "rpart")) {
    fit$terms
  } else if (is.list(fit) && !is.null(fit[[1]]$h)) {
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
  predict(fit, newdata, type = "margin", verbose = FALSE, check_inputs = FALSE)
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
draw_colorbar <- function(alpha, labels) {
  # centered in the margin above the panel like the class legend; its ends
  # carry the class names, so no caption
  usr <- graphics::par("usr")
  width <- usr[2] - usr[1]
  height <- usr[4] - usr[3]
  left <- (usr[1] + usr[2]) / 2 - 0.18 * width
  right <- (usr[1] + usr[2]) / 2 + 0.18 * width
  bottom <- usr[4] + 0.045 * height
  top <- bottom + 0.028 * height

  n_colors <- 64L
  edges <- seq(left, right, length.out = n_colors + 1L)
  graphics::rect(
    utils::head(edges, -1),
    bottom,
    utils::tail(edges, -1),
    top,
    col = grDevices::adjustcolor(
      viridisLite::viridis(n_colors),
      alpha.f = alpha
    ),
    border = NA,
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

  low_label <- if (is.null(labels)) "negative" else labels[1]
  high_label <- if (is.null(labels)) "positive" else labels[2]
  graphics::text(
    left - 0.015 * width,
    (bottom + top) / 2,
    low_label,
    adj = c(1, 0.5),
    cex = 0.72,
    col = "grey25",
    xpd = NA
  )
  graphics::text(
    right + 0.015 * width,
    (bottom + top) / 2,
    high_label,
    adj = c(0, 0.5),
    cex = 0.72,
    col = "grey25",
    xpd = NA
  )
}

#' @rdname tutplot
#' @export
tutplot_logocv <- function(
  res,
  bootstrap = TRUE,
  n_resample = 1000,
  metric = "auroc",
  bands = c(0.50, 0.80, 0.95),
  ylab = toupper(metric),
  file = NULL,
  width = tutplot_opts$col[["width"]],
  height = tutplot_opts$col[["height"]],
  pointsize = tutplot_opts$pointsize
) {
  if (!inherits(res, "logo_cv")) {
    stop("`res` must be a result of logo_cv().", call. = FALSE)
  }
  if (!bootstrap) {
    res$ci <- NULL
  } else if (is.null(res$ci)) {
    # the method, not the generic: the argument `bootstrap` shadows its name
    res <- bootstrap.logo_cv(res, n_resample = n_resample)
  }

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  old_par <- set_figure_par(reserve_legend = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  # no title and no footnote: the caption names the figure and the flags
  facts <- draw_logo_cv(
    res,
    metric = metric,
    levels = bands,
    main = NULL,
    ylab = ylab,
    add_footnote = FALSE
  )
  invisible(facts)
}

#' @rdname tutplot
#' @export
tutplot_logoscheme <- function(
  group,
  proportional = TRUE,
  levels = NULL,
  file = NULL,
  width = tutplot_opts$full[["width"]],
  height = tutplot_opts$full[["height"]],
  pointsize = tutplot_opts$pointsize
) {
  if (length(group) < 1L) {
    stop("`group` must name the project each row belongs to.", call. = FALSE)
  }
  # keep the caller's order: table() would reorder the manuscript's figure
  if (is.null(levels)) {
    levels <- if (is.factor(group)) base::levels(group) else unique(group)
  }
  unknown_levels <- setdiff(levels, as.character(group))
  if (length(unknown_levels)) {
    stop(
      "`levels` names a project that is not in `group`: ",
      paste(unknown_levels, collapse = ", "),
      call. = FALSE
    )
  }
  sizes <- table(factor(as.character(group), levels = levels))
  n_projects <- length(sizes)
  labels <- toupper(names(sizes))

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  # row heights carry project sizes; equal blocks would imply comparable folds
  heights <- if (proportional) {
    as.numeric(sizes) / sum(sizes)
  } else {
    rep(1 / n_projects, n_projects)
  }
  edges <- cumsum(c(0, heights))
  bottoms <- 1 - edges[-1]
  tops <- 1 - edges[-(n_projects + 1)]
  colors <- viridisLite::viridis(n_projects, end = 0.92)

  # no bottom label: the caption and column headers say it
  old_par <- graphics::par(mar = c(0.4, 2.0, 1.3, 0.3))
  on.exit(graphics::par(old_par), add = TRUE)
  plot(
    NULL,
    xlim = c(-0.62, n_projects + 0.5),
    ylim = c(-0.26, 1.10),
    axes = FALSE,
    xlab = "",
    ylab = "",
    xaxs = "i",
    yaxs = "i"
  )

  # carry the horizontal gap through inches, so it looks the same vertically
  usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  gap <- 0.09
  gap_y <- gap * (usr[4] - usr[3]) / (usr[2] - usr[1]) * pin[1] / pin[2]
  inset_x <- 0.03
  inset_y <- 0.006

  # drop a label whose band is too thin, so the smallest project still fits
  fits <- function(i) (tops[i] - bottoms[i]) > 0.055

  graphics::mtext("Dataset", side = 2, line = 0.7, cex = 0.72, col = "grey25")
  for (i in seq_len(n_projects)) {
    draw_roundrect(
      -0.58,
      bottoms[i] + inset_y,
      0.44,
      tops[i] - inset_y,
      col = grDevices::adjustcolor(colors[i], alpha.f = 0.32)
    )
    if (fits(i)) {
      graphics::text(
        -0.07,
        (bottoms[i] + tops[i]) / 2,
        labels[i],
        cex = 0.7,
        col = "grey15"
      )
    }
  }
  graphics::text(
    seq_len(n_projects),
    1.05,
    sprintf("Iteration %d", seq_len(n_projects)),
    cex = 0.7,
    col = "grey25"
  )

  for (j in seq_len(n_projects)) {
    for (i in seq_len(n_projects)) {
      held_out <- i == j
      # color says which project, opacity whether it is held out
      fill <- if (held_out) {
        colors[i]
      } else {
        grDevices::adjustcolor(colors[i], alpha.f = 0.22)
      }
      draw_roundrect(
        j - 0.5 + inset_x,
        bottoms[i] + inset_y,
        j + 0.5 - inset_x,
        tops[i] - inset_y,
        col = fill
      )
      if (fits(i)) {
        graphics::text(
          j,
          (bottoms[i] + tops[i]) / 2,
          if (held_out) "Test" else "Training",
          col = if (held_out) choose_ink(colors[i]) else "grey30",
          cex = 0.66
        )
      }
    }
    draw_roundrect(
      j - 0.5 + inset_x,
      -gap_y - 0.14,
      j + 0.5 - inset_x,
      -gap_y,
      col = colors[j]
    )
    graphics::text(
      j,
      -gap_y - 0.07,
      sprintf("Performance\n%s", labels[j]),
      col = choose_ink(colors[j]),
      cex = 0.62
    )
  }
  invisible(list(n = sizes, k = n_projects, levels = levels, heights = heights))
}
