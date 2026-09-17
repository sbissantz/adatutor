#' @title Redraw the Tutorial's Manuscript Figures
#'
#' @description Draws a figure from the AMPPS tutorial. These live in the
#'   package, rather than in a vignette that never executes, so that the code
#'   behind every printed figure is documented and under test.
#'
#' @details \strong{Where the output goes.} With the default \code{file = NULL}
#'   nothing opens a device and the figure is drawn on the current one, which is
#'   what an interactive session and the vignette want. Give a path and a
#'   \code{pdf()} device is opened at the size below and closed again on exit.
#'
#'   Note that a headless session (\code{Rscript}, \code{R CMD build},
#'   \code{testthat}) has no current device, so R opens its default one and
#'   writes \code{Rplots.pdf} into the working directory. That is ordinary R
#'   behavior rather than anything these functions do, but it is why the tests
#'   wrap their calls in \code{pdf(NULL)}.
#'
#'   \strong{Size applies only when a file is written.} On the current device
#'   the device decides. \code{width}, \code{height} and \code{pointsize} are
#'   \code{grDevices::pdf()}'s arguments unchanged, defaulting to the
#'   manuscript's own geometry. Enlarging \code{width} without raising
#'   \code{pointsize} leaves the type at 9 point, which is the mistake the
#'   printed sizes are chosen to avoid.
#'
#'   \strong{One figure sets its own margins.} Every function here shares the
#'   margins in \code{\link[adatutor]{tutplot_opts}} except
#'   \code{tutplot_boundary()}, which leaves them to
#'   \code{\link[adatutor]{plot_adabound}}. That function computes its top
#'   margin from what it actually draws -- a legend, a sub-title and a title are
#'   each paid for only if present -- so imposing a fixed \code{mar} would
#'   either crop the legend or leave a gap above it. It is also the only figure
#'   at the manuscript's full text width rather than one column.
#'
#'   \strong{Weights come from the fit.} A fitted \code{rpart} object already
#'   carries the observation weights it was built with, and neither
#'   \code{\link[adatutor]{viridis_tree}} nor \code{rpart.plot::rpart.plot()}
#'   consults them separately. So the weighted stump of the manuscript's
#'   \code{fig:cstumpwght} is drawn by handing this function that fit, not by
#'   passing weights alongside it.
#'
#' @param fit A fitted model. \code{tutplot_cstump()} draws a tree, so it takes
#'   an \code{\link[rpart]{rpart}} stump only. \code{tutplot_boundary()} draws
#'   a decision boundary, which either learner has, so it takes an
#'   \code{rpart} tree or an ensemble from \code{\link[adatutor]{adaboost}}.
#'
#' @param extra Node annotation, passed to \code{rpart.plot::rpart.plot()}. The
#'   manuscript uses \code{102} for \code{fig:cstump} and the simplified
#'   \code{100} for \code{fig:cstumpwght}.
#'
#' @param file Path to write a PDF to, or \code{NULL} to draw on the current
#'   device. A full path rather than a directory, because one function draws
#'   more than one of the manuscript's figures.
#'
#' @param x_seq The proportions the Gini curve is evaluated at. Must lie in
#'   \eqn{[0, 1]}, since it is a proportion of one class. The default steps by
#'   0.01, which is smooth at print size; coarsen it to show the curve as a
#'   series of evaluated points rather than a line.
#'
#' @param lwd Line width of the curve, passed to \code{plot()}.
#'
#' @param alpha One model weight per curve. \code{tutplot_updatefactor()} draws
#'   a curve and its two end points from each, so a marker cannot sit off the
#'   line it belongs to.
#'
#' @param pch Plotting characters for the end points, recycled across
#'   \code{alpha}.
#'
#' @param lty Line type of the curves. The legend keys inherit it, along with
#'   \code{lwd}, so the two cannot disagree. Recycled across the curves, which
#'   is why \code{tutplot_importance()} carries a fourth entry: a reader who
#'   splices their own learning rate into the three reference ones gets four
#'   curves, and three line types would give the first and the fourth the same
#'   one.
#'
#' @param ylim Vertical range. Defaults to the manuscript's, so the default call
#'   reproduces the printed figure; a different \code{alpha} may want another.
#'
#' @param eta One learning rate per curve. \code{tutplot_importance()} sizes its
#'   panel from every curve rather than from the first, so the order they are
#'   given in cannot clip one of them.
#'
#' @param from Where performance starts. The default 0.5 is chance, where a
#'   stump earns no importance at all.
#'
#' @param mark_perf A learner's performance, marked on the figure so a reader
#'   can find their own stump in it. \code{NULL} draws no mark. Must lie in
#'   \eqn{[from, 1)}: at chance the importance is zero, and at perfect
#'   performance it is infinite.
#'
#' @param mark_eta Which curve the mark belongs to. Defaults to the first, which
#'   is the learning rate the tutorial uses.
#'
#' @param data Training data the first boosting round is computed from when
#'   \code{d1} and \code{d2} are left \code{NULL}.
#'
#' @param d1,d2 Observation weights before and after one round of boosting.
#'   \code{NULL} computes them from \code{data}.
#'
#' @param chi Which points the learner got right: \code{+1} where it was
#'   correct, \code{-1} where it was not, as the tutorial's \code{chi1}.
#'   \strong{Required whenever \code{d1} or \code{d2} is supplied.} Weights
#'   alone cannot say which points were missed: a round grows the misclassified
#'   ones only while the learner beats chance, and grows the correctly
#'   classified ones below that. The grown set is the minority either way, so
#'   not even its size tells the two apart.
#'
#' @param n How many data points to draw. 25 is what fits a column.
#'
#' @param scaling Bubble size multiplier.
#'
#' @param data The data frame behind a decision boundary: it sets the plotting
#'   range and supplies the points drawn on top. Required, and required for both
#'   learners -- an ensemble does carry its training data, but an \code{rpart}
#'   tree carries none, and a default that worked for only one of the two
#'   figures would be worse than no default.
#'
#' @param shade,xlab,ylab,... Passed to
#'   \code{\link[adatutor]{plot_adabound}}. \code{shade} defaults to
#'   \code{"margin"} here rather than to that function's own \code{"class"},
#'   because both printed boundaries are shaded by the score: it is what makes
#'   the stump's two flat blocks and the ensemble's many-valued surface
#'   comparable. The axis labels default to \code{NULL}, which draws the
#'   feature names; the manuscript's wording is passed at the call site rather
#'   than baked in, so plotting a different pair of features cannot mislabel
#'   the axes.
#'
#' @param group One project label per row of the data, as
#'   \code{\link[adatutor]{lpocv}} takes it. \code{tutplot_lpocv()} derives
#'   everything from it: one iteration per project, and the block sizes from
#'   how much data each project holds.
#'
#' @param proportional Whether the rows are sized by project. \code{TRUE}, the
#'   default, makes the imbalance visible -- in the tutorial's own data one
#'   project is 59\% of the rows, so its fold trains on 62 studies and tests on
#'   90. \code{FALSE} draws the equal blocks the manuscript prints, which imply
#'   five comparable folds.
#'
#' @param levels The project order, top to bottom. \code{NULL} takes a factor's
#'   own levels, or a character vector's order of appearance. Given explicitly
#'   it fixes the order, which is what the manuscript's figure needs:
#'   \code{table()} would sort alphabetically and silently rearrange it.
#'
#' @param width,height,pointsize Passed to \code{grDevices::pdf()}; ignored when
#'   \code{file} is \code{NULL}.
#'
#' @return Invisibly, the facts the figure's caption states, so a caller can
#'   assert on the numbers rather than on the existence of a file.
#'   \code{tutplot_cstump()} returns the fit, the variable split on, its
#'   cutpoint and the leaf sizes; \code{tutplot_gini()} returns the curve it
#'   drew and the proportion at which impurity peaks;
#'   \code{tutplot_lpocv()} returns the project sizes, their count, the order
#'   drawn and the row heights;
#'   \code{tutplot_boundary()} passes on \code{plot_adabound()}'s grid -- the
#'   two axis sequences, the matrix of scores and the feature names -- so the
#'   surface can be inspected without redrawing it.
#'
#' @examples
#' data(altmejd_splits)
#' train <- altmejd_splits$train
#'
#' # The stump behind Figure 2: two reviewer metrics, so the decision boundary
#' # of Figure 3 can be drawn in two dimensions
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
#' # Figure 4 needs nothing at all -- it is arithmetic, not a model
#' gini <- tutplot_gini()
#' gini$peak
#'
#' # Figure 1: the LPO-CV scheme, drawn from the grouping itself
#' data(altmejd)
#' folds <- tutplot_lpocv(altmejd$pid)
#' folds$n
#'
#' # Figure 3: that same stump's decision boundary. `resolution` is passed
#' # through to plot_adabound(); the printed figure uses its default of 150
#' grid <- tutplot_boundary(h, train, resolution = 60)
#' grid$features
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

  # Color-blind palette, taken from the fit's own class proportions
  sty <- viridis_tree(fit)

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  rpart.plot::rpart.plot(
    fit,
    extra = extra,
    digits = 4,
    box.col = sty$box,
    col = sty$text
  )

  # What the caption claims, so a test can check it. A stump that never split
  # has no `splits` matrix at all, hence the guard.
  leaf <- fit$frame$var == "<leaf>"
  split <- if (is.null(fit$splits)) NULL else fit$splits[1, "index"]

  invisible(list(
    fit = fit,
    variable = as.character(fit$frame$var[1]),
    cutpoint = split,
    leaves = fit$frame$n[leaf]
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
  # a proportion of one class, so the same range check the splitters use
  check_numeric(x_seq)
  check_prop(x_seq)

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  op <- tut_par(legend = FALSE)
  on.exit(graphics::par(op), add = TRUE)

  # a single curve, so one color rather than a scale
  col <- viridisLite::viridis(1, end = tutplot_opts$viridis_end)

  # Gini impurity for a two-class problem
  x <- x_seq
  y <- 1 - (x^2) - (1 - x)^2

  # empty frame, grid, then the curve -- the shape the other figures use, so
  # the grid lies under what it is there to help read
  plot(
    NULL,
    xlim = range(x),
    ylim = range(y),
    xlab = "Proportion of Successes",
    ylab = "Impurity"
  )
  graphics::grid()
  graphics::lines(x, y, lwd = lwd, col = col)
  graphics::axis(side = 1, at = seq(0, 1, by = 0.1))

  invisible(list(x = x, y = y, peak = x[which.max(y)]))
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

  op <- tut_par(legend = TRUE)
  on.exit(graphics::par(op), add = TRUE)

  # same ordered palette as the eta curves: model weights are ordered too
  pal <- viridisLite::viridis(length(alpha), end = tutplot_opts$viridis_end)
  pch <- rep_len(pch, length(alpha))

  # the update factor: chi is +1 where the learner was right, -1 where wrong
  feat <- function(at, x) exp(-at * x)

  # empty frame first, so the grid lies under every curve rather than under all
  # but the one that happened to draw the axes
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
    graphics::curve(
      feat(alpha[i], x),
      from = -1,
      to = 1,
      col = pal[i],
      lty = lty,
      lwd = lwd,
      add = TRUE
    )
    graphics::points(
      c(1, -1),
      c(feat(alpha[i], 1), feat(alpha[i], -1)),
      pch = pch[i],
      cex = 2,
      col = pal[i]
    )
  }

  # a factor of one is break-even: below it a weight shrinks, above it grows
  graphics::abline(h = 1, lty = 2, lwd = 1.5, col = "gray30")

  # built from `alpha`, so a label cannot outlive the curve it names
  tut_legend(
    as.expression(lapply(
      alpha,
      function(a) bquote(a[t] * " = " * .(format(a)))
    )),
    cex = 1,
    col = pal,
    lty = lty,
    lwd = lwd,
    pch = pch
  )
  graphics::axis(1, at = c(-1, 1))

  invisible(list(
    alpha = alpha,
    correct = feat(alpha, 1),
    wrong = feat(alpha, -1)
  ))
}

#' @rdname tutplot
#' @export
tutplot_importance <- function(
  eta = c(1, 0.5, 0.1),
  lty = c(1, 2, 4, 5),
  lwd = 2,
  from = 0.5,
  mark_perf = NULL,
  mark_eta = eta[1],
  file = NULL,
  width = tutplot_opts$col[["width"]],
  height = tutplot_opts$col[["height"]],
  pointsize = tutplot_opts$pointsize
) {
  check_numeric(eta)
  if (!is.null(mark_perf)) {
    check_numeric(mark_perf)
    # below `from` the importance is negative and off the panel; at 1 it is
    # infinite. Refuse rather than draw a mark the reader cannot see.
    if (length(mark_perf) != 1L || mark_perf < from || mark_perf >= 1) {
      msg <- paste0("`mark_perf` must be one value in [", from, ", 1). ")
      sug <- paste0("Got ", paste(format(mark_perf), collapse = ", "), ".")
      stop(c(msg, sug), call. = FALSE)
    }
  }

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  op <- tut_par(legend = TRUE)
  on.exit(graphics::par(op), add = TRUE)

  # ordered learning rates, so the palette runs with them
  pal <- viridisLite::viridis(length(eta), end = tutplot_opts$viridis_end)
  lty <- rep_len(lty, length(eta))

  # a stump's importance as a function of its performance
  fat <- function(x, eta) 1 / 2 * log(x / (1 - x)) * eta

  # `to = 1` is the asymptote: log(x / (1 - x)) is infinite there, and R drops
  # that single non-finite value, so each curve ends just short of it
  xs <- seq(from, 1, length.out = 101)
  ys <- vapply(eta, function(e) fat(xs, e), numeric(length(xs)))

  # size the panel from every curve, not just the first one drawn. Sizing from
  # the first works only while the rates descend; ascending ones would clip
  plot(
    NULL,
    xlim = c(from, 1),
    ylim = range(ys[is.finite(ys)]),
    xlab = "Performance",
    ylab = "Importance"
  )
  graphics::grid()

  # the reader's own learner, before the curves so they stay on top. Segments
  # rather than full rules: they reach the crosshair and stop, instead of
  # cutting across all three curves
  mark_alpha <- NULL
  if (!is.null(mark_perf)) {
    mark_alpha <- fat(mark_perf, mark_eta)
    graphics::segments(
      mark_perf,
      0,
      mark_perf,
      mark_alpha,
      lty = 2,
      col = "gray30"
    )
    graphics::segments(
      from,
      mark_alpha,
      mark_perf,
      mark_alpha,
      lty = 2,
      col = "gray30"
    )
    graphics::points(mark_perf, mark_alpha, pch = 19, cex = 1.1, col = "gray20")
  }

  for (i in seq_along(eta)) {
    graphics::lines(xs, ys[, i], col = pal[i], lty = lty[i], lwd = lwd)
  }

  # Legend sits in the margin above the panel (where `main` is), so it never
  # covers a curve. `xpd = NA` is what lets it draw outside the plotting
  # region; the default top margin already reserves the room. `text.width = NA`
  # gives each entry its own width -- without it `horiz` pads every column out
  # to the widest label, which strings short entries across the whole panel.
  # built from `eta`, so a label cannot outlive the curve it names
  tut_legend(
    as.expression(lapply(
      eta,
      function(e) bquote(eta * " = " * .(format(e)))
    )),
    cex = 0.9,
    col = pal,
    lty = lty,
    lwd = lwd
  )

  invisible(list(
    eta = eta,
    performance = xs,
    importance = ys,
    alpha = mark_alpha
  ))
}

# One round of AdaBoost on `data`: the weights it starts from, and the weights
# it leaves behind. Repeats Listings 17--22 of the code-along script, so
# `tutplot_weightone()` needs nothing from the reader's session.
tut_round_one <- function(
  data,
  predictors = c("power.o", "effect_size.o", "n.o", "p_value.o")
) {
  # the reviewer metrics only. `replicate ~ .` on the whole frame would hand
  # `eid` to the learner, which identifies every row, so the error would be
  # zero, alpha infinite and every weight NaN
  data <- data[, c(predictors, "replicate")]

  y <- data[["replicate"]]
  m <- nrow(data)

  # every observation equally important, to begin with
  d1 <- rep(1, m) / m

  h1 <- rpart::rpart(
    replicate ~ .,
    data = data,
    method = "class",
    maxdepth = 1,
    maxsurrogate = 0,
    weights = d1
  )
  yretro <- stats::predict(h1, newdata = data, type = "class")

  # the learner's weighted error, and the say it earns because of it
  e1 <- sum(d1 * (yretro != y))
  alpha1 <- 0.5 * log((1 - e1) / e1)

  # +1 where it was right, -1 where it was wrong
  chi1 <- (yretro == y) * 1 + (yretro != y) * -1
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
    round_one <- tut_round_one(data)
    if (is.null(d1)) {
      d1 <- round_one$d1
    }
    if (is.null(d2)) {
      d2 <- round_one$d2
    }
    # only when the caller supplied no weights of their own: a `chi` from this
    # round would not describe someone else's
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
  # Weights alone cannot say which points were missed. A round grows the
  # misclassified ones when alpha is positive and the correctly classified ones
  # when it is negative, and the grown set is the minority either way -- so its
  # size gives nothing away. Supply weights, supply `chi` with them.
  if (is.null(chi)) {
    msg <- "`chi` is required when `d1` or `d2` is supplied. "
    sug <- paste0(
      "Weights cannot say which points were missed: a round grows the ",
      "misclassified ones only while the learner beats chance."
    )
    stop(c(msg, sug), call. = FALSE)
  }
  check_numeric(chi)
  if (length(chi) != length(d1) || !all(chi %in% c(-1, 1))) {
    msg <- "`chi` must be +1 or -1, one per weight. "
    sug <- paste0(
      "Got ",
      length(chi),
      " value(s) for ",
      length(d1),
      " weights."
    )
    stop(c(msg, sug), call. = FALSE)
  }
  n <- min(n, length(d1))

  wrong <- which(chi[seq_len(n)] == -1)

  if (is.null(ylim)) {
    ylim <- c(0, max(c(d1, d2)) * 1.15)
  }

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  op <- tut_par(legend = TRUE)
  on.exit(graphics::par(op), add = TRUE)

  # d1 and d2 keep a color each. `end = 0.5` leaves d2 teal rather than
  # yellow-green, which would go muddy against the yellow band below
  pal <- viridisLite::viridis(2, end = 0.5)
  band <- grDevices::adjustcolor(viridisLite::viridis(1, begin = 1), 0.35)

  # area, not diameter: cex scales the width of the point, so the eye reads a
  # doubled weight as four times the ink unless we take the square root
  bubble <- function(w) sqrt(w / mean(d1)) * scaling

  plot(
    c(0.5, n + 0.5),
    ylim,
    type = "n",
    xaxt = "n",
    xlab = "Data Point",
    ylab = "Weight"
  )
  # the band goes behind everything: misclassification belongs to the data
  # point, not to either of its two weights
  graphics::rect(
    wrong - 0.42,
    ylim[1],
    wrong + 0.42,
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
    w <- list(d1, d2)[[i]]
    graphics::points(
      seq_len(n),
      w[seq_len(n)],
      pch = 21,
      bg = pal[i],
      col = "white",
      cex = bubble(w[seq_len(n)]),
      lwd = 1.3
    )
  }

  tut_legend(
    c(expression(D[1]), expression(D[2]), "misclassified"),
    cex = 0.9,
    pt.bg = c(pal, band),
    col = c("white", "white", "grey60"),
    pch = c(21, 21, 22),
    pt.cex = 1.3
  )

  invisible(list(d1 = d1, d2 = d2, chi = chi, wrong = wrong, n = n))
}

#' @rdname tutplot
#' @export
tutplot_boundary <- function(
  fit,
  data,
  shade = "margin",
  xlab = NULL,
  ylab = NULL,
  file = NULL,
  width = tutplot_opts$full[["width"]],
  height = tutplot_opts$full[["height"]],
  pointsize = tutplot_opts$pointsize,
  ...
) {
  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  # no tut_par() here: plot_adabound() sizes its own top margin from what it
  # actually draws -- legend, sub-title and title are each paid for or not --
  # so a fixed `mar` would either crop the legend or leave a gap above it
  invisible(plot_adabound(
    fit,
    data = data,
    shade = shade,
    xlab = xlab,
    ylab = ylab,
    ...
  ))
}

#' @rdname tutplot
#' @export
tutplot_lpocv <- function(
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
  # order is the caller's, not `table()`'s: sorting alphabetically would
  # silently reorder the manuscript's figure
  if (is.null(levels)) {
    levels <- if (is.factor(group)) base::levels(group) else unique(group)
  }
  absent <- setdiff(levels, as.character(group))
  if (length(absent)) {
    stop(
      "`levels` names a project that is not in `group`: ",
      paste(absent, collapse = ", "),
      call. = FALSE
    )
  }
  n <- table(factor(as.character(group), levels = levels))
  k <- length(n)
  nm <- toupper(names(n))

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  # row heights carry the project sizes, which is the whole point of the
  # proportional variant -- equal blocks imply five comparable folds
  h <- if (proportional) as.numeric(n) / sum(n) else rep(1 / k, k)
  edge <- cumsum(c(0, h))
  y0 <- 1 - edge[-1]
  y1 <- 1 - edge[-(k + 1)]
  pal <- viridisLite::viridis(k, end = 0.92)

  # no bottom label: the numbered caption names the figure, and the column
  # headers already say which iteration is which
  op <- graphics::par(mar = c(0.4, 2.0, 1.3, 0.3))
  on.exit(graphics::par(op), add = TRUE)
  plot(
    NULL,
    xlim = c(-0.62, k + 0.5),
    ylim = c(-0.26, 1.10),
    axes = FALSE,
    xlab = "",
    ylab = "",
    xaxs = "i",
    yaxs = "i"
  )

  # x and y user units are not comparable on a non-square panel, so carry the
  # horizontal gap through inches to get a vertical one that looks identical
  usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  pad <- 0.09
  pad_y <- pad * (usr[4] - usr[3]) / (usr[2] - usr[1]) * pin[1] / pin[2]
  gx <- 0.03
  gy <- 0.006

  # a label is dropped rather than overflowed when its band is too thin, which
  # is what lets the smallest project stay on the figure at all
  fits <- function(i) (y1[i] - y0[i]) > 0.055

  graphics::mtext("Dataset", side = 2, line = 0.7, cex = 0.72,
                  col = "grey25")
  for (i in seq_len(k)) {
    tut_roundrect(
      -0.58,
      y0[i] + gy,
      0.44,
      y1[i] - gy,
      col = grDevices::adjustcolor(pal[i], alpha.f = 0.32)
    )
    if (fits(i)) {
      graphics::text(
        -0.07,
        (y0[i] + y1[i]) / 2,
        nm[i],
        cex = 0.7,
        col = "grey15"
      )
    }
  }
  graphics::text(
    seq_len(k),
    1.05,
    sprintf("Iteration %d", seq_len(k)),
    cex = 0.7,
    col = "grey25"
  )

  for (j in seq_len(k)) {
    for (i in seq_len(k)) {
      test <- i == j
      # colour says which project, opacity says whether it is held out
      fill <- if (test) {
        pal[i]
      } else {
        grDevices::adjustcolor(pal[i], alpha.f = 0.22)
      }
      tut_roundrect(
        j - 0.5 + gx,
        y0[i] + gy,
        j + 0.5 - gx,
        y1[i] - gy,
        col = fill
      )
      if (fits(i)) {
        graphics::text(
          j,
          (y0[i] + y1[i]) / 2,
          if (test) "Test" else "Training",
          col = if (test) tut_ink(pal[i]) else "grey30",
          cex = 0.66
        )
      }
    }
    tut_roundrect(
      j - 0.5 + gx,
      -pad_y - 0.14,
      j + 0.5 - gx,
      -pad_y,
      col = pal[j]
    )
    graphics::text(
      j,
      -pad_y - 0.07,
      sprintf("Performance\n%s", nm[j]),
      col = tut_ink(pal[j]),
      cex = 0.62
    )
  }
  invisible(list(n = n, k = k, levels = levels, heights = h))
}
