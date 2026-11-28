#' Draw figures for AdaBoost and cross-validation
#'
#' Each function draws one figure, on the current device or into a PDF file.
#'
#' * `tutplot_cstump()`: a classification stump.
#' * `tutplot_gini()`: Gini impurity by class proportion.
#' * `tutplot_updatefactor()`: the factor by which one round multiplies an
#'   observation's weight.
#' * `tutplot_modelweight()`: the model weight a stump earns, by its
#'   performance.
#' * `tutplot_weightupdate()`: observation weights before and after one round.
#'   Both dots of a point are filled with the color of its observed class.
#' * `tutplot_importance()`: the relative importance of each predictor, as a
#'   lollipop chart.
#' * `tutplot_logoscheme()`: the leave-one-group-out cross-validation scheme.
#' * `tutplot_logocv()`: the leave-one-group-out estimates per project, with
#'   or without bootstrap intervals.
#'
#' @param fit A classification stump from [rpart::rpart()].
#' @param extra Node labels, passed to `rpart.plot::rpart.plot()`.
#' @param labels Readable names to draw in place of variable names, named by
#'   the variables as they appear in the data, such as
#'   `c(n.o = "Sample Size")`. A variable without an entry keeps its name. A
#'   name that is not a predictor of `fit`, or not a variable in `importance`,
#'   is an error. `tutplot_importance()` also takes plotmath in a named list,
#'   such as `list(p_value.o = quote(italic(p) ~ Value))`; `tutplot_cstump()`
#'   takes plain text only.
#' @param file A path to write a PDF to, or `NULL` to draw on the current
#'   device.
#' @param x_seq The class proportions at which to evaluate the Gini curve,
#'   between 0 and 1.
#' @param lwd The line width.
#' @param alpha The model weights, one curve each. `NULL` draws three: 0.7,
#'   the model weight of a first boosting round on the training set of
#'   [altmext_splits] (rounded to three digits), and 0.3.
#' @param pch The plotting symbols for the end points, recycled over `alpha`.
#' @param lty The line types. `tutplot_modelweight()` recycles them over the
#'   curves.
#' @param ylim The vertical range. For `tutplot_weightupdate()`, `NULL` runs
#'   from 0 to 15 percent above the largest weight.
#' @param eta The learning rates, one curve each.
#' @param from Where performance starts. 0.5 is chance.
#' @param marked_perf A performance to mark on the figure. Must be at least
#'   `from` and below 1. `NULL` marks the weighted performance of a first
#'   boosting round on the training set of [altmext_splits], or nothing when
#'   that lies below `from`. `NA` draws no mark.
#' @param marked_eta The learning rate of the curve the mark belongs to.
#' @param data The training data. Each point's observed class is read from
#'   its `replicate` column. When `d1` or `d2` is `NULL`, the first boosting
#'   round runs on its columns `power.o`, `effect_size.o`, `n.o` and
#'   `p_value.o`.
#' @param d1,d2 The observation weights before and after one round, one per
#'   row of `data` and in its row order. `NULL` computes them from `data`.
#' @param n The number of data points to draw.
#' @param scaling The bubble size multiplier.
#' @param importance A `gauge` object from [gauge()].
#' @param top_n The largest number of predictors to draw.
#' @param group One project label per row, such as the values of the `group`
#'   column in [logo_cv()].
#' @param proportional Whether to size the rows by project. `FALSE` draws
#'   equal blocks.
#' @param levels The projects to show, in drawing order, top to bottom. A
#'   project left out is dropped from the figure. `NULL` shows every project,
#'   in a factor's level order or the order of first appearance.
#' @param min_height The smallest share of the height a project gets when
#'   `proportional = TRUE`. 0 gives exact proportions.
#' @param res A result of [logo_cv()].
#' @param bootstrap Whether to draw bootstrap intervals. `TRUE` uses the
#'   intervals attached by [bootstrap()], or resamples when there are none;
#'   set the seed first. These are percentile intervals within each project.
#' @param n_resample The number of resamples per project, when `bootstrap`
#'   has to resample.
#' @param metric The metric to show, one of the names [assess()] returns.
#' @param bands The levels of the bootstrap bands, such as 0.95.
#' @param ylab The axis label. Defaults to a readable name for `metric`, such
#'   as "AUROC" for `"auroc"`. A metric without one is shown as typed.
#' @param width,height,pointsize Passed to [grDevices::pdf()]; ignored when
#'   `file` is `NULL`.
#'
#' @return Invisibly, the values behind the figure. Variables and projects
#'   keep their names in the data, not the labels drawn.
#' * `tutplot_cstump()`: The fit, the split variable, the cutpoint and the
#'   leaf sizes.
#' * `tutplot_gini()`: The curve and its peak.
#' * `tutplot_updatefactor()`: The factors, the legend labels and the named
#'   ticks, `NULL` when ordinary ticks were drawn.
#' * `tutplot_modelweight()`: The learning rates, the performance grid, the
#'   model weights and the marked weight, `NULL` without a mark.
#' * `tutplot_weightupdate()`: The weights, the class and colors of each
#'   point drawn, the legend labels and the named ticks, `NULL` when ordinary
#'   ticks were drawn.
#' * `tutplot_importance()`: The rows it drew, most important first.
#' * `tutplot_logoscheme()`: The project sizes, their count, the order and
#'   the row heights.
#' * `tutplot_logocv()`: One row per project with its estimate, the widest
#'   interval, and the share of resamples its bootstrap rejected.
#'
#' @family tutorial plots
#' @seealso [tutplot_boundary()] to draw a decision boundary.
#'
#' @examples
#' data(altmext_splits)
#' train <- altmext_splits$train
#'
#' # a stump on two predictors
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
#' # the Gini curve needs no model: it is arithmetic
#' gini <- tutplot_gini()
#' gini$peak
#'
#' # the LOGO-CV scheme, drawn from the grouping itself. HOLZ is
#' # kept back for the final test, so six projects take part
#' data(altmext)
#' df_noholz <- altmext[altmext$pid != "holz", ]
#' folds <- tutplot_logoscheme(df_noholz$pid)
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
  labels = NULL,
  file = NULL,
  width = tutplot_opts$col[["width"]],
  height = tutplot_opts$col[["height"]],
  pointsize = tutplot_opts$pointsize
) {
  if (!inherits(fit, "rpart")) {
    stop("`fit` must be an rpart object.", call. = FALSE)
  }
  if (!is.null(labels)) {
    if (!is.character(labels)) {
      stop(
        "`labels` must be a named character vector: rpart.plot() draws ",
        "plain text only.",
        call. = FALSE
      )
    }
    check_labels(labels, attr(fit$terms, "term.labels"), "`fit`")
  }

  # color-blind palette from the fit's own class proportions
  colors <- viridis_tree(fit)

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  drawn <- fit
  attr(drawn, "ylevels") <- tools::toTitleCase(attr(fit, "ylevels"))

  # branch tags in title case and upright; with node numbers off, `nn.font`
  # sets only the tags' font, which is italic by default
  rpart.plot::rpart.plot(
    drawn,
    extra = extra,
    digits = 4,
    box.col = colors$box,
    col = colors$text,
    split.fun = relabel_splits(labels),
    yes.text = "Yes",
    no.text = "No",
    nn.font = 1
  )

  # read off the fit as given rather than the drawn copy; a stump that never
  # split has no `splits` matrix
  terminal <- fit$frame$var == "<leaf>"
  cutpoint <- if (is.null(fit$splits)) NULL else fit$splits[1, "index"]

  invisible(list(
    fit = fit,
    variable = as.character(fit$frame$var[1]),
    cutpoint = cutpoint,
    leaves = fit$frame$n[terminal]
  ))
}

#' Build a split.fun for rpart.plot() that swaps variable names for labels
#'
#' @return A function, or `NULL` for no labels, which is rpart.plot()'s own
#'   default.
#' @noRd
relabel_splits <- function(labels) {
  if (is.null(labels)) {
    return(NULL)
  }
  # relabel the drawn text, not the fit: renamed variables are missing from
  # the data, so rpart.plot() would stop rounding integer cutpoints
  function(x, labs, digits, varlen, faclen) {
    drawn <- labs
    for (variable in names(labels)) {
      # name plus the space before the operator, so `n` cannot match `n.o`;
      # matched on the original text, so one label cannot match another
      hit <- startsWith(labs, paste0(variable, " "))
      drawn[hit] <- paste0(
        labels[[variable]],
        substring(labs[hit], nchar(variable) + 1L)
      )
    }
    drawn
  }
}

#' Stop unless `labels` is named by variables of the model
#' @noRd
check_labels <- function(labels, variables, source) {
  label_names <- names(labels)
  if (is.null(label_names) || any(is.na(label_names) | !nzchar(label_names))) {
    stop("`labels` must be named by the variables it relabels.", call. = FALSE)
  }
  unknown <- setdiff(label_names, variables)
  if (length(unknown)) {
    stop(
      "`labels` names a variable that is not in ",
      source,
      ": ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(labels)
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
  # run before the dev.off() above, so par() is restored on the right device
  on.exit(graphics::par(old_par), add = TRUE, after = FALSE)

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
  alpha = NULL,
  pch = 20,
  lty = 3,
  lwd = 1,
  ylim = c(0.3, 2.2),
  file = NULL,
  width = tutplot_opts$col[["width"]],
  height = tutplot_opts$col[["height"]],
  pointsize = tutplot_opts$pointsize
) {
  if (is.null(alpha)) {
    alpha <- resolve_figure_defaults()$alpha
  }
  check_numeric(alpha)
  # before pdf(), so a failing round one writes no file
  legend_labels <- label_alpha(alpha)

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  old_par <- set_figure_par(reserve_legend = TRUE)
  # run before the dev.off() above, so par() is restored on the right device
  on.exit(graphics::par(old_par), add = TRUE, after = FALSE)

  # same ordered palette as the eta curves: model weights are ordered too
  colors <- viridisLite::viridis(length(alpha), end = tutplot_opts$viridis_end)
  pch <- rep_len(pch, length(alpha))

  # chi is +1 where the learner was right, -1 where wrong
  rescale_weight <- function(alpha, chi) exp(-alpha * chi)

  # a step under the family's 0.9, so the three lower factors fit, and no
  # smaller than tutplot_boundary()'s tick labels, 0.85
  cex_ticks <- 0.85
  # every factor drawn, named; more than three curves would crowd the axis
  ticks <- NULL
  if (length(alpha) <= 3L) {
    factors <- c(rescale_weight(alpha, 1), rescale_weight(alpha, -1))
    ticks <- plan_named_ticks(
      at = c(factors, 1),
      labels = c(sprintf("%.2f", factors), "1"),
      ylim = ylim,
      cex = cex_ticks
    )
  }
  shift <- if (is.null(ticks)) 0 else ticks$shift
  graphics::par(mar = graphics::par("mar") + c(0, shift, 0, 0))

  # empty frame first, so the grid lies under every curve
  plot(
    NULL,
    xlim = c(-1, 1),
    ylim = ylim,
    xlab = "Classification",
    ylab = "",
    xaxt = "n",
    yaxt = if (is.null(ticks)) "s" else "n"
  )
  graphics::title(
    ylab = "Weight Update Factor",
    line = tutplot_opts$mgp[1] + shift
  )
  # named ticks carry the values, so a grid would only add lines
  if (is.null(ticks)) {
    graphics::grid()
  } else {
    graphics::axis(
      2,
      at = ticks$at,
      labels = ticks$labels,
      las = 1,
      cex.axis = cex_ticks
    )
  }

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

  # line and dot merge into one key: legend() leaves 0.3 character widths
  # after a label but 0.7 (`x.intersp`) before it; 0.4 more evens the two
  draw_legend(
    legend_labels,
    cex = 1,
    col = colors,
    lty = lty,
    lwd = lwd,
    pch = pch,
    pad = 0.4
  )
  graphics::axis(1, at = c(-1, 1))

  invisible(list(
    alpha = alpha,
    correct = rescale_weight(alpha, 1),
    wrong = rescale_weight(alpha, -1),
    legend = legend_labels,
    ticks = ticks
  ))
}

#' Label the curves of the update-factor figure
#'
#' @return An expression vector, one entry per curve.
#' @noRd
label_alpha <- function(alpha) {
  decreasing <- length(alpha) == 3L && isTRUE(all(diff(alpha) < 0))
  # the first stump's weight, compared at the three digits the default keeps
  if (
    decreasing &&
      isTRUE(round(alpha[2], 3) == resolve_figure_defaults()$alpha[2])
  ) {
    return(expression("Larger" ~ alpha[t], alpha[1], "Smaller" ~ alpha[t]))
  }
  as.expression(lapply(
    alpha,
    \(value) bquote(alpha[t] * " = " * .(format(value)))
  ))
}

#' Plan named y ticks, or give way to ordinary ones
#'
#' @return `NULL` when two labels would collide or no tick lies on the panel,
#'   or a list with the ticks on the panel, their labels, and `shift`: the
#'   lines by which the left margin and the axis title move out to make room
#'   for the labels.
#' @noRd
plan_named_ticks <- function(at, labels, ylim, cex) {
  # reads the current device: call after set_figure_par() and before plot()

  # plot() widens `ylim` by 4 percent at each end (yaxs = "r"), so a point
  # just past `ylim` is still drawn and still gets its tick
  panel <- range(ylim) + c(-1, 1) * 0.04 * diff(range(ylim))
  inside <- at >= panel[1] & at <= panel[2]
  at <- at[inside]
  labels <- labels[inside]
  drawn <- order(at)
  at <- at[drawn]
  labels <- labels[drawn]
  if (!length(at)) {
    return(NULL)
  }

  # strheight() can read a closed device's cached height of "M"; measuring at
  # another size first makes the next call measure on this device
  graphics::strheight("M", units = "inches", cex = 2 * cex)
  label_height <- max(graphics::strheight(labels, units = "inches", cex = cex))
  # `mar[2]` does not change the panel's height, so it can be read before the
  # margin moves
  units_per_inch <- diff(panel) / graphics::par("pin")[2]
  if (has_collision(at, label_height * units_per_inch)) {
    return(NULL)
  }

  # horizontal labels reach out by their width, rotated ones by their height;
  # the difference keeps the title as far from the labels as plot() puts it
  label_width <- max(graphics::strwidth(labels, units = "inches", cex = cex))
  shift <- (label_width - label_height) / graphics::par("csi")
  list(at = at, labels = labels, shift = max(shift, 0))
}

#' Tell whether two tick labels would overlap
#' @noRd
has_collision <- function(at, min_gap) {
  # a horizontal label reaches half its height above and below its tick, so
  # two labels collide when their ticks sit less than one label height apart
  length(at) > 1L && any(diff(sort(at)) < min_gap)
}

#' @rdname tutplot
#' @export
tutplot_modelweight <- function(
  eta = c(1, 0.5, 0.1),
  lty = 1,
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
  if (is.null(marked_perf)) {
    marked_perf <- resolve_figure_defaults()$marked_perf
    # a mark the caller never asked for is left out, not refused
    if (marked_perf < from) {
      marked_perf <- NA
    }
  }
  # NA, not NULL, asks for no mark: NULL is the first round's stump
  draw_mark <- !(length(marked_perf) == 1L && is.na(marked_perf))
  if (draw_mark) {
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
  # run before the dev.off() above, so par() is restored on the right device
  on.exit(graphics::par(old_par), add = TRUE, after = FALSE)

  # ordered learning rates, so the palette runs with them; solid lines by
  # default, so color alone tells the curves apart
  colors <- viridisLite::viridis(length(eta), end = tutplot_opts$viridis_end)
  lty <- rep_len(lty, length(eta))

  weigh <- function(accuracy, eta) 1 / 2 * log(accuracy / (1 - accuracy)) * eta

  # stop short of the asymptote at 1: R drops the one infinite value
  performance <- seq(from, 1, length.out = 101)
  weight <- vapply(
    eta,
    \(rate) weigh(performance, rate),
    numeric(length(performance))
  )

  # size the panel from every curve; the first alone clips ascending rates
  plot(
    NULL,
    xlim = c(from, 1),
    ylim = range(weight[is.finite(weight)]),
    xlab = "Performance",
    ylab = "Model Weight"
  )
  graphics::grid()

  # drawn first so the curves stay on top; segments stop at the crosshair
  # instead of crossing every curve
  marked_alpha <- NULL
  if (draw_mark) {
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
      weight[, i],
      col = colors[i],
      lty = lty[i],
      lwd = lwd
    )
  }

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
    weight = weight,
    alpha = marked_alpha
  ))
}

#' Resolve what the bare figure calls draw
#'
#' @return A list with the default `alpha` of tutplot_updatefactor() and
#'   `marked_perf` of tutplot_modelweight(), from a first boosting round on
#'   the training set of `altmext_splits`.
#' @noRd
resolve_figure_defaults <- function() {
  # `adatutor::` finds the data whether or not the package is attached
  round_one <- run_round_one(adatutor::altmext_splits$train)
  list(
    alpha = c(0.7, round(round_one$alpha, 3), 0.3),
    marked_perf = round_one$perf
  )
}

# one round of AdaBoost on `data`, so the figures need nothing from the
# reader's session
run_round_one <- function(
  data,
  predictors = c("power.o", "effect_size.o", "n.o", "p_value.o")
) {
  # the predictors only: `eid` identifies every row, so it would give zero
  # error, infinite alpha and NaN weights
  data <- data[, c(predictors, "replicate")]

  y <- data[["replicate"]]
  m <- nrow(data)

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

  # in this order, so the results stay the same to the last bit
  perf <- sum(d1 * (y_retro == y))
  alpha1 <- 0.5 * log(perf / (1 - perf))

  chi1 <- (y_retro == y) * 1 + (y_retro != y) * -1
  d2_raw <- d1 * exp(-alpha1 * chi1)

  list(
    d1 = d1,
    d2 = d2_raw / sum(d2_raw),
    chi = chi1,
    fit = h1,
    alpha = alpha1,
    perf = perf
  )
}

#' @rdname tutplot
#' @export
tutplot_weightupdate <- function(
  data = adatutor::altmext_splits$train,
  d1 = NULL,
  d2 = NULL,
  n = 25,
  scaling = 2,
  ylim = NULL,
  file = NULL,
  width = tutplot_opts$col[["width"]],
  height = tutplot_opts$col[["height"]],
  pointsize = tutplot_opts$pointsize
) {
  check_df(data)
  if (!"replicate" %in% names(data)) {
    stop(
      "`data` must have a `replicate` column: it gives each point's class.",
      call. = FALSE
    )
  }
  # stops for an outcome that is not two classes, as tutplot_boundary() does
  observed <- as_binary(data[["replicate"]])
  if (is.null(d1) || is.null(d2)) {
    round_one <- run_round_one(data)
    if (is.null(d1)) {
      d1 <- round_one$d1
    }
    if (is.null(d2)) {
      d2 <- round_one$d2
    }
  }
  check_numeric(d1)
  check_numeric(d2)
  if (length(d1) != length(d2)) {
    stop("`d1` and `d2` must be the same length.", call. = FALSE)
  }
  # the class is read row by row, so each weight must sit on its own row
  if (length(d1) != nrow(data)) {
    stop(
      "`d1` and `d2` must have one value per row of `data`, in row order. ",
      "Got ",
      length(d1),
      " weights for ",
      nrow(data),
      " rows.",
      call. = FALSE
    )
  }
  n <- min(n, length(d1))
  shown <- seq_len(n)

  if (is.null(ylim)) {
    ylim <- c(0, max(c(d1, d2)) * 1.15)
  }

  # every point in full color: after a round that beats chance, the dots that
  # moved up are the misclassified ones
  palette <- viridisLite::viridis(2)
  labels <- tools::toTitleCase(read_class_labels(data, "replicate"))
  look <- style_points(observed[shown], rep(FALSE, n), palette)
  # one dark ring: on this white panel a white ring vanishes and shrinks the dot
  ring <- choose_ink("white")
  look$ring <- rep(ring, n)

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  old_par <- set_figure_par(reserve_legend = TRUE)
  # run before the dev.off() above, so par() is restored on the right device
  on.exit(graphics::par(old_par), add = TRUE, after = FALSE)

  # zero and every weight drawn, named; more than four weights crowd the axis.
  # Distinct as stored, so weights that differ in the last bit count as two
  weights_drawn <- sort(unique(c(d1[shown], d2[shown])))
  ticks <- NULL
  if (length(weights_drawn) <= 4L) {
    nonzero <- weights_drawn[weights_drawn != 0]
    # two significant digits for the smallest, the same decimals for all
    ticks <- plan_named_ticks(
      at = c(0, nonzero),
      labels = c(
        "0",
        format(nonzero, digits = 2, scientific = FALSE, trim = TRUE)
      ),
      ylim = ylim,
      cex = graphics::par("cex.axis")
    )
  }
  shift <- if (is.null(ticks)) 0 else ticks$shift
  graphics::par(mar = graphics::par("mar") + c(0, shift, 0, 0))

  # scale by area, not diameter: sqrt keeps a doubled weight at double the ink
  size_bubble <- function(weights) sqrt(weights / mean(d1)) * scaling

  plot(
    c(0.5, n + 0.5),
    ylim,
    type = "n",
    xaxt = "n",
    yaxt = if (is.null(ticks)) "s" else "n",
    xlab = "Data Point",
    ylab = ""
  )
  graphics::title(ylab = "Weight", line = tutplot_opts$mgp[1] + shift)
  graphics::box()
  if (!is.null(ticks)) {
    graphics::axis(2, at = ticks$at, labels = ticks$labels, las = 1)
  }

  # every fifth point: 25 labels do not fit the width of a column
  graphics::axis(1, at = c(1, seq(5, n, 5)))
  # and a short, unnumbered tick under every point, so each one can be found
  graphics::axis(1, at = seq_len(n), labels = FALSE, tcl = -0.22)
  graphics::segments(
    seq_len(n),
    d1[seq_len(n)],
    seq_len(n),
    d2[seq_len(n)],
    lty = 3,
    col = "grey45"
  )

  # both dots of a point in its class's color; only the size shows the weight
  for (weights in list(d1, d2)) {
    graphics::points(
      shown,
      weights[shown],
      pch = 21,
      bg = look$fill,
      col = look$ring,
      cex = size_bubble(weights[shown]),
      lwd = look$lwd
    )
  }

  # the boundary figure's key: the second class first, ringed like the dots
  draw_legend(
    rev(labels),
    cex = 0.9,
    pt.bg = rev(palette),
    col = ring,
    pch = 21,
    pt.cex = 1.3,
    pt.lwd = 1.1
  )

  invisible(list(
    d1 = d1,
    d2 = d2,
    n = n,
    class = observed[shown],
    fill = look$fill,
    ring = look$ring,
    legend = rev(labels),
    ticks = ticks
  ))
}

#' @rdname tutplot
#' @export
tutplot_importance <- function(
  importance,
  top_n = 15,
  labels = NULL,
  file = NULL,
  width = tutplot_opts$col[["width"]],
  height = tutplot_opts$col[["height"]],
  pointsize = tutplot_opts$pointsize
) {
  check_gauge(importance)
  if (!is.null(labels)) {
    # every row, not only the top_n drawn: a label for a cut row is no error
    check_labels(labels, importance$variable, "`importance`")
    is_drawable <- function(label) {
      (is.character(label) && length(label) == 1L) || is.language(label)
    }
    if (!all(vapply(labels, is_drawable, logical(1)))) {
      stop(
        "Each entry of `labels` must be one string or a plotmath expression.",
        call. = FALSE
      )
    }
  }

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  # title case, as APA asks of a figure; plot.gauge() uses sentence case
  drawn <- draw_importance(
    importance,
    top_n,
    labels = labels,
    xlab = "Relative Importance (%)"
  )
  invisible(drawn)
}

#' Stop unless `importance` is a gauge with a row to draw
#' @noRd
check_gauge <- function(importance) {
  if (!inherits(importance, "gauge")) {
    stop("`importance` must be the result of `gauge()`.", call. = FALSE)
  }
  if (nrow(importance) == 0L) {
    stop(
      "no variable was split on, so there is no importance to plot.",
      call. = FALSE
    )
  }
  invisible(importance)
}

#' Draw the lollipop chart of a gauge on the current device
#'
#' @return The rows drawn, most important first.
#' @noRd
draw_importance <- function(importance, top_n, labels, xlab) {
  # top_n predictors only, so the chart stays readable; gauge() sorts them
  drawn <- utils::head(importance, min(top_n, nrow(importance)))
  n_shown <- nrow(drawn)

  # ascending, so the most important predictor plots at the top
  shown <- drawn[order(drawn$importance, decreasing = FALSE), ]
  rows <- seq_len(n_shown)

  old_par <- set_figure_par(reserve_legend = FALSE)
  on.exit(graphics::par(old_par), add = TRUE)

  # the left margin fits the longest drawn label plus the tick label offset:
  # the family's margin clips the names, a fixed wide one wastes panel
  axis_labels <- label_variables(shown$variable, labels)
  mar <- graphics::par("mar")
  mar[2] <- compute_left_margin(axis_labels)
  graphics::par(mar = mar)

  # the family's palette, stopped before yellow; reversed, so the darkest
  # color goes to the most important predictor
  colors <- rev(viridisLite::viridis(n_shown, end = tutplot_opts$viridis_end))

  # headroom past the longest line: pretty() ticks can stop well below it
  xmax <- min(max(shown$importance) + 5, 100)

  graphics::plot.new()
  graphics::plot.window(
    xlim = c(0, xmax),
    ylim = c(0.5, n_shown + 0.5),
    xaxs = "i"
  )

  # faint grid at the ticks, drawn first so it stays behind the lollipops
  graphics::abline(v = graphics::axTicks(1), col = "grey90", lwd = 0.8)

  # one lollipop per predictor: a line from zero and a dot at its share; xpd
  # so a dot at 100 is not cut in half
  graphics::segments(
    0,
    rows,
    shown$importance,
    rows,
    col = colors,
    lwd = 3,
    lend = "butt"
  )
  graphics::points(
    shown$importance,
    rows,
    pch = 19,
    cex = 2.2,
    col = colors,
    xpd = NA
  )

  graphics::axis(1)
  graphics::axis(2, at = rows, labels = axis_labels, las = 1, lwd = 0)
  graphics::title(xlab = xlab)

  drawn
}

#' Swap variable names for their labels, keeping the rest
#' @noRd
label_variables <- function(variables, labels = NULL) {
  if (is.null(labels)) {
    return(variables)
  }
  drawn <- lapply(variables, function(variable) {
    if (variable %in% names(labels)) labels[[variable]] else variable
  })
  if (all(vapply(drawn, is.character, logical(1)))) {
    return(unlist(drawn))
  }
  # one expression vector, so strwidth() and axis() read the same labels
  as.expression(lapply(drawn, function(label) {
    if (is.expression(label)) label[[1]] else label
  }))
}

#' Size the left margin, in lines, to the widest y-axis label
#' @noRd
compute_left_margin <- function(labels) {
  # reads the current device and par(), so call it after set_figure_par()
  label_width <- max(graphics::strwidth(
    labels,
    units = "inches",
    cex = tutplot_opts$cex_axis
  ))
  label_width / graphics::par("csi") + tutplot_opts$mgp[2] + 1
}

#' @rdname tutplot
#' @export
tutplot_logocv <- function(
  res,
  bootstrap = TRUE,
  n_resample = 1000,
  metric = "auroc",
  bands = c(0.50, 0.80, 0.95),
  ylab = label_metric(metric),
  file = NULL,
  width = tutplot_opts$col[["width"]],
  height = tutplot_opts$col[["height"]],
  pointsize = tutplot_opts$pointsize
) {
  if (!inherits(res, "logo_cv")) {
    stop("`res` must be a result of logo_cv().", call. = FALSE)
  }
  # before the bootstrap, so a misspelled metric costs no resampling
  read_metric_rows(res, metric)
  if (!bootstrap) {
    res$ci <- NULL
  } else if (is.null(res$ci)) {
    # the method, not the generic: the argument `bootstrap` shadows its name
    res <- bootstrap.logo_cv(res, n_resample = n_resample)
  }

  # before pdf() writes a file or par() opens a device
  figure <- prepare_logo_cv(
    res,
    metric = metric,
    levels = bands,
    baseline = "auto",
    ylab = ylab,
    add_keys = FALSE
  )

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  # no keys above the panel, so no room reserved for them
  old_par <- set_figure_par(reserve_legend = FALSE)
  # run before the dev.off() above, so par() is restored on the right device
  on.exit(graphics::par(old_par), add = TRUE, after = FALSE)

  # project names in capitals, as in tutplot_logoscheme()
  facts <- draw_logo_cv(
    figure,
    main = NULL,
    label_project = toupper,
    italic_n = TRUE
  )
  invisible(facts)
}

#' Label a metric for a figure axis
#' @noRd
label_metric <- function(metric) {
  metric <- as.character(metric)
  labels <- c(
    tp = "True Positives",
    tn = "True Negatives",
    fp = "False Positives",
    fn = "False Negatives",
    sens = "Sensitivity",
    spec = "Specificity",
    ppv = "PPV",
    npv = "NPV",
    acc = "Accuracy",
    bacc = "Balanced Accuracy",
    f1 = "F1",
    mcc = "MCC",
    auroc = "AUROC",
    auprc = "AUPRC",
    patk = "R-Precision"
  )
  if (metric %in% names(labels)) {
    return(labels[[metric]])
  }
  # any budget assess() takes: patk_3, patk_5, patk_10, ...
  if (grepl("^patk_[0-9]+$", metric)) {
    return(paste("Precision at", sub("^patk_", "", metric)))
  }
  metric
}

#' @rdname tutplot
#' @export
tutplot_logoscheme <- function(
  group,
  proportional = TRUE,
  levels = NULL,
  min_height = 0.06,
  file = NULL,
  width = tutplot_opts$full[["width"]],
  height = tutplot_opts$full[["height"]],
  pointsize = tutplot_opts$pointsize
) {
  if (length(group) < 1L) {
    stop("`group` must name the project each row belongs to.", call. = FALSE)
  }
  # keep the caller's order: table() would reorder the figure
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
  check_numeric(min_height)
  if (proportional && min_height * n_projects > 1) {
    stop(
      "`min_height` = ",
      min_height,
      " cannot fit ",
      n_projects,
      " projects. Use at most 1/",
      n_projects,
      " = ",
      format(1 / n_projects, digits = 3),
      ".",
      call. = FALSE
    )
  }

  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height, pointsize = pointsize)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  # row heights carry project sizes; equal blocks would imply comparable folds
  heights <- set_heights(as.numeric(sizes), proportional, min_height)
  edges <- cumsum(c(0, heights))
  bottoms <- 1 - edges[-1]
  tops <- 1 - edges[-(n_projects + 1)]
  # by name, so a project has the color tutplot_logocv() gives it
  colors <- unname(color_projects(names(sizes)))

  # no bottom label: the column headers say it
  old_par <- graphics::par(mar = c(0.4, 2.0, 1.3, 0.3))
  # run before the dev.off() above, so par() is restored on the right device
  on.exit(graphics::par(old_par), add = TRUE, after = FALSE)
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
  # each project in its full color and ink, as its test box draws it
  for (i in seq_len(n_projects)) {
    draw_roundrect(
      -0.58,
      bottoms[i] + inset_y,
      0.44,
      tops[i] - inset_y,
      col = colors[i]
    )
    if (fits(i)) {
      graphics::text(
        -0.07,
        (bottoms[i] + tops[i]) / 2,
        labels[i],
        cex = 0.7,
        col = choose_ink(colors[i])
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

#' Share the figure's height among the projects
#' @noRd
set_heights <- function(sizes, proportional, min_height) {
  n_projects <- length(sizes)
  if (!proportional) {
    return(rep(1 / n_projects, n_projects))
  }
  # raise thin rows to the minimum and share the rest by size; repeat, because
  # shrinking the rest can push another row under
  raised <- sizes / sum(sizes) < min_height
  repeat {
    rest <- 1 - sum(raised) * min_height
    heights <- ifelse(raised, min_height, sizes / sum(sizes[!raised]) * rest)
    newly_raised <- !raised & heights < min_height
    if (!any(newly_raised)) {
      return(heights)
    }
    raised <- raised | newly_raised
  }
}
