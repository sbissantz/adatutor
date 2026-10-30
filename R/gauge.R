#' Gauge the importance of each predictor
#'
#' Sums, for each predictor, the model weights of the trees that split on it,
#' and scales the sums to 100. A stump gives its whole weight a_t to its one
#' variable. A deeper tree splits its weight across its variables by rpart's
#' improvement measure. A tree that never split adds nothing.
#'
#' @section What it does not tell you:
#' The numbers are relative: they show how the ensemble divided its weight,
#' not what a variable is worth on its own. They have no sign, correlated
#' predictors share their weight, and the ranking changes with the
#' hyperparameters and the training data.
#'
#' @param fit A fit from [adaboost()].
#' @param x A `gauge` object from `gauge()`.
#' @param top_n The largest number of predictors to plot. Defaults to 15.
#' @param ... Ignored.
#'
#' @return `gauge()` returns a data frame of class `gauge` with the columns
#'   `variable` and `importance` (a percentage), sorted from most to least
#'   important. `plot()` draws a lollipop chart, a line and a dot per
#'   feature, and returns `NULL` invisibly.
#'
#' @family tutorial plots
#'
#' @examples
#' data(altmejd)
#' prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
#'
#' h <- rpart::rpart(
#'   replicate ~ .,
#'   data = altmejd[, c(prednms, "replicate")],
#'   maxdepth = 1,
#'   model = TRUE
#' )
#' fit <- adaboost(h, n_iter = 50, eta = 1, verbose = FALSE, check_inputs = FALSE)
#'
#' imp <- gauge(fit)
#' imp
#'
#' plot(imp)
#'
#' @name gauge
#' @export
gauge <- function(fit) {
  check_ada_fit(fit)

  totals <- list()

  for (t in seq_along(fit)) {
    tree <- fit[[t]]$h
    weight <- fit[[t]]$a

    # rpart's improvement: NULL for a bare leaf, one entry for a stump
    improvement <- tree$variable.importance

    if (is.null(improvement) || sum(improvement) == 0) {
      next
    }

    # split the tree's model weight across its variables; a stump gets it all
    share <- weight * improvement / sum(improvement)

    for (variable in names(share)) {
      if (is.null(totals[[variable]])) {
        totals[[variable]] <- share[[variable]]
      } else {
        totals[[variable]] <- totals[[variable]] + share[[variable]]
      }
    }
  }

  # no tree in the ensemble split on anything
  if (length(totals) == 0) {
    return(structure(
      data.frame(
        variable = character(0),
        importance = numeric(0),
        stringsAsFactors = FALSE
      ),
      class = c("gauge", "data.frame")
    ))
  }

  importance <- data.frame(
    variable = names(totals),
    importance = unlist(totals),
    stringsAsFactors = FALSE
  )

  # percent of the total: easier to read
  total <- sum(importance$importance)
  importance$importance <- importance$importance / total * 100

  importance <- importance[order(-importance$importance), ]
  rownames(importance) <- NULL

  # the class is what lets plot(gauge(fit)) find its method
  structure(importance, class = c("gauge", "data.frame"))
}

#' @rdname gauge
#' @export
plot.gauge <- function(x, top_n = 15, ...) {
  if (nrow(x) == 0L) {
    stop(
      "no variable was split on, so there is no importance to plot.",
      call. = FALSE
    )
  }

  # top_n features only, so the chart stays readable
  n_shown <- min(top_n, nrow(x))
  shown <- utils::head(x, n_shown)

  # ascending, so the most important feature plots at the top
  shown <- shown[order(shown$importance, decreasing = FALSE), ]
  rows <- seq_len(n_shown)

  # size the palette by the features drawn, so the gradient spans its full
  # range; reversed, so the darkest color goes to the most important one
  colors <- rev(viridisLite::viridis(n_shown))

  # wide left margin for long variable names
  old_par <- graphics::par(mar = c(5, 10, 4, 2) + 0.1)
  on.exit(graphics::par(old_par), add = TRUE)

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

  # one lollipop per feature: a line from zero and a dot at its share; xpd so
  # a dot at 100 is not cut in half
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
  graphics::axis(2, at = rows, labels = shown$variable, las = 1, lwd = 0)
  graphics::title(xlab = "Relative Importance (%)", main = "Feature Importance")

  invisible(NULL)
}
