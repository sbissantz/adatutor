#' Gauge the importance of each predictor
#'
#' Sums, for each predictor, the model weights \eqn{\alpha_t}{alpha_t} of the
#' trees that split on it, and scales the sums to 100. A tree that splits on
#' several predictors divides its weight among them by the gain of its primary
#' splits.
#'
#' @param fit A fit from [adaboost()].
#' @param x A result of `gauge()`.
#' @param top_n The largest number of predictors to draw.
#' @param ... Ignored.
#'
#' @return `gauge()` returns a data frame of class `gauge` with the columns
#'   `variable` and `importance` (in percent), one row per predictor of the
#'   formula, from most to least important. A predictor that no tree splits on
#'   gets 0. `plot()` draws a lollipop chart and returns `NULL` invisibly.
#'
#' @family tutorial plots
#' @seealso [tutplot_importance()] to draw the chart with custom labels.
#'
#' @examples
#' data(altmext_splits)
#' train <- altmext_splits$train
#' prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
#'
#' h <- rpart::rpart(
#'   replicate ~ .,
#'   data = train[, c(prednms, "replicate")],
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

  # every predictor in formula order: an unused one keeps 0, and the sums run
  # in the order of a by-hand loop, so the two agree to the last digit
  predictors <- attr(fit[[1]]$h$terms, "term.labels")
  totals <- stats::setNames(numeric(length(predictors)), predictors)

  for (t in seq_along(fit)) {
    tree <- fit[[t]]$h
    weight <- fit[[t]]$alpha

    gain <- sum_primary_gain(tree)

    if (is.null(gain) || sum(gain) == 0) {
      next
    }

    # divide first, so a stump gets its whole weight exactly
    share <- weight * (gain / sum(gain))

    unlabeled <- setdiff(names(share), names(totals))
    totals[unlabeled] <- 0
    totals[names(share)] <- totals[names(share)] + share
  }

  if (sum(totals) == 0) {
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
    importance = unname(totals),
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

#' Sum the gain of a tree's primary splits, per variable
#' @return A named vector, or `NULL` for a tree that never split.
#' @noRd
sum_primary_gain <- function(tree) {
  frame <- tree$frame

  # not rpart's variable.importance, which also credits surrogates and so
  # depends on maxsurrogate: read only the primary rows of `splits`
  split_nodes <- which(frame$var != "<leaf>")
  if (length(split_nodes) == 0L) {
    return(NULL)
  }

  # each split node owns one primary row, then its competitors and surrogates
  rows_per_node <- 1 +
    frame$ncompete[split_nodes] +
    frame$nsurrogate[split_nodes]
  primary_rows <- 1 + cumsum(c(0, rows_per_node))[seq_along(split_nodes)]

  gain <- tree$splits[primary_rows, "improve"]
  c(tapply(gain, as.character(frame$var[split_nodes]), sum))
}

#' @rdname gauge
#' @export
plot.gauge <- function(x, top_n = 15, ...) {
  check_gauge(x)
  # a plot() method titles in sentence case
  draw_importance(x, top_n, labels = NULL, xlab = "Relative importance (%)")
  invisible(NULL)
}
