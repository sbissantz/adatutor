#' Gauge the importance of each predictor
#'
#' Sums, for each predictor, the model weights of the trees that split on it,
#' and scales the sums to 100. A stump gives its whole weight alpha_t to its one
#' variable. A deeper tree splits its weight across its variables by the gain
#' of its primary splits, rpart's improvement measure; surrogate splits earn
#' nothing. A tree that never split adds nothing.
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
#'   `variable` and `importance` (a percentage), one row per predictor of the
#'   formula, sorted from most to least important; a predictor no tree split
#'   on has 0. `plot()` draws a lollipop chart, a line and a dot per
#'   predictor, by calling [tutplot_importance()], and returns `NULL`
#'   invisibly.
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

  # every predictor of the formula, in its order: a predictor no tree split on
  # keeps a zero, and the sums run in the order a by-hand loop uses, so the
  # two agree to the last digit
  predictors <- attr(fit[[1]]$h$terms, "term.labels")
  totals <- setNames(numeric(length(predictors)), predictors)

  for (t in seq_along(fit)) {
    tree <- fit[[t]]$h
    weight <- fit[[t]]$alpha

    # the gain of the tree's splits: NULL for a bare root, one entry for a stump
    gain <- sum_primary_gain(tree)

    if (is.null(gain) || sum(gain) == 0) {
      next
    }

    # split the tree's model weight across its variables; a stump gets it all,
    # exactly: gain / sum(gain) is 1 before the product
    share <- weight * (gain / sum(gain))

    # a variable the formula did not label still gets a slot
    unlabeled <- setdiff(names(share), names(totals))
    totals[unlabeled] <- 0
    totals[names(share)] <- totals[names(share)] + share
  }

  # no tree in the ensemble split on anything
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
#'
#' Returns a named vector, or NULL for a tree that never split.
#' @noRd
sum_primary_gain <- function(tree) {
  frame <- tree$frame

  # not rpart's variable.importance: that also credits surrogate splits, which
  # only stand in for missing values, so it depends on maxsurrogate. The measure
  # credits the splits a tree makes, so it reads the primary rows of `splits`,
  # as rpart's importance() does before it adds the surrogates
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
  # the manuscript's figure function draws, so the listing's plot() and the
  # printed figure are one picture
  tutplot_importance(x, top_n = top_n)
  invisible(NULL)
}
