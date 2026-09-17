#' @title Gauge the Importance of Each Predictor
#'
#' @description Extracts the relative importance of the predictor variables from
#'   an ensemble fitted with \code{\link[adatutor]{adaboost}}. Each tree
#'   contributes its model weight \eqn{a_t}, distributed across the variables it
#'   splits on in proportion to the improvement each split achieves. A decision
#'   stump splits once, so it passes its whole weight \eqn{a_t} to that single
#'   variable; deeper trees spread it over every splitting variable rather than
#'   only the one at the root.
#'
#' @details A boosted ensemble is not one tree but hundreds, so no single split
#'   explains it. What can be read off it is how much of the ensemble's total
#'   weight each variable was responsible for, which is what this returns.
#'
#'   The tally is built in two steps. \code{rpart} records an
#'   improvement-based importance for every variable a tree splits on, so within
#'   tree \eqn{t} the shares are \eqn{a_t \cdot imp_v / \sum_v imp_v}. Summing
#'   those shares over all \eqn{T} trees and normalizing to 100 gives the result.
#'   A tree that never split -- a bare root, which happens at small \code{eta}
#'   where the weights barely move -- contributes nothing rather than
#'   contributing zero to everything.
#'
#'   \strong{What it does not tell you.} The numbers are relative and sum to
#'   100, so they say how the ensemble divided its attention, not how much any
#'   variable is worth on its own. They carry no sign: a variable can be
#'   important because high values predict success or because they predict
#'   failure, and this does not distinguish the two. Correlated predictors split
#'   their share rather than each showing the full effect. And the ranking is a
#'   property of \emph{this} fit on \emph{this} training data, so it moves with
#'   the hyperparameters -- deeper trees spread weight across more variables by
#'   construction.
#'
#' @param fit A trained AdaBoost model from \code{\link[adatutor]{adaboost}}:
#'   a list of trees and their corresponding model weights.
#'
#' @param x A \code{"gauge"} object, as returned by \code{gauge()}.
#'
#' @param top_n The maximum number of variables to display (defaults to 15). If
#'   the ensemble split on fewer variables, all of them are shown. The viridis
#'   gradient is stretched across however many bars are drawn, running from the
#'   darkest purple for the most important variable to yellow for the least.
#'
#' @param ... Ignored.
#'
#' @return \code{gauge()} returns a data frame of class \code{"gauge"}, sorted
#'   from most to least important, with columns \code{variable} and
#'   \code{importance} (a percentage). \code{plot()} is called for its side
#'   effect and returns \code{NULL} invisibly.
#'
#' @seealso \code{\link[adatutor]{adaboost}}, \code{\link[adatutor]{assess}}
#'
#' @examples
#' data(altmejd)
#' prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
#'
#' fit <- adaboost(
#'   replicate ~ .,
#'   data = altmejd[, c(prednms, "replicate")],
#'   T = 50,
#'   eta = 1,
#'   verbose = FALSE,
#'   input_checks = FALSE
#' )
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

  # Initialize an empty list to store our weight tallies
  var_wght <- list()

  # Loop through every single tree in the model
  for (t in seq_along(fit)) {
    tree <- fit[[t]]$h
    wght <- fit[[t]]$a

    # rpart records an improvement-based importance for every variable the tree
    # splits on. It is NULL for a tree that never split (a bare leaf) and has a
    # single entry for a stump.
    imp <- tree$variable.importance

    # Safety check: ensure the tree actually made a split
    if (is.null(imp) || sum(imp) == 0) {
      next
    }

    # Distribute this tree's model weight across the variables it split on. A
    # stump has one entry, so it receives the full weight.
    share <- wght * imp / sum(imp)

    for (v in names(share)) {
      if (is.null(var_wght[[v]])) {
        var_wght[[v]] <- share[[v]]
      } else {
        var_wght[[v]] <- var_wght[[v]] + share[[v]]
      }
    }
  }

  # No tree in the ensemble split on anything
  if (length(var_wght) == 0) {
    return(structure(
      data.frame(
        variable = character(0),
        importance = numeric(0),
        stringsAsFactors = FALSE
      ),
      class = c("gauge", "data.frame")
    ))
  }

  # Convert tallied list into 'clean' data frame
  imp_df <- data.frame(
    variable = names(var_wght),
    importance = unlist(var_wght),
    stringsAsFactors = FALSE
  )

  # normalize to [0,1] and scale to 100%: easier to read
  imp_df$importance <- (imp_df$importance / sum(imp_df$importance)) * 100

  # sort from most important to least important
  imp_df <- imp_df[order(-imp_df$importance), ]
  rownames(imp_df) <- NULL

  # the class is what lets plot(gauge(fit)) find its method
  structure(imp_df, class = c("gauge", "data.frame"))
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

  # Limit to top N features so chart isn't crowded
  n_plot <- min(top_n, nrow(x))
  top_imp <- utils::head(x, n_plot)

  # Reverse order so highest value plots at top
  top_imp <- top_imp[order(top_imp$importance, decreasing = FALSE), ]

  # Size the palette by the bars actually drawn, i.e. the smaller of `top_n` and
  # the number of features available. The gradient then spans its full range
  # either way: when the plot is truncated to `top_n`, and when the ensemble
  # split on fewer variables than requested.
  pal <- viridisLite::viridis(n_plot)

  # viridis runs dark purple -> yellow, so pal[1] is the darkest. The bars are
  # sorted ascending for the horizontal layout, so reversing hands pal[1] to the
  # last (highest) bar and the lighter end to the least important ones.
  bar_cols <- rev(pal)

  # Expand left margin (side 2) so long variable names don't get cut off
  old_par <- graphics::par(mar = c(5, 10, 4, 2) + 0.1)
  on.exit(graphics::par(old_par), add = TRUE)

  # Give the axis headroom past the longest bar, so its tick marks actually
  # reach the bar instead of stopping short (pretty()'s default ticks can
  # land well below the true max, e.g. ticks at 0/10/20/30 for a 39% bar)
  xmax <- min(max(top_imp$importance) + 5, 100)

  # Generate the plot
  graphics::barplot(
    top_imp$importance,
    names.arg = top_imp$variable,
    horiz = TRUE, # horizontal bars
    las = 1, # text horizontal
    xlim = c(0, xmax), # headroom past the longest bar
    col = bar_cols, # viridis, darkest purple = most important
    border = NA, # remove black borders from bars
    xlab = "Relative Importance (%)",
    main = "Feature Importance"
  )

  invisible(NULL)
}
