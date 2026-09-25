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
#' h <- rpart::rpart(
#'   replicate ~ .,
#'   data = altmejd[, c(prednms, "replicate")],
#'   maxdepth = 1,
#'   model = TRUE
#' )
#' fit <- adaboost(h, n_iter = 50, eta = 1, verbose = FALSE, input_checks = FALSE)
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

  var_wght <- list()

  for (t in seq_along(fit)) {
    tree <- fit[[t]]$h
    wght <- fit[[t]]$a

    # improvement-based importance: NULL for a bare leaf, one entry for a stump
    imp <- tree$variable.importance

    if (is.null(imp) || sum(imp) == 0) {
      next
    }

    # split the tree's model weight across its variables; a stump gets it all
    share <- wght * imp / sum(imp)

    for (v in names(share)) {
      if (is.null(var_wght[[v]])) {
        var_wght[[v]] <- share[[v]]
      } else {
        var_wght[[v]] <- var_wght[[v]] + share[[v]]
      }
    }
  }

  # no tree in the ensemble split on anything
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

  imp_df <- data.frame(
    variable = names(var_wght),
    importance = unlist(var_wght),
    stringsAsFactors = FALSE
  )

  # percent of the total: easier to read
  imp_df$importance <- (imp_df$importance / sum(imp_df$importance)) * 100

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

  # top_n features only, so the chart stays readable
  n_plot <- min(top_n, nrow(x))
  top_imp <- utils::head(x, n_plot)

  # ascending, so the highest bar plots at the top
  top_imp <- top_imp[order(top_imp$importance, decreasing = FALSE), ]

  # size the palette by the bars drawn, so the gradient spans its full range
  pal <- viridisLite::viridis(n_plot)

  # reverse viridis so the darkest color goes to the most important bar
  bar_cols <- rev(pal)

  # wide left margin for long variable names
  old_par <- graphics::par(mar = c(5, 10, 4, 2) + 0.1)
  on.exit(graphics::par(old_par), add = TRUE)

  # headroom past the longest bar: pretty() ticks can stop well below it
  xmax <- min(max(top_imp$importance) + 5, 100)

  graphics::barplot(
    top_imp$importance,
    names.arg = top_imp$variable,
    horiz = TRUE,
    las = 1, # horizontal labels
    xlim = c(0, xmax),
    col = bar_cols,
    border = NA,
    xlab = "Relative Importance (%)",
    main = "Feature Importance"
  )

  invisible(NULL)
}
