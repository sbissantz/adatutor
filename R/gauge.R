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
#'   important. `plot()` draws a bar chart and returns `NULL` invisibly.
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
