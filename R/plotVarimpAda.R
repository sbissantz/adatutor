#' @title Plot AdaBoost Feature Importance
#'
#' @description Generates a horizontal bar chart of the top N most important features.
#'
#' @param imp_df The data frame output from \code{\link[adatutor]{varimpAda}}.
#' @param top_n The maximum number of variables to display (defaults to 15). If
#'   the ensemble split on fewer variables, all of them are shown. The viridis
#'   gradient is stretched across however many bars are drawn, running from the
#'   darkest purple for the most important variable to yellow for the least.
#' @export
plotVarimpAda <- function(imp_df, top_n = 15) {
  # Limit to top N features so chart isn't crowded
  n_plot <- min(top_n, nrow(imp_df))
  top_imp <- utils::head(imp_df, n_plot)

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
  # Restore original plot margins
  graphics::par(old_par)
}
