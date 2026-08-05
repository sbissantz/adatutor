#' @title Plot AdaBoost Feature Importance
#'
#' @description Generates a horizontal bar chart of the top N most important features.
#'
#' @param imp_df The data frame output from \code{\link[adatutor]{varimpAda}}.
#' @param top_n The number of top variables to display (defaults to 15).
#' @export
plotVarimpAda <- function(imp_df, top_n = 15) {
  # Limit to top N features so chart isn't crowded
  n_plot <- min(top_n, nrow(imp_df))
  top_imp <- head(imp_df, n_plot)

  # Reverse order so highest value plots at top
  top_imp <- top_imp[order(top_imp$importance, decreasing = FALSE), ]

  # Expand left margin (side 2) so long variable names don't get cut off
  old_par <- par(mar = c(5, 10, 4, 2) + 0.1)

  # Give the axis headroom past the longest bar, so its tick marks actually
  # reach the bar instead of stopping short (pretty()'s default ticks can
  # land well below the true max, e.g. ticks at 0/10/20/30 for a 39% bar)
  xmax <- min(max(top_imp$importance) + 5, 100)

  # Generate the plot
  barplot(
    top_imp$importance,
    names.arg = top_imp$variable,
    horiz = TRUE,          # horizontal bars
    las = 1,               # text horizontal
    xlim = c(0, xmax),     # headroom past the longest bar
    col = viridisLite::viridis(1), # single darkest (purple) viridis color
    border = NA,           # remove black borders from bars
    xlab = "Relative Importance (%)",
    main = "Feature Importance"
  )
  # Restore original plot margins
  par(old_par)
}
