#' Viridis colors for an rpart tree
#'
#' Builds the node fill and text colors that [rpart.plot::rpart.plot()] needs
#' to draw a classification tree on the viridis scale. Each fill comes from
#' the node's fitted probability, and the text turns white on dark fills so it
#' stays readable. The same probability gets the same color in every tree.
#'
#' @param fit A classification tree from [rpart::rpart()].
#' @param palette A function that takes a count and returns that many colors.
#'   Defaults to [viridisLite::viridis()].
#' @param n The number of steps in the color scale. Defaults to 100.
#'
#' @return A list with `box` and `text`, each with one color per node of
#'   `fit`, in rpart's node order.
#'
#' @family tutorial plots
#'
#' @examples
#' data(altmejd)
#' cstump <- rpart::rpart(
#'   replicate ~ power.o + n.o,
#'   data = altmejd,
#'   method = "class"
#' )
#' sty <- viridis_tree(cstump)
#' rpart.plot::rpart.plot(cstump, box.col = sty$box, col = sty$text)
#'
#' @export
viridis_tree <- function(fit, palette = viridisLite::viridis, n = 100) {
  if (!inherits(fit, "rpart")) {
    stop("`fit` must be an rpart object.", call. = FALSE)
  }
  yv <- fit$frame$yval2
  # yval, class counts, class probabilities, node probability: 2 + 2 * nclass
  if (is.null(dim(yv)) || ncol(yv) < 6L || ncol(yv) %% 2L != 0L) {
    stop(
      "`fit` must be a classification tree, fitted with method = \"class\".",
      call. = FALSE
    )
  }

  # probability of the last class: the column before the node probability
  prob <- yv[, ncol(yv) - 1L]

  ramp <- palette(n)
  # cut [0, 1] absolutely, not by quantiles, so a probability keeps its color
  idx <- pmin(n, pmax(1L, ceiling(prob * n)))
  box <- ramp[idx]

  list(box = box, text = contrast_stroke(box))
}

#' Pick black or white to draw on a fill color
#'
#' Uses Rec. 709 luminance with a cut at 0.55, so any palette works.
#' @noRd
contrast_stroke <- function(cols) {
  rgb <- grDevices::col2rgb(cols) / 255
  # unname(): a single color would carry the channel's rowname into `col`
  lum <- unname(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
  ifelse(lum > 0.55, "grey15", "white")
}
