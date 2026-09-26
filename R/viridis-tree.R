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
  node_stats <- fit$frame$yval2
  # yval, class counts, class probabilities, node probability: 2 + 2 * nclass
  if (
    is.null(dim(node_stats)) ||
      ncol(node_stats) < 6L ||
      ncol(node_stats) %% 2L != 0L
  ) {
    stop(
      "`fit` must be a classification tree, fitted with method = \"class\".",
      call. = FALSE
    )
  }

  # probability of the last class: the column before the node probability
  prob <- node_stats[, ncol(node_stats) - 1L]

  ramp <- palette(n)
  # cut [0, 1] absolutely, not by quantiles, so a probability keeps its color
  steps <- pmin(n, pmax(1L, ceiling(prob * n)))
  box <- ramp[steps]

  list(box = box, text = choose_ink(box))
}

#' Choose black or white ink to draw on a fill color
#'
#' Uses Rec. 709 luminance with a cut at 0.55, so any palette works.
#' @noRd
choose_ink <- function(colors) {
  channels <- grDevices::col2rgb(colors) / 255
  # unname(): a single color would carry the channel's rowname along
  luminance <- unname(
    0.2126 * channels[1, ] + 0.7152 * channels[2, ] + 0.0722 * channels[3, ]
  )
  ifelse(luminance > 0.55, "grey15", "white")
}
