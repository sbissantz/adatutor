#' @title Viridis Colors for an rpart Tree
#'
#' @description Builds the two color vectors \code{\link[rpart.plot]{rpart.plot}}
#'   needs to draw a classification tree on the viridis scale: one fill per node,
#'   taken from the node's fitted probability, and one text color per node,
#'   chosen so the label stays legible on that fill.
#'
#' @details Passing viridis to \code{rpart.plot}'s own \code{box.palette} does
#'   not work. Roughly half the scale is too dark for the black node text, so the
#'   labels in the darkest boxes disappear. \code{box.col} and \code{col} both
#'   accept one value per node, which is what this function supplies: the full
#'   scale is available for the boxes because the text turns white wherever the
#'   box is dark.
#'
#'   The mapping is also absolute rather than relative. \code{box.palette} splits
#'   the fitted values at their quantiles, so the same probability draws a
#'   different color in different trees. Here a node at .65 is the same color in
#'   every tree.
#'
#' @param fit An \code{\link[rpart]{rpart}} object fitted with
#'   \code{method = "class"}.
#'
#' @param palette A palette function taking a count and returning that many
#'   colors. Defaults to \code{\link[viridisLite]{viridis}}.
#'
#' @param n The number of steps to cut the scale into. Defaults to 100.
#'
#' @return A list with \code{box} and \code{text}, both character vectors with
#'   one entry per node of \code{fit}, in the order \code{rpart} stores them.
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
  # a classification fit stores yval, the class counts, the class probabilities
  # and the node probability, so the width is 2 + 2 * nclass
  if (is.null(dim(yv)) || ncol(yv) < 6L || ncol(yv) %% 2L != 0L) {
    stop(
      "`fit` must be a classification tree, fitted with method = \"class\".",
      call. = FALSE
    )
  }

  # probability of the last class: the column before the node probability
  prob <- yv[, ncol(yv) - 1L]

  ramp <- palette(n)
  # an absolute cut of [0, 1], never quantiles of the fitted values, so the same
  # probability is the same color in every tree
  idx <- pmin(n, pmax(1L, ceiling(prob * n)))
  box <- ramp[idx]

  list(box = box, text = contrast_stroke(box))
}
