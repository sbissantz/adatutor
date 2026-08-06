#' @title Generate Predictions or Retrodictions From a Set of (Adaptively) Boosted Trees and Model Weights
#'
#' @description This function implements the testing phase of the AdaBoost
#'   algorithm. It extracts the adaptively boosted weak learners (e.g.
#'   classification stumps) and their corresponding weights to combine them into
#'   the weighted sum \deqn{H(\mathbf{x}) = \sum_{i=1}^Ta_th_t(\mathbf x),}
#'   which yields AdaBoost's predictions or retrodictions on a given set.
#'
#' @param fit A model fitted with \code{\link[adatutor]{trainAda}}. This is a
#'   list of boosted trees and theirs weights. More specifically, each element
#'   of the list contains a weak learner (e.g., a decision stump) and its
#'   corresponding weight.
#'
#' @param data A data frame containing the test set for predictions. The
#'   structure should match the training set used to generate \code{h}.
#'
#' @param type A character string indicating the type of prediction to return.
#'   Options are \code{"class"} for hard class labels (-1, 1) or \code{"margin"}
#'   for the raw continuous boosting score. Defaults to \code{"class"}. Use
#'   \code{"margin"} for rank-based measures such as the ROC AUC: the margin
#'   carries the ranking that hard class labels throw away.
#'
#' @param input_checks A logical value indicating whether to perform input
#'   validation checks. Defaults to \code{TRUE}.
#'
#' @param verbose A logical value specifying whether to display progress
#'   messages and animations. Defaults to \code{TRUE}.
#'
#' @return A numeric vector containing the final predictions from the AdaBoost
#'   model. Depending on the \code{type} argument, this will be class labels or
#'   margins.
#'
#' @export
testAda <- function(
  fit,
  data,
  type = c("class", "margin"),
  input_checks = TRUE,
  verbose = TRUE
) {
  # Match the requested type (defaults to "class")
  type <- match.arg(type)

  if (verbose) {
    color_message("Start the AdaBoost test process:\n", color_code = 1)
  }
  if (input_checks) {
    if (verbose) {
      color_message("Run mild input checks", color_code = 30)
    }
    check_list(fit)
    check_length(fit)
    check_df(data)
    check_length(data)
    fcl <- match.call()
    test_pos <- match("data", names(fcl), nomatch = 0L)
    trainnme <- attr(fit, "train")
    check_train(trainnme, fcl[[test_pos]])
  }
  if (verbose) {
    walking_colordots()
    color_message("Extract the trees", color_code = 30)
    walking_colordots()
  }
  h <- lapply(fit, "[[", "h")
  check_length(h)
  if (verbose) {
    color_message("Extract the model weights", color_code = 30)
    walking_colordots()
  }
  a <- vapply(fit, "[[", numeric(1), "a")
  check_length(a)

  if (verbose) {
    color_message("Make predictions/retrodictions\n", color_code = 30)
  }

  # Define expected N for vapply
  N <- nrow(data)
  # Use vapply instead of sapply (faster). Wrap in matrix() because vapply
  # simplifies to a plain vector when N == 1, which would turn the matrix
  # product below into a T x T outer product.
  y12_stumps <- matrix(
    vapply(
      h,
      function(tree) {
        stats::predict(tree, newdata = data, type = "vector")
      },
      FUN.VALUE = numeric(N)
    ),
    nrow = N
  )

  if (verbose) {
    color_message("Combine predictions/retrodictions", color_code = 30)
  }

  ypred_stumps <- 2 * y12_stumps - 3

  # Calculate the continuous raw margin
  raw_margin <- as.vector(a %*% t(ypred_stumps))

  if (verbose) {
    walking_colordots()
    color_message("Test process successfully completed.\n", color_code = 1)
  }

  # Return based on requested type
  if (type == "class") {
    sign(raw_margin)
  } else {
    raw_margin
  }
}
