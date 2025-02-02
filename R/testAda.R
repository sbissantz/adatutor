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
#' @param input_checks A logical value indicating whether to perform input
#'   validation checks. Defaults to \code{TRUE}.
#'
#' @param verbose A logical value specifying whether to display progress
#'   messages and animations. Defaults to \code{TRUE}.
#'
#' @details The function proceeds as follows:
#' \enumerate{
#'   \item If \code{input_checks} is \code{TRUE}, basic input validation is
#'   performed to ensure that \code{fit} is a valid AdaBoost model and
#'   \code{data} is appropriate for predictions.
#'   \item Progress messages and animations are shown if \code{verbose} is
#'   \code{TRUE}.
#'   \item Boosted weak learners (\code{h}) and their weights (\code{a}) are extracted
#'   from \code{fit}.
#'   \item Predictions or retrodictions are generated for each weak learner
#'   using the test set.
#'   \item Individual predictions are combined using the weights from the
#'   AdaBoost model to produce the final predictions.
#' }
#'
#' @return A numeric vector containing the final predictions from the AdaBoost
#'   model. The predictions are in the form of \code{-1} or \code{1},
#'   corresponding to binary classification outcomes.
#'
#' @examples
#' # Example usage:
#' # Assume `fit` is a trained model and `test` is a data frame.
#' # ypred <- testAda(fit, test)
#'
#'@export
testAda <- function(fit, data, input_checks = TRUE, verbose = TRUE) {
  if (verbose) {
    color_message("Start the AdaBoost test process:\n", color_code = 1)
  }
  if(input_checks) {
  if (verbose) {
    color_message("Run mild input checks", color_code = 30)
  }
    check_list(fit)
    check_length(fit)
    check_df(data)
    check_length(data)
    fcl <- match.call()
    test_pos <- match("data", names(fcl), nomatch = 0L)
    trainnme <-  attr(fit, "train")
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
    pb <- utils::txtProgressBar(min = 0, max = length(h), style = 3)
  }
  y12_stumps <- sapply(seq_along(h), function(t) {
  if (verbose) {
    utils::setTxtProgressBar(pb, t)
  }
    stats::predict(h[[t]], newdata = data, type = "vector")
  })
  if (verbose) {
    close(pb)  # Close progress bar when done
    color_message("Combine predictions/retrodictions", color_code = 30)
  }
  ypred_stumps <- 2 * y12_stumps - 3
  ypred_ada <- sign(a %*% t(ypred_stumps))
  if (verbose) {
    walking_colordots()
    color_message("Test process successfully completed.\n", color_code = 1)
  }
  ypred_ada
}
