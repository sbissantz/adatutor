#' @title Train a Set of Classification Trees Using AdaBoost
#'
#' @description The function trains a set of classification trees using
#'   AdaBoost. It is built on top of the \code{rpart} package, so the full range
#'   of tree hyperparameters can be used to fine-tune the trees (see, e.g.,
#'   \code{\link[rpart]{rpart.control}}). By default, it uses hyperparameter 
#'   settings heavily optimized for boosting (no internal CV, no missing data 
#'   splits, zero complexity parameter). Users can overwrite these or add 
#'   additional parameters via the \code{treehypar} argument.
#'
#' @param formula A \code{\link[stats]{formula}} specifying the relationship
#'   between the outcome and the predictors: `outcome ~ predictors`. The
#'   predictors should be included as additive terms (e.g., `X1 + X2 + ...`),
#'   Interactions are not supported (see the \code{formula} argument in
#'   \code{\link[rpart]{rpart}}).
#'
#' @param data A data frame containing the variables in the model.
#'
#' @param T An integer specifying the number of trees.
#'
#' @param eta A numeric value representing the learning rate of the algorithm.
#' 
#' @param maxdepth An integer specifying the maximum depth of the trees. 
#'   Defaults to 1 (decision stumps).
#'
#' @param treehypar An optional list of additional control parameters for decision
#'   trees (passed to \code{\link[rpart]{rpart.control}}). These will be safely
#'   merged with the internal AdaBoost speed optimizations. Defaults to \code{NULL}.
#'
#' @param calibrate A logical value indicating whether to attach a probability
#'   calibrator (Platt scaling) to the fitted model. When \code{TRUE}, a
#'   calibrator is fitted via internal cross-validation (see
#'   \code{\link[adatutor]{calibrateAda}}) and stored on the returned object, so
#'   that \code{\link[adatutor]{testAda}} with \code{type = "prob"} returns
#'   calibrated probabilities. Defaults to \code{FALSE} (raw logistic transform).
#'
#' @param nfolds An integer giving the number of cross-validation folds used to
#'   build the calibrator when \code{calibrate = TRUE}. Defaults to 5.
#'
#' @param input_checks A logical value indicating whether to perform input
#'   validation checks. Defaults to `TRUE`.
#'
#' @param verbose A logical value indicating whether to display verbose output
#'   during the training process. Defaults to `TRUE`.
#'
#' @details The function implements the AdaBoost algorithm with a specified
#'   number of iterations (`T`). It initializes observation weights, trains a
#'   sequence of decision trees, and updates the weights at each iteration based
#'   on prediction errors. A final ensemble of weak learners is produced.
#'
#' Key steps in the algorithm:
#' 1. Initialize observation weights.
#' 2. Train a decision tree using the current weights.
#' 3. Compute the weighted classification error and update the observation weights.
#' 4. Store the weak learner and its associated weight.
#'
#' @return A list containing the trained weak learners (`h`) and their
#' associated weights (`a`). Additional attributes may be included for model
#' tracking purposes.
#'
#' @export
trainAda <- function(formula, data, T, eta, maxdepth = 1, treehypar = NULL, calibrate = FALSE, nfolds = 5, input_checks = TRUE, verbose = TRUE) {

  if(verbose) {
    color_message("Start the AdaBoost training process:\n", color_code = 1, newline = TRUE)
  }

  if(input_checks) {
    if(verbose) color_message("Run mild input checks", color_code = 30)
    check_df(data)
    check_length(data)
    check_eta(eta)
    check_numeric(T)
    if(verbose) walking_colordots()
  }

  if(verbose) {
    color_message("Start the initialization process", color_code = 30)
  }

  # Set fast default control parameters for rpart
  def_ctrl <- rpart::rpart.control(
    maxdepth = maxdepth,
    xval = 0,
    maxsurrogate = 0,
    cp = 0
  )

  # Overwrite defaults with user-specified parameters if provided
  if (!is.null(treehypar) && is.list(treehypar)) {
    treehypar <- utils::modifyList(def_ctrl, treehypar)
  } else {
    treehypar <- def_ctrl
  }

  # We still use match.call() ONLY to grab the literal name of the dataset 
  # so we can attach it as a tracking attribute at the very end.
  data_name <- match.call()[["data"]]

  # The magic bridge: binds the formula to this function's local environment
  environment(formula) <- environment()

  # Clean, direct extraction of the target variable
  mf <- stats::model.frame(formula, data)
  y_train <- stats::model.response(mf)

  # Setup weights and containers
  m <- nrow(data)
  D <- rep(1, m) / m
  H <- vector("list", T)

  if(verbose) {
    walking_colordots()
    color_message("Steps 1-4: Run through the algorithm steps\n", color_code = 30)
    pb <- utils::txtProgressBar(min = 0, max = T, style = 3)
  }

  for (t in seq_len(T)) {

    # Clean, direct standard evaluation function call
    h <- rpart::rpart(
      formula  = formula,
      data     = data,
      weights  = D,
      method   = "class",
      control  = treehypar,
      model    = FALSE,
      y        = FALSE
    )

    # Clean, direct prediction
    y_retro <- stats::predict(h, newdata = data, type = "class")

    # Math and weight updates
    correct <- (y_train == y_retro)
    # Weighted error, clamped away from 0 and 1 so the model weight stays finite
    # (e -> 0 or 1 would otherwise send `a` to +/-Inf). A stump worse than chance
    # (e > 0.5) legitimately yields a negative `a` and is handled by the update below.
    e <- min(max(sum(D[!correct]), 1e-10), 1 - 1e-10)
    a <- 0.5 * log((1 - e) / e) * eta

    D_unorm <- D
    D_unorm[ correct] <- D[ correct] * exp(-a)
    D_unorm[!correct] <- D[!correct] * exp( a)
    D <- D_unorm / sum(D_unorm)

    # Memory cleanup before storing
    h$where <- NULL
    h$call  <- NULL

    H[[t]] <- list("h" = h, "a" = a)

    if(verbose) utils::setTxtProgressBar(pb, t)
  }

  if(verbose) {
    close(pb)
    color_message("Create output", color_code = 30)
    walking_colordots()
    color_message("Training process successfully completed.\n", color_code = 1, newline = TRUE)
  }

  names(H) <- paste0("t", seq_len(T))
  attr(H, "train") <- data_name

  # Stash the hyperparameters (with a clean formula environment to avoid
  # capturing the training data) so the model can be refitted later, e.g. for
  # cross-validated probability calibration.
  form_store <- formula
  environment(form_store) <- globalenv()
  attr(H, "formula") <- form_store
  attr(H, "T") <- T
  attr(H, "eta") <- eta
  attr(H, "maxdepth") <- maxdepth

  # Optionally attach a Platt-scaling calibrator built via internal CV.
  if (calibrate) {
    if (verbose) color_message("Fit probability calibrator (Platt scaling)", color_code = 30)
    attr(H, "calibrator") <- calibrateAda(H, data, nfolds = nfolds, verbose = FALSE)
    if (verbose) walking_colordots()
  }

  return(H)
}