#' @title Train a Set of Classification Trees Using AdaBoost
#'
#' @description The function trains a set of classification trees using
#'   AdaBoost. It is built on top of the \code{rpart} package, so the full range
#'   of tree hyperparameters can be used to fine-tune the trees (see, e.g.,
#'   \code{\link[rpart]{rpart.control}}). In addition, the implementation
#'   allows customization of the number of trees and the learning rate. It also
#'   provides verbose output to track the training progress.
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
#' @param maxdepth An integer specifying the maximum depth of the decision trees.
#'   Defaults to 1.
#'
#' @param treehypar An optional list of additional control parameters for decision trees (passed to rpart.control). These will be safely merged with the internal AdaBoost speed optimizations. Defaults to \code{formula}.
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
#' 3. Compute the weighted classification error and update the observation
#' weights.
#' 4. Store the weak learner and its associated weight.
#'
#' @return A list containing the trained weak learners (`h`) and their
#' associated weights (`a`). Additional attributes may be included for model
#' tracking purposes.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' library(rpart)
#' data(iris)
#' # Prepare binary classification data
#' irisbin <- iris[iris$Species != "setosa", ]
#' irisbin$Species <- factor(irisbin$Species)
#'
#' # Set tree hyperparameters
#' treehypar <- rpart::rpart.control(maxdepth = 1, cp = 0)
#'
#' # Train AdaBoost model
#' model <- trainAda(Species ~ ., data = irisbin, T = 10, eta = 1,
#' treehypar = treehypar)
#' }
#'
#' @export
trainAda <- function(formula, data, T, eta, maxdepth = 1, treehypar = NULL, input_checks = TRUE, verbose = TRUE) {
  if(verbose) {
    color_message("Start the AdaBoost training process:\n",
                  color_code = 1, newline = TRUE)
  }
  if(input_checks) {
    if(verbose) {
      color_message("Run mild input checks", color_code = 30)
    }
    check_df(data)
    check_length(data)
    check_eta(eta)
    check_numeric(T)
    if(verbose) {
      walking_colordots()
    }
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
  
  # Match the function call
  fcl <- match.call()
  # Manually build the model frame function call ...
  mtch <- match(c("formula", "data"), names(fcl), nomatch = 0L)
  tmp <- fcl[c(1L, mtch)] # Use a tmp variable to store process details
  tmp[[1L]] <- quote(stats::model.frame) # Change the function name
  mf <- eval.parent(tmp) # Evaluate the model frame
  
  # Some relevant variables
  y_train <- stats::model.response(mf) # Store the model response
  m <- nrow(mf) # Store the number of rows
  D <- rep(1, m)/m # Calculate the initialization weights
  H <- vector("list", T) # Empty container for the trees and weights

  # Manually build the rpart function call ...
  tmp[[1L]] <- quote(rpart::rpart) # Change the function name
  tmp$weights <- as.symbol("D")
  tmp$method <- "class"
  tmp$control <- as.symbol("treehypar")

  # Assign tmp to cl_rpart
  cl_rpart <- tmp
  # Tell rpart to drop memory-heavy objects
  cl_rpart$model <- FALSE 
  cl_rpart$y <- FALSE

  # Manually build the prediction function call ...
  mtch <- match(c("data", "method"), names(tmp), nomatch = 0L)
  tmp <- tmp[c(1L, mtch)]
  tmp[[1L]] <- quote(stats::predict) # Change the function name
  names(tmp)[2:3] <- c("newdata", "type")
  tmp$object <- as.symbol("h")
  cl_pred <- tmp

  # Verbose output for the training process
  if(verbose) {
    walking_colordots()
    color_message("Steps 1-4: Run through the algorithm steps\n",
                  color_code = 30)
    pb <- utils::txtProgressBar(min = 0, max = T, style = 3)
  }

  # Algorithmic steps (optimized: speed & memory)
  for (t in seq_len(T)) {
    h <- eval(cl_rpart)
    y_retro <- eval(cl_pred)
    correct <- (y_train == y_retro)
    e_init <- sum(D[!correct]) 
    e <- max(e_init, 1e-10) 
    a <- 0.5 * log((1 - e) / e) * eta
    exp_a <- exp(a)
    exp_ma <- exp(-a)
    D_unorm <- D
    D_unorm[correct] <- D[correct] * exp_ma
    D_unorm[!correct] <- D[!correct] * exp_a
    D <- D_unorm / sum(D_unorm)
    h$where <- NULL
    h$call <- NULL
    H[[t]] <- list("h" = h, "a" = a)
    if(verbose) {
      utils::setTxtProgressBar(pb, t)
    }
  }

  # Verbose output for the training process
  if(verbose) {
    close(pb)
    color_message("Create output", color_code = 30)
    walking_colordots()
  }

  # Name the list elements for clarity
  names(H) <- paste0("t", seq_len(T))
  if(verbose) {
    color_message("Training process successfully completed.\n", color_code = 1,
                newline = TRUE)
  }

  # Add name as attribute for tracking
  attr(H, "train") <- cl_pred$newdata

  # Return object
  H
}
