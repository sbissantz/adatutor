#' Train an AdaBoost Model
#'
#' This function trains an AdaBoost model using decision trees as weak learners.
#' The implementation allows for customization of the number of iterations,
#' learning rate, and decision tree hyperparameters. It also provides verbose
#' output to track the training progress.
#'
#' @param formula A symbolic description of the model to be fitted (e.g.,
#'   `response ~ predictors`).
#' @param data A data frame containing the variables in the model.
#' @param T An integer specifying the number of boosting iterations.
#' @param eta A numeric value representing the learning rate of the algorithm.
#' @param tree_hyperpar A list of control parameters for the decision tree
#'   learner (passed to `rpart::rpart` as the `control` argument).
#' @param input_checks A logical value indicating whether to perform input
#'   validation checks. Defaults to `TRUE`.
#' @param verbose A logical value indicating whether to display verbose output
#'   during the training process. Defaults to `TRUE`.
#'
#' @details The function implements the AdaBoost algorithm with a specified
#' number of iterations (`T`). It initializes observation weights, trains a
#' sequence of decision trees, and updates the weights at each iteration based
#' on prediction errors. A final ensemble of weak learners is produced.
#'
#' Key steps in the algorithm:
#' 1. Initialize observation weights.
#' 2. Train a decision tree using the current weights.
#' 3. Compute the weighted classification error and update the observation
#' weights.
#' 4. Store the weak learner and its associated weight.
#'
#' @return
#' A list containing the trained weak learners (`h`) and their associated weights (`a`).
#' Additional attributes may be included for model tracking purposes.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' library(rpart)
#' data(iris)
#' # Prepare binary classification data
#' iris_binary <- iris[iris$Species != "setosa", ]
#' iris_binary$Species <- factor(iris_binary$Species)
#'
#' # Set tree hyperparameters
#' tree_control <- rpart.control(maxdepth = 1, cp = 0)
#'
#' # Train AdaBoost model
#' model <- train_adaboost3(Species ~ ., data = iris_binary, T = 10,
#'                          eta = 0.5, tree_hyperpar = tree_control)
#' }
#'
#' @export
train_adaboost3 <- function(formula, data, T, eta, tree_hyperpar, input_checks = TRUE, verbose = TRUE) {
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
  # Match the function call
  fcl <- match.call()
  # Manually build the model frame function call ...
  mtch <- match(c("formula", "data"), names(fcl), nomatch = 0L)
  tmp <- fcl[c(1L, mtch)] # Use a tmp variable to store process details
  tmp[[1L]] <- quote(stats::model.frame) # Change the function name
  mf <- eval.parent(tmp) # Evaluate the model frame
  # Some relevant variables
  y_train <- model.response(mf) # Store the model response
  m <- nrow(mf) # Store the number of rows
  D <- rep(1, m)/m # Calculate the initialization weights
  H <- vector("list", T) # Empty container for the trees
  # Manually build the rpart function call ...
  tmp[[1L]] <- quote(rpart::rpart) # Change the function name
  tmp$weights <- as.symbol("D")
  tmp$method <- "class"
  tmp$control <- as.symbol("tree_hyperpar")
  cl_rpart <- tmp
  # Manually build the prediction function call ...
  mtch <- match(c("data", "method"), names(tmp), nomatch = 0L)
  tmp <- tmp[c(1L, mtch)]
  tmp[[1L]] <- quote(predict) # Change the function name
  names(tmp)[2:3] <- c("newdata", "type")
  tmp$object <- as.symbol("h")
  cl_pred <- tmp
  if(verbose) {
    walking_colordots()
    color_message("Steps 1-4: Run through the algorithm steps\n",
                  color_code = 30)
    pb <- txtProgressBar(min = 0, max = T, style = 3)
  }
  for (t in seq_len(T)) {
    h <- eval(cl_rpart)
    y_retro <- eval(cl_pred)
    e <- sum(D * (y_train != y_retro))
    a <- 0.5 * log((1 - e) / e) * eta
    chi <- (y_train == y_retro) * 1 + (y_train != y_retro) * -1
    D_unorm <- (D * exp(-a * chi))
    D <- D_unorm / sum(D_unorm)
    H[[t]] <- list("h" = h, "a" = a)
    if(verbose) {
      setTxtProgressBar(pb, t)
    }
  }
  if(verbose) {
    close(pb)
    color_message("Create output", color_code = 30)
    walking_colordots()
  }
  names(H) <- paste0("t", seq_len(T))
  if(verbose) {
    color_message("Training process successfully completed.\n", color_code = 1,
                newline = TRUE)
  }
  attr(H, "train") <- cl_pred$newdata
  H
}
