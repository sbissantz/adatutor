#'@export
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
