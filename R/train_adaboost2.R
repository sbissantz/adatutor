#'@export
train_adaboost2 <- function(formula, data, T, eta, tree_hyperpar, input_checks = TRUE) {
  message("Start the AdaBoost training process:")
  if(input_checks) {
    message("Run mild input checks", appendLF = FALSE)
    check_df(data)
    check_length(data)
    check_eta(eta)
    check_numeric(T)
    walking_dots()
  }
  message("Start the initialization process", appendLF = FALSE)
  # Match the function call
  fcl <- match.call()
  # Manually build the model frame function call ...
  mtch <- match(c("formula", "data"), names(fcl), nomatch = 0L)
  tmp <- fcl[c(1L, mtch)] # Use a tmp variable to store process details
  tmp[[1L]] <- quote(stats::model.frame) # Change the function name
  mf <- eval.parent(tmp) # Evaluate the model frame
  # Some relevant variables
  y_train <- model.response(mf) # Store the model response
  m <- nrow(data) # Store the number of rowns
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
  # Container for the ensemble
  walking_dots()
  message("Steps 1-4: Run through the algorithm steps.")
  pb <- txtProgressBar(min = 0, max = T, style = 3)
  for (t in seq_len(T)) {
    h <- eval(cl_rpart)
    y_retro <- eval(cl_pred)
    e <- sum(D * (y_train != y_retro))
    a <- 0.5 * log((1 - e) / e) * eta
    chi <- (y_train == y_retro) * 1 + (y_train != y_retro) * -1
    D_unorm <- (D * exp(-a * chi))
    D <- D_unorm / sum(D_unorm)
    H[[t]] <- list("h" = h, "a" = a)
    setTxtProgressBar(pb, t)
  }
  close(pb)
  message("Training process successfully completed.\n")
  names(H) <- paste0("t", seq_len(T))
  H
}
