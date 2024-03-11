#'@export
train_adaboost <- function(X, y, eta, n_trees, tree_hyperpar) {
  # Extract the number of rows
  n_samples <- nrow(X)
  # Initialize the weights
  D <- rep(1, n_samples)/n_samples
  # Pre-allocate the empty ensemble
  H <- vector("list", n_trees)
  # Pre-allocate the data set
  df <- data.frame(X, y)
  # Sequentially build the homogeneous ensemble
  for (tree in 1:n_trees) {
    # Train a single weak learner (h_t) with normalized(!) sample weights (D)
    h <- rpart::rpart(formula = y ~ ., data = df , method = "class",
                      weights = D, control = tree_hyperpar)
    # Make predictions with the weak learner (h_t)
    y_pred <- predict(object = h, newdata = X, type = "class")
    # y_pred <- 2*as.numeric(y_class) - 3
    wacc <- sum(D * (y == y_pred)) / sum(D)
    # Compute the training error (e_t) of the weak learner (h_t)
    # This is simply one minus the weighted accuracy, given weights D
    e <- 1 - wacc
    # Equivalent but less readable: e <- sum(D * (y != y_pred)) / sum(D)
    # Compute the the weight (a_t) of the weak learner (h_t)
    # Reduce the learner weight by a factor eta (learning rate)
    a <- 0.5 * log( (1 - e)  / e) * eta
    # Pre-Prunning or Early Stopping:
    # Do not forget to add the parameter (prunning threshold)
    # if (a < prunning_threshold) {
    #  message("Prunning threshold reached")
    #  break
    # }
    # Updates the weights
    # Increase for misclassified examples: (y == y_pred)
    # decrease for correctly classified examples: (y!=y_pred)
    m <- (y == y_pred) * 1 + (y != y_pred) * -1
    # Update the sample weights and limit the contribution of h_t by eta
    D <- D * exp(-a * m)
    # Normalize weights (probabilistic interpretation)
    D <- D/sum(D)
    # Save the weak learner and its weight
    H[[tree]] <- list("learner" = h, "learner_weight" = a)
    # H[[t]] <- list("weight" = a, "weaklearner" = h, "sampleweight" = D)
    # Store sample weights for visualization
    # cat(t,"weighted accuracy:", wacc, "\n")
    # Plot
    # rattle::fancyRpartPlot(h, caption = NULL)
    # Sys.sleep(2)
  }
  # Return the ensemble
  H
}
