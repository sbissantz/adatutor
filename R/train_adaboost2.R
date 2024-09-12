#'@export
train_adaboost2 <- function(fml, data, T, eta, tree_hyperpar, input_checks = TRUE) {
  message("Start the AdaBoost training process:")
  if(isTRUE(input_checks)) {
    message("Run mild input checks.")
    check_df(data) ; check_length(data)
    check_eta(eta) ; check_numeric(T)
  }
  message("Start the initialization process.")
  H <- vector("list", T)
  m <- nrow(data)
  D <- rep(1, m)/m
  y_nme <- all.vars(as.formula(fml))[1]
  y_train <- data[[y_nme]]
  message("Steps 1-4: Run through the algorithm steps.")
  pb <- txtProgressBar(min = 0, max = T, style = 3)
  for (t in seq_len(T)) {
    h <- rpart::rpart(as.formula(fml), data, D, method = "class",
                      control = tree_hyperpar)
    y_retro <- predict(h, data, type = "class")
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
