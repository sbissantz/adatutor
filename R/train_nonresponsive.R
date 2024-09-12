train_nonresponsive <- function(fml, data, T, eta, tree_hyperpar) {
  H <- vector("list", T)
  m <- nrow(data)
  D <- rep(1, m)/m
  y_nme <- all.vars(as.formula(fml))[1]
  y_train <- data[[y_nme]]
  for (t in seq_len(T)) {
    h <- rpart::rpart(fml, data, D, method = "class",  control = tree_hyperpar)
    y_retro <- predict(h, data, type = "class")
    e <- sum(D * (y_train != y_retro))
    a <- 0.5 * log((1 - e) / e) * eta
    chi <- (y_train == y_retro) * 1 + (y_train != y_retro) * -1
    D_unorm <- (D * exp(-a * chi))
    D <- D_unorm / sum(D_unorm)
    H[[t]] <- list("h" = h, "a" = a)
  }
  names(H) <- paste0("t", seq_len(T))
  H
}
