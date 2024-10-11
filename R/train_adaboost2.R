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
  fcl <- match.call()
  mtch <- match(c("formula", "data"), names(fcl), nomatch = 0L)
  tmp <- fcl[c(1L, mtch)]
  tmp[[1L]] <- quote(stats::model.frame)
  mf <- eval.parent(tmp)
  y_train <- model.response(mf)
  m <- nrow(data)
  D <- rep(1, m)/m
  tmp[[1L]] <- quote(rpart::rpart)
  tmp$weights <- as.symbol("D")
  tmp$method <- "class"
  tmp$control <- as.symbol("tree_hyperpar")
  rpart_call <- tmp
  mtch <- match(c("data", "method"), names(tmp), nomatch = 0L)
  tmp <- tmp[c(1L, mtch)]
  tmp[[1L]] <- quote(predict)
  names(tmp)[2:3] <- c("newdata", "type")
  tmp$object <- as.symbol("h")
  pred_call <- tmp
  H <- vector("list", T)
  walking_dots()
  message("Steps 1-4: Run through the algorithm steps.")
  pb <- txtProgressBar(min = 0, max = T, style = 3)
  for (t in seq_len(T)) {
    h <- eval(rpart_call)
    y_retro <- eval(pred_call)
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
