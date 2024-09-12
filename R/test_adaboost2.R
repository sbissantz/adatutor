#'@export
# TODO: Make a response and maybe a nonresponsive version with switch!
test_adaboost2 <- function(H, data, input_checks = TRUE) {
  message("Start the AdaBoost test process:")
  if(isTRUE(input_checks)) {
    message("Run mild input checks.")
    check_list(H) ; check_empty(H)
    check_list(data) ; check_empty(data)
  }
  message("Step 1: Extract the trees.")
  h <- lapply(H, "[[", "h") ; check_empty(h)
  message("Step 2: Extract the model weights.")
  a <- vapply(H, "[[", FUN.VALUE = numeric(1), "a")
  message("Step 3: Make predictions/retrodictions.")
  pb <- txtProgressBar(min = 0, max = length(h), style = 3)
  y12_stumps <- sapply(seq_along(h), function(i) {
    setTxtProgressBar(pb, i)
    predict(h[[i]], newdata = data, type = "vector")
  })
  close(pb)  # Close progress bar when done
  message("Step 4: Combine predictions/retrodictions.")
  ypred_stumps <- 2 * y12_stumps - 3
  ypred_ada <- sign(a %*% t(ypred_stumps))
  # Completion message
  message("Test process successfully completed.\n")
  ypred_ada
}
