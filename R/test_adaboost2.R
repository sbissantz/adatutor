#'@export
test_adaboost2 <- function(H, data, input_checks = TRUE) {
  message("Start the AdaBoost test process:")
  if(input_checks) {
    message("Run mild input checks.", appendLF = FALSE)
    check_list(H)
    check_length(H)
    check_df(data)
    check_length(data)
    walking_dots()
  }
  message("Step 1: Extract the trees.", appendLF = FALSE)
  walking_dots()
  h <- lapply(H, "[[", "h") ; check_length(h)
  message("Step 2: Extract the model weights.", appendLF = FALSE)
  walking_dots()
  a <- vapply(H, "[[", numeric(1), "a") ; check_length(a)
  message("Step 3: Make predictions/retrodictions.")
  pb <- txtProgressBar(min = 0, max = length(h), style = 3)
  y12_stumps <- sapply(seq_along(h), function(t) {
    setTxtProgressBar(pb, t)
    predict(h[[t]], newdata = data, type = "vector")
  })
  close(pb)  # Close progress bar when done
  message("Step 4: Combine predictions/retrodictions.", appendLF = FALSE)
  ypred_stumps <- 2 * y12_stumps - 3
  ypred_ada <- sign(a %*% t(ypred_stumps))
  walking_dots()
  message("Test process successfully completed.\n")
  ypred_ada
}
