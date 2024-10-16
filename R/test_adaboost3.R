#'@export
test_adaboost3 <- function(H, data, input_checks = TRUE) {
  color_message("Start the AdaBoost test process:\n", color_code = 1)
  if(isTRUE(input_checks)) {
    color_message("Run mild input checks", color_code = 30)
    check_list(H)
    check_length(H)
    check_df(data)
    check_length(data)
    walking_colordots()
  }
  color_message("Extract the trees", color_code = 30)
  walking_colordots()
  h <- lapply(H, "[[", "h") ; check_length(h)
  color_message("Extract the model weights", color_code = 30)
  walking_colordots()
  a <- vapply(H, "[[", numeric(1), "a") ; check_length(a)
  color_message("Make predictions/retrodictions\n", color_code = 30)
  pb <- txtProgressBar(min = 0, max = length(h), style = 3)
  y12_stumps <- sapply(seq_along(h), function(t) {
    setTxtProgressBar(pb, t)
    predict(h[[t]], newdata = data, type = "vector")
  })
  close(pb)  # Close progress bar when done
  color_message("Combine predictions/retrodictions", color_code = 30)
  ypred_stumps <- 2 * y12_stumps - 3
  ypred_ada <- sign(a %*% t(ypred_stumps))
  walking_colordots()
  color_message("Test process successfully completed.\n", color_code = 1)
  ypred_ada
}
