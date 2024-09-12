#'@export
train_adaboost2 <- function(fml, data, T, eta, tree_hyperpar,
                            input_checks = TRUE, responsive = TRUE) {
  if (responsive) {
    message("Start the AdaBoost training process:")
  }
  if(input_checks) {
    if (responsive) {
      message("Run mild input checks", appendLF = FALSE)
    }
    check_df(data) ; check_length(data)
    check_eta(eta) ; check_numeric(T)
    if (responsive) {
      walking_dots()
    }
  }
  if (responsive) {
    train_responsive(fml, data, T, eta, tree_hyperpar)
  } else if (!responsive) {
    train_nonresponsive(fml, data, T, eta, tree_hyperpar)
  } else {
    stop("The value of `responsive` must be either `TRUE` or `FALSE`.",
         call. = FALSE)
  }
}
