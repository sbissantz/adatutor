check_df <- function(x) {
  if (!is.data.frame(x)) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp),"` is not a data frame. ")
    sug <- paste0("Check that ", "`", deparse(x_inp),"` is valid.")
    stop(c(msg, sug), call. = FALSE)
   }
}

check_eta <- function(x) {
  x_inp <- substitute(x)
  if (!is.numeric(x)) {
    msg <- paste0("`", deparse(x_inp),"` is not a numeric value. ")
    sug <- paste0("Check that ", "`", deparse(x_inp),"` is valid.")
    stop(c(msg, sug), call. = FALSE)
  } else {
    if (x > 1) {
    msg <- paste0("The value of `eta` is greater than 1. ")
    sug <- paste0("Think about overfitting.")
    message(c(msg, sug))
    }
  }
}

check_length <- function(x) {
  if (length(x) == 0) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp),"` has no elements. ")
    sug <- paste0("Check that ", "`", deparse(x_inp),"` is valid.")
    stop(c(msg, sug), call. = FALSE)
   }
}

check_list <- function(x) {
  if (!is.list(x)) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp),"` is not a list. ")
    sug <- paste0("Check that ", "`", deparse(x_inp),"` is valid.")
    stop(c(msg, sug), call. = FALSE)
   }
}

check_numeric <- function(x) {
  if (!is.numeric(x)) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp),"` is not a numeric value. ")
    sug <- paste0("Check that ", "`", deparse(x_inp),"` is valid.")
    stop(c(msg, sug), call. = FALSE)
   }
}

check_train <- function(train_nme, test_nme) {
    if(train_nme == test_nme) {
      msg <- paste0("`", deparse(train_nme),"` was also used for training.")
      stop(msg, call. = FALSE)
    }
}

check_prop <- function(x) {
    if(min(x) < 0 | max(x) > 1) {
      msg <- paste0("Specified proportion ", "`", deparse(x),"` is not between 0 and 1.")
      stop(msg, call. = FALSE)
    }
    if(min(x) == 0 | max(x) == 1) {
      msg <- paste0("Specified proportion ", "`", deparse(x),"` is not practical.")
      warning(msg, call. = FALSE)
    }
}
