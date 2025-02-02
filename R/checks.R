#' @title Miscellaneous Functions for Input Validation
#'
#' @description A set of input validation functions for internal use that ensure
#'   the passed arguments meet the expected criteria (e.g., type, length, and
#'   value ranges). These functions catch common input errors, provide
#'   meaningful error messages and guidance for correction.
#'
#' @return All functions are called for their side effects. If the input is
#' invalid, an error, or warning is generated with helpful suggestions for
#' troubleshooting.
#'
#' @param x A generic input to be validated. The expected type depends on the
#'   specific function.
#'
#' @param trainnme A character string representing the name of the training
#'   set.
#'
#' @param testnme A character string representing the name of the test
#'   set.
#'
#' @name checks
#'
NULL

#' @rdname checks
#'
#' @description \code{check_df()} ensures that \code{x} is a data frame. If it
#'   is not, an error is thrown with a message indicating the issue.
#'
#' @examples
#' # Example usage:
#' check_df(altmext)  # Passes since altmext is a data frame
#' \dontrun{check_df(5)}  # Fails with an error message
#'
check_df <- function(x) {
  if (!is.data.frame(x)) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp),"` is not a data frame. ")
    sug <- paste0("Check that ", "`", deparse(x_inp),"` is valid.")
    stop(c(msg, sug), call. = FALSE)
   }
}

#' @rdname checks
#'
#' @description \code{check_eta()} validates that the input is a numeric value
#'   and, if it is greater than 1, provides a warning about potential
#'   overfitting.
#'
#' @examples
#' # Example usage:
#' check_eta(0.5)  # Passes
#' \dontrun{check_eta("a")}  # Fails with an error
#'
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

#' @rdname checks
#'
#' @description \code{check_length()} ensures that \code{x} contains at least
#'   one element.
#'
#' @details Ensures that the input is not empty. If the input has a length of 0,
#' an error message is raised with a suggestion to check the input.
#'
#' @examples
#' # Example usage:
#' check_length(c(1, 2, 3))  # Passes
#' \dontrun{check_length(character(0))}  # Fails with an error
#'
check_length <- function(x) {
  if (length(x) == 0) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp),"` has no elements. ")
    sug <- paste0("Check that ", "`", deparse(x_inp),"` is valid.")
    stop(c(msg, sug), call. = FALSE)
   }
}

#' @rdname checks
#'
#' @description \code{check_list()} validates that the input is a list.
#'
#' @details
#' Ensures that the input is a valid list. If not, an error message is raised.
#'
#' @examples
#' # Example usage:
#' check_list(list(a = 1, b = 2))  # Passes
#' \dontrun{check_list(5)}  # Fails with an error
#'
check_list <- function(x) {
  if (!is.list(x)) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp),"` is not a list. ")
    sug <- paste0("Check that ", "`", deparse(x_inp),"` is valid.")
    stop(c(msg, sug), call. = FALSE)
   }
}

#' @rdname checks
#'
#' @description \code{check_numeric()} validates that the input is a numeric
#'   value.
#'
#' @details
#' Ensures that the input is numeric. If not, an error message is raised with a
#' suggestion to validate the input.
#'
#' @examples
#' # Example usage:
#' check_numeric(5)  # Passes
#' \dontrun{check_numeric("a")}  # Fails with an error
#'
check_numeric <- function(x) {
  if (!is.numeric(x)) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp),"` is not a numeric value. ")
    sug <- paste0("Check that ", "`", deparse(x_inp),"` is valid.")
    stop(c(msg, sug), call. = FALSE)
   }
}

#' @rdname checks
#'
#' @description \code{check_train()} ensures that the training and test
#'   set names are not the same.
#'
#' @examples
#' # Example usage:
#' check_train("train", "test")  # Passes
#' \dontrun{check_train("train", "train")}  # Fails with an error
#'
check_train <- function(trainnme, testnme) {
    if(trainnme == testnme) {
      msg <- paste0("`", deparse(trainnme),"` was also used for training.")
      message(msg, call. = FALSE)
    }
}

#' @rdname checks
#'
#' @description \code{check_prop()} validates that the input is a proportion (a
#'   numeric value between 0 and 1).
#'
#' @examples
#' # Example usage:
#' check_prop(0.7)  # Passes
#' \dontrun{check_prop(1.5)}  # Fails with an error
#' \dontrun{check_prop(0)}  # Issues a warning
#'
check_prop <- function(x) {
    if(x < 0 | x > 1) {
      msg <- paste0("Specified proportion ", "`", deparse(x),"` is not between 0 and 1.")
      stop(msg, call. = FALSE)
    }
    if(x == 0 | x == 1) {
      msg <- paste0("Specified proportion ", "`", deparse(x),"` is not practical.")
      sug <- "Use a value greater than 0 and less than 1."
      warning(msg, call. = FALSE)
    }
}
