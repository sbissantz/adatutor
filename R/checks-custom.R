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
#' @param fit A fitted AdaBoost ensemble from \code{\link[adatutor]{adaboost}}.
#'
#' @param cm A confusion matrix from \code{\link[adatutor]{confusion}}, or any
#'   named vector carrying \code{tp}, \code{tn}, \code{fp} and \code{fn}.
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
#' \dontrun{check_df(5)}  # Fails with an error message
#'
check_df <- function(x) {
  if (!is.data.frame(x)) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp), "` is not a data frame. ")
    sug <- paste0("Check that ", "`", deparse(x_inp), "` is valid.")
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
#' \dontrun{check_eta("a")}  # Fails with an error
#'
check_eta <- function(x) {
  x_inp <- substitute(x)
  if (!is.numeric(x)) {
    msg <- paste0("`", deparse(x_inp), "` is not a numeric value. ")
    sug <- paste0("Check that ", "`", deparse(x_inp), "` is valid.")
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
#' \dontrun{check_length(character(0))}  # Fails with an error
#'
check_length <- function(x) {
  if (length(x) == 0) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp), "` has no elements. ")
    sug <- paste0("Check that ", "`", deparse(x_inp), "` is valid.")
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
#' \dontrun{check_list(5)}  # Fails with an error
#'
check_list <- function(x) {
  if (!is.list(x)) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp), "` is not a list. ")
    sug <- paste0("Check that ", "`", deparse(x_inp), "` is valid.")
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
#' \dontrun{check_numeric("a")}  # Fails with an error
#'
check_numeric <- function(x) {
  if (!is.numeric(x)) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp), "` is not a numeric value. ")
    sug <- paste0("Check that ", "`", deparse(x_inp), "` is valid.")
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
#' \dontrun{check_train("train", "train")}  # Fails with an error
#'
overlap_state <- function(trainnme, testnme, trainset = NULL, newdata = NULL) {
  # Exact when the fit kept its training frame: compares the rows themselves, so
  # a renamed variable or a subset is still seen. Falls back to the deparsed
  # name when `keep_data = FALSE`, which can only ever answer all-or-nothing --
  # and answers NA when even that is unavailable, because "we cannot tell" is a
  # different statement from "no overlap".
  if (!is.null(trainset) && !is.null(newdata)) {
    shared <- intersect(names(trainset), names(newdata))
    if (length(shared)) {
      rows <- do.call(paste, c(newdata[shared], sep = "\r"))
      tr <- do.call(paste, c(trainset[shared], sep = "\r"))
      hits <- sum(rows %in% tr)
      return(structure(
        if (hits == 0L) {
          "none"
        } else if (hits == length(rows)) {
          "all"
        } else {
          "some"
        },
        hits = hits,
        n = length(rows)
      ))
    }
  }
  if (!is.null(testnme) && identical(trainnme, testnme)) {
    return("all")
  }
  NA_character_
}

check_train <- function(
  state,
  testnme = NULL,
  verbose = FALSE,
  fell_back = FALSE
) {
  # Only "all" is reported. A partial match is not evidence: `altmejd` contains
  # genuinely duplicated rows, so two of the 23 rows in the shipped test set
  # match a training row on all four predictors and the outcome by coincidence.
  # Reporting "some" therefore fires on the package's own canonical test set
  # every time. "all" is sound -- every row matching means it really is the
  # training data -- which still catches a renamed variable and a subset, since
  # every row of a subset matches.
  if (is.na(state) || state != "all") {
    return(invisible(NULL))
  }

  # Two ways to end up here, and they deserve different first sentences: you
  # handed over the training frame, or you handed over nothing and predict()
  # fell back to it.
  lead <- if (fell_back) {
    "! No `newdata` specified. The model scored its training data."
  } else {
    what <- if (!is.null(testnme)) {
      paste0("`", deparse(testnme), "`")
    } else {
      "that data"
    }
    paste0("! ", what, " was also used for training.")
  }

  # Wrapped by hand rather than left to the terminal: a long line broken
  # mid-word is exactly what a reader skims past.
  body <- c(
    lead,
    "  Outpus are retrodictions, not predictions."
  )

  if (verbose) {
    # 33 is yellow. The transcript above this is bold, grey and green, so a
    # plain message would read as one more step rather than a caveat on all of
    # them. The leading blank line separates it from the run it follows.
    color_message(
      paste0(paste(body, collapse = "\n"), "\n"),
      color_code = 33
    )
  } else {
    message(paste(body, collapse = "\n"))
  }
}

#' @rdname checks
#'
#' @description \code{check_prop()} validates that the input is a proportion (a
#'   numeric value between 0 and 1).
#'
#' @examples
#' # Example usage:
#' \dontrun{check_prop(0)}  # Issues a warning
#'
check_prop <- function(x) {
  if (x < 0 | x > 1) {
    msg <- paste0(
      "Specified proportion ",
      "`",
      deparse(x),
      "` is not between 0 and 1."
    )
    stop(msg, call. = FALSE)
  }
  if (x == 0 | x == 1) {
    msg <- paste0(
      "Specified proportion ",
      "`",
      deparse(x),
      "` is not practical."
    )
    sug <- "Use a value greater than 0 and less than 1."
    warning(msg, call. = FALSE)
  }
}

#' @rdname checks
#'
#' @description \code{check_confusion()} ensures that \code{cm} carries the four
#'   counts \code{\link[adatutor]{confusion}} produces. It checks the names
#'   rather than the class, so a plain named vector assembled by hand still
#'   passes -- the measures need the counts, not the wrapper.
#'
#' @examples
#' # Example usage:
#' \dontrun{check_confusion(c(tp = 1, tn = 2))}  # Fails: fp and fn are missing
#'
check_confusion <- function(cm) {
  needed <- c("tp", "tn", "fp", "fn")
  if (is.null(names(cm)) || !all(needed %in% names(cm))) {
    msg <- "`cm` must be the output of confusion(): "
    sug <- "a vector with tp, tn, fp, fn."
    stop(c(msg, sug), call. = FALSE)
  }
}

#' @rdname checks
#'
#' @description \code{check_ada_fit()} ensures that \code{fit} is an ensemble
#'   from \code{\link[adatutor]{adaboost}}: a non-empty list whose elements each
#'   carry a tree in \code{h} and its model weight in \code{a}. It checks the
#'   shape rather than the class, since \code{adaboost()} returns a plain list.
#'
#' @examples
#' # Example usage:
#' \dontrun{check_ada_fit(list())}  # Fails: the ensemble is empty
#'
check_ada_fit <- function(fit) {
  if (!is.list(fit) || length(fit) == 0L) {
    msg <- "`fit` is not a fitted ensemble. "
    sug <- "Pass the list returned by adaboost()."
    stop(c(msg, sug), call. = FALSE)
  }
  ok <- vapply(
    fit,
    function(z) is.list(z) && !is.null(z$h) && !is.null(z$a),
    logical(1)
  )
  if (!all(ok)) {
    msg <- paste0(
      "`fit` element ",
      which(!ok)[1L],
      " has no `h` and `a`. "
    )
    sug <- "Pass the list returned by adaboost()."
    stop(c(msg, sug), call. = FALSE)
  }
}
