#' Input checks
#'
#' Internal checks that stop with a message saying what is wrong and how to
#' fix it.
#'
#' @noRd
NULL

#' Stop unless `x` is a data frame
#' @noRd
check_df <- function(x) {
  if (!is.data.frame(x)) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp), "` is not a data frame. ")
    sug <- paste0("Check that ", "`", deparse(x_inp), "` is valid.")
    stop(c(msg, sug), call. = FALSE)
  }
}

#' Stop unless `eta` is numeric; note when it is above 1
#' @noRd
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

#' Stop when `x` is empty
#' @noRd
check_length <- function(x) {
  if (length(x) == 0) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp), "` has no elements. ")
    sug <- paste0("Check that ", "`", deparse(x_inp), "` is valid.")
    stop(c(msg, sug), call. = FALSE)
  }
}

#' Stop unless `x` is a list
#' @noRd
check_list <- function(x) {
  if (!is.list(x)) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp), "` is not a list. ")
    sug <- paste0("Check that ", "`", deparse(x_inp), "` is valid.")
    stop(c(msg, sug), call. = FALSE)
  }
}

#' Stop unless `x` is numeric
#' @noRd
check_numeric <- function(x) {
  if (!is.numeric(x)) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp), "` is not a numeric value. ")
    sug <- paste0("Check that ", "`", deparse(x_inp), "` is valid.")
    stop(c(msg, sug), call. = FALSE)
  }
}

#' How much of `newdata` was also used for training
#'
#' Returns "all", "some", "none", or `NA` when it cannot tell.
#' @noRd
detect_overlap <- function(
  train_name,
  newdata_name,
  trainset = NULL,
  newdata = NULL
) {
  # compare rows when the fit kept its frame (catches renames and subsets),
  # else the deparsed name (all or nothing); NA means unknown, not no overlap
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
  if (!is.null(newdata_name) && identical(train_name, newdata_name)) {
    return("all")
  }
  NA_character_
}

#' Report retrodictions when the scored data were used for training
#'
#' A message, not a warning: retrodicting on purpose is normal.
#' @noRd
check_train <- function(
  overlap,
  newdata_name = NULL,
  verbose = FALSE,
  defaulted = FALSE
) {
  # report only "all": `altmejd` has duplicated rows, so 2 of the 23 shipped
  # test rows match training by chance; "all" still catches renames and subsets
  if (is.na(overlap) || overlap != "all") {
    return(invisible(NULL))
  }

  # two routes here: the training frame was passed, or predict() fell back to it
  reason <- if (defaulted) {
    "  No `newdata` specified. The model scored its training data."
  } else {
    what <- if (!is.null(newdata_name)) {
      paste0("`", deparse(newdata_name), "`")
    } else {
      "that data"
    }
    paste0("  ", what, " was also used for training.")
  }

  # the conclusion carries the `!`; two lines, since a terminal-wrapped line
  # gets skimmed
  conclusion <- "! Outputs are retrodictions, not predictions."

  if (verbose) {
    # faint reason, bold teal conclusion; unstyled, RStudio would paint the
    # reason red
    color_message(paste0(reason, "\n"), color_code = ansi_dim)
    color_message(paste0(conclusion, "\n"), color_code = ansi_note)
  } else {
    message(paste(c(reason, conclusion), collapse = "\n"))
  }
}

#' Stop unless every proportion is between 0 and 1; warn on a single 0 or 1
#' @noRd
check_prop <- function(x) {
  # vectorized: partition() passes one proportion per set
  if (any(x < 0 | x > 1)) {
    msg <- paste0(
      "Specified proportion ",
      "`",
      deparse(x),
      "` is not between 0 and 1."
    )
    stop(msg, call. = FALSE)
  }
  # single proportion only: with several, a zero is a deliberate empty set
  if (length(x) == 1L && (x == 0 | x == 1)) {
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

#' Stop unless `cm` names the four counts
#'
#' Checks names, not class, so a hand-built named vector passes.
#' @noRd
check_confusion <- function(cm) {
  needed <- c("tp", "tn", "fp", "fn")
  if (is.null(names(cm)) || !all(needed %in% names(cm))) {
    msg <- "`cm` must be the output of confusion(): "
    sug <- "a vector with tp, tn, fp, fn."
    stop(c(msg, sug), call. = FALSE)
  }
}

#' Stop unless `fit` looks like an adaboost() ensemble
#' @noRd
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
