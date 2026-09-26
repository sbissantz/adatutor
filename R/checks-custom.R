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
    argument <- deparse(substitute(x))
    stop(
      "`",
      argument,
      "` is not a data frame. ",
      "Check that `",
      argument,
      "` is valid.",
      call. = FALSE
    )
  }
}

#' Stop unless `eta` is numeric; note when it is above 1
#' @noRd
check_eta <- function(x) {
  argument <- deparse(substitute(x))
  if (!is.numeric(x)) {
    stop(
      "`",
      argument,
      "` is not a numeric value. ",
      "Check that `",
      argument,
      "` is valid.",
      call. = FALSE
    )
  } else {
    if (x > 1) {
      message("The value of `eta` is greater than 1. Think about overfitting.")
    }
  }
}

#' Stop when `x` is empty
#' @noRd
check_length <- function(x) {
  if (length(x) == 0) {
    argument <- deparse(substitute(x))
    stop(
      "`",
      argument,
      "` has no elements. ",
      "Check that `",
      argument,
      "` is valid.",
      call. = FALSE
    )
  }
}

#' Stop unless `x` is a list
#' @noRd
check_list <- function(x) {
  if (!is.list(x)) {
    argument <- deparse(substitute(x))
    stop(
      "`",
      argument,
      "` is not a list. ",
      "Check that `",
      argument,
      "` is valid.",
      call. = FALSE
    )
  }
}

#' Stop unless `x` is numeric
#' @noRd
check_numeric <- function(x) {
  if (!is.numeric(x)) {
    argument <- deparse(substitute(x))
    stop(
      "`",
      argument,
      "` is not a numeric value. ",
      "Check that `",
      argument,
      "` is valid.",
      call. = FALSE
    )
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
    shared_columns <- intersect(names(trainset), names(newdata))
    if (length(shared_columns)) {
      new_rows <- do.call(paste, c(newdata[shared_columns], sep = "\r"))
      train_rows <- do.call(paste, c(trainset[shared_columns], sep = "\r"))
      hits <- sum(new_rows %in% train_rows)
      return(structure(
        if (hits == 0L) {
          "none"
        } else if (hits == length(new_rows)) {
          "all"
        } else {
          "some"
        },
        hits = hits,
        n = length(new_rows)
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
    stop(
      "Specified proportion `",
      deparse(x),
      "` is not between 0 and 1.",
      call. = FALSE
    )
  }
  # single proportion only: with several, a zero is a deliberate empty set
  if (length(x) == 1L && (x == 0 | x == 1)) {
    warning(
      "Specified proportion `",
      deparse(x),
      "` is not practical.",
      call. = FALSE
    )
  }
}

#' Stop unless `cm` names the four counts
#'
#' Checks names, not class, so a hand-built named vector passes.
#' @noRd
check_confusion <- function(cm) {
  needed <- c("tp", "tn", "fp", "fn")
  if (is.null(names(cm)) || !all(needed %in% names(cm))) {
    stop(
      "`cm` must be the output of confusion(): a vector with tp, tn, fp, fn.",
      call. = FALSE
    )
  }
}

#' Stop unless `fit` looks like an adaboost() ensemble
#' @noRd
check_ada_fit <- function(fit) {
  if (!is.list(fit) || length(fit) == 0L) {
    stop(
      "`fit` is not a fitted ensemble. Pass the list returned by adaboost().",
      call. = FALSE
    )
  }
  ok <- vapply(
    fit,
    \(element) is.list(element) && !is.null(element$h) && !is.null(element$a),
    logical(1)
  )
  if (!all(ok)) {
    stop(
      "`fit` element ",
      which(!ok)[1L],
      " has no `h` and `a`. Pass the list returned by adaboost().",
      call. = FALSE
    )
  }
}
