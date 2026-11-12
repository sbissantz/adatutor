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
    argument <- deparse1(substitute(x))
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
  argument <- deparse1(substitute(x))
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
    argument <- deparse1(substitute(x))
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
    argument <- deparse1(substitute(x))
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
    argument <- deparse1(substitute(x))
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
      paste0("`", deparse1(newdata_name), "`")
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
      deparse1(x),
      "` is not between 0 and 1.",
      call. = FALSE
    )
  }
  # single proportion only: with several, a zero is a deliberate empty set
  if (length(x) == 1L && (x == 0 | x == 1)) {
    warning(
      "Specified proportion `",
      deparse1(x),
      "` is not practical. Use a value greater than 0 and less than 1.",
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
    \(element) {
      is.list(element) && !is.null(element$h) && !is.null(element$alpha)
    },
    logical(1)
  )
  if (!all(ok)) {
    stop(
      "`fit` element ",
      which(!ok)[1L],
      " has no `h` and `alpha`. Pass the list returned by adaboost().",
      call. = FALSE
    )
  }
}

#' Stop unless `x` is a classification tree; `fn` names the caller
#' @noRd
check_tree_fit <- function(x, fn) {
  if (!identical(x$method, "class")) {
    stop(
      "`x` must be a classification tree, fitted with `method = \"class\"`: ",
      fn,
      " scores its predicted probabilities.",
      call. = FALSE
    )
  }
}

#' Stop unless `x` is a classification forest fitted with a formula and
#' randomForest is installed; `fn` names the caller
#' @noRd
check_forest_fit <- function(x, fn) {
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop(
      "A random forest needs the randomForest package. ",
      "Install it with install.packages(\"randomForest\").",
      call. = FALSE
    )
  }
  if (!identical(x$type, "classification")) {
    stop(
      "`x` must be a classification forest, fitted on a factor outcome: ",
      fn,
      " scores its predicted probabilities.",
      call. = FALSE
    )
  }
  if (is.null(x$terms)) {
    stop(
      "`x` must be fitted with a formula, as in ",
      "randomForest(replicate ~ ., data = d): ",
      fn,
      " refits its call on each fold's rows.",
      call. = FALSE
    )
  }
}

#' Stop unless `x` is an XGBoost fit that a refit from its stored parameters
#' can repeat and xgboost is installed; `fn` names the caller
#' @noRd
check_xgb_fit <- function(x, fn) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop(
      "An XGBoost fit needs the xgboost package. ",
      "Install it with install.packages(\"xgboost\").",
      call. = FALSE
    )
  }
  if (!identical(attr(x, "params")$objective, "binary:logistic")) {
    stop(
      "`x` must be fitted with the objective \"binary:logistic\", which ",
      "xgboost() chooses for a factor outcome with two levels: ",
      fn,
      " scores its predicted probabilities.",
      call. = FALSE
    )
  }
  if (is.null(xgboost::getinfo(x, "feature_name"))) {
    stop(
      "`x` has no feature names. Fit it on a data frame, as in ",
      "xgboost(x = d[, predictors], y = d$replicate): ",
      fn,
      " finds the predictors in `data` by those names.",
      call. = FALSE
    )
  }
  # arguments outside the stored parameters, which a refit would drop
  lost <- intersect(
    names(attr(x, "call")),
    c(
      "weights",
      "base_margin",
      "eval_set",
      "early_stopping_rounds",
      "feature_weights"
    )
  )
  if (length(lost)) {
    stop(
      "`x` was fitted with ",
      paste0("`", lost, "`", collapse = ", "),
      ", which its stored parameters do not keep: ",
      fn,
      " refits from those parameters on each fold's rows. ",
      "Fit the model without them.",
      call. = FALSE
    )
  }
}

#' Stop unless `outcome` names a factor column of `data` with the levels of
#' the XGBoost fit `x`, and `data` holds the fit's predictors
#' @noRd
check_xgb_data <- function(x, data, outcome) {
  if (missing(outcome)) {
    stop(
      "`outcome` is missing. An XGBoost fit keeps no formula, so name the ",
      "outcome column of `data`, as in `outcome = \"replicate\"`.",
      call. = FALSE
    )
  }
  if (!is.character(outcome) || length(outcome) != 1L || is.na(outcome)) {
    stop("`outcome` must be a single column name.", call. = FALSE)
  }
  if (!outcome %in% names(data)) {
    stop(
      "`outcome` column `",
      outcome,
      "` not found in `data`.",
      call. = FALSE
    )
  }
  levels <- attr(x, "metadata")$y_levels
  if (
    !is.factor(data[[outcome]]) || !identical(levels(data[[outcome]]), levels)
  ) {
    stop(
      "`outcome` column `",
      outcome,
      "` must be a factor with the levels the fit was trained on, ",
      paste0("\"", levels, "\"", collapse = " and "),
      ", in that order.",
      call. = FALSE
    )
  }
  absent <- setdiff(xgboost::getinfo(x, "feature_name"), names(data))
  if (length(absent)) {
    stop(
      "`data` lacks the fit's predictor column(s): ",
      paste(absent, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
}

#' Stop when `x` is an XGBoost fit that is not from xgboost()
#' @noRd
check_not_booster <- function(x) {
  # xgb.train() returns this class alone; xgboost() adds "xgboost" in front
  if (inherits(x, "xgb.Booster")) {
    stop(
      "`x` is an xgb.Booster, as xgb.train() returns. Fit the model with ",
      "xgboost::xgboost(x = <data frame>, y = <factor>) instead, from ",
      "xgboost 3.0 or later.",
      call. = FALSE
    )
  }
}

#' Stop when a method gets arguments it does not take
#'
#' A setting passed here would otherwise be dropped without a word, and the
#' fit's own setting used instead.
#' @noRd
check_dots_empty <- function(...) {
  if (...length() == 0L) {
    return(invisible(NULL))
  }
  labels <- ...names() %||% rep("", ...length())
  labels <- ifelse(nzchar(labels), paste0("`", labels, "`"), "an unnamed value")
  stop(
    "Unused argument(s): ",
    paste(labels, collapse = ", "),
    ". A fit's settings belong to the fit: refit the model with other ",
    "settings, or put settings to compare in the `grid` of tune_logo_cv().",
    call. = FALSE
  )
}
