#' Refit a model on one fold and score the held-out rows
#'
#' The adaboost and xgboost methods score a group of grid rows and return one
#' column per row; the other methods score one row and return a vector.
#' `...` carries `outcome`, the name of the outcome column, for a fit without
#' a formula.
#' @noRd
refit_score <- function(fit, train, test, setting, ...) {
  UseMethod("refit_score")
}

#' Boost the fit's recipe once for grid rows that differ only in `n_iter`,
#' and score the margin after each row's `n_iter`
#' @exportS3Method NULL
#' @noRd
refit_score.adaboost <- function(fit, train, test, setting, ...) {
  formula <- attr(fit, "formula")
  outcome <- all.vars(formula)[1L]
  predictors <- all.vars(formula)[-1L]
  if (identical(predictors, ".")) {
    predictors <- setdiff(names(train), outcome)
  }
  variables <- c(predictors, outcome)

  # the fit's control, then the rows' rpart controls, then their depth; the
  # rows share all three, so the first row speaks for the group
  shared <- setting[1L, , drop = FALSE]
  control <- attr(fit, "control")
  row_controls <- intersect(names(shared), list_tree_controls())
  control <- utils::modifyList(
    control,
    as.list(shared[, row_controls, drop = FALSE])
  )
  control$maxdepth <- shared[["depth"]]

  # model = TRUE carries the data adaboost() needs
  h <- rpart::rpart(
    formula,
    data = train[, variables, drop = FALSE],
    method = "class",
    control = control,
    # fits made before the split attribute existed used gini
    parms = list(split = attr(fit, "split") %||% "gini"),
    model = TRUE
  )
  # round t depends only on the rounds before it, so the first n rounds of
  # the longest run are the n-round run
  n_iter <- setting[["n_iter"]]
  refit <- adaboost(
    h,
    n_iter = max(n_iter),
    eta = shared[["eta"]],
    # each fit is thrown away, so keep no data for retrodictions
    keep_data = FALSE,
    verbose = FALSE,
    check_inputs = FALSE
  )

  # each tree's vote on each held-out row, coded as predict.adaboost() codes
  # it; matrix() keeps a single held-out row a row
  newdata <- test[, predictors, drop = FALSE]
  n_rows <- nrow(newdata)
  codes <- matrix(
    vapply(
      refit,
      \(round) stats::predict(round$h, newdata = newdata, type = "vector"),
      numeric(n_rows)
    ),
    nrow = n_rows
  )
  votes <- 2 * codes - 3
  alpha <- vapply(refit, `[[`, numeric(1), "alpha")

  # the margin, not labels: labels collapse auroc onto balanced accuracy.
  # The product predict.adaboost() takes, not a running sum, so each column
  # is identical to a refit's margin: cumsum() accumulates in extended
  # precision on some platforms and differs in the last digits.
  margins <- vapply(
    n_iter,
    \(n) {
      used <- seq_len(min(n, length(alpha)))
      as.vector(alpha[used] %*% t(votes[, used, drop = FALSE]))
    },
    numeric(n_rows)
  )
  matrix(margins, nrow = n_rows)
}

#' Refit a tree's call with the row's depth and controls, and score the
#' probability of the second class
#' @exportS3Method NULL
#' @noRd
refit_score.rpart <- function(fit, train, test, setting, ...) {
  # a control list overrides rpart()'s direct arguments, so the row goes into
  # the fit's full control list, whose own values give back the fit's call
  row_controls <- intersect(names(setting), list_tree_controls())
  control <- utils::modifyList(
    fit$control,
    as.list(setting[1L, row_controls, drop = FALSE])
  )
  control$maxdepth <- setting[["depth"]]
  # the stored call may name rpart() bare, which needs it attached
  refit <- refit_call(
    fit,
    train,
    fun = quote(rpart::rpart),
    args = list(control = control)
  )
  prob <- stats::predict(refit, newdata = test, type = "prob")
  prob[, ncol(prob)]
}

#' Refit a logistic regression's call and score its probability
#' @exportS3Method NULL
#' @noRd
refit_score.glm <- function(fit, train, test, setting, ...) {
  # code the outcome 0/1 as for the other learners; an expression such as
  # I(y == "a") is left for glm() to read
  outcome <- read_formula(fit)[[2L]]
  if (is.name(outcome)) {
    outcome <- as.character(outcome)
    train[[outcome]] <- as_binary(train[[outcome]])
  }
  refit <- refit_call(fit, train)
  stats::predict(refit, newdata = test, type = "response")
}

#' Refit a forest's call with the row's arguments, and score the probability
#' of the second class
#' @exportS3Method NULL
#' @noRd
refit_score.randomForest <- function(fit, train, test, setting, ...) {
  # an NA keeps the call's own value; the grid of logo_cv() is all NA, so
  # there the call runs as it was
  values <- as.list(
    setting[1L, intersect(names(setting), list_forest_settings()), drop = FALSE]
  )
  # what options(warnPartialMatchArgs = TRUE) makes of seq(along = m), in the
  # session's language
  own_warning <- gettextf(
    "partial argument match of '%s' to '%s'",
    "along",
    "along.with",
    domain = "R"
  )
  refit <- withCallingHandlers(
    # the stored call names randomForest() bare, which needs it attached
    refit_call(
      fit,
      train,
      fun = quote(randomForest::randomForest),
      args = values[!is.na(values)]
    ),
    # seq(along = m) is randomForest's code, not ours: mute that warning only
    warning = function(w) {
      if (identical(conditionMessage(w), own_warning)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  prob <- stats::predict(refit, newdata = test, type = "prob")
  prob[, ncol(prob)]
}

#' Fit XGBoost once from the fit's stored parameters for grid rows that
#' differ only in `n_iter`, and score the probability of the second level
#' after each row's `n_iter`
#' @exportS3Method NULL
#' @noRd
refit_score.xgboost <- function(fit, train, test, setting, outcome, ...) {
  # not the stored call, whose objects may be gone by now
  params <- attr(fit, "params")
  # xgboost() names it `nthreads`; validate_parameters is xgb.train()'s own
  names(params)[names(params) == "nthread"] <- "nthreads"
  params$validate_parameters <- NULL
  # the rows share all but n_iter, so the first row speaks for the group
  arguments <- list_xgb_settings()
  shared <- setdiff(names(arguments), "n_iter")
  params[arguments[shared]] <- as.list(setting[1L, shared, drop = FALSE])
  n_iter <- setting[["n_iter"]]
  params$nrounds <- max(n_iter)

  features <- xgboost::getinfo(fit, "feature_name")
  refit <- do.call(
    xgboost::xgboost,
    c(list(x = train[, features, drop = FALSE], y = train[[outcome]]), params)
  )
  # the first n rounds of the longest run are the n-round run; the name is
  # iteration_range: iterationrange falls into `...` and returns the full run
  newdata <- test[, features, drop = FALSE]
  n_rows <- nrow(newdata)
  probs <- vapply(
    n_iter,
    \(n) unname(stats::predict(refit, newdata, iteration_range = c(1, n))),
    numeric(n_rows)
  )
  # matrix() keeps a single held-out row a row
  matrix(probs, nrow = n_rows)
}

#' Run a fit's own call again on other rows, with `args` replacing its
#' arguments of those names
#' @noRd
refit_call <- function(fit, rows, fun = NULL, args = list()) {
  formula <- read_formula(fit)
  call <- stats::getCall(fit)
  if (!is.null(fun)) {
    call[[1L]] <- fun
  }
  # the expanded formula, so a `.` cannot reach the group column; the rows
  # themselves, so the call needs no name for them
  call$formula <- formula
  call$data <- rows
  for (name in names(args)) {
    call[[name]] <- args[[name]]
  }
  # not update(), which runs the call in this package: here the call's other
  # names resolve where they did for the original fit
  eval(call, environment(formula))
}
