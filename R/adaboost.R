#' Boost a classification tree with AdaBoost
#'
#' Boosts a fitted classification tree. The tree is the weak learner: it is
#' refit `n_iter` times on reweighted data, and the ensemble is the weighted
#' vote of those fits. The tree's depth, `cp` and other
#' [rpart::rpart.control()] settings come from `h`, not from this function.
#'
#' @section What comes from `h`:
#' All of `h$control` is kept (`maxdepth`, `cp`, `minsplit`, `minbucket`),
#' except `xval` and `maxsurrogate`. These are set to 0 to save time; they do
#' not change the tree. The splitting rule is kept too.
#'
#' `cp` is whatever `h` was fitted with: 0.01 for a plain `rpart()` call. The
#' textbook advice is unpruned trees, `cp = 0` (Hastie, Tibshirani & Friedman,
#' 2009). On these data that makes no difference for stumps, and `cp = 0` does
#' slightly worse at depth 4 (see [hypergrid]).
#'
#' @section What is refused:
#' `adaboost()` stops with an error rather than boost a different model than
#' the one you passed. It refuses a tree that is not `method = "class"`, one
#' fitted with `weights`, and one with a `prior` or a `loss` matrix.
#'
#' @param h A classification tree from [rpart::rpart()], fitted with
#'   `model = TRUE`. Without it, rpart drops the predictors and the tree
#'   cannot be refit.
#' @param n_iter The number of boosting rounds.
#' @param eta The learning rate.
#' @param keep_data Whether to store the training data on the fit. Defaults to
#'   `TRUE`. This lets `predict()` run without `newdata` and makes the
#'   retrodiction check exact. It costs no memory, but a saved file grows by
#'   the size of the data; set `FALSE` when you save many fits.
#' @param check_inputs Whether to check the inputs. Defaults to `TRUE`.
#' @param verbose Whether to show progress. Defaults to `TRUE`.
#'
#' @return An object of class `adaboost`: a list with one element per round,
#'   each holding the tree (`h`) and its model weight (`a`). Attributes store
#'   the training data, the formula, `n_iter`, `eta`, the tree control and the
#'   splitting rule, so [logo_cv()] can refit the same recipe.
#'
#' @seealso [predict.adaboost()] to score new data.
#'
#' @examples
#' data(altmejd_splits)
#' train <- altmejd_splits$train
#'
#' # fit the weak learner first; model = TRUE keeps its data
#' h <- rpart::rpart(
#'   replicate ~ power.o + n.o,
#'   data = train,
#'   maxdepth = 1,
#'   model = TRUE
#' )
#'
#' fit <- h |> adaboost(n_iter = 20, eta = 1, verbose = FALSE)
#' attr(fit, "n_iter")
#' attr(fit, "control")$maxdepth
#'
#' # a deeper learner is a different tree, not a different argument
#' rpart::rpart(replicate ~ power.o + n.o, data = train, maxdepth = 3,
#'              model = TRUE) |>
#'   adaboost(n_iter = 20, eta = 1, verbose = FALSE) |>
#'   attr("control") |>
#'   getElement("maxdepth")
#'
#' @export
adaboost <- function(
  h,
  n_iter,
  eta,
  keep_data = TRUE,
  check_inputs = TRUE,
  verbose = TRUE
) {
  validate_learner(h)

  frame <- h$model
  y_train <- stats::model.response(frame)

  # fit `<response> ~ .` over the frame, which holds evaluated terms such as
  # `log(power.o)`; this environment lets rpart find `D`, not stats::D
  frame_formula <- stats::as.formula(
    paste0("`", names(frame)[attr(h$terms, "response")], "` ~ ."),
    env = environment()
  )

  # point the terms at globalenv(), or this frame rides on every saved tree
  learner_terms <- h$terms
  attr(learner_terms, ".Environment") <- globalenv()

  # drop xval and surrogates: they cost time and leave the tree unchanged
  control <- h$control
  control$xval <- 0
  control$maxsurrogate <- 0

  split_rule <- read_split_rule(h)

  progress <- NULL
  finished <- FALSE
  if (verbose) {
    on.exit(close_progress(progress, finished), add = TRUE)
    color_message(
      "Start the AdaBoost training process:\n",
      color_code = ansi_bold
    )
  }

  if (check_inputs) {
    if (verbose) {
      color_message("Run mild input checks", color_code = ansi_dim)
    }
    check_df(frame)
    check_length(frame)
    check_eta(eta)
    check_numeric(n_iter)
    if (verbose) mark_done()
  }

  if (verbose) {
    color_message("Start the initialization process", color_code = ansi_dim)
  }

  m <- nrow(frame)
  D <- rep(1, m) / m
  H <- vector("list", n_iter)

  if (verbose) {
    mark_done()
    color_message(
      "Steps 1-4: Run through the algorithm steps",
      color_code = ansi_dim
    )
    progress <- open_progress(n_iter)
  }

  for (t in seq_len(n_iter)) {
    # h_t, not h: the prototype must survive the loop; model = FALSE keeps
    # one data copy on the ensemble, not one per tree
    h_t <- rpart::rpart(
      formula = frame_formula,
      data = frame,
      weights = D,
      method = "class",
      control = control,
      # keep the split rule; let rpart derive prior and loss from the weights
      parms = list(split = split_rule),
      model = FALSE,
      y = FALSE
    )
    correct <- y_train == stats::predict(h_t, newdata = frame, type = "class")
    a <- weigh_learner(sum(D[!correct]), eta)
    D <- update_weights(D, correct, a)
    H[[t]] <- list("h" = strip_tree(h_t, learner_terms), "a" = a)

    if (verbose) utils::setTxtProgressBar(progress, t)
  }

  finished <- TRUE

  if (verbose) {
    close(progress)
    color_message("Create output", color_code = ansi_dim)
    mark_done()
    color_message(
      "Training process successfully completed.\n",
      color_code = ansi_bold,
      newline = TRUE
    )
  }

  names(H) <- paste0("t", seq_len(n_iter))

  structure(
    H,
    class = "adaboost",
    trainset = if (keep_data) frame,
    train = h$call[["data"]],
    formula = stats::formula(learner_terms),
    n_iter = n_iter,
    eta = eta,
    control = control,
    split = split_rule
  )
}

#' Stop unless adaboost() can refit `h` as it was fitted
#'
#' Always runs, whatever `check_inputs` says: boosting a different model than
#' the one handed in would go unnoticed.
#' @noRd
validate_learner <- function(h) {
  if (!inherits(h, "rpart")) {
    stop("`h` must be a tree fitted with rpart().", call. = FALSE)
  }
  if (!identical(h$method, "class")) {
    stop(
      "`h` must be a classification tree: every round is refitted with ",
      "`method = \"class\"`, so a \"",
      h$method,
      "\" learner would come back as something other than what was handed in.",
      call. = FALSE
    )
  }
  if (is.null(h$model)) {
    stop(
      "`h` carries no training data: refit it with `model = TRUE`.",
      call. = FALSE
    )
  }
  if ("(weights)" %in% names(h$model)) {
    stop(
      "`h` was fitted with `weights`, which AdaBoost cannot honor: it sets ",
      "its own observation weights, starting at 1/n and reweighting every ",
      "round. Refit `h` without `weights`.",
      call. = FALSE
    )
  }

  # prior is recomputed from each round's weights and loss is not passed on,
  # so a value that differs from the data's default was set by hand
  counts <- table(stats::model.response(h$model))
  if (
    !isTRUE(all.equal(
      as.numeric(h$parms$prior),
      as.numeric(counts / sum(counts))
    ))
  ) {
    stop(
      "`h` carries a `prior` that adaboost() cannot use: boosting recomputes ",
      "it from each round's observation weights. Refit `h` without `prior` ",
      "(or without `weights`, which sets one).",
      call. = FALSE
    )
  }
  if (
    !isTRUE(all.equal(
      as.numeric(h$parms$loss),
      as.numeric(1 - diag(length(counts)))
    ))
  ) {
    stop(
      "`h` carries a custom `loss` matrix, which adaboost() does not pass on ",
      "to the boosted trees. Refit `h` without `loss`.",
      call. = FALSE
    )
  }
  invisible(h)
}

#' Read the splitting rule of `h` by name
#' @noRd
read_split_rule <- function(h) {
  # rpart stores a code; passing the code back gives NA and a silent gini
  rule <- c("gini", "information")[h$parms$split]
  if (is.na(rule)) "gini" else rule
}

#' Open a faint progress bar on stderr
#' @noRd
open_progress <- function(n_iter) {
  # stdout would mix the bar into the results, and plain stderr is red in
  # RStudio; close_progress() resets the style
  message("\033[", ansi_dim, "m", appendLF = FALSE)
  utils::txtProgressBar(min = 0, max = n_iter, style = 3, file = stderr())
}

#' Reset the progress style and close a half-finished line
#'
#' Runs from on.exit(), which unlike tryCatch() also runs on a user interrupt.
#' @noRd
close_progress <- function(progress, finished) {
  if (!is.null(progress)) {
    cat("\033[0m", file = stderr())
  }
  if (!finished) {
    # closing the bar breaks the line; without one, break it directly
    if (!is.null(progress)) close(progress) else message("")
  }
}

#' Weigh a learner by its weighted error `e`
#' @noRd
weigh_learner <- function(e, eta) {
  # clamp so a perfect or useless learner keeps a finite weight; e > 0.5
  # gives a negative weight, which update_weights() handles
  e <- min(max(e, 1e-10), 1 - 1e-10)
  0.5 * log((1 - e) / e) * eta
}

#' Shift weight toward the misclassified observations
#' @noRd
update_weights <- function(D, correct, a) {
  D_raw <- D * exp(ifelse(correct, -a, a))
  D_raw / sum(D_raw)
}

#' Strip a tree to what predict() needs
#' @noRd
strip_tree <- function(tree, learner_terms) {
  tree$where <- NULL
  tree$call <- NULL
  # predict() evaluates transformations on raw newdata; the fitted terms
  # would carry adaboost()'s frame and its data
  tree$terms <- learner_terms
  tree
}

#' Predict from a boosted ensemble
#'
#' Combines the boosted trees into the weighted vote
#' \deqn{H(x) = \sum_{t=1}^T a_t h_t(x)}{H(x) = sum over t of a_t h_t(x)}
#' and returns class labels or the vote itself, the margin.
#'
#' @section Prediction or retrodiction:
#' Scoring the data the model was trained on is a *retrodiction*: it shows how
#' well the model fits data it has already seen, not how well it predicts.
#' Scoring held-out data is a prediction. The call looks the same either way,
#' so `predict()` tells you when the data were also used for training. With no
#' `newdata`, it scores the training data on purpose, like `predict()` for
#' [stats::lm()].
#'
#' @param object A fit from [adaboost()].
#' @param newdata A data frame with the predictors the model was trained on.
#' @param type `"class"` for class labels (-1 or 1), or `"margin"` for the
#'   continuous score. Use `"margin"` for ranking measures such as [auroc()].
#' @param check_inputs Whether to check the inputs. Defaults to `TRUE`.
#' @param verbose Whether to show progress. Defaults to `TRUE`.
#' @param ... Ignored.
#'
#' @return A numeric vector of class labels or margins, depending on `type`.
#'
#' @seealso [adaboost()] to fit the ensemble.
#'
#' @export
predict.adaboost <- function(
  object,
  newdata,
  type = c("class", "margin"),
  check_inputs = TRUE,
  verbose = TRUE,
  ...
) {
  type <- match.arg(type)

  # no newdata: score the training data, as predict.lm() does; check_train()
  # below reports the retrodiction
  trainset <- attr(object, "trainset")
  defaulted <- missing(newdata)
  if (defaulted) {
    if (is.null(trainset)) {
      stop(
        "`newdata` is required. This fit was built with `keep_data = FALSE`, ",
        "so it has no training data to fall back on. Pass the training frame ",
        "for retrodictions, the test frame for predictions.",
        call. = FALSE
      )
    }
    newdata <- trainset
  }

  # break a line left open for " Done" if an error hits in between
  finished <- FALSE
  if (verbose) {
    on.exit(if (!finished) message(""), add = TRUE)
  }

  # work this out whenever anything reports it, not only with the checks
  newdata_name <- match.call()[["newdata"]]
  overlap <- if (check_inputs || verbose) {
    detect_overlap(attr(object, "train"), newdata_name, trainset, newdata)
  } else {
    NA_character_
  }
  kind <- if (is.na(overlap)) {
    "predictions/retrodictions"
  } else if (overlap == "all") {
    "retrodictions"
  } else {
    "predictions"
  }

  if (verbose) {
    color_message("Start the AdaBoost test process:\n", color_code = ansi_bold)
  }
  if (check_inputs) {
    if (verbose) {
      color_message("Run mild input checks", color_code = ansi_dim)
    }
    check_list(object)
    check_length(object)
    check_df(newdata)
    check_length(newdata)
    if (verbose) mark_done()
  }
  if (verbose) {
    color_message("Extract the trees", color_code = ansi_dim)
    mark_done()
  }
  h <- lapply(object, "[[", "h")
  check_length(h)
  if (verbose) {
    color_message("Extract the model weights", color_code = ansi_dim)
    mark_done()
  }
  a <- vapply(object, "[[", numeric(1), "a")
  check_length(a)

  if (verbose) {
    color_message(paste0("Make ", kind, "\n"), color_code = ansi_dim)
  }

  n <- nrow(newdata)
  # matrix(): with one row, vapply() returns a vector and the product below
  # becomes an outer product
  codes <- matrix(
    vapply(
      h,
      \(tree) stats::predict(tree, newdata = newdata, type = "vector"),
      numeric(n)
    ),
    nrow = n
  )

  if (verbose) {
    color_message(paste0("Combine ", kind), color_code = ansi_dim)
  }

  # rpart codes the classes 1 and 2; the vote needs -1 and 1
  votes <- 2 * codes - 3
  margin <- as.vector(a %*% t(votes))

  finished <- TRUE

  if (verbose) {
    mark_done()
    color_message(
      "Test process successfully completed.\n",
      color_code = ansi_bold
    )
  }

  # last: a transcript scrolls, so the note sits next to its values
  if (check_inputs) {
    check_train(overlap, newdata_name, verbose = verbose, defaulted = defaulted)
  }

  if (type == "class") sign(margin) else margin
}
