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
#' @param input_checks Whether to check the inputs. Defaults to `TRUE`.
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
  input_checks = TRUE,
  verbose = TRUE
) {
  # preconditions, not checks: run whatever `input_checks` says
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

  # expanded formula: `outcome ~ .` arrives with its terms resolved; the
  # environment lets rpart find `D` (in globalenv() it finds stats::D)
  formula <- stats::formula(h$terms)
  environment(formula) <- environment()

  mf <- h$model

  # refuse what cannot be carried: boosting a different model than the one
  # handed in would go unnoticed
  if ("(weights)" %in% names(mf)) {
    stop(
      "`h` was fitted with `weights`, which AdaBoost cannot honour: it sets ",
      "its own observation weights, starting at 1/n and reweighting every ",
      "round. Refit `h` without `weights`.",
      call. = FALSE
    )
  }
  y_train <- stats::model.response(mf)
  # prior is recomputed from each round's weights and loss is not passed on,
  # so a value that differs from the data's default was set by hand
  tab <- table(y_train)
  if (
    !isTRUE(all.equal(
      as.numeric(h$parms$prior),
      as.numeric(tab / sum(tab))
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
      as.numeric(1 - diag(length(tab)))
    ))
  ) {
    stop(
      "`h` carries a custom `loss` matrix, which adaboost() does not pass on ",
      "to the boosted trees. Refit `h` without `loss`.",
      call. = FALSE
    )
  }

  # the model frame holds evaluated terms (`log(power.o)` as a column), so fit
  # `<response> ~ .` over it; the original terms go back onto each tree below
  fit_formula <- stats::as.formula(
    paste0("`", names(mf)[attr(h$terms, "response")], "` ~ ."),
    env = environment()
  )

  # point the terms at globalenv(), or this frame (`mf`, `D`, `H`) rides on
  # every saved tree
  learner_terms <- h$terms
  attr(learner_terms, ".Environment") <- globalenv()

  # keep the caller's controls; drop xval and surrogates, which cost time and
  # leave the tree unchanged
  ctrl <- h$control
  ctrl$xval <- 0
  ctrl$maxsurrogate <- 0

  # map rpart's split code back to its name: given the code, rpart stores NA
  # and silently falls back to gini
  split_rule <- c("gini", "information")[h$parms$split]
  if (is.na(split_rule)) {
    split_rule <- "gini"
  }

  # dataset name from the tree's call, for the `train` attribute
  data_name <- h$call[["data"]]

  # close a half-finished progress line on error; unlike tryCatch(), on.exit()
  # also runs on a user interrupt
  pb <- NULL
  finish <- FALSE
  if (verbose) {
    on.exit(
      {
        # stop the bar's color leaking past this call
        if (!is.null(pb)) {
          cat("\033[0m", file = stderr())
        }
        if (!finish) {
          # closing the bar breaks the line; without one, break it directly
          if (!is.null(pb)) close(pb) else message("")
        }
      },
      add = TRUE
    )
  }

  if (verbose) {
    color_message(
      "Start the AdaBoost training process:\n",
      color_code = ansi_bold
    )
  }

  if (input_checks) {
    if (verbose) {
      color_message("Run mild input checks", color_code = ansi_dim)
    }
    check_df(mf)
    check_length(mf)
    check_eta(eta)
    check_numeric(n_iter)
    if (verbose) walking_colordots()
  }

  if (verbose) {
    color_message("Start the initialization process", color_code = ansi_dim)
  }

  m <- nrow(mf)
  D <- rep(1, m) / m
  H <- vector("list", n_iter)

  if (verbose) {
    walking_colordots()
    color_message(
      "Steps 1-4: Run through the algorithm steps",
      color_code = ansi_dim
    )
    # faint, on stderr: stdout would mix the bar into the results, and plain
    # stderr is red in RStudio; on.exit() above resets the attribute
    message("\033[", ansi_dim, "m", appendLF = FALSE)
    pb <- utils::txtProgressBar(
      min = 0,
      max = n_iter,
      style = 3,
      file = stderr()
    )
  }

  for (t in seq_len(n_iter)) {
    # h_t, not h: the prototype must survive the loop; model = FALSE keeps one
    # data copy on the ensemble instead of one per tree
    h_t <- rpart::rpart(
      formula = fit_formula,
      data = mf,
      weights = D,
      method = "class",
      control = ctrl,
      # keep the split rule; let rpart derive prior and loss from the weights
      parms = list(split = split_rule),
      model = FALSE,
      y = FALSE
    )

    y_retro <- stats::predict(h_t, newdata = mf, type = "class")

    correct <- (y_train == y_retro)
    # clamp so a perfect or useless learner keeps a finite weight; e > 0.5
    # gives a negative `a`, which the update handles
    e <- min(max(sum(D[!correct]), 1e-10), 1 - 1e-10)
    a <- 0.5 * log((1 - e) / e) * eta

    D_unorm <- D
    D_unorm[correct] <- D[correct] * exp(-a)
    D_unorm[!correct] <- D[!correct] * exp(a)
    D <- D_unorm / sum(D_unorm)

    # drop what predict() does not need
    h_t$where <- NULL
    h_t$call <- NULL
    # restore the learner's terms: predict() evaluates transformations on raw
    # newdata, and the fitted terms would carry this frame and its data
    h_t$terms <- learner_terms

    H[[t]] <- list("h" = h_t, "a" = a)

    if (verbose) utils::setTxtProgressBar(pb, t)
  }

  finish <- TRUE

  if (verbose) {
    close(pb)
    color_message("Create output", color_code = ansi_dim)
    walking_colordots()
    color_message(
      "Training process successfully completed.\n",
      color_code = ansi_bold,
      newline = TRUE
    )
  }

  names(H) <- paste0("t", seq_len(n_iter))

  # one data copy, on the ensemble
  trainset <- if (keep_data) mf else NULL

  # store the formula without this frame, so the fit holds no training data;
  # keep the whole control list so every setting stays visible
  form_store <- formula
  environment(form_store) <- globalenv()

  structure(
    H,
    class = "adaboost",
    trainset = trainset,
    train = data_name,
    formula = form_store,
    n_iter = n_iter,
    eta = eta,
    control = ctrl,
    split = split_rule
  )
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
#' @param input_checks Whether to check the inputs. Defaults to `TRUE`.
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
  input_checks = TRUE,
  verbose = TRUE,
  ...
) {
  type <- match.arg(type)

  # no newdata: fall back to the training data, as predict.lm() does;
  # check_train() below reports the retrodiction
  trainset <- attr(object, "trainset")
  fell_back <- missing(newdata)
  if (missing(newdata)) {
    if (is.null(trainset)) {
      msg <- "`newdata` is required. "
      sug <- paste0(
        "This fit was built with `keep_data = FALSE`, so it has no training ",
        "data to fall back on. Pass the training frame for retrodictions, the ",
        "test frame for predictions."
      )
      stop(c(msg, sug), call. = FALSE)
    }
    newdata <- trainset
  }

  # break a line left open for " Done" if an error hits in between
  finish <- FALSE
  if (verbose) {
    on.exit(if (!finish) message(""), add = TRUE)
  }

  # work this out whenever anything reports it, not only with the checks
  fcl <- match.call()
  test_pos <- match("newdata", names(fcl), nomatch = 0L)
  testnme <- if (test_pos) fcl[[test_pos]] else NULL
  state <- if (input_checks || verbose) {
    overlap_state(attr(object, "train"), testnme, trainset, newdata)
  } else {
    NA_character_
  }
  # "all" retrodictions, "none" predictions, the slash form when unknown
  noun <- if (is.na(state)) {
    "predictions/retrodictions"
  } else if (state == "all") {
    "retrodictions"
  } else {
    "predictions"
  }

  if (verbose) {
    color_message("Start the AdaBoost test process:\n", color_code = ansi_bold)
  }
  if (input_checks) {
    if (verbose) {
      color_message("Run mild input checks", color_code = ansi_dim)
    }
    check_list(object)
    check_length(object)
    check_df(newdata)
    check_length(newdata)
  }
  if (verbose && input_checks) {
    walking_colordots()
  }
  if (verbose) {
    color_message("Extract the trees", color_code = ansi_dim)
    walking_colordots()
  }
  h <- lapply(object, "[[", "h")
  check_length(h)
  if (verbose) {
    color_message("Extract the model weights", color_code = ansi_dim)
    walking_colordots()
  }
  a <- vapply(object, "[[", numeric(1), "a")
  check_length(a)

  if (verbose) {
    color_message(paste0("Make ", noun, "\n"), color_code = ansi_dim)
  }

  N <- nrow(newdata)
  # matrix(): vapply returns a vector when N == 1, which would turn the
  # product below into a T x T outer product
  y12_stumps <- matrix(
    vapply(
      h,
      function(tree) {
        stats::predict(tree, newdata = newdata, type = "vector")
      },
      FUN.VALUE = numeric(N)
    ),
    nrow = N
  )

  if (verbose) {
    color_message(paste0("Combine ", noun), color_code = ansi_dim)
  }

  ypred_stumps <- 2 * y12_stumps - 3

  raw_margin <- as.vector(a %*% t(ypred_stumps))

  finish <- TRUE

  if (verbose) {
    walking_colordots()
    color_message(
      "Test process successfully completed.\n",
      color_code = ansi_bold
    )
  }

  # last: a transcript scrolls, so the note sits next to its values
  if (input_checks) {
    check_train(state, testnme, verbose = verbose, fell_back = fell_back)
  }

  if (type == "class") {
    sign(raw_margin)
  } else {
    raw_margin
  }
}
