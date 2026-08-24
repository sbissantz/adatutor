#' @title Train a Set of Classification Trees Using AdaBoost
#'
#' @description The function trains a set of classification trees using
#'   AdaBoost. It is built on top of the \code{rpart} package, so the full range
#'   of tree hyperparameters can be used to fine-tune the trees (see, e.g.,
#'   \code{\link[rpart]{rpart.control}}). By default, it uses hyperparameter
#'   settings optimized for boosting (no internal CV, no missing data splits).
#'   The complexity parameter is left at the \code{rpart} default of 0.01, so
#'   that \code{maxdepth = d} and
#'   \code{treehypar = rpart.control(maxdepth = d)} fit the same trees. Users
#'   can overwrite these or add additional parameters via the \code{treehypar}
#'   argument.
#'
#' @param formula A \code{\link[stats]{formula}} specifying the relationship
#'   between the outcome and the predictors: `outcome ~ predictors`. The
#'   predictors should be included as additive terms (e.g., `X1 + X2 + ...`),
#'   Interactions are not supported (see the \code{formula} argument in
#'   \code{\link[rpart]{rpart}}).
#'
#' @param data A data frame containing the variables in the model.
#'
#' @param T An integer specifying the number of trees.
#'
#' @param eta A numeric value representing the learning rate of the algorithm.
#'
#' @param maxdepth An integer specifying the maximum depth of the trees.
#'   Defaults to 1 (decision stumps).
#'
#' @param treehypar An optional list of additional control parameters for decision
#'   trees (passed to \code{\link[rpart]{rpart.control}}). These will be safely
#'   merged with the internal AdaBoost speed optimizations. Defaults to \code{NULL}.
#'   This is also how to switch pruning off, \code{treehypar = list(cp = 0)}, for
#'   the fixed-size-and-no-pruning form the boosting literature describes; see
#'   Details for what that costs here.
#'
#' @param keep_data Whether to store the training data on the fit. Defaults to
#'   \code{TRUE}. Keeping it lets \code{predict()} work with no \code{newdata},
#'   returning retrodictions, and lets the retrodiction check compare the data
#'   itself rather than guess from the variable's name -- so a renamed variable
#'   or a subset of the training rows is still caught.
#'
#'   The cost is asymmetric and worth knowing. In memory it is nothing: R stores
#'   a reference, so the fit points at the same frame already in your session.
#'   It is only on \code{\link[base]{saveRDS}} that the file grows, by the size
#'   of the data -- once for the whole ensemble, not once per tree. Set
#'   \code{FALSE} when saving many fits, or when the data is large next to the
#'   model.
#'
#' @param input_checks A logical value indicating whether to perform input
#'   validation checks. Defaults to `TRUE`.
#'
#' @param verbose A logical value indicating whether to display verbose output
#'   during the training process. Defaults to `TRUE`.
#'
#' @details The function implements the AdaBoost algorithm with a specified
#'   number of iterations (\code{T}). It initializes observation weights, trains
#'   a sequence of decision trees, and updates the weights at each iteration
#'   based on prediction errors. A final ensemble of weak learners is produced.
#'
#' Key steps in the algorithm:
#' 1. Initialize observation weights.
#' 2. Train a decision tree using the current weights.
#' 3. Compute the weighted classification error and update the observation weights.
#' 4. Store the weak learner and its associated weight.
#'
#' \strong{The complexity parameter.} \code{cp} is left at \code{rpart}'s own
#' default of 0.01 -- kept rather than chosen, and the one control here that is
#' not tuned for boosting. It earns its place by keeping the two ways of setting
#' depth equivalent: \code{maxdepth = d} and
#' \code{treehypar = rpart.control(maxdepth = d)} fit identical trees only
#' because both end up at 0.01.
#'
#' It is worth knowing that this departs from the textbook formulation. AdaBoost
#' as published specifies no base learner at all -- it is defined over any weak
#' learner whose weighted error stays below one half, and its training-error
#' bound says nothing about complexity. The guidance that followed (Friedman,
#' Hastie & Tibshirani, 2000; Hastie, Tibshirani & Friedman, 2009, on
#' right-sized trees for boosting) is to grow each learner to a \emph{fixed
#' size} and not prune it, because the regularization belongs to the number of
#' rounds and the learning rate rather than to the individual trees. In
#' \code{rpart} terms that is \code{cp = 0}, which
#' \code{treehypar = list(cp = 0)} gives you.
#'
#' The default is kept anyway, because on these data it is not the worse choice.
#' Both complexity parameters were scored across a 480-setting grid in
#' \code{\link[adatutor]{hypergrid}}; paired over those settings, \code{cp = 0}
#' is behind by .0044 on mean AUROC (95 percent CI .0021 to .0067). The gap is
#' entirely at depth 4, where unpruned learners overfit the reweighted data
#' (-.0152, 95 percent CI -.0214 to -.0090). At depth 1 -- the stumps this package
#' teaches with -- pruning makes no difference at all (+.0001, 95 percent CI
#' -.0012 to .0014).
#'
#' @return A list containing the trained weak learners (`h`) and their
#' associated weights (`a`). Additional attributes may be included for model
#' tracking purposes.
#'
#' @export
adaboost <- function(
  formula,
  data,
  T,
  eta,
  maxdepth = 1,
  treehypar = NULL,
  keep_data = TRUE,
  input_checks = TRUE,
  verbose = TRUE
) {
  # Progress is written a piece at a time, so a line is usually half finished
  # when something goes wrong: `color_message()` leaves the line open for the
  # " Done" that `walking_colordots()` will add, and the progress bar is only
  # closed after the loop. Either way an error would print onto the same line.
  # `on.exit()` closes whichever is open, and unlike `tryCatch()` it also runs
  # on a user interrupt -- which is how a thousand-round fit usually ends early.
  pb <- NULL
  finish <- FALSE
  if (verbose) {
    on.exit(
      {
        if (!finish) {
          # closing the bar breaks the line; without one, do it directly
          if (!is.null(pb)) close(pb) else message("")
        }
      },
      add = TRUE
    )
  }

  if (verbose) {
    color_message(
      "Start the AdaBoost training process:\n",
      color_code = 1,
      newline = TRUE
    )
  }

  if (input_checks) {
    if (verbose) {
      color_message("Run mild input checks", color_code = 30)
    }
    check_df(data)
    check_length(data)
    check_eta(eta)
    check_numeric(T)
    if (verbose) walking_colordots()
  }

  if (verbose) {
    color_message("Start the initialization process", color_code = 30)
  }

  # Set fast default control parameters for rpart. cp stays at rpart's own
  # 0.01 so that maxdepth = d and treehypar = rpart.control(maxdepth = d)
  # agree; only xval and maxsurrogate are tuned for speed.
  def_ctrl <- rpart::rpart.control(
    maxdepth = maxdepth,
    xval = 0,
    maxsurrogate = 0,
    cp = 0.01
  )

  # Overwrite defaults with user-specified parameters if provided
  if (!is.null(treehypar) && is.list(treehypar)) {
    treehypar <- utils::modifyList(def_ctrl, treehypar)
  } else {
    treehypar <- def_ctrl
  }

  # We still use match.call() ONLY to grab the literal name of the dataset
  # so we can attach it as a tracking attribute at the very end.
  data_name <- match.call()[["data"]]

  # The magic bridge: binds the formula to this function's local environment
  environment(formula) <- environment()

  # Clean, direct extraction of the target variable
  mf <- stats::model.frame(formula, data)
  y_train <- stats::model.response(mf)

  # Setup weights and containers
  m <- nrow(data)
  D <- rep(1, m) / m
  H <- vector("list", T)

  if (verbose) {
    walking_colordots()
    color_message(
      "Steps 1-4: Run through the algorithm steps\n",
      color_code = 30
    )
    # stderr, like every other piece of progress here: txtProgressBar writes to
    # stdout by default, which would put the bar in with the results
    pb <- utils::txtProgressBar(
      min = 0,
      max = T,
      style = 3,
      file = stderr()
    )
  }

  for (t in seq_len(T)) {
    # Clean, direct standard evaluation function call
    h <- rpart::rpart(
      formula = formula,
      data = data,
      weights = D,
      method = "class",
      control = treehypar,
      model = FALSE,
      y = FALSE
    )

    # Clean, direct prediction
    y_retro <- stats::predict(h, newdata = data, type = "class")

    # Math and weight updates
    correct <- (y_train == y_retro)
    # Weighted error, clamped away from 0 and 1 so the model weight stays finite
    # (e -> 0 or 1 would otherwise send `a` to +/-Inf). A stump worse than chance
    # (e > 0.5) legitimately yields a negative `a` and is handled by the update below.
    e <- min(max(sum(D[!correct]), 1e-10), 1 - 1e-10)
    a <- 0.5 * log((1 - e) / e) * eta

    D_unorm <- D
    D_unorm[correct] <- D[correct] * exp(-a)
    D_unorm[!correct] <- D[!correct] * exp(a)
    D <- D_unorm / sum(D_unorm)

    # Memory cleanup before storing
    h$where <- NULL
    h$call <- NULL
    # `terms` carries this function's frame, which holds `data`, `D` and `H`, so
    # leaving it attached makes every saved fit a copy of the training set. The
    # frame's parent chain already runs through globalenv(), and that is where
    # the stored `formula` attribute points too, so nothing a formula can
    # legitimately resolve is lost -- only the locals we are trying to drop.
    attr(h$terms, ".Environment") <- globalenv()

    H[[t]] <- list("h" = h, "a" = a)

    if (verbose) utils::setTxtProgressBar(pb, t)
  }

  finish <- TRUE

  if (verbose) {
    close(pb)
    color_message("Create output", color_code = 30)
    walking_colordots()
    color_message(
      "Training process successfully completed.\n",
      color_code = 1,
      newline = TRUE
    )
  }

  names(H) <- paste0("t", seq_len(T))

  # Stash the hyperparameters (with a clean formula environment to avoid
  # capturing the training data) so the model can be refitted later.
  form_store <- formula
  environment(form_store) <- globalenv()

  # One copy, on the ensemble -- not one per tree. `mf` is the model frame, so
  # it holds exactly the columns the formula named and nothing else.
  trainset <- if (keep_data) mf else NULL

  structure(
    H,
    class = "adaboost",
    trainset = trainset,
    train = data_name,
    formula = form_store,
    T = T,
    eta = eta,
    maxdepth = maxdepth
  )
}

#' @title Predict From a Boosted Ensemble
#'
#' @description This function implements the testing phase of the AdaBoost
#'   algorithm. It extracts the adaptively boosted weak learners (e.g.
#'   classification stumps) and their corresponding weights to combine them into
#'   the weighted sum \deqn{H(\mathbf{x}) = \sum_{i=1}^Ta_th_t(\mathbf x),}
#'   which yields AdaBoost's predictions or retrodictions on a given set.
#'
#' @param object A model fitted with \code{\link[adatutor]{adaboost}}. This is
#'   a list of boosted trees and their weights: each element holds a weak
#'   learner (e.g., a decision stump) and its corresponding weight.
#'
#' @param newdata A data frame containing the set to predict. Its columns should
#'   match the predictors the model was trained on.
#'
#' @param type A character string indicating the type of prediction to return.
#'   Options are \code{"class"} for hard class labels (-1, 1) or \code{"margin"}
#'   for the raw continuous boosting score. Defaults to \code{"class"}. Use
#'   \code{"margin"} for rank-based measures such as the ROC AUC: the margin
#'   carries the ranking that hard class labels throw away.
#'
#' @param input_checks A logical value indicating whether to perform input
#'   validation checks. Defaults to \code{TRUE}.
#'
#' @param verbose A logical value specifying whether to display progress
#'   messages and animations. Defaults to \code{TRUE}.
#'
#' @param ... Ignored.
#'
#' @details \strong{Prediction or retrodiction?} The same call does both, and
#'   which one you get depends entirely on the data you hand it. Scoring the
#'   frame the model was trained on is a \emph{retrodiction}: it measures how
#'   well the model fits data it has already seen, which is not a measure of
#'   predictive performance. Scoring data held out from training is a
#'   prediction.
#'
#'   Nothing in the syntax distinguishes them, which is exactly why the mistake
#'   is easy, so this reports it. When the fit kept its training data (see
#'   \code{keep_data} in \code{\link[adatutor]{adaboost}}) the comparison is
#'   exact and catches a renamed variable or a subset of the training rows;
#'   otherwise it falls back to comparing the variable's name. Calling
#'   \code{predict()} with no \code{newdata} deliberately returns
#'   retrodictions, the way \code{predict()} does for \code{\link[stats]{lm}}.
#'
#'   It is a message rather than a warning: retrodicting on purpose is a normal
#'   thing to do -- the tutorial does it to show that training performance is
#'   near-perfect and therefore uninformative.
#'
#' @return A numeric vector containing the final predictions from the AdaBoost
#'   model. Depending on the \code{type} argument, this will be class labels or
#'   margins.
#'
#' @seealso \code{\link[adatutor]{adaboost}}
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
  # Match the requested type (defaults to "class")
  type <- match.arg(type)

  # No newdata: fall back to the training data, the way predict() does for lm().
  # What comes back is a retrodiction, and `check_train()` below says so.
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

  # `color_message()` leaves its line open for the " Done" that
  # `walking_colordots()` adds, so an error in between would print onto the same
  # line. Break it on the way out. See adaboost(), which also has a progress bar
  # to close.
  finish <- FALSE
  if (verbose) {
    on.exit(if (!finish) message(""), add = TRUE)
  }

  # Which of the two this is decides the wording below, so it is worked out
  # whenever anything will say it -- not only when the checks run.
  fcl <- match.call()
  test_pos <- match("newdata", names(fcl), nomatch = 0L)
  testnme <- if (test_pos) fcl[[test_pos]] else NULL
  state <- if (input_checks || verbose) {
    overlap_state(attr(object, "train"), testnme, trainset, newdata)
  } else {
    NA_character_
  }
  # "all" retrodictions, "none" predictions, and the slash form when we cannot
  # tell -- which is the honest label, not a vague one.
  noun <- if (is.na(state)) {
    "predictions/retrodictions"
  } else if (state == "all") {
    "retrodictions"
  } else {
    "predictions"
  }

  if (verbose) {
    color_message("Start the AdaBoost test process:\n", color_code = 1)
  }
  if (input_checks) {
    if (verbose) {
      color_message("Run mild input checks", color_code = 30)
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
    color_message("Extract the trees", color_code = 30)
    walking_colordots()
  }
  h <- lapply(object, "[[", "h")
  check_length(h)
  if (verbose) {
    color_message("Extract the model weights", color_code = 30)
    walking_colordots()
  }
  a <- vapply(object, "[[", numeric(1), "a")
  check_length(a)

  if (verbose) {
    color_message(paste0("Make ", noun, "\n"), color_code = 30)
  }

  # Define expected N for vapply
  N <- nrow(newdata)
  # Use vapply instead of sapply (faster). Wrap in matrix() because vapply
  # simplifies to a plain vector when N == 1, which would turn the matrix
  # product below into a T x T outer product.
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
    color_message(paste0("Combine ", noun), color_code = 30)
  }

  ypred_stumps <- 2 * y12_stumps - 3

  # Calculate the continuous raw margin
  raw_margin <- as.vector(a %*% t(ypred_stumps))

  finish <- TRUE

  if (verbose) {
    walking_colordots()
    color_message("Test process successfully completed.\n", color_code = 1)
  }

  # Last, not first: a transcript scrolls, and nobody reads upwards. At the end
  # it sits where the cursor lands, next to the values it is about.
  if (input_checks) {
    check_train(state, testnme, verbose = verbose, fell_back = fell_back)
  }

  # Return based on requested type
  if (type == "class") {
    sign(raw_margin)
  } else {
    raw_margin
  }
}
