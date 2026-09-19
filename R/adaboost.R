#' @title Boost a Weak Learner With AdaBoost
#'
#' @description Boosts a fitted classification tree. The tree is the weak
#'   learner: it is refit \code{n_iter} times on reweighted data, and the
#'   ensemble is the weighted vote of those fits. AdaBoost specifies no base
#'   learner of its own -- it is defined over any learner whose weighted error
#'   stays below one half -- so the choice of a tree is the caller's, and it
#'   arrives already made. Depth, complexity parameter and every other
#'   \code{\link[rpart]{rpart.control}} setting are read off \code{h} rather
#'   than set here.
#'
#' @param h A classification tree fitted with \code{\link[rpart]{rpart}}, and
#'   fitted with \code{model = TRUE}. It supplies three things: the formula, the
#'   tree hyperparameters and the training data. \code{rpart} keeps the response
#'   but discards the predictors unless \code{model = TRUE}, so a tree fitted
#'   without it cannot be refit, and is rejected rather than guessed at.
#'
#' @param n_iter An integer specifying the number of boosting rounds --
#'   \eqn{T} in the algorithm, one weak learner per round.
#'
#' @param eta A numeric value representing the learning rate of the algorithm.
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
#'   number of rounds (\code{n_iter}). It initializes observation weights,
#'   refits \code{h} once per round under those weights, and updates them from
#'   the round's errors. A final ensemble of weak learners is produced.
#'
#' Key steps in the algorithm:
#' 1. Initialize observation weights.
#' 2. Train a decision tree using the current weights.
#' 3. Compute the weighted classification error and update the observation weights.
#' 4. Store the weak learner and its associated weight.
#'
#' \strong{What is read off \code{h}, and what is overridden.} The whole of
#' \code{h$control} is carried over -- \code{maxdepth}, \code{cp},
#' \code{minsplit}, \code{minbucket} -- except \code{xval} and
#' \code{maxsurrogate}, which are forced to zero. Those two are speed, not
#' structure: a thousand rounds should neither cross-validate each tree nor hunt
#' for surrogate splits, and neither setting changes the tree that results.
#' Everything a reader chose, they keep.
#'
#' \strong{The complexity parameter.} \code{cp} therefore arrives at whatever
#' the tree was fitted with, which for a plain \code{rpart()} call is its own
#' default of 0.01. That departs from the textbook formulation. The guidance
#' after the original algorithm (Friedman, Hastie & Tibshirani, 2000; Hastie,
#' Tibshirani & Friedman, 2009, on right-sized trees for boosting) is to grow
#' each learner to a \emph{fixed size} and not prune it, because the
#' regularization belongs to the number of rounds and the learning rate rather
#' than to the individual trees. In \code{rpart} terms that is \code{cp = 0},
#' which is now something the caller sets on their own tree.
#'
#' Leaving it at 0.01 is not the worse choice on these data, which is why the
#' tutorial does. Both complexity parameters were scored across a 480-setting
#' grid in
#' \code{\link[adatutor]{hypergrid}}; paired over those settings, \code{cp = 0}
#' is behind by .0044 on mean AUROC (95 percent CI .0021 to .0067). The gap is
#' entirely at depth 4, where unpruned learners overfit the reweighted data
#' (-.0152, 95 percent CI -.0214 to -.0090). At depth 1 -- the stumps this package
#' teaches with -- pruning makes no difference at all (+.0001, 95 percent CI
#' -.0012 to .0014).
#'
#' @return A list containing the trained weak learners (`h`) and their
#' associated weights (`a`). Additional attributes carry the training data, the
#' expanded formula, `n_iter`, `eta` and the `control` the trees were grown
#' with.
#'
#' @examples
#' data(altmejd_splits)
#' train <- altmejd_splits$train
#'
#' # The weak learner is fitted first, because which learner to boost is the
#' # caller's choice. `model = TRUE` so the tree carries the data it was grown
#' # on -- rpart keeps the response but drops the predictors without it.
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
#' # A deeper learner is a different tree, not a different argument here
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
  # preconditions, not diagnostics: without these there is nothing to boost, so
  # they run whether or not `input_checks` is on
  if (!inherits(h, "rpart")) {
    stop("`h` must be a tree fitted with rpart().", call. = FALSE)
  }
  if (is.null(h$model)) {
    stop(
      "`h` carries no training data: refit it with `model = TRUE`.",
      call. = FALSE
    )
  }

  # Read the learner apart. `h` supplies the formula, the tree hyperparameters
  # and the data; boosting adds only n_iter and eta.
  #
  # formula(h$terms) is the *expanded* formula, so `outcome ~ .` arrives with
  # its term labels already resolved.
  #
  # The magic bridge: rpart evaluates `weights` in the formula's environment,
  # so binding it to this frame is what lets `weights = D` below find `D`. Bind
  # it to globalenv() instead and `D` resolves to stats::D, the derivative
  # function. A globalenv() copy is stored on the ensemble further down.
  formula <- stats::formula(h$terms)
  environment(formula) <- environment()

  # h$model is already the model frame, so there is nothing to build
  mf <- h$model

  # A model frame holds *evaluated* terms, so `log(power.o)` is a column name
  # rather than a call and refitting the original formula against it fails on
  # "object 'power.o' not found". `<response> ~ .` over the frame finds the same
  # values under the same names; the original terms go back onto each tree
  # below, so predict() can still evaluate the transformation on raw newdata.
  fit_formula <- stats::as.formula(
    paste0("`", names(mf)[attr(h$terms, "response")], "` ~ ."),
    env = environment()
  )

  # one terms object for every round, pointing at globalenv() rather than at
  # this frame -- which holds `mf`, `D` and `H`, and would otherwise ride along
  # on all n_iter saved trees
  learner_terms <- h$terms
  attr(learner_terms, ".Environment") <- globalenv()

  # everything the caller chose is kept. xval and maxsurrogate are speed, not
  # structure: a thousand rounds should neither cross-validate nor hunt
  # surrogates, and neither changes the tree that comes out.
  ctrl <- h$control
  ctrl$xval <- 0
  ctrl$maxsurrogate <- 0

  # the literal name of the dataset, read off the tree's own call, for the
  # tracking attribute at the very end
  data_name <- h$call[["data"]]

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
        # whatever happens, stop the bar's colour leaking past this call
        if (!is.null(pb)) {
          cat("\033[0m", file = stderr())
        }
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
      color_code = ansi_bold,
      newline = TRUE
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

  y_train <- stats::model.response(mf)

  # Setup weights and containers
  m <- nrow(mf)
  D <- rep(1, m) / m
  H <- vector("list", n_iter)

  if (verbose) {
    walking_colordots()
    color_message(
      "Steps 1-4: Run through the algorithm steps\n",
      color_code = ansi_dim
    )
    # stderr, like every other piece of progress here: txtProgressBar writes to
    # stdout by default, which would put the bar in with the results.
    #
    # It emits no escape codes of its own, so as *uncoloured* stderr it inherits
    # whatever the console paints that -- red, in RStudio. Opening a colour
    # before it and resetting after makes it filler like the dots instead. The
    # reset is in on.exit() above, so an error mid-loop cannot leave the colour
    # bleeding into everything that follows.
    message("\033[", ansi_dim, "m", appendLF = FALSE)
    pb <- utils::txtProgressBar(
      min = 0,
      max = n_iter,
      style = 3,
      file = stderr()
    )
  }

  for (t in seq_len(n_iter)) {
    # `h_t`, not `h`: the argument is the prototype and must survive the loop.
    # model = FALSE keeps the round's tree from carrying its own copy of the
    # data -- one frame on the ensemble, never n_iter of them.
    h_t <- rpart::rpart(
      formula = fit_formula,
      data = mf,
      weights = D,
      method = "class",
      control = ctrl,
      model = FALSE,
      y = FALSE
    )

    # Clean, direct prediction
    y_retro <- stats::predict(h_t, newdata = mf, type = "class")

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
    h_t$where <- NULL
    h_t$call <- NULL
    # The learner's own terms, not the frame-name ones this round was fitted
    # with: predict() evaluates these against raw newdata, so a transformation
    # has to survive as a call. The split variable names are the same either
    # way, so the tree still matches what it is told to look up.
    #
    # `terms` also carries this function's frame, which holds `mf`, `D` and `H`,
    # so leaving that attached makes every saved fit a copy of the training set.
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

  # One copy, on the ensemble -- not one per tree. `mf` is the model frame, so
  # it holds exactly the columns the formula named and nothing else.
  trainset <- if (keep_data) mf else NULL

  # Stash the hyperparameters (with a clean formula environment to avoid
  # capturing the training data) so the model can be refitted later. `ctrl` is
  # the whole control list rather than depth alone, so a reader can see every
  # setting the trees were grown with.
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
    control = ctrl
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
    color_message(paste0("Combine ", noun), color_code = ansi_dim)
  }

  ypred_stumps <- 2 * y12_stumps - 3

  # Calculate the continuous raw margin
  raw_margin <- as.vector(a %*% t(ypred_stumps))

  finish <- TRUE

  if (verbose) {
    walking_colordots()
    color_message(
      "Test process successfully completed.\n",
      color_code = ansi_bold
    )
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
