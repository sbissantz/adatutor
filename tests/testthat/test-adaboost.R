test_that("adaboost() and predict() work", {
  # Test 1: Silent and no input checks
  # Notice: No need to pass treehypar; the fast defaults handle it natively
  fit <- rpart::rpart(
    Species ~ .,
    data = iris,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 10, eta = 1, verbose = FALSE, input_checks = FALSE)

  expect_length(fit, 10)
  expect_equal(round(fit$t1$a, 1), 0.3)
  expect_equal(round(fit$t10$a, 1), 0.5)
  expect_match(class(fit$t1$h), "rpart")
  expect_equal(attr(fit, "train"), as.name("iris"))

  ypred <- predict(fit, iris, input_checks = FALSE, verbose = FALSE)
  expect_equal(ypred[1:6], rep(-1, 6))

  # Test 2: Verbose and with input checks
  fit2 <- rpart::rpart(
    Species ~ .,
    data = iris,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 10, eta = 1, verbose = TRUE, input_checks = TRUE)

  expect_length(fit2, 10)
  expect_equal(round(fit2$t1$a, 1), 0.3)
  expect_equal(round(fit2$t10$a, 1), 0.5)
  expect_match(class(fit2$t1$h), "rpart")
  expect_equal(attr(fit2, "train"), as.name("iris"))

  ypred2 <- predict(fit2, iris, input_checks = FALSE, verbose = FALSE)
  expect_equal(ypred2[1:6], rep(-1, 6))

  ypred3 <- predict(fit2, iris, input_checks = TRUE, verbose = TRUE)
  expect_equal(ypred3[1:6], rep(-1, 6))
})

test_that("predict() handles single-row newdata", {
  # vapply() simplifies to a plain vector when the test set has one row, which
  # used to turn the margin computation into a T x T outer product.
  data(altmejd)
  prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  voinms <- c(prednms, "replicate")

  fit <- rpart::rpart(
    replicate ~ .,
    data = altmejd[1:60, voinms],
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE, input_checks = FALSE)

  for (ty in c("margin", "class")) {
    many <- predict(
      fit,
      altmejd[61:70, prednms],
      type = ty,
      verbose = FALSE,
      input_checks = FALSE
    )
    one <- predict(
      fit,
      altmejd[61, prednms, drop = FALSE],
      type = ty,
      verbose = FALSE,
      input_checks = FALSE
    )

    expect_length(one, 1L)
    # the value must match the multi-row call, not merely the length
    expect_equal(one, many[1])
  }
})

test_that("adaboost() stashes hyperparameters and predict() returns margins", {
  # A genuinely noisy binary problem, so margins are moderate (not separable)
  data(altmejd)
  prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  voinms <- c(prednms, "replicate")

  fit <- rpart::rpart(
    replicate ~ .,
    data = altmejd[, voinms],
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 20, eta = 0.5, verbose = FALSE, input_checks = FALSE)

  # adaboost() stashes the hyperparameters needed to refit the model later.
  # Depth is not one of its own: it arrives on the learner and is stored as the
  # whole control the trees were grown with.
  expect_equal(attr(fit, "n_iter"), 20)
  expect_equal(attr(fit, "eta"), 0.5)
  expect_equal(attr(fit, "control")$maxdepth, 1)
  expect_s3_class(attr(fit, "formula"), "formula")

  margin <- predict(
    fit,
    altmejd[, prednms],
    type = "margin",
    verbose = FALSE,
    input_checks = FALSE
  )
  class_lab <- predict(
    fit,
    altmejd[, prednms],
    type = "class",
    verbose = FALSE,
    input_checks = FALSE
  )

  # the class label is the sign of the margin, so the margin carries the
  # ranking that the label discards
  expect_equal(class_lab, sign(margin))
  expect_true(is.numeric(margin) && !all(margin %in% c(-1, 1)))

  # "prob" was removed: probabilities belong to beyondada::calibrateAda()
  expect_error(predict(
    fit,
    altmejd[, prednms],
    type = "prob",
    verbose = FALSE,
    input_checks = FALSE
  ))
})

# ---- clean failure ---------------------------------------------------------
#
# Progress is written a piece at a time, so a line is normally half finished
# when something goes wrong. Without the on.exit() cleanup the error text is
# printed onto that same line, and a progress bar is left unterminated.

stderr_of <- function(expr) {
  utils::capture.output(
    tryCatch(expr, error = function(e) message("Error: ", conditionMessage(e))),
    type = "message"
  )
}

test_that("an error before the loop starts on its own line", {
  d <- altmejd_splits$train[, c("power.o", "n.o", "replicate")]

  h <- rpart::rpart(replicate ~ ., data = d, maxdepth = 1, model = TRUE)
  # `eta` is missing, so forcing it inside check_eta() raises R's own
  # missing-argument error -- after the "Run mild input checks" line is open
  # and before the loop starts, which is the placement under test
  out <- stderr_of(adaboost(h, n_iter = 4))
  expect_false(any(grepl("input checksError", out)))
  expect_true(any(grepl("^Error: ", out)))

  out <- stderr_of(predict(list(), newdata = "not a data frame"))
  expect_false(any(grepl("input checksError", out)))
  expect_true(any(grepl("^Error: ", out)))
})

test_that("an error inside the loop closes the progress bar", {
  d <- altmejd_splits$train[, c("power.o", "n.o", "replicate")]

  # make rpart throw on the third call: the first builds the learner, so the
  # third is the second boosting round -- well inside the loop, after the bar
  # has been opened. trace() affects this session only.
  #
  # The counter lives in its own environment, spliced into the tracer by
  # bquote(): the tracer runs in rpart's frame, so `<<-` would walk rpart's
  # namespace and never see a local here.
  counter <- new.env(parent = emptyenv())
  counter$i <- 0
  suppressMessages(trace(
    rpart::rpart,
    tracer = bquote({
      e <- .(counter)
      e$i <- e$i + 1
      if (e$i == 3) stop("forced failure")
    }),
    print = FALSE
  ))
  on.exit(suppressMessages(untrace(rpart::rpart)), add = TRUE)

  out <- stderr_of(
    rpart::rpart(
      replicate ~ .,
      data = d,
      maxdepth = 1,
      model = TRUE
    ) |>
      adaboost(n_iter = 6, eta = 1)
  )

  # the bar was reached, so this exercises close(pb) rather than the fallback
  expect_true(any(grepl("Steps 1-4", out)))
  # and the error is not glued onto the bar's last percentage
  expect_false(any(grepl("%Error", out)))
  expect_true(any(grepl("^Error: ", out)))
})

test_that("verbose = FALSE prints nothing, even when it fails", {
  d <- altmejd_splits$train[, c("power.o", "n.o", "replicate")]

  quiet <- utils::capture.output(
    fit <- rpart::rpart(
      replicate ~ .,
      data = d,
      maxdepth = 1,
      model = TRUE
    ) |>
      adaboost(n_iter = 2, eta = 1, verbose = FALSE),
    type = "message"
  )
  expect_identical(quiet, character(0))

  failed <- stderr_of(
    adaboost(
      rpart::rpart(replicate ~ ., data = d, maxdepth = 1, model = TRUE),
      verbose = FALSE
    )
  )
  # only the error itself, no stray newline from the cleanup
  expect_length(failed, 1L)
  expect_match(failed, "^Error: ")
})

test_that("a fit leaks nothing beyond the frame it was asked to keep", {
  # `terms` on each tree used to hold adaboost()'s own environment, which
  # contains `data`, `D` and `H` -- so a saved fit was a copy of the training
  # set whether you wanted one or not. object.size() cannot see this: it does
  # not traverse environments, so the assertion has to go through serialize().
  base <- altmejd_splits$train[, c("power.o", "n.o", "replicate")]
  big <- base[rep(seq_len(nrow(base)), 200), ]
  data_bytes <- length(serialize(big, NULL))

  # keep_data = FALSE is the condition this guards: nothing retained at all
  lean <- rpart::rpart(
    replicate ~ .,
    data = big,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, keep_data = FALSE, verbose = FALSE)
  expect_lt(length(serialize(lean, NULL)), data_bytes / 4)

  # and with keep_data = TRUE the growth is the stored frame and nothing more,
  # which is what rules out the environment leak coming back alongside it
  full <- rpart::rpart(
    replicate ~ .,
    data = big,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE)
  grew <- length(serialize(full, NULL)) - length(serialize(lean, NULL))
  frame_bytes <- length(serialize(attr(full, "trainset"), NULL))
  expect_lt(abs(grew - frame_bytes), 0.01 * frame_bytes)

  for (f in list(lean, full)) {
    for (i in seq_along(f)) {
      env <- attr(f[[i]]$h$terms, ".Environment")
      expect_identical(env, globalenv())
      expect_false(exists("data", envir = env, inherits = FALSE))
    }
    # the separately scrubbed formula attribute is unaffected
    expect_identical(environment(attr(f, "formula")), globalenv())
  }
})

test_that("scrubbing the terms environment leaves predictions intact", {
  base <- altmejd_splits$train[, c("power.o", "n.o", "replicate")]
  nd <- base[, c("power.o", "n.o")]

  plain <- rpart::rpart(
    replicate ~ .,
    data = base,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE)
  # a transformation has to resolve at predict time from the scrubbed terms
  trans <- rpart::rpart(
    replicate ~ log(power.o) + n.o,
    data = base,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE)

  for (f in list(plain, trans)) {
    m <- predict(f, nd, type = "margin", verbose = FALSE)
    expect_length(m, nrow(base))
    expect_false(anyNA(m))
  }
  # and the fit is still usable by everything that reads it
  expect_gt(nrow(gauge(plain)), 0L)
})

# ---- retrodiction versus prediction ----------------------------------------
#
# The fit keeps its training data (keep_data = TRUE) so the check can compare
# the data itself rather than the variable's name. The name match is a fallback
# for keep_data = FALSE, and it misses a rename or a subset.

retro_msg <- function(expr) {
  out <- utils::capture.output(invisible(expr), type = "message")
  any(grepl("^! ", out))
}

test_that("the retrodiction check compares data, not the variable name", {
  train <- altmejd_splits$train[, c("power.o", "n.o", "replicate")]
  test <- altmejd_splits$test[, c("power.o", "n.o", "replicate")]
  fit <- rpart::rpart(
    replicate ~ .,
    data = train,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 8, eta = 1, verbose = FALSE)

  expect_true(retro_msg(predict(fit, newdata = train, verbose = FALSE)))

  # a renamed variable: the old name-based check was silent here
  x <- train
  expect_true(retro_msg(predict(fit, newdata = x, verbose = FALSE)))

  # a subset is still training data
  expect_true(retro_msg(predict(fit, newdata = train[1:50, ], verbose = FALSE)))

  # and the test set must stay silent -- a false positive is worse than a miss
  expect_false(retro_msg(predict(fit, newdata = test, verbose = FALSE)))
})

test_that("it informs rather than warns", {
  train <- altmejd_splits$train[, c("power.o", "n.o", "replicate")]
  fit <- rpart::rpart(
    replicate ~ .,
    data = train,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE)
  # the tutorial retrodicts on purpose (Listing 27), so this must not warn
  expect_message(predict(fit, newdata = train, verbose = FALSE))
  expect_no_warning(suppressMessages(
    predict(fit, newdata = train, verbose = FALSE)
  ))
})

test_that("predict() with no newdata returns retrodictions", {
  train <- altmejd_splits$train[, c("power.o", "n.o", "replicate")]
  fit <- rpart::rpart(
    replicate ~ .,
    data = train,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 8, eta = 1, verbose = FALSE)

  bare <- suppressMessages(predict(fit, type = "margin", verbose = FALSE))
  named <- suppressMessages(
    predict(fit, newdata = train, type = "margin", verbose = FALSE)
  )
  expect_identical(bare, named)
  expect_true(retro_msg(predict(fit, verbose = FALSE)))
})

test_that("keep_data = FALSE drops the frame and falls back to the name", {
  train <- altmejd_splits$train[, c("power.o", "n.o", "replicate")]
  lean <- rpart::rpart(
    replicate ~ .,
    data = train,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 8, eta = 1, keep_data = FALSE, verbose = FALSE)
  full <- rpart::rpart(
    replicate ~ .,
    data = train,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 8, eta = 1, verbose = FALSE)

  expect_null(attr(lean, "trainset"))
  expect_false(is.null(attr(full, "trainset")))
  # serialize(), not object.size(): the latter does not traverse environments
  expect_lt(length(serialize(lean, NULL)), length(serialize(full, NULL)))

  expect_error(predict(lean, verbose = FALSE), "keep_data = FALSE")
  # the name match still works when the data is gone
  expect_true(retro_msg(predict(lean, newdata = train, verbose = FALSE)))
})

test_that("only the formula's columns are stored", {
  train <- altmejd_splits$train
  fit <- rpart::rpart(
    replicate ~ power.o + n.o,
    data = train,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE)
  expect_setequal(
    names(attr(fit, "trainset")),
    c("replicate", "power.o", "n.o")
  )
})

# ---- the transcript says which one it is ------------------------------------

has_notice <- function(expr) {
  # split before anchoring: `^` against the collapsed transcript matches only
  # its very start, so once the notice moved to the end it could never match
  any(grepl("^! ", strsplit(verbose_lines(expr), "\n")[[1]]))
}

verbose_lines <- function(expr) {
  out <- utils::capture.output(invisible(expr), type = "message")
  gsub("\033\\[[0-9;]*m", "", paste(out, collapse = "\n"))
}

test_that("progress messages name predictions or retrodictions", {
  train <- altmejd_splits$train[, c("power.o", "n.o", "replicate")]
  test <- altmejd_splits$test[, c("power.o", "n.o", "replicate")]
  fit <- rpart::rpart(
    replicate ~ .,
    data = train,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE)

  on_test <- verbose_lines(predict(fit, newdata = test, type = "margin"))
  expect_match(on_test, "Make predictions\n")
  expect_match(on_test, "Combine predictions")
  expect_false(grepl("retrodictions", on_test))

  on_train <- verbose_lines(predict(fit, newdata = train, type = "margin"))
  expect_match(on_train, "Make retrodictions")
  expect_match(on_train, "Combine retrodictions")
})

test_that("the slash wording is kept for the case we cannot tell", {
  train <- altmejd_splits$train[, c("power.o", "n.o", "replicate")]
  test <- altmejd_splits$test[, c("power.o", "n.o", "replicate")]
  lean <- rpart::rpart(
    replicate ~ .,
    data = train,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, keep_data = FALSE, verbose = FALSE)
  out <- verbose_lines(predict(lean, newdata = test, type = "margin"))
  expect_match(out, "Make predictions/retrodictions")
})

test_that("the wording does not depend on input_checks", {
  # the state is computed whenever anything will say it, not only when the
  # checks run -- this is what proves it was lifted out of check_train()
  train <- altmejd_splits$train[, c("power.o", "n.o", "replicate")]
  fit <- rpart::rpart(
    replicate ~ .,
    data = train,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE)
  out <- verbose_lines(
    predict(fit, newdata = train, type = "margin", input_checks = FALSE)
  )
  expect_match(out, "Make retrodictions")
  expect_false(has_notice(
    predict(fit, newdata = train, type = "margin", input_checks = FALSE)
  ))
})

test_that("the notice comes after the transcript, on its own line", {
  # it was once emitted into the open line left by "Run mild input checks",
  # which waits for walking_colordots() to close it. It now lands at the very
  # end: a transcript scrolls, and nobody reads upwards.
  train <- altmejd_splits$train[, c("power.o", "n.o", "replicate")]
  fit <- rpart::rpart(
    replicate ~ .,
    data = train,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE)
  # split first: R's `.` matches a newline, so a collapsed string would span
  # lines and the assertion would pass whatever the layout
  lines <- strsplit(
    verbose_lines(predict(fit, newdata = train, type = "margin")),
    "\n"
  )[[1]]
  expect_false(any(grepl("input checks.*^! ", lines)))
  # the reason, quiet and indented; then the conclusion, carrying the `!`
  expect_true(any(grepl("^  `train` was also used for training", lines)))
  expect_true(any(grepl("^! Outputs are retrodictions", lines)))
  expect_gt(
    grep("^! Outputs are retrodictions", lines)[1],
    grep("^  `train` was also used", lines)[1]
  )

  # last, where the cursor lands -- not inside the run and not above it
  notice <- grep("^! ", lines)
  done <- grep("Test process successfully completed", lines)
  expect_gt(notice[1], done[1])
})

test_that("a partial overlap is deliberately not reported", {
  # `altmejd` holds genuinely duplicated rows: two of the 22 shipped test rows
  # match a training row on all four predictors and the outcome. Reporting
  # partial overlap would therefore fire on the canonical test set every time.
  train <- altmejd_splits$train[, c("power.o", "n.o", "replicate")]
  test <- altmejd_splits$test[, c("power.o", "n.o", "replicate")]
  fit <- rpart::rpart(
    replicate ~ .,
    data = train,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE)

  # the notice is matched by its marker, not its wording -- the sentence is a
  # preference and should be free to change without breaking these
  noticed <- function(nd) {
    has_notice(predict(fit, newdata = nd, type = "margin"))
  }
  expect_false(noticed(test))
  expect_false(noticed(rbind(train[1:20, ], test)))
  # but a subset is every row, so it is still caught
  expect_true(noticed(train[1:50, ]))
})

test_that("predict() with no newdata names the fall-back, not \"this\"", {
  # testnme is NULL when there is no `newdata` argument to deparse, so the
  # message used to name no source at all, reading "this ..."
  train <- altmejd_splits$train[, c("power.o", "n.o", "replicate")]
  fit <- rpart::rpart(
    replicate ~ .,
    data = train,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE)

  out <- verbose_lines(predict(fit, type = "margin"))
  # matched loosely: the sentence is a preference, the concept is not
  expect_match(out, "No `newdata`")
  # the reason line is indented, not bulleted, so that is where "this" would show
  expect_false(any(grepl("^  this ", strsplit(out, "\n")[[1]])))
  expect_match(out, "retrodiction")
})

test_that("the splitting rule travels with the learner", {
  # rpart keeps the splitting rule in `parms`, not in `control`, so it used to
  # be dropped: an information-gain stump produced a gini ensemble in silence
  tr <- altmejd_splits$train[, c(
    "power.o",
    "n.o",
    "effect_size.o",
    "replicate"
  )]

  info <- rpart::rpart(
    replicate ~ .,
    data = tr,
    method = "class",
    maxdepth = 2,
    parms = list(split = "information"),
    model = TRUE
  ) |>
    adaboost(n_iter = 8, eta = 1, verbose = FALSE)
  # 2 is information gain, 1 is gini
  expect_true(all(vapply(info, function(z) z$h$parms$split, numeric(1)) == 2))

  gini <- rpart::rpart(
    replicate ~ .,
    data = tr,
    method = "class",
    maxdepth = 2,
    model = TRUE
  ) |>
    adaboost(n_iter = 8, eta = 1, verbose = FALSE)
  expect_true(all(vapply(gini, function(z) z$h$parms$split, numeric(1)) == 1))
})

test_that("a learner adaboost() cannot honour is refused, not ignored", {
  tr <- altmejd_splits$train[, c(
    "power.o",
    "n.o",
    "effect_size.o",
    "replicate"
  )]

  # a regression tree: the loop refits with method = "class", so accepting one
  # would hand back a different kind of model than was passed in
  num <- tr
  num$y <- as.numeric(num$replicate) - 1
  num$replicate <- NULL
  expect_error(
    adaboost(
      rpart::rpart(
        y ~ .,
        data = num,
        method = "anova",
        maxdepth = 1,
        model = TRUE
      ),
      n_iter = 3,
      eta = 1,
      verbose = FALSE
    ),
    "classification tree"
  )

  # observation weights: AdaBoost sets its own, from 1/n upwards. This also
  # used to fail deep inside predict.rpart with "Tree has variables not found
  # in new data", because rpart puts a `(weights)` column in the model frame.
  w <- ifelse(tr$replicate == levels(tr$replicate)[1], 9, 1)
  expect_error(
    adaboost(
      rpart::rpart(
        replicate ~ .,
        data = tr,
        weights = w,
        method = "class",
        maxdepth = 1,
        model = TRUE
      ),
      n_iter = 3,
      eta = 1,
      verbose = FALSE
    ),
    "weights"
  )

  # a hand-set prior cannot survive either: boosting recomputes it from each
  # round's weights
  expect_error(
    adaboost(
      rpart::rpart(
        replicate ~ .,
        data = tr,
        method = "class",
        maxdepth = 1,
        parms = list(prior = c(0.9, 0.1)),
        model = TRUE
      ),
      n_iter = 3,
      eta = 1,
      verbose = FALSE
    ),
    "prior"
  )

  # and a loss matrix is not passed on to the boosted trees
  expect_error(
    adaboost(
      rpart::rpart(
        replicate ~ .,
        data = tr,
        method = "class",
        maxdepth = 1,
        parms = list(loss = matrix(c(0, 5, 1, 0), 2)),
        model = TRUE
      ),
      n_iter = 3,
      eta = 1,
      verbose = FALSE
    ),
    "loss"
  )
})
