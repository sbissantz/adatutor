# gauge(): each tree's model weight, shared by the gain of its splits

voi <- c("replicate", "power.o", "n.o", "effect_size.o", "p_value.o")

fit_boosted <- function(maxdepth, n_iter) {
  rpart::rpart(
    replicate ~ .,
    data = altmext[, voi],
    method = "class",
    maxdepth = maxdepth,
    model = TRUE
  ) |>
    adaboost(n_iter = n_iter, eta = 1, verbose = FALSE)
}

as_vector <- function(imp) {
  setNames(imp$importance, imp$variable)
}

test_that("gauge() equals the tutorial's stump loop", {
  fit <- fit_boosted(maxdepth = 1, n_iter = 50)
  imp <- gauge(fit)

  expect_s3_class(imp, c("gauge", "data.frame"))
  expect_named(imp, c("variable", "importance"))
  expect_equal(sum(imp$importance), 100)
  expect_true(all(diff(imp$importance) <= 0))

  loop <- setNames(numeric(4), voi[-1])
  for (t in seq_along(fit)) {
    predictor <- fit[[t]]$h$frame$var[1]
    if (predictor == "<leaf>") {
      next
    }
    loop[predictor] <- loop[predictor] + fit[[t]]$alpha
  }
  loop <- loop / sum(loop) * 100

  expect_identical(as_vector(imp), loop[imp$variable])
})

test_that("gauge() shares a deeper tree's weight by the gain of its splits", {
  fit <- fit_boosted(maxdepth = 4, n_iter = 10)
  imp <- gauge(fit)

  # adaboost() fits no surrogates, so variable.importance is the primary gain
  loop <- setNames(numeric(4), voi[-1])
  for (t in seq_along(fit)) {
    gain <- fit[[t]]$h$variable.importance
    if (is.null(gain)) {
      next
    }
    share <- fit[[t]]$alpha * gain / sum(gain)
    loop[names(share)] <- loop[names(share)] + share
  }
  loop <- loop / sum(loop) * 100

  expect_equal(as_vector(imp), loop[imp$variable])
})

test_that("gauge() credits primary splits only, not surrogates", {
  with_surrogates <- rpart::rpart(
    replicate ~ .,
    data = altmext[, voi],
    method = "class",
    maxdepth = 4,
    cp = 0.001,
    minsplit = 5,
    minbucket = 2
  )
  without <- update(with_surrogates, maxsurrogate = 0)

  # the tree must exercise the case: several primary splits plus surrogates
  expect_gt(sum(with_surrogates$frame$var != "<leaf>"), 1L)
  expect_gt(sum(with_surrogates$frame$nsurrogate), 0L)
  expect_gt(
    length(with_surrogates$variable.importance),
    length(without$variable.importance)
  )

  imp_with <- gauge(list(t1 = list(h = with_surrogates, alpha = 1)))
  imp_without <- gauge(list(t1 = list(h = without, alpha = 1)))
  expect_equal(imp_with, imp_without)

  # every predictor of the formula is listed; one the tree never used has 0
  gain <- without$variable.importance
  expected <- setNames(numeric(4), voi[-1])
  expected[names(gain)] <- gain / sum(gain) * 100
  expect_setequal(imp_with$variable, voi[-1])
  expect_equal(as_vector(imp_with), expected[imp_with$variable])
})

test_that("gauge() returns no rows when no tree split, and plot() refuses it", {
  bare <- rpart::rpart(replicate ~ ., data = altmext[, voi], cp = 1)
  expect_identical(bare$frame$var, "<leaf>")

  imp <- gauge(list(t1 = list(h = bare, alpha = 0.5)))
  expect_s3_class(imp, "gauge")
  expect_equal(nrow(imp), 0L)
  expect_error(plot(imp), "no variable was split on")
})

test_that("plot.gauge() draws top_n lollipops and returns NULL invisibly", {
  imp <- gauge(fit_boosted(maxdepth = 1, n_iter = 20))

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_invisible(plot(imp))
  expect_null(plot(imp))
  expect_null(plot(imp, top_n = 2))
})
