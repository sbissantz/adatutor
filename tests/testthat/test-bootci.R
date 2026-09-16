make_fold <- function(n_pos, n_neg, seed = 1) {
  set.seed(seed)
  y <- c(rep(1L, n_pos), rep(0L, n_neg))
  list(y = y, s = rnorm(length(y)) + y)
}

test_that("bootCI() returns one row per measure with the observed estimate", {
  f <- make_fold(20, 20)
  set.seed(1)
  ci <- bootCI(f$y, f$s, n_resample = 200)

  observed <- assess(f$y, f$s)
  expect_equal(ci$metric, names(observed))
  # the point estimate comes from the data, never from the resamples
  expect_equal(ci$estimate, unname(observed))
  expect_true(all(ci$lower <= ci$estimate | is.na(ci$lower)))
  expect_true(all(ci$upper >= ci$estimate | is.na(ci$upper)))

  expect_equal(attr(ci, "n_resample"), 200)
  expect_false(attr(ci, "stratified"))
  expect_equal(attr(ci, "conf"), 0.95)
})

test_that("every measure is scored on the same resamples", {
  # One assess() call per draw is what keeps the intervals coherent, and it also
  # means a single `drop` count describes the whole table.
  f <- make_fold(15, 15)
  set.seed(2)
  ci <- bootCI(f$y, f$s, n_resample = 100)
  expect_length(unique(ci$drop), 1L)
})

test_that("n_resample counts usable resamples, not attempts", {
  # A fold with 2 negatives in 13 rejects roughly (11/13)^13 ~ 11% of draws.
  f <- make_fold(11, 2, seed = 3)
  set.seed(3)
  ci <- suppressWarnings(bootCI(f$y, f$s, n_resample = 500))

  # the requested number was collected despite the rejections
  expect_true(all(is.finite(ci$lower[ci$metric == "auroc"])))
  expect_gt(unique(ci$drop), 0)

  # a balanced fold of the same size rejects far less often
  g <- make_fold(7, 6, seed = 3)
  set.seed(3)
  ci2 <- suppressWarnings(bootCI(g$y, g$s, n_resample = 500))
  expect_lt(unique(ci2$drop), unique(ci$drop))
})

test_that("a high rejection rate is reported rather than hidden", {
  f <- make_fold(11, 2, seed = 4)
  set.seed(4)
  expect_warning(
    ci <- bootCI(f$y, f$s, n_resample = 300),
    "rejected as single-class"
  )
  expect_true(any(grepl("optimistic", attr(ci, "notes"))))
})

test_that("stratified resampling never rejects and warns about what it costs", {
  f <- make_fold(11, 2, seed = 5)
  set.seed(5)
  expect_warning(
    ci <- bootCI(f$y, f$s, n_resample = 300, stratified = TRUE),
    "prevalence-dependent"
  )
  # holding the class counts fixed makes a single-class draw impossible
  expect_equal(unique(ci$drop), 0)
  expect_true(attr(ci, "stratified"))
})

test_that("stratified intervals are narrower for prevalence-dependent measures", {
  # auroc ignores the base rate, so fixing it changes little; auprc depends on
  # it, so fixing it removes a genuine source of variation.
  f <- make_fold(20, 20, seed = 6)
  set.seed(7)
  plain <- suppressWarnings(bootCI(f$y, f$s, n_resample = 800))
  set.seed(7)
  strat <- suppressWarnings(bootCI(
    f$y,
    f$s,
    n_resample = 800,
    stratified = TRUE
  ))

  width <- function(ci, m) {
    ci$upper[ci$metric == m] - ci$lower[ci$metric == m]
  }
  expect_lt(width(strat, "auprc"), width(plain, "auprc"))
})

test_that("a degenerate fold gives NA rather than hanging", {
  # No number of attempts can make a single-class fold produce a usable draw.
  expect_warning(
    ci <- bootCI(rep(1, 8), rnorm(8), n_resample = 20),
    "cannot reliably produce"
  )
  expect_true(all(is.na(ci$lower)))
  expect_true(all(is.na(ci$upper)))
  # the attempt count is still reported
  expect_equal(unique(ci$drop), 20 * 100)
})

test_that("conf widens the interval", {
  f <- make_fold(25, 25, seed = 8)
  set.seed(9)
  narrow <- bootCI(f$y, f$s, n_resample = 400, conf = 0.5)
  set.seed(9)
  wide <- bootCI(f$y, f$s, n_resample = 400, conf = 0.99)

  w <- function(ci) {
    ci$upper[ci$metric == "auroc"] - ci$lower[ci$metric == "auroc"]
  }
  expect_gt(w(wide), w(narrow))
})

test_that("bootCI() rejects malformed input", {
  f <- make_fold(10, 10)
  expect_error(bootCI(f$y, f$s[1:5]), "same length")
  expect_error(bootCI(f$y, f$s, n_resample = 0), "at least 1")
  expect_error(bootCI(f$y, f$s, conf = 0), "between 0 and 1")
  expect_error(bootCI(f$y, f$s, conf = 1), "between 0 and 1")
})

test_that("intervals on the altmejd folds track fold size", {
  # notes 4: the interval is wide exactly where the fold is small. This is the
  # point the forest plot has to make, so it is worth pinning.
  skip_on_cran()
  data(altmejd)
  prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  voinms <- c(prednms, "replicate")

  widths <- vapply(
    c("ml3", "rpp"),
    function(rp) {
      tr <- altmejd[altmejd$pid != rp, ]
      te <- altmejd[altmejd$pid == rp, ]
      fit <- adaboost(
        replicate ~ .,
        data = tr[, voinms],
        T = 10,
        eta = 1,
        verbose = FALSE,
        input_checks = FALSE
      )
      m <- predict(
        fit,
        te[, prednms],
        type = "margin",
        verbose = FALSE,
        input_checks = FALSE
      )
      set.seed(112)
      ci <- suppressWarnings(bootCI(te$replicate, m, n_resample = 500))
      r <- ci[ci$metric == "auroc", ]
      r$upper - r$lower
    },
    numeric(1)
  )

  # ml3 holds 10 studies, rpp holds 90
  expect_gt(widths[["ml3"]], widths[["rpp"]])
})
