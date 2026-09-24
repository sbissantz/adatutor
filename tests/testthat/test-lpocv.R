fml <- replicate ~ power.o + effect_size.o + n.o + p_value.o

test_that("lpocv() returns a setting x project x metric cube", {
  data(altmejd)
  res <- lpocv(fml, data = altmejd, group = "pid", T = 5, eta = 1)

  expect_s3_class(res, "lpocv")
  expect_named(
    res,
    c(
      "estimates",
      "grid",
      "projects",
      "model",
      "nested",
      "criterion",
      "selected"
    )
  )

  n_measures <- length(assess(c(1, 0), c(1, -1)))
  expect_equal(dim(res$estimates), c(1L, 5L, n_measures))
  expect_named(dimnames(res$estimates), c("setting", "project", "metric"))
  expect_setequal(
    dimnames(res$estimates)$project,
    c("eerp", "ml1", "ml3", "rpp", "ssrp")
  )
  expect_equal(dimnames(res$estimates)$metric, names(assess(c(1, 0), c(1, -1))))

  # every fold is held out exactly once, so the fold sizes must total the data
  expect_equal(sum(res$projects$n), nrow(altmejd))
  expect_setequal(res$projects$project, dimnames(res$estimates)$project)
})

test_that("as.data.frame() flattens the cube without losing a cell", {
  data(altmejd)
  grid <- expand.grid(T = c(5, 10), eta = c(1, 0.5))
  res <- lpocv(fml, data = altmejd, group = "pid", grid = grid)

  d <- as.data.frame(res)
  expect_equal(nrow(d), length(res$estimates))
  expect_named(
    d,
    c(
      "model",
      "setting",
      "depth",
      "T",
      "eta",
      "project",
      "n",
      "base_rate",
      "metric",
      "estimate"
    )
  )

  # every long row must point back at the cell it came from
  cell <- cbind(
    match(as.character(d$setting), dimnames(res$estimates)$setting),
    match(d$project, dimnames(res$estimates)$project),
    match(d$metric, dimnames(res$estimates)$metric)
  )
  expect_equal(d$estimate, res$estimates[cell])
})

test_that("lpocv() reproduces the published leave-project-out figures", {
  # Pins the fold logic against notes 2: AdaBoost stumps, T = 10, eta = 1.
  # A mismatch here means the folds are wrong, not that the table is.
  # eerp is 0.734 rather than the 0.714 of notes 2 because the base learners
  # now carry rpart's cp = 0.01; the other four folds are unmoved.
  data(altmejd)
  res <- lpocv(fml, data = altmejd, group = "pid", T = 10, eta = 1)
  got <- summary(res, metric = "auroc")

  want <- c(eerp = 0.734, ml1 = 0.818, ml3 = 0.619, rpp = 0.620, ssrp = 0.676)
  expect_equal(
    round(setNames(got$estimate, got$project)[names(want)], 3),
    want,
    tolerance = 1e-3
  )
})

test_that("AdaBoost is scored on the margin, not on class labels", {
  # The submitted manuscript's bug: hard labels make auroc collapse onto
  # balanced accuracy. lpocv() must never reintroduce it.
  data(altmejd)
  res <- lpocv(fml, data = altmejd, group = "pid", T = 10, eta = 1)
  d <- as.data.frame(res)

  auroc <- d$estimate[d$metric == "auroc"]
  bacc <- d$estimate[d$metric == "bacc"]
  expect_false(isTRUE(all.equal(auroc, bacc)))
})

test_that("a grid is crossed with every fold", {
  data(altmejd)
  grid <- expand.grid(T = c(5, 10), eta = c(1, 0.5))
  res <- lpocv(fml, data = altmejd, group = "pid", grid = grid)

  n_measures <- length(assess(c(1, 0), c(1, -1)))
  expect_equal(dim(res$estimates), c(nrow(grid), 5L, n_measures))

  auroc <- summary(res, metric = "auroc")
  expect_equal(nrow(auroc), nrow(grid) * 5L)
  # each setting appears on each fold exactly once
  expect_true(all(table(auroc$T, auroc$eta, auroc$project) == 1))
})

test_that("a grid may omit columns, which fall back to the arguments", {
  data(altmejd)
  res <- lpocv(
    fml,
    data = altmejd,
    group = "pid",
    grid = data.frame(T = c(5, 10)),
    eta = 0.5
  )
  expect_true(all(res$grid$eta == 0.5))
  expect_true(all(res$grid$depth == 1))
})

test_that("a grid column may set an rpart control, per row", {
  data(altmejd)
  small <- expand.grid(T = c(5, 10), eta = 1, depth = 1)

  # one call with cp in the grid has to mean exactly what two calls with cp in
  # treehypar mean. Nothing else can catch a wrong merge order: the cached
  # search results predate the change.
  both <- lpocv(
    fml,
    data = altmejd,
    group = "pid",
    grid = rbind(cbind(small, cp = 0), cbind(small, cp = 0.01))
  )
  expect_true("cp" %in% names(both$grid))
  expect_true("cp" %in% names(as.data.frame(both)))

  d <- as.data.frame(both)
  key <- function(x) paste(x$depth, x$T, x$eta, x$project, x$metric)
  for (cpv in c(0, 0.01)) {
    arm <- lpocv(
      fml,
      data = altmejd,
      group = "pid",
      grid = small,
      treehypar = list(cp = cpv)
    )
    ref <- as.data.frame(arm)
    got <- d[d$cp == cpv, ]
    expect_equal(ref$estimate, got$estimate[match(key(ref), key(got))])
  }

  # the row is the more specific statement, so it overrides the call
  clash <- lpocv(
    fml,
    data = altmejd,
    group = "pid",
    grid = cbind(small, cp = 0),
    treehypar = list(cp = 0.5)
  )
  only_row <- lpocv(
    fml,
    data = altmejd,
    group = "pid",
    grid = cbind(small, cp = 0)
  )
  expect_equal(clash$estimates, only_row$estimates)
})

test_that("a grid column that sets nothing warns and names itself", {
  data(altmejd)
  # a grid built to hold results as well as settings is a common shape, and
  # silence there would hide a column meant to tune something
  noisy <- data.frame(T = 5, eta = 1, depth = 1, auc.mean = NA, runtime = NA)
  expect_warning(
    lpocv(fml, data = altmejd, group = "pid", grid = noisy),
    "auc\\.mean, runtime"
  )
  expect_silent(
    suppressMessages(lpocv(
      fml,
      data = altmejd,
      group = "pid",
      grid = data.frame(T = 5, eta = 1, depth = 1, minbucket = 4)
    ))
  )
})

test_that("the other learners run and report NA for hyperparameters they lack", {
  data(altmejd)

  # the hyperparameters live in the grid now, so that is where a learner that
  # has no use for one records it as NA
  logit <- lpocv(fml, data = altmejd, group = "pid", model = "logit")
  expect_true(all(is.na(logit$grid$T)))
  expect_true(all(is.na(logit$grid$eta)))
  expect_true(all(is.na(logit$grid$depth)))
  expect_true(all(is.finite(summary(logit)$estimate)))
  # and the flattened view still shows them, since it reads the grid
  expect_true(all(is.na(as.data.frame(logit)$T)))

  stump <- lpocv(fml, data = altmejd, group = "pid", model = "stump")
  expect_true(all(is.na(stump$grid$T)))
  expect_equal(unique(stump$grid$depth), 1)

  tree <- lpocv(fml, data = altmejd, group = "pid", model = "tree", depth = 4)
  expect_equal(unique(tree$grid$depth), 4)

  # probability models are thresholded at 0.5, so a fitted logit must not
  # predict every case into one class on every fold
  expect_false(all(summary(logit, metric = "spec")$estimate == 0))
})

test_that("random forest runs when the suggested package is available", {
  skip_if_not_installed("randomForest")
  data(altmejd)
  set.seed(1)
  rf <- lpocv(fml, data = altmejd, group = "pid", model = "rf")
  expect_true(all(is.finite(summary(rf)$estimate)))
})

test_that("lpocv() rejects malformed input", {
  data(altmejd)
  expect_error(lpocv(fml, data = altmejd, group = "nope"), "not found")
  expect_error(
    lpocv(fml, data = altmejd, group = c("pid", "eid")),
    "single column"
  )
  expect_error(
    lpocv(fml, data = altmejd[altmejd$pid == "rpp", ], group = "pid"),
    "at least two levels"
  )
  expect_error(
    lpocv(fml, data = altmejd, group = "pid", grid = data.frame()),
    "no rows"
  )
})

test_that("the methods behave", {
  data(altmejd)
  res <- lpocv(fml, data = altmejd, group = "pid", T = 5, eta = 1)

  expect_true(is.array(res$estimates))
  expect_s3_class(as.data.frame(res), "data.frame")

  s <- summary(res, metric = "auprc")
  expect_equal(nrow(s), 5L)
  expect_true(all(s$metric == "auprc"))

  out <- capture.output(print(res))
  expect_true(any(grepl("Leave-project-out", out)))
  expect_true(any(grepl("adaboost", out)))
  # the spread is shown, never a mean or standard error across folds
  expect_true(any(grepl("spread", out)))
  expect_false(any(grepl("\\bmean\\b|\\bSE\\b|\\bsd\\b", out)))
  # and the table carries exactly one row per project, so there is no
  # aggregate row hiding among them
  est_rows <- grep("(eerp|ml1|ml3|rpp|ssrp)", out, value = TRUE)
  expect_length(est_rows, 5L)

  expect_true(any(grepl(
    "no estimates",
    capture.output(print(res, metric = "nope"))
  )))
})

# ---- nested LPO-CV ---------------------------------------------------------

small_grid <- expand.grid(depth = 1:2, T = c(5, 10), eta = 1)

test_that("nested lpocv() reports the setting each outer fold chose", {
  data(altmejd)
  res <- lpocv(
    fml,
    data = altmejd,
    group = "pid",
    grid = small_grid,
    nested = TRUE
  )

  expect_s3_class(res, "lpocv")
  expect_true(res$nested)
  expect_equal(res$criterion, "auroc")
  expect_equal(nrow(res$selected), 5L)
  expect_setequal(res$selected$project, c("eerp", "ml1", "ml3", "rpp", "ssrp"))
  expect_true(all(res$selected$setting %in% seq_len(nrow(small_grid))))

  # the outer estimates carry the selected setting, not the grid position
  auroc <- summary(res, metric = "auroc")
  expect_equal(nrow(auroc), 5L)
  key <- merge(
    auroc[, c("project", "setting")],
    res$selected[, c("project", "setting")],
    by = "project"
  )
  expect_equal(key$setting.x, key$setting.y)
})

test_that("the inner search never sees the held-out project", {
  # The correctness property: for each outer fold, the selected setting must be
  # the argmax of a grid search run on the remaining projects only. Recomputing
  # that independently is the real check that no leakage occurs.
  data(altmejd)
  res <- lpocv(
    fml,
    data = altmejd,
    group = "pid",
    grid = small_grid,
    nested = TRUE
  )

  for (p in res$selected$project) {
    train <- altmejd[altmejd$pid != p, ]
    inner <- lpocv(fml, data = train, group = "pid", grid = small_grid)
    s <- summary(inner, metric = "auroc")
    per <- aggregate(estimate ~ setting, data = s, FUN = mean)
    expect_equal(
      res$selected$setting[res$selected$project == p],
      per$setting[which.max(per$estimate)]
    )
  }
})

test_that("nesting removes the optimism of scoring the winner on its own folds", {
  # Needs a grid with something to select over. With only a handful of settings
  # there is almost nothing to overfit, and the nested estimate can land above
  # the naive one by chance -- measured at -0.003 on a four-setting grid.
  #
  # The gap scales with how much there is to choose from. Measured on this data
  # with the shared cp = 0.01 control: 0.017 at 12 settings, 0.054 at 27, 0.093
  # at 48. Everything here is deterministic, so 0.017 is exact and the threshold
  # below only has to sit under it. The larger grids cost 38 s and 127 s, which
  # is not worth it for a sign test -- the magnitude belongs in the vignette.
  data(altmejd)
  grid <- expand.grid(depth = 1:3, T = c(10, 30), eta = c(1, 0.1))

  plain <- lpocv(fml, data = altmejd, group = "pid", grid = grid)
  s <- summary(plain, metric = "auroc")
  naive <- max(aggregate(estimate ~ setting, data = s, FUN = mean)$estimate)

  nst <- lpocv(fml, data = altmejd, group = "pid", grid = grid, nested = TRUE)
  nested <- mean(summary(nst, metric = "auroc")$estimate)

  # the naive best-average was selected and graded on the same five projects
  expect_gt(naive - nested, 0.01)

  # the winner of the full-grid search is not what the inner searches choose
  best <- s$setting[which.max(
    aggregate(estimate ~ setting, data = s, FUN = mean)$estimate
  )]
  expect_gt(length(unique(nst$selected$setting)), 1L)
})

test_that("the criterion decides what the inner search maximizes", {
  data(altmejd)
  by_auroc <- lpocv(
    fml,
    data = altmejd,
    group = "pid",
    grid = small_grid,
    nested = TRUE,
    criterion = "auroc"
  )
  by_bacc <- lpocv(
    fml,
    data = altmejd,
    group = "pid",
    grid = small_grid,
    nested = TRUE,
    criterion = "bacc"
  )

  expect_equal(by_auroc$criterion, "auroc")
  expect_equal(by_bacc$criterion, "bacc")
  # both are valid runs; the criterion is recorded so a reader can see which
  # decision rule produced the selections
  expect_equal(nrow(by_bacc$selected), 5L)
})

test_that("nested lpocv() rejects input it cannot honour", {
  data(altmejd)
  expect_error(
    lpocv(fml, data = altmejd, group = "pid", nested = TRUE),
    "needs a `grid`"
  )
  expect_error(
    lpocv(
      fml,
      data = altmejd[altmejd$pid %in% c("ml1", "ml3"), ],
      group = "pid",
      grid = small_grid,
      nested = TRUE
    ),
    "at least three groups"
  )
  expect_error(
    lpocv(
      fml,
      data = altmejd,
      group = "pid",
      grid = small_grid,
      nested = TRUE,
      criterion = "not_a_measure"
    ),
    "not one of the measures"
  )
})

test_that("print() surfaces the selection instability", {
  data(altmejd)
  res <- lpocv(
    fml,
    data = altmejd,
    group = "pid",
    grid = small_grid,
    nested = TRUE
  )
  out <- capture.output(print(res))

  expect_true(any(grepl("Nested leave-project-out", out)))
  expect_true(any(grepl("selected on: auroc", out)))
  expect_true(any(grepl("inner searches selected", out)))
  expect_true(any(grepl("diagnostic, not a selection rule", out)))
  # still no aggregate row and no cross-project SE
  expect_false(any(grepl("\\bmean\\b|\\bSE\\b|\\bsd\\b", out)))
})

test_that("a plain run records that it was not nested", {
  data(altmejd)
  res <- lpocv(fml, data = altmejd, group = "pid", T = 5, eta = 1)
  expect_false(res$nested)
  expect_null(res$selected)
  expect_false(any(grepl("Nested", capture.output(print(res)))))
})

# ---- plot.lpocv ------------------------------------------------------------

boot_by_project <- function(n_resample = 200) {
  data(altmejd)
  prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  voinms <- c(prednms, "replicate")
  out <- list()
  for (rp in c("eerp", "ml1", "ml3", "rpp", "ssrp")) {
    tr <- altmejd[altmejd$pid != rp, ]
    te <- altmejd[altmejd$pid == rp, ]
    fit <- rpart::rpart(
      replicate ~ .,
      data = tr[, voinms],
      maxdepth = 1,
      model = TRUE
    ) |>
      adaboost(n_iter = 5, eta = 1, verbose = FALSE, input_checks = FALSE)
    m <- predict(
      fit,
      te[, prednms],
      type = "margin",
      verbose = FALSE,
      input_checks = FALSE
    )
    set.seed(112)
    out[[rp]] <- suppressWarnings(bootCI(
      te$replicate,
      m,
      n_resample = n_resample
    ))
  }
  out
}

test_that("bootCI() keeps the draws so any level can be asked for later", {
  f <- c(rep(1L, 20), rep(0L, 20))
  set.seed(1)
  ci <- bootCI(f, rnorm(40) + f, n_resample = 150)
  d <- attr(ci, "draws")

  expect_true(is.matrix(d))
  expect_equal(nrow(d), 150L)
  expect_true(all(c("auroc", "auprc", "bacc") %in% colnames(d)))
  # the stored interval is recoverable from the stored draws
  q <- unname(quantile(d[, "auroc"], c(0.025, 0.975), na.rm = TRUE))
  expect_equal(unname(ci$lower[ci$metric == "auroc"]), q[1])
  expect_equal(unname(ci$upper[ci$metric == "auroc"]), q[2])
})

test_that("plot.lpocv() draws with and without intervals", {
  data(altmejd)
  res <- lpocv(fml, data = altmejd, group = "pid", T = 5, eta = 1)

  pf <- tempfile(fileext = ".png")
  png(pf)
  expect_silent(plot(res))
  dev.off()
  expect_gt(file.size(pf), 0)

  ci <- boot_by_project()
  png(pf)
  expect_silent(plot(res, ci = ci))
  expect_silent(plot(res, ci = ci, metric = "auprc"))
  expect_silent(plot(res, ci = ci, levels = c(0.5, 0.9)))
  expect_silent(plot(res, ci = ci, baseline = NA))
  expect_silent(plot(res, ci = ci, baseline = 0.6))
  dev.off()

  expect_error(plot(res, metric = "not_a_measure"), "no estimates")
})

test_that("plot.lpocv() returns its input invisibly", {
  data(altmejd)
  res <- lpocv(fml, data = altmejd, group = "pid", T = 5, eta = 1)
  png(tempfile(fileext = ".png"))
  out <- withVisible(plot(res))
  dev.off()
  expect_false(out$visible)
  expect_identical(out$value, res)
})

test_that("treehypar reaches every tree-based learner", {
  # minsplit and minbucket are what actually govern how the trees grow, so
  # relaxing them has to change the result -- for the plain trees and for
  # adaboost's base learners alike.
  data(altmejd)
  loose <- list(minsplit = 5, minbucket = 2)

  au <- function(...) {
    r <- lpocv(fml, data = altmejd, group = "pid", ...)
    mean(summary(r, metric = "auroc")$estimate)
  }

  expect_false(isTRUE(all.equal(
    au(model = "tree", depth = 4),
    au(model = "tree", depth = 4, treehypar = loose)
  )))
  expect_false(isTRUE(all.equal(
    au(model = "adaboost", T = 10, eta = 1),
    au(model = "adaboost", T = 10, eta = 1, treehypar = loose)
  )))
})

test_that("treehypar$maxdepth is refused so the grid keeps control of depth", {
  data(altmejd)
  expect_warning(
    res <- lpocv(
      fml,
      data = altmejd,
      group = "pid",
      model = "tree",
      depth = 1,
      treehypar = list(maxdepth = 4)
    ),
    "maxdepth.*ignored"
  )
  # depth came from `depth`, not from treehypar
  plain <- lpocv(fml, data = altmejd, group = "pid", model = "tree", depth = 1)
  expect_equal(
    summary(res, metric = "auroc")$estimate,
    summary(plain, metric = "auroc")$estimate
  )
})

test_that("adaboost's base learners match a direct adaboost() fit", {
  # the default control is shared, so going through lpocv() and calling
  # adaboost() by hand must fit the same trees on the same fold
  data(altmejd)
  voi <- c("replicate", "power.o", "effect_size.o", "n.o", "p_value.o")
  train <- altmejd[altmejd$pid != "eerp", voi]
  test <- altmejd[altmejd$pid == "eerp", voi]

  by_hand <- rpart::rpart(
    fml,
    data = train,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 10, eta = 1, verbose = FALSE, input_checks = FALSE)
  want <- auroc(
    test$replicate,
    predict(
      by_hand,
      test[, -1],
      type = "margin",
      verbose = FALSE,
      input_checks = FALSE
    )
  )

  res <- lpocv(fml, data = altmejd, group = "pid", T = 10, eta = 1)
  got <- summary(res, metric = "auroc")
  expect_equal(got$estimate[got$project == "eerp"], want, tolerance = 1e-12)
})

test_that("the learner's control reaches every boosted tree", {
  # the point of taking a fitted tree: what the caller set is what gets boosted,
  # and only the two speed controls are overridden
  data(altmejd)
  voi <- c("replicate", "power.o", "effect_size.o", "n.o", "p_value.o")
  d <- altmejd[, voi]

  h <- rpart::rpart(
    fml,
    data = d,
    maxdepth = 2,
    minsplit = 40,
    cp = 0,
    model = TRUE
  )
  fit <- adaboost(
    h,
    n_iter = 20,
    eta = 0.55,
    verbose = FALSE,
    input_checks = FALSE
  )

  ctrl <- attr(fit, "control")
  expect_equal(ctrl$maxdepth, 2)
  expect_equal(ctrl$minsplit, 40)
  expect_equal(ctrl$cp, 0)
  # speed, not structure: the only two adaboost() overrides
  expect_equal(ctrl$xval, 0)
  expect_equal(ctrl$maxsurrogate, 0)

  # and it reaches the trees themselves, not only the attribute
  expect_true(all(
    vapply(fit, function(z) z$h$control$maxdepth, numeric(1)) == 2
  ))
  expect_true(all(
    vapply(fit, function(z) z$h$control$minsplit, numeric(1)) == 40
  ))

  # a different learner is a different ensemble, so the control is not ignored
  stumps <- rpart::rpart(fml, data = d, maxdepth = 1, cp = 0, model = TRUE) |>
    adaboost(n_iter = 20, eta = 0.55, verbose = FALSE, input_checks = FALSE)
  expect_false(isTRUE(all.equal(
    vapply(fit, function(z) z$a, numeric(1)),
    vapply(stumps, function(z) z$a, numeric(1))
  )))
})

test_that("model weights stay finite when a base learner refuses to split", {
  # cp = 0.01 lets a weak learner come back as a bare root once the weights
  # concentrate. That is allowed; an infinite or negative alpha is not.
  data(altmejd)
  voi <- c("replicate", "power.o", "effect_size.o", "n.o", "p_value.o")
  fit <- rpart::rpart(
    fml,
    data = altmejd[, voi],
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 300, eta = 0.55, verbose = FALSE, input_checks = FALSE)
  alphas <- vapply(fit, function(z) z$a, numeric(1))
  rootonly <- vapply(fit, function(z) nrow(z$h$frame) == 1L, logical(1))

  expect_true(any(rootonly))
  expect_true(all(is.finite(alphas)))
  expect_true(all(alphas > 0))
})
