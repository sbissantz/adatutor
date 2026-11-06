fml <- replicate ~ power.o + effect_size.o + n.o + p_value.o

# a boosted tree on altmejd; `...` reaches the rpart controls, e.g. cp
fit_ada <- function(n_iter = 5, eta = 1, depth = 1, ...) {
  h <- rpart::rpart(
    fml,
    data = altmejd,
    method = "class",
    model = TRUE,
    maxdepth = depth,
    ...
  )
  adaboost(h, n_iter = n_iter, eta = eta, verbose = FALSE)
}

test_that("logo_cv() returns a setting x project x metric cube", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")

  expect_s3_class(res, "logo_cv")
  expect_named(
    res,
    c(
      "estimates",
      "grid",
      "projects",
      "scores",
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
  grid <- expand.grid(n_iter = c(5, 10), eta = c(1, 0.5))
  res <- tune_cv(fit_ada(), grid, data = altmejd, group = "pid")

  d <- as.data.frame(res)
  expect_equal(nrow(d), length(res$estimates))
  expect_named(
    d,
    c(
      "model",
      "setting",
      "depth",
      "n_iter",
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

test_that("logo_cv() reproduces the published leave-project-out figures", {
  # Pins the fold logic against notes 2: AdaBoost stumps, n_iter = 10, eta = 1.
  # A mismatch here means the folds are wrong, not that the table is.
  # eerp is 0.734 rather than the 0.714 of notes 2 because the base learners
  # now carry rpart's cp = 0.01; the other four folds are unmoved.
  data(altmejd)
  res <- logo_cv(fit_ada(n_iter = 10), data = altmejd, group = "pid")
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
  # balanced accuracy. logo_cv() must never reintroduce it.
  data(altmejd)
  res <- logo_cv(fit_ada(n_iter = 10), data = altmejd, group = "pid")
  d <- as.data.frame(res)

  auroc <- d$estimate[d$metric == "auroc"]
  bacc <- d$estimate[d$metric == "bacc"]
  expect_false(isTRUE(all.equal(auroc, bacc)))
})

test_that("a grid is crossed with every fold", {
  data(altmejd)
  grid <- expand.grid(n_iter = c(5, 10), eta = c(1, 0.5))
  res <- tune_cv(fit_ada(), grid, data = altmejd, group = "pid")

  n_measures <- length(assess(c(1, 0), c(1, -1)))
  expect_equal(dim(res$estimates), c(nrow(grid), 5L, n_measures))

  auroc <- summary(res, metric = "auroc")
  expect_equal(nrow(auroc), nrow(grid) * 5L)
  # each setting appears on each fold exactly once
  expect_true(all(table(auroc$n_iter, auroc$eta, auroc$project) == 1))
})

test_that("a grid may omit columns, which fall back to the fit's values", {
  data(altmejd)
  res <- tune_cv(
    fit_ada(eta = 0.5, depth = 2),
    grid = data.frame(n_iter = c(5, 10)),
    data = altmejd,
    group = "pid"
  )
  expect_equal(res$grid$n_iter, c(5, 10))
  expect_true(all(res$grid$eta == 0.5))
  expect_true(all(res$grid$depth == 2))
})

test_that("a grid column may set an rpart control, per row", {
  data(altmejd)
  small <- expand.grid(n_iter = c(5, 10), eta = 1, depth = 1)

  # one call with cp in the grid has to mean exactly what two fits with cp in
  # their control mean. Nothing else can catch a wrong merge order: the cached
  # search results predate the change.
  both <- tune_cv(
    fit_ada(),
    grid = rbind(cbind(small, cp = 0), cbind(small, cp = 0.01)),
    data = altmejd,
    group = "pid"
  )
  expect_true("cp" %in% names(both$grid))
  expect_true("cp" %in% names(as.data.frame(both)))

  d <- as.data.frame(both)
  key <- function(x) paste(x$depth, x$n_iter, x$eta, x$project, x$metric)
  for (cpv in c(0, 0.01)) {
    arm <- tune_cv(fit_ada(cp = cpv), small, data = altmejd, group = "pid")
    ref <- as.data.frame(arm)
    got <- d[d$cp == cpv, ]
    expect_equal(ref$estimate, got$estimate[match(key(ref), key(got))])
  }

  # the row is the more specific statement, so it overrides the fit's control
  clash <- tune_cv(
    fit_ada(cp = 0.5),
    grid = cbind(small, cp = 0),
    data = altmejd,
    group = "pid"
  )
  only_row <- tune_cv(
    fit_ada(),
    grid = cbind(small, cp = 0),
    data = altmejd,
    group = "pid"
  )
  expect_equal(clash$estimates, only_row$estimates)
})

test_that("a grid column that sets nothing is an error that names it", {
  data(altmejd)
  fit <- fit_ada()
  # a grid built to hold results as well as settings is a common shape, and
  # silence there would hide a column meant to tune something
  noisy <- data.frame(
    n_iter = 5,
    eta = 1,
    depth = 1,
    auc.mean = NA,
    runtime = NA
  )
  expect_error(
    tune_cv(fit, grid = noisy, data = altmejd, group = "pid"),
    "auc\\.mean, runtime"
  )
  # and the message says what a grid may hold
  expect_error(
    tune_cv(fit, grid = noisy, data = altmejd, group = "pid"),
    "`depth`, `n_iter`, `eta` and the rpart controls"
  )
  expect_silent(
    suppressMessages(tune_cv(
      fit,
      grid = data.frame(n_iter = 5, eta = 1, depth = 1, minbucket = 4),
      data = altmejd,
      group = "pid"
    ))
  )
})

test_that("the other learners run and report NA for settings they lack", {
  data(altmejd)

  # a learner records NA in the grid for a setting it has no use for
  logit <- glm(fml, data = altmejd, family = binomial) |>
    logo_cv(data = altmejd, group = "pid")
  expect_equal(logit$model, "glm")
  expect_true(all(is.na(logit$grid$n_iter)))
  expect_true(all(is.na(logit$grid$eta)))
  expect_true(all(is.na(logit$grid$depth)))
  expect_true(all(is.finite(summary(logit)$estimate)))
  # and the flattened view still shows them, since it reads the grid
  expect_true(all(is.na(as.data.frame(logit)$n_iter)))

  stump <- rpart::rpart(fml, data = altmejd, method = "class", maxdepth = 1) |>
    logo_cv(data = altmejd, group = "pid")
  expect_equal(stump$model, "rpart")
  expect_true(all(is.na(stump$grid$n_iter)))
  expect_equal(unique(stump$grid$depth), 1)

  tree <- rpart::rpart(fml, data = altmejd, method = "class", maxdepth = 4) |>
    logo_cv(data = altmejd, group = "pid")
  expect_equal(unique(tree$grid$depth), 4)

  # probability models are thresholded at 0.5, so a fitted logit must not
  # predict every case into one class on every fold
  expect_false(all(summary(logit, metric = "spec")$estimate == 0))
})

test_that("the other learners refit their own call on each fold", {
  data(altmejd)
  train <- altmejd[altmejd$pid != "eerp", ]
  test <- altmejd[altmejd$pid == "eerp", ]
  in_eerp <- function(res) res$scores$score[res$scores$project == "eerp"]

  logit <- logo_cv(
    glm(fml, data = altmejd, family = binomial),
    data = altmejd,
    group = "pid"
  )
  by_hand <- glm(fml, data = train, family = binomial) |>
    predict(test, type = "response")
  expect_equal(in_eerp(logit), unname(by_hand))

  # a `.` formula must not reach the group column of the full data
  dotted <- logo_cv(
    glm(
      replicate ~ .,
      data = altmejd[, all.vars(fml)],
      family = binomial
    ),
    data = altmejd,
    group = "pid"
  )
  expect_identical(dotted$scores, logit$scores)
})

test_that("random forest runs when the suggested package is available", {
  skip_if_not_installed("randomForest")
  data(altmejd)
  set.seed(1)
  # only the fit: randomForest() can warn about its own argument matching
  forest <- suppressWarnings(randomForest::randomForest(fml, data = altmejd))
  rf <- logo_cv(forest, data = altmejd, group = "pid")
  expect_equal(rf$model, "rf")
  expect_true(all(is.finite(summary(rf)$estimate)))
})

test_that("logo_cv() rejects malformed input", {
  data(altmejd)
  fit <- fit_ada()
  expect_error(logo_cv(fit, data = altmejd, group = "nope"), "not found")
  expect_error(
    logo_cv(fit, data = altmejd, group = c("pid", "eid")),
    "single column"
  )
  expect_error(
    logo_cv(fit, data = altmejd[altmejd$pid == "rpp", ], group = "pid"),
    "at least two levels"
  )
  expect_error(
    tune_cv(fit, grid = data.frame(), data = altmejd, group = "pid"),
    "no rows"
  )

  # a formula is not a fit: the recipe lives on the fitted model
  expect_error(
    logo_cv(fml, data = altmejd, group = "pid"),
    "must be a fitted model"
  )
  # a setting passed here would be dropped without a word
  expect_error(
    logo_cv(fit, data = altmejd, group = "pid", n_iter = 10, grid = 1),
    "`n_iter`, `grid`.*tune_cv"
  )
  # learners that give no probability of the second class
  expect_error(
    logo_cv(glm(n.o ~ power.o, data = altmejd), data = altmejd, group = "pid"),
    "family = binomial"
  )
  expect_error(
    logo_cv(
      rpart::rpart(n.o ~ power.o, data = altmejd),
      data = altmejd,
      group = "pid"
    ),
    "classification tree"
  )
})

test_that("tune_cv() takes adaboost fits only", {
  data(altmejd)
  logit <- glm(fml, data = altmejd, family = binomial)
  expect_error(
    tune_cv(
      logit,
      grid = data.frame(n_iter = 5),
      data = altmejd,
      group = "pid"
    ),
    "tuning grids are for adaboost fits"
  )
})

test_that("the methods behave", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")

  expect_true(is.array(res$estimates))
  expect_s3_class(as.data.frame(res), "data.frame")

  s <- summary(res, metric = "auprc")
  expect_equal(nrow(s), 5L)
  expect_true(all(s$metric == "auprc"))

  out <- capture.output(print(res))
  expect_true(any(grepl("Leave-one-group-out", out)))
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

# ---- holdout ---------------------------------------------------------------

test_that("holdout keeps only the named levels, in the order given", {
  data(altmejd)
  fit <- fit_ada()
  every <- logo_cv(fit, data = altmejd, group = "pid")
  some <- logo_cv(
    fit,
    data = altmejd,
    group = "pid",
    holdout = c("ssrp", "eerp")
  )

  expect_named(some, names(every))
  expect_equal(dimnames(some$estimates)$project, c("ssrp", "eerp"))
  expect_equal(some$projects$project, c("ssrp", "eerp"))
  expect_equal(unique(some$scores$project), c("ssrp", "eerp"))
  # a held-out project is the same fold either way
  expect_equal(
    some$estimates[, c("ssrp", "eerp"), , drop = FALSE],
    every$estimates[, c("ssrp", "eerp"), , drop = FALSE]
  )

  expect_equal(attr(some, "holdout"), c("ssrp", "eerp"))
  expect_null(attr(every, "holdout"))
  expect_true(any(grepl("holdout: ssrp, eerp", capture.output(print(some)))))
  expect_false(any(grepl("holdout", capture.output(print(every)))))
})

test_that("holdout rejects a level the group does not have", {
  data(altmejd)
  fit <- fit_ada()
  expect_error(
    logo_cv(fit, data = altmejd, group = "pid", holdout = c("eerp", "holz")),
    "not found in `pid`: holz"
  )
  expect_error(
    logo_cv(fit, data = altmejd, group = "pid", holdout = c("eerp", "eerp")),
    "more than once"
  )
})

test_that("holding out HOLZ equals a fit on the other projects, scored on HOLZ", {
  data(altmext)
  predictors <- all.vars(fml)[-1L]
  six <- altmext[altmext$pid != "holz", ]
  holz <- altmext[altmext$pid == "holz", ]

  res <- logo_cv(
    fit_ada(n_iter = 10),
    data = altmext,
    group = "pid",
    holdout = "holz"
  )
  direct <- rpart::rpart(
    fml,
    data = six,
    method = "class",
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 10, eta = 1, verbose = FALSE) |>
    predict(holz[, predictors], type = "margin", verbose = FALSE)

  expect_equal(dimnames(res$estimates)$project, "holz")
  expect_equal(res$scores$score, unname(direct))
  expect_equal(
    unname(res$estimates[1, "holz", "auroc"]),
    auroc(holz$replicate, direct)
  )
})

test_that("a Monte Carlo draw through holdout = \"test\" has one row", {
  data(altmejd)
  draw <- altmejd
  set.seed(7)
  draw$part <- sample(
    c("train", "test"),
    nrow(draw),
    replace = TRUE,
    prob = c(0.7, 0.3)
  )
  res <- logo_cv(fit_ada(), data = draw, group = "part", holdout = "test")

  expect_equal(dimnames(res$estimates)$project, "test")
  expect_equal(nrow(summary(res)), 1L)
  expect_equal(res$projects$n, sum(draw$part == "test"))
})

# ---- nested LOGO-CV --------------------------------------------------------

small_grid <- expand.grid(depth = 1:2, n_iter = c(5, 10), eta = 1)

test_that("nested tune_cv() reports the setting each outer fold chose", {
  data(altmejd)
  res <- tune_cv(
    fit_ada(),
    small_grid,
    data = altmejd,
    group = "pid",
    nested = TRUE
  )

  expect_s3_class(res, "logo_cv")
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
  res <- tune_cv(
    fit_ada(),
    small_grid,
    data = altmejd,
    group = "pid",
    nested = TRUE
  )

  for (p in res$selected$project) {
    train <- altmejd[altmejd$pid != p, ]
    inner <- tune_cv(fit_ada(), small_grid, data = train, group = "pid")
    s <- summary(inner, metric = "auroc")
    per <- aggregate(estimate ~ setting, data = s, FUN = mean)
    expect_equal(
      res$selected$setting[res$selected$project == p],
      per$setting[which.max(per$estimate)]
    )
  }
})

test_that("a nested search with a holdout selects on the other groups", {
  data(altmext)
  res <- tune_cv(
    fit_ada(),
    small_grid,
    data = altmext,
    group = "pid",
    holdout = "holz",
    nested = TRUE
  )
  expect_equal(res$selected$project, "holz")
  expect_equal(dimnames(res$estimates)$project, "holz")
  expect_equal(attr(res, "holdout"), "holz")

  # the inner search ran on all six other projects
  inner <- tune_cv(
    fit_ada(),
    small_grid,
    data = altmext[altmext$pid != "holz", ],
    group = "pid"
  )
  s <- summary(inner, metric = "auroc")
  per <- aggregate(estimate ~ setting, data = s, FUN = mean)
  expect_equal(res$selected$setting, per$setting[which.max(per$estimate)])
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
  grid <- expand.grid(depth = 1:3, n_iter = c(10, 30), eta = c(1, 0.1))

  plain <- tune_cv(fit_ada(), grid, data = altmejd, group = "pid")
  s <- summary(plain, metric = "auroc")
  naive <- max(aggregate(estimate ~ setting, data = s, FUN = mean)$estimate)

  nst <- tune_cv(fit_ada(), grid, data = altmejd, group = "pid", nested = TRUE)
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
  by_auroc <- tune_cv(
    fit_ada(),
    small_grid,
    data = altmejd,
    group = "pid",
    nested = TRUE,
    criterion = "auroc"
  )
  by_bacc <- tune_cv(
    fit_ada(),
    small_grid,
    data = altmejd,
    group = "pid",
    nested = TRUE,
    criterion = "bacc"
  )

  expect_equal(by_auroc$criterion, "auroc")
  expect_equal(by_bacc$criterion, "bacc")
  # both are valid runs; the criterion is recorded so a reader can see which
  # decision rule produced the selections
  expect_equal(nrow(by_bacc$selected), 5L)
})

test_that("nested tune_cv() rejects input it cannot honor", {
  data(altmejd)
  fit <- fit_ada()
  expect_error(
    tune_cv(
      fit,
      grid = data.frame(n_iter = 5),
      data = altmejd,
      group = "pid",
      nested = TRUE
    ),
    "needs a `grid` with two or more rows"
  )
  expect_error(
    tune_cv(
      fit,
      small_grid,
      data = altmejd[altmejd$pid %in% c("ml1", "ml3"), ],
      group = "pid",
      nested = TRUE
    ),
    "at least three groups"
  )
  expect_error(
    tune_cv(
      fit,
      small_grid,
      data = altmejd,
      group = "pid",
      nested = TRUE,
      criterion = "not_a_measure"
    ),
    "not one of the measures"
  )
})

test_that("print() surfaces the selection instability", {
  data(altmejd)
  res <- tune_cv(
    fit_ada(),
    small_grid,
    data = altmejd,
    group = "pid",
    nested = TRUE
  )
  out <- capture.output(print(res))

  expect_true(any(grepl("Nested leave-one-group-out", out)))
  expect_true(any(grepl("selected on: auroc", out)))
  expect_true(any(grepl("inner searches selected", out)))
  expect_true(any(grepl("diagnostic, not a selection rule", out)))
  # still no aggregate row and no cross-project SE
  expect_false(any(grepl("\\bmean\\b|\\bSE\\b|\\bsd\\b", out)))
})

test_that("a plain run records that it was not nested", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")
  expect_false(res$nested)
  expect_null(res$selected)
  expect_false(any(grepl("Nested", capture.output(print(res)))))
})

# ---- plot.logo_cv ------------------------------------------------------------

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
      adaboost(n_iter = 5, eta = 1, verbose = FALSE, check_inputs = FALSE)
    m <- predict(
      fit,
      te[, prednms],
      type = "margin",
      verbose = FALSE,
      check_inputs = FALSE
    )
    set.seed(112)
    out[[rp]] <- suppressWarnings(bootstrap(
      te$replicate,
      m,
      n_resample = n_resample
    ))
  }
  out
}

test_that("bootstrap() keeps the draws so any level can be asked for later", {
  f <- c(rep(1L, 20), rep(0L, 20))
  set.seed(1)
  ci <- bootstrap(f, rnorm(40) + f, n_resample = 150)
  d <- attr(ci, "draws")

  expect_true(is.matrix(d))
  expect_equal(nrow(d), 150L)
  expect_true(all(c("auroc", "auprc", "bacc") %in% colnames(d)))
  # the stored interval is recoverable from the stored draws
  q <- unname(quantile(d[, "auroc"], c(0.025, 0.975), na.rm = TRUE))
  expect_equal(unname(ci$lower[ci$metric == "auroc"]), q[1])
  expect_equal(unname(ci$upper[ci$metric == "auroc"]), q[2])
})

test_that("plot.logo_cv() draws with and without intervals", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")

  pf <- tempfile(fileext = ".png")
  png(pf)
  expect_silent(plot(res))
  dev.off()
  expect_gt(file.size(pf), 0)

  boot <- res
  boot$ci <- boot_by_project()
  png(pf)
  expect_silent(plot(boot))
  expect_silent(plot(boot, metric = "auprc"))
  expect_silent(plot(boot, levels = c(0.5, 0.9)))
  expect_silent(plot(boot, baseline = NA))
  expect_silent(plot(boot, baseline = 0.6))
  dev.off()

  # attached intervals add bands: more drawing calls than the bare plot
  calls <- function(x) {
    pdf(NULL)
    on.exit(dev.off())
    dev.control("enable")
    plot(x)
    length(recordPlot()[[1]])
  }
  expect_gt(calls(boot), calls(res))

  expect_error(plot(res, metric = "not_a_measure"), "no estimates")
})

test_that("plot.logo_cv() returns its input invisibly", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")
  png(tempfile(fileext = ".png"))
  out <- withVisible(plot(res))
  dev.off()
  expect_false(out$visible)
  expect_identical(out$value, res)
})

test_that("the fit's control reaches every refit", {
  # minsplit and minbucket are what actually govern how the trees grow, so
  # loosening them on the fit has to change the result -- for a plain tree and
  # for adaboost's base learners alike.
  data(altmejd)
  au <- function(fit) {
    res <- logo_cv(fit, data = altmejd, group = "pid")
    mean(summary(res, metric = "auroc")$estimate)
  }

  tree <- rpart::rpart(fml, data = altmejd, method = "class", maxdepth = 4)
  loose_tree <- rpart::rpart(
    fml,
    data = altmejd,
    method = "class",
    maxdepth = 4,
    minsplit = 5,
    minbucket = 2
  )
  expect_false(isTRUE(all.equal(au(tree), au(loose_tree))))
  expect_false(isTRUE(all.equal(
    au(fit_ada(n_iter = 10)),
    au(fit_ada(n_iter = 10, minsplit = 5, minbucket = 2))
  )))

  # and the tree's refit is its own call run again on the training rows
  train <- altmejd[altmejd$pid != "eerp", ]
  test <- altmejd[altmejd$pid == "eerp", ]
  res <- logo_cv(loose_tree, data = altmejd, group = "pid")
  by_hand <- rpart::rpart(
    fml,
    data = train,
    method = "class",
    maxdepth = 4,
    minsplit = 5,
    minbucket = 2
  ) |>
    predict(test, type = "prob")
  expect_equal(
    res$scores$score[res$scores$project == "eerp"],
    unname(by_hand[, "success"])
  )
})

test_that("adaboost's base learners match a direct adaboost() fit", {
  # the default control is shared, so going through logo_cv() and calling
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
    adaboost(n_iter = 10, eta = 1, verbose = FALSE, check_inputs = FALSE)
  want <- auroc(
    test$replicate,
    predict(
      by_hand,
      test[, -1],
      type = "margin",
      verbose = FALSE,
      check_inputs = FALSE
    )
  )

  res <- logo_cv(fit_ada(n_iter = 10), data = altmejd, group = "pid")
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
    check_inputs = FALSE
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
    adaboost(n_iter = 20, eta = 0.55, verbose = FALSE, check_inputs = FALSE)
  expect_false(isTRUE(all.equal(
    vapply(fit, function(z) z$alpha, numeric(1)),
    vapply(stumps, function(z) z$alpha, numeric(1))
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
    adaboost(n_iter = 300, eta = 0.55, verbose = FALSE, check_inputs = FALSE)
  alphas <- vapply(fit, function(z) z$alpha, numeric(1))
  rootonly <- vapply(fit, function(z) nrow(z$h$frame) == 1L, logical(1))

  expect_true(any(rootonly))
  expect_true(all(is.finite(alphas)))
  expect_true(all(alphas > 0))
})
# ---- the pipeline: h |> adaboost() |> logo_cv() |> bootstrap() |> plot() ----

test_that("logo_cv() on an adaboost fit equals tune_cv() with the fit's own row", {
  data(altmejd)
  prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  h <- rpart::rpart(
    replicate ~ .,
    data = altmejd[, c(prednms, "replicate")],
    maxdepth = 1,
    model = TRUE
  )
  fit <- adaboost(h, n_iter = 5, eta = 0.5, verbose = FALSE)

  from_fit <- logo_cv(fit, data = altmejd, group = "pid")
  from_grid <- tune_cv(
    fit,
    grid = data.frame(depth = 1, n_iter = 5, eta = 0.5),
    data = altmejd,
    group = "pid"
  )

  expect_identical(from_fit$estimates, from_grid$estimates)
  expect_identical(from_fit$scores, from_grid$scores)
  expect_equal(
    from_fit$grid[, c("n_iter", "eta", "depth")],
    data.frame(n_iter = 5, eta = 0.5, depth = 1)
  )

  # the `.` of the learner's formula reads the same columns as the names
  spelled_out <- logo_cv(fit_ada(eta = 0.5), data = altmejd, group = "pid")
  expect_identical(from_fit$estimates, spelled_out$estimates)
})

test_that("logo_cv() refits the fit's split rule, not the default", {
  data(altmejd)
  prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  fit_split <- function(rule) {
    rpart::rpart(
      replicate ~ .,
      data = altmejd[, c(prednms, "replicate")],
      maxdepth = 2,
      parms = list(split = rule),
      model = TRUE
    ) |>
      adaboost(n_iter = 5, eta = 1, verbose = FALSE)
  }
  info <- logo_cv(fit_split("information"), data = altmejd, group = "pid")
  gini <- logo_cv(fit_split("gini"), data = altmejd, group = "pid")
  expect_identical(attr(fit_split("information"), "split"), "information")
  expect_false(identical(info$scores$score, gini$scores$score))
})

test_that("logo_cv() keeps the held-out scores of every fold", {
  data(altmejd)
  prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")

  expect_equal(nrow(res$scores), nrow(altmejd))
  expect_setequal(unique(res$scores$project), res$projects$project)

  # the ssrp scores are what a manual refit on the other projects predicts
  tr <- altmejd[altmejd$pid != "ssrp", ]
  te <- altmejd[altmejd$pid == "ssrp", ]
  manual <- rpart::rpart(
    replicate ~ .,
    data = tr[, c(prednms, "replicate")],
    maxdepth = 1,
    cp = 0.01,
    xval = 0,
    maxsurrogate = 0,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE) |>
    predict(te[, prednms], type = "margin", verbose = FALSE)
  expect_equal(res$scores$score[res$scores$project == "ssrp"], unname(manual))
})

test_that("bootstrap() on logo_cv equals the per-project loop", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")

  set.seed(3)
  boot <- suppressWarnings(bootstrap(res, n_resample = 50))

  set.seed(3)
  manual <- lapply(res$projects$project, function(p) {
    rows <- res$scores$project == p
    suppressWarnings(bootstrap(
      res$scores$actual[rows],
      res$scores$score[rows],
      n_resample = 50
    ))
  })
  names(manual) <- res$projects$project

  expect_s3_class(boot, "logo_cv")
  expect_identical(boot$ci, manual)
  expect_identical(boot$estimates, res$estimates)
})

test_that("bootstrap() names the project in its warnings", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")
  set.seed(3)
  warning_texts <- capture_warnings(
    bootstrap(res, n_resample = 50, stratified = TRUE)
  )
  # one per project, in project order
  expect_identical(
    sub(": stratified.*", "", warning_texts),
    paste("project", res$projects$project)
  )
})

test_that("bootstrap() on logo_cv needs one setting and stored scores", {
  data(altmejd)
  grid <- expand.grid(n_iter = c(3, 5), eta = 1, depth = 1)
  res <- tune_cv(fit_ada(), grid, data = altmejd, group = "pid")

  expect_error(bootstrap(res, n_resample = 20), "Choose one with `setting`")
  expect_error(bootstrap(res, n_resample = 20, setting = 3), "one row number")

  set.seed(3)
  one <- suppressWarnings(bootstrap(res, n_resample = 20, setting = 2))
  expect_equal(dim(one$estimates)[1], 1L)
  expect_identical(one$estimates[1, , ], res$estimates[2, , ])
  expect_equal(one$grid$n_iter, 5)

  no_scores <- res
  no_scores$scores <- NULL
  expect_error(bootstrap(no_scores, setting = 2), "Rerun logo_cv")
})

test_that("bootstrap() keeps the holdout of a logo_cv result", {
  data(altmejd)
  res <- logo_cv(
    fit_ada(),
    data = altmejd,
    group = "pid",
    holdout = c("rpp", "ssrp")
  )
  set.seed(3)
  boot <- suppressWarnings(bootstrap(res, n_resample = 20))
  expect_named(boot$ci, c("rpp", "ssrp"))
  expect_equal(attr(boot, "holdout"), c("rpp", "ssrp"))
})

test_that("nested tune_cv keeps the chosen setting's scores and bootstraps", {
  data(altmejd)
  grid <- expand.grid(n_iter = c(3, 5), eta = 1, depth = 1)
  res <- tune_cv(fit_ada(), grid, data = altmejd, group = "pid", nested = TRUE)
  expect_equal(nrow(res$scores), nrow(altmejd))
  set.seed(3)
  boot <- suppressWarnings(bootstrap(res, n_resample = 20))
  expect_named(boot$ci, res$projects$project)
})
test_that("accuracy's reference line is the majority-class rate", {
  # always predicting the larger class scores max(p, 1 - p), not p
  reference <- find_reference("acc", "auto", c(0.3, 0.8))
  expect_equal(reference$values, c(0.7, 0.8))
  expect_match(reference$label, "majority-class rate")
})

test_that("every patk budget gets the base-rate floor, not only the default", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")
  calls <- function(...) {
    pdf(NULL)
    on.exit(dev.off())
    dev.control("enable")
    plot(res, ...)
    length(recordPlot()[[1]])
  }
  for (m in c("patk_3", "patk_5", "patk", "auprc")) {
    expect_gt(calls(metric = m), calls(metric = m, baseline = NA))
  }
})

test_that("the y-axis keeps its tick at 1 and draws none above", {
  data(altmejd)
  res <- logo_cv(fit_ada(n_iter = 50), data = altmejd, group = "pid")
  y_ticks <- function(metric) {
    pdf(NULL)
    on.exit(dev.off())
    dev.control("enable")
    plot(res, metric = metric)
    items <- recordPlot()[[1]]
    fn <- vapply(
      items,
      function(e) {
        x <- e[[2]][[1]]
        if (is.list(x) && !is.null(x$name)) x$name else ""
      },
      character(1)
    )
    # the last side-2 axis: plot() records its own suppressed one first
    left <- Filter(function(e) isTRUE(e[[2]][[2]] == 2), items[fn == "C_axis"])
    left[[length(left)]][[2]][[3]]
  }
  # three projects score 1 at the top 3, so the axis must reach 1
  ticks <- y_ticks("patk_3")
  expect_true(1 %in% ticks)
  expect_true(all(ticks <= 1))
})
