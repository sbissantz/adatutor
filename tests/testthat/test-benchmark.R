test_that("benchmark holds one table per measure, a row per model", {
  data(benchmark)
  projects <- c("eerp", "ml1", "ml2", "ml3", "rpp", "ssrp", "holz")
  models <- c("logit", "tree", "rf", "adaboost", "xgboost")
  expect_named(benchmark, list_measures())
  for (table in benchmark) {
    expect_named(table, c("model", projects))
    expect_identical(table$model, models)
  }
  expect_identical(attr(benchmark, "projects")$project, projects)
})

test_that("benchmark's settings hold one row per model and hyperparameter", {
  data(benchmark)
  settings <- attr(benchmark, "settings")
  models <- c("tree", "rf", "adaboost", "xgboost")
  expect_named(settings, c("model", "hyperparameter", "value"))
  expect_type(settings$value, "character")
  expect_identical(unique(settings$model), models)
  expect_identical(
    split(settings$hyperparameter, factor(settings$model, models)),
    list(
      tree = "depth",
      rf = c("ntree", "mtry"),
      adaboost = c("depth", "n_iter", "eta"),
      xgboost = c("depth", "n_iter", "eta", "min_child_weight", "lambda")
    )
  )
  expect_identical(
    settings$value[settings$model == "adaboost"],
    c("2", "2000", "0.02")
  )
})

test_that("benchmark's projects are those of altmext", {
  data(benchmark)
  data(altmext)
  info <- attr(benchmark, "projects")
  expect_equal(info$n, as.vector(table(altmext$pid)[info$project]))
  expect_equal(
    info$base_rate,
    as.vector(
      tapply(altmext$replicate == "success", altmext$pid, mean)[info$project]
    )
  )
})

test_that("benchmark's AUROC is complete and between 0 and 1", {
  data(benchmark)
  scores <- as.matrix(benchmark$auroc[-1])
  expect_false(anyNA(scores))
  expect_true(all(scores >= 0 & scores <= 1))
})

test_that("benchmark's AdaBoost HOLZ cell matches a direct fit", {
  skip_on_cran()
  data(benchmark)
  data(altmext)
  predictors <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  tune <- altmext[altmext$pid != "holz", ]
  holz <- altmext[altmext$pid == "holz", ]
  h <- rpart::rpart(
    replicate ~ .,
    data = tune[, c(predictors, "replicate")],
    maxdepth = 2,
    model = TRUE
  )
  fit <- adaboost(h, n_iter = 2000, eta = 0.02, verbose = FALSE)
  margin <- predict(fit, holz[, predictors], type = "margin", verbose = FALSE)
  stored <- benchmark$auroc$holz[benchmark$auroc$model == "adaboost"]
  expect_equal(stored, auroc(holz$replicate, margin))
})

test_that("benchmark's logit cells match logo_cv() on a glm", {
  data(benchmark)
  data(altmext)
  predictors <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  tune <- altmext[altmext$pid != "holz", ]
  projects <- sort(unique(tune$pid))
  fit <- glm(
    replicate ~ .,
    data = tune[, c(predictors, "replicate")],
    family = binomial
  )
  tuning <- logo_cv(fit, data = tune, group = "pid")
  final <- logo_cv(fit, data = altmext, group = "pid", holdout = "holz")
  for (measure in names(benchmark)) {
    row <- benchmark[[measure]][benchmark[[measure]]$model == "logit", ]
    expect_equal(
      unname(unlist(row[projects])),
      unname(tuning$estimates[1, projects, measure]),
      label = measure
    )
    expect_equal(row$holz, final$estimates[1, "holz", measure], label = measure)
  }
})
