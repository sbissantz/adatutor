test_that("hypergrid holds one table per measure, a row per setting", {
  data(hypergrid)
  projects <- c("eerp", "ml1", "ml2", "ml3", "rpp", "ssrp")
  expect_named(hypergrid, list_measures())
  for (table in hypergrid) {
    expect_named(
      table,
      c("depth", "n_iter", "eta", projects, "mean", "sd", "min", "max")
    )
    expect_equal(nrow(table), 2640)
    expect_false(anyDuplicated(table[c("depth", "n_iter", "eta")]) > 0)
  }
  expect_identical(hypergrid$auroc[1:3], hypergrid$patk[1:3])
})

test_that("hypergrid's projects are altmext without HOLZ", {
  data(hypergrid)
  data(altmext)
  tune <- altmext[altmext$pid != "holz", ]
  info <- attr(hypergrid, "projects")
  expect_identical(info$project, sort(unique(tune$pid)))
  expect_equal(info$n, as.vector(table(tune$pid)[info$project]))
  expect_equal(
    info$base_rate,
    as.vector(tapply(tune$replicate == "success", tune$pid, mean)[info$project])
  )
})

test_that("hypergrid's mean, sd, min and max summarize the projects", {
  data(hypergrid)
  projects <- attr(hypergrid, "projects")$project
  for (measure in names(hypergrid)) {
    table <- hypergrid[[measure]]
    scores <- as.matrix(table[projects])
    expect_equal(table$mean, apply(scores, 1, mean), label = measure)
    expect_equal(table$sd, apply(scores, 1, sd), label = measure)
    expect_equal(table$min, apply(scores, 1, min), label = measure)
    expect_equal(table$max, apply(scores, 1, max), label = measure)
  }

  # An NA project makes all four NA: ML3 in every row of patk_5, and only there
  summaries <- c("mean", "sd", "min", "max")
  expect_true(all(is.na(hypergrid$patk_5$ml3)))
  expect_true(all(is.na(hypergrid$patk_5[summaries])))
  for (measure in setdiff(names(hypergrid), "patk_5")) {
    expect_false(anyNA(hypergrid[[measure]][summaries]), label = measure)
  }
  expect_length(hypergrid, 17)
})

test_that("hypergrid's AUROC is complete and between 0 and 1", {
  data(hypergrid)
  scores <- as.matrix(hypergrid$auroc[attr(hypergrid, "projects")$project])
  expect_false(anyNA(scores))
  expect_true(all(scores >= 0 & scores <= 1))
})

test_that("a stored hypergrid score matches a direct fit", {
  data(hypergrid)
  data(altmext)
  predictors <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  tune <- altmext[altmext$pid != "holz", ]
  train <- tune[tune$pid != "ml3", c(predictors, "replicate")]
  test <- tune[tune$pid == "ml3", ]
  h <- rpart::rpart(replicate ~ ., data = train, maxdepth = 1, model = TRUE)
  fit <- adaboost(h, n_iter = 10, eta = 1, verbose = FALSE)
  margin <- predict(fit, test[, predictors], type = "margin", verbose = FALSE)
  auc <- hypergrid$auroc
  stored <- auc$ml3[auc$depth == 1 & auc$n_iter == 10 & auc$eta == 1]
  expect_equal(stored, auroc(test$replicate, margin))
})

test_that("tune_logo_cv() reproduces stored settings on every measure", {
  data(hypergrid)
  data(altmext)
  tune <- altmext[altmext$pid != "holz", ]
  projects <- attr(hypergrid, "projects")$project
  recipe <- rpart::rpart(
    replicate ~ power.o + effect_size.o + n.o + p_value.o,
    data = tune,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 10, eta = 1, verbose = FALSE)

  # three settings drawn among those up to 100 rounds, which keeps it fast
  settings <- hypergrid$auroc[c("depth", "n_iter", "eta")]
  set.seed(2026)
  rows <- sample(which(settings$n_iter <= 100), 3)
  res <- tune_logo_cv(recipe, settings[rows, ], data = tune, group = "pid")

  for (measure in names(hypergrid)) {
    expect_equal(
      unname(res$estimates[, projects, measure]),
      unname(as.matrix(hypergrid[[measure]][rows, projects])),
      label = measure
    )
  }
})
