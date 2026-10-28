test_that("hypergrid holds one table per measure, a row per setting", {
  data(hypergrid)
  projects <- c("eerp", "ml1", "ml2", "ml3", "rpp", "ssrp")
  expect_named(hypergrid, list_measures())
  for (table in hypergrid) {
    expect_named(table, c("depth", "T", "eta", projects))
    expect_equal(nrow(table), 2640)
    expect_false(anyDuplicated(table[c("depth", "T", "eta")]) > 0)
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
  stored <- auc$ml3[auc$depth == 1 & auc$T == 10 & auc$eta == 1]
  expect_equal(stored, auroc(test$replicate, margin))
})
