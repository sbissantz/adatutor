test_that("nested is a nested logo_cv result over the seven projects", {
  data(nested)
  data(altmext)
  projects <- unique(as.character(altmext$pid))
  expect_s3_class(nested, "logo_cv")
  expect_named(
    nested,
    c(
      "estimates",
      "grid",
      "projects",
      "scores",
      "model",
      "nested",
      "metric",
      "sort_by",
      "selected"
    )
  )
  expect_true(nested$nested)
  expect_identical(nested$model, "adaboost")
  expect_identical(nested$metric, "auroc")
  expect_identical(nested$sort_by, "lower")
  # every project was an outer fold, so there is no holdout
  expect_null(attr(nested, "holdout"))

  expect_length(projects, 7)
  expect_true("holz" %in% projects)
  expect_identical(
    dimnames(nested$estimates),
    list(project = projects, metric = list_metrics())
  )
  expect_identical(nested$projects$project, projects)
  expect_equal(nested$projects$n, as.vector(table(altmext$pid)[projects]))
  expect_equal(
    nested$projects$base_rate,
    as.vector(tapply(altmext$replicate == "success", altmext$pid, mean)[
      projects
    ])
  )
  expect_identical(nested$selected$project, projects)
  expect_named(
    nested$selected,
    c("project", "setting", "depth", "n_iter", "eta", "inner_value")
  )
})

test_that("nested searched hypergrid's 1,320 settings in its row order", {
  data(nested)
  data(hypergrid)
  expect_equal(nrow(nested$grid), 1320)
  expect_named(nested$grid, c("n_iter", "eta", "depth"))
  for (column in c("depth", "n_iter", "eta")) {
    expect_identical(
      nested$grid[[column]],
      hypergrid$auroc[[column]],
      label = column
    )
  }
})

test_that("nested's HOLZ fold repeats the final test of the benchmark", {
  data(nested)
  data(hypergrid)
  data(benchmark)
  # hypergrid's pick on the six tuning projects, row 438
  best <- summary(hypergrid)[1, ]
  expect_identical(rownames(best), "438")

  holz <- nested$selected[nested$selected$project == "holz", ]
  expect_equal(nrow(holz), 1)
  expect_equal(holz$setting, 438)
  expect_equal(holz$depth, 2)
  expect_equal(holz$n_iter, 2000)
  expect_equal(holz$eta, 0.02)
  expect_equal(holz$inner_value, best$lower)
  expect_equal(
    nested$estimates["holz", "auroc"],
    benchmark$auroc$holz[benchmark$auroc$model == "adaboost"]
  )
  # the same on every metric
  for (metric in names(benchmark)) {
    table <- benchmark[[metric]]
    expect_equal(
      nested$estimates["holz", metric],
      table$holz[table$model == "adaboost"],
      label = metric
    )
  }
})

test_that("every fold's pick is a row of the grid", {
  data(nested)
  selected <- nested$selected
  expect_true(all(selected$setting %in% seq_len(nrow(nested$grid))))
  for (column in c("depth", "n_iter", "eta")) {
    expect_identical(
      selected[[column]],
      nested$grid[[column]][selected$setting],
      label = column
    )
  }
})

test_that("nested's AUROC estimates are complete and match its scores", {
  data(nested)
  auc <- nested$estimates[, "auroc"]
  expect_false(anyNA(auc))
  expect_true(all(auc >= 0 & auc <= 1))
  # the held-out margins give the estimates, so bootstrap() needs no refit
  scores <- nested$scores
  expect_identical(unique(scores$project), names(auc))
  rescored <- vapply(
    names(auc),
    function(project) {
      rows <- scores$project == project
      auroc(scores$actual[rows], scores$score[rows])
    },
    numeric(1)
  )
  expect_equal(rescored, auc)
})

test_that("summary() and print() run on nested", {
  data(nested)
  auc <- nested$estimates[, "auroc"]
  top <- summary(nested)
  expect_named(top, c("mean", "sd", "lower", "min", "max"))
  expect_equal(nrow(top), 1)
  expect_equal(top$mean, mean(auc))
  expect_equal(top$sd, sd(auc))
  expect_equal(top$lower, mean(auc) - sd(auc))

  expect_output(print(nested), "Nested leave-one-group-out cross-validation")
  expect_output(print(nested), "settings  1320", fixed = TRUE)
  expect_output(print(nested), "selected  lower auroc", fixed = TRUE)
  expect_output(print(nested), "The 7 inner searches selected", fixed = TRUE)
})
