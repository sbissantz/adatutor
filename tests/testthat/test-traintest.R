test_that("trainAda() and testAda() work", {
  fit <- trainAda(Species ~ ., iris, T = 10, eta = 1,
                  treehypar = rpart::rpart.control(maxdepth = 1, cp = 0),
                  verbose = FALSE, input_checks = FALSE)
  expect_length(fit, 10)
  expect_equal(round(fit$t1$a, 1), 0.3)
  expect_equal(round(fit$t10$a, 1), 0.5)
  expect_match(class(fit$t1$h), "rpart")
  expect_equal(attr(fit, "train"), as.name("iris"))
  ypred <- testAda(fit, iris, input_checks = FALSE, verbose = FALSE)
  expect_equal(ypred[1:6], rep(-1,6))
  fit <- trainAda(Species ~ ., iris, T = 10, eta = 1,
                  treehypar = rpart::rpart.control(maxdepth = 1, cp = 0),
                  verbose = TRUE, input_checks = TRUE)
  expect_length(fit, 10)
  expect_equal(round(fit$t1$a, 1), 0.3)
  expect_equal(round(fit$t10$a, 1), 0.5)
  expect_match(class(fit$t1$h), "rpart")
  expect_equal(attr(fit, "train"), as.name("iris"))
  ypred <- testAda(fit, iris, input_checks = FALSE, verbose = FALSE)
  expect_equal(ypred[1:6], rep(-1,6))
  ypred <- testAda(fit, iris, input_checks = TRUE, verbose = TRUE)
  expect_equal(ypred[1:6], rep(-1,6))
})
