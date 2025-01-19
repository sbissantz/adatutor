test_that("check_df() works", {
  expect_error(check_df(list()))
  expect_no_error(check_df(data.frame()))
})

test_that("check_eta() works", {
  expect_message(check_eta(2))
  expect_error(check_eta("abc"))
  expect_no_error(check_eta(0.8))
})

test_that("check_length() works", {
  expect_error(check_length(list()))
  expect_no_error(check_length(NA))
})

test_that("check_list() works", {
  expect_error(check_list(numeric()))
  expect_no_error(check_list(list()))
})

test_that("check_numeric() works", {
  expect_error(check_numeric(list()))
  expect_no_error(check_numeric(numeric()))
})

test_that("check_train() works", {
  expect_message(check_train("train", "train"))
})

test_that("check_prop() works", {
  expect_error(check_prop(3L))
  expect_warning(check_prop(0))
  expect_warning(check_prop(1))
  expect_no_error(check_prop(0.8))
})
