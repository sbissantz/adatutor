test_that("walking_dots() works", {
  expect_message(walking_dots())
})

test_that("color_message() works", {
  expect_equal(color_message("abc", 32), cat("abc"))
})

test_that("walking_dots() works", {
  expect_equal(walking_colordots(), cat("... Done"))
})
