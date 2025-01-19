test_that("random_split() works", {
  split <- random_split(altmejd, prop = 0.7)
  expect_length(split, 2)
  expect_equal(dim(split$set1), c(106,7))
  expect_equal(dim(split$set2), c(46,7))
})

test_that("stratified_split() works", {
  split <- stratified_split(altmejd, group = "replicate", id = "eid",
                            prop = 0.7)
  expect_length(split, 2)
  expect_equal(dim(split$set1), c(107,7))
  expect_equal(dim(split$set2), c(45,7))
})
