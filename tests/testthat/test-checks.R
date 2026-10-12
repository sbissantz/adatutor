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
  # it now takes the overlap state, which detect_overlap() works out
  expect_message(check_train("all", quote(train)))
  expect_no_message(check_train("none", quote(test)))
  expect_no_message(check_train(NA_character_, quote(test)))
  # a partial match is deliberately silent; see check_train()'s comment
  expect_no_message(check_train("some", quote(mixed)))
})

test_that("check_train() names a long expression on one line", {
  # deparse() splits a long call over several lines, which once printed the
  # "was also used" line once per piece
  long <- quote(train_data_with_a_long_name[
    train_data_with_a_long_name$column_with_a_long_name > 10,
    c("first_column_name", "second_column_name")
  ])
  shown <- capture_messages(check_train("all", long))
  expect_length(grep("was also used for training", shown), 1L)
})

test_that("detect_overlap() reads the data, not the name", {
  tr <- data.frame(a = 1:5, b = letters[1:5])
  expect_identical(as.character(detect_overlap(NULL, NULL, tr, tr)), "all")
  expect_identical(
    as.character(detect_overlap(NULL, NULL, tr, tr[1:2, ])),
    "all"
  )
  expect_identical(
    as.character(detect_overlap(NULL, NULL, tr, data.frame(a = 9, b = "z"))),
    "none"
  )
  expect_identical(
    as.character(detect_overlap(
      NULL,
      NULL,
      tr,
      rbind(tr[1, ], data.frame(a = 9, b = "z"))
    )),
    "some"
  )
  # with no stored frame it can only compare names, and says NA when it cannot
  expect_identical(detect_overlap(quote(d), quote(x)), NA_character_)
  expect_identical(as.character(detect_overlap(quote(d), quote(d))), "all")
})

test_that("check_prop() works", {
  expect_error(check_prop(3L))
  expect_warning(check_prop(0))
  expect_warning(check_prop(1), "greater than 0 and less than 1")
  expect_no_error(check_prop(0.8))
})
