test_that("as_binary() maps every accepted encoding onto 0/1", {
  want <- c(1L, 0L, 1L)
  expect_identical(as_binary(c(1, 0, 1)), want)
  expect_identical(as_binary(c(TRUE, FALSE, TRUE)), want)
  expect_identical(as_binary(c(1, -1, 1)), want)
  # the second level is the positive class
  expect_identical(as_binary(factor(c("b", "a", "b"))), want)
  expect_identical(
    as_binary(factor(
      c("success", "failure"),
      levels = c("failure", "success")
    )),
    c(1L, 0L)
  )
})

test_that("as_binary() refuses an encoding it cannot read", {
  expect_error(as_binary(factor(c("a", "b", "c"))), "exactly two levels")
  expect_error(as_binary(c(1, 2, 3)), "two-level factor")
})

test_that("color_message() wraps the text in the code and resets it", {
  out <- capture.output(
    color_message("abc", color_code = 32),
    type = "message"
  )
  expect_identical(out, "\033[32mabc\033[0m")
})

test_that("feedback goes to the message stream, not stdout", {
  # progress is diagnostic, so it must not land in captured results
  expect_identical(capture.output(color_message("abc")), character(0))
  expect_identical(capture.output(mark_done(n = 1)), character(0))
  expect_message(color_message("abc"))
})

test_that("color_message() honors `newline`", {
  # `newline` was accepted but ignored until it was restored, so this asserts on
  # the raw bytes: capture.output() splits on newlines and would hide it
  raw <- function(...) capture_stderr(color_message(...))
  # built from the palette constants rather than literal codes, so a re-theme
  # does not have to be re-typed here; gray is the default, for step lines
  plain <- paste0("\033[", ansi_gray, "mabc\033[0m")
  expect_identical(raw("abc", newline = FALSE), plain)
  expect_identical(raw("abc", newline = TRUE), paste0(plain, "\n"))
})

test_that("mark_done() colors `Done`, not the dots", {
  out <- capture_stderr(mark_done(n = 2, color_code = 34))
  # dots stay gray whatever `color_code` says. A mid-gray rather than
  # ANSI 30, plain black, which vanishes on a dark terminal.
  expect_identical(out, "\033[38;5;244m..\033[0m \033[34mDone\033[0m\n")
  expect_identical(strip_ansi(out), ".. Done\n")
})

test_that("mark_done() defaults to plain teal Done", {
  out <- capture_stderr(mark_done())
  expect_true(grepl(paste0("\033[", ansi_teal, "mDone"), out, fixed = TRUE))
  expect_identical(strip_ansi(out), "... Done\n")
  expect_false(has_bold(out))
})

test_that("mark_done() does not pause", {
  expect_false("Sys.sleep" %in% all.names(body(mark_done)))
  # loose: ten calls took three seconds with the old pause
  elapsed <- system.time(
    for (i in 1:10) {
      suppressMessages(mark_done())
    }
  )[["elapsed"]]
  expect_lt(elapsed, 1)
})

test_that("nothing in the palette is bold", {
  palette <- c(ansi_gray, ansi_teal)
  expect_false(any(palette == "1" | startsWith(palette, "1;")))
  namespace <- asNamespace("adatutor")
  expect_false(exists("ansi_bold", envir = namespace, inherits = FALSE))
  expect_false(exists("ansi_note", envir = namespace, inherits = FALSE))
})

test_that("style_ansi() colors only when asked", {
  expect_identical(
    style_ansi("abc", ansi_gray, use = TRUE),
    paste0("\033[", ansi_gray, "mabc\033[0m")
  )
  expect_identical(style_ansi("abc", ansi_gray, use = FALSE), "abc")
  # never under testthat, which is not a live console
  expect_false(use_ansi())
})
