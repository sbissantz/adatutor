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
  expect_identical(
    capture.output(mark_done(n = 1, delay = 0)),
    character(0)
  )
  expect_message(color_message("abc"))
})

test_that("color_message() honors `newline`", {
  # `newline` was accepted but ignored until it was restored, so this asserts on
  # the raw bytes: capture.output() splits on newlines and would hide it
  raw <- function(...) {
    f <- tempfile()
    on.exit(unlink(f), add = TRUE)
    con <- file(f, open = "wt")
    sink(con, type = "message")
    color_message(...)
    sink(type = "message")
    close(con)
    readChar(f, file.size(f), useBytes = TRUE)
  }
  # built from the palette constants rather than literal codes, so a re-theme
  # does not have to be re-typed here
  plain <- paste0("\033[", ansi_teal, "mabc\033[0m")
  expect_identical(raw("abc", newline = FALSE), plain)
  expect_identical(raw("abc", newline = TRUE), paste0(plain, "\n"))
})

test_that("mark_done() colors `Done`, not the dots", {
  out <- paste(
    capture.output(
      mark_done(n = 2, delay = 0, color_code = 34),
      type = "message"
    ),
    collapse = ""
  )
  # dots stay faint whatever `color_code` says. Faint rather than a grey:
  # ANSI 30 is plain black, which vanishes on a dark terminal.
  expect_true(grepl(paste0("\033[", ansi_dim, "m."), out, fixed = TRUE))
  # and "Done" takes the requested colour, which was ignored before
  expect_true(grepl("\033[34m Done", out, fixed = TRUE))
  expect_false(grepl("\033[34m.\033", out, fixed = TRUE))
})

test_that("mark_done() defaults to bold teal Done", {
  out <- paste(
    capture.output(mark_done(n = 1, delay = 0), type = "message"),
    collapse = ""
  )
  # the same style as the retrodiction notice, on purpose
  expect_true(grepl(paste0("\033[", ansi_note, "m Done"), out, fixed = TRUE))
})

test_that("style_ansi() colors only when asked", {
  expect_identical(
    style_ansi("abc", ansi_note, use = TRUE),
    paste0("\033[", ansi_note, "mabc\033[0m")
  )
  expect_identical(style_ansi("abc", ansi_note, use = FALSE), "abc")
  # never under testthat, which is not a live console
  expect_false(use_ansi())
})
