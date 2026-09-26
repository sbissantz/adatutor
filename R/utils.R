#' Map outcome labels onto 0/1
#'
#' Accepts a two-level factor (the second level is positive), logical, 0/1,
#' or -1/1, which is what `predict(type = "class")` returns. Anything else is
#' an error: silent coercion would turn a 1/2 vector into a wrong answer.
#'
#' @param actual The labels.
#' @return An integer vector of 0 and 1.
#' @noRd
as_binary <- function(actual) {
  if (is.factor(actual)) {
    if (nlevels(actual) != 2L) {
      stop("`actual` must have exactly two levels.", call. = FALSE)
    }
    return(as.integer(actual) - 1L)
  }
  if (is.logical(actual)) {
    return(as.integer(actual))
  }

  vals <- unique(actual[!is.na(actual)])
  if (all(vals %in% c(0, 1))) {
    return(as.integer(actual))
  }
  if (all(vals %in% c(-1, 1))) {
    # predict(type = "class") returns -1/1
    return(as.integer((actual + 1) / 2))
  }
  stop(
    "`actual` must be a two-level factor, 0/1, -1/1, or logical.",
    call. = FALSE
  )
}

#' Console feedback while a fit runs
#'
#' Dots, styled text and both combined. They write to stderr, so progress
#' stays out of results, and adaboost() and predict() call them when
#' `verbose = TRUE`.
#'
#' @noRd
NULL

# one place to re-theme the transcript; only mid-viridis is legible on both
# light and dark terminals, and faint (2) replaces black, which vanished on dark
ansi_bold <- "1"
ansi_dim <- "2"
ansi_teal <- "38;5;30"
ansi_note <- "1;38;5;30"

#' Print text in an ANSI style, then reset it
#' @noRd
color_message <- function(text, color_code = ansi_teal, newline = FALSE) {
  styled <- paste0("\033[", color_code, "m", text, "\033[0m")
  message(styled, appendLF = newline)
  invisible(NULL)
}

#' Print faint dots, then "Done" in `color_code`
#'
#' The dots stay faint so the eye follows the "Done" markers.
#' @noRd
mark_done <- function(n = 3, delay = 0.1, color_code = ansi_note) {
  for (i in seq_len(n)) {
    # faint, always: the dots are filler and should not compete with "Done"
    color_message(".", color_code = ansi_dim)
    Sys.sleep(delay)
  }
  color_message(" Done", color_code = color_code, newline = TRUE)
}

#' Whether printed output may carry color
#'
#' Only in a live console: print() output also lands in knitr documents and
#' captured output, where escape codes show as garbage. `NO_COLOR` turns it
#' off.
#' @noRd
use_ansi <- function() {
  interactive() &&
    !isTRUE(getOption("knitr.in.progress")) &&
    !nzchar(Sys.getenv("NO_COLOR"))
}

#' Wrap text in an ANSI style when `use` is `TRUE`
#' @noRd
style_ansi <- function(text, color_code, use = use_ansi()) {
  if (!use) {
    return(text)
  }
  paste0("\033[", color_code, "m", text, "\033[0m")
}
