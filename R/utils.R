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

  values <- unique(actual[!is.na(actual)])
  if (all(values %in% c(0, 1))) {
    return(as.integer(actual))
  }
  if (all(values %in% c(-1, 1))) {
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
#' Gray step lines, gray dots that end in "Done", and a gray progress bar.
#' They write to stderr, so progress stays out of results. adaboost(),
#' predict(), logo_cv() and tune_logo_cv() call them when `verbose = TRUE`.
#'
#' @noRd
NULL

# one place to re-theme the console: plain text informs, gray marks labels
# and steps, teal marks "Done" only, and nothing is bold. Mid-viridis teal and
# mid-gray are legible on light and dark terminals; a fixed gray, since
# Positron ignores the faint code (2)
ansi_gray <- "38;5;244"
ansi_teal <- "38;5;30"

#' Print text in an ANSI style, then reset it
#' @noRd
color_message <- function(text, color_code = ansi_gray, newline = FALSE) {
  styled <- paste0("\033[", color_code, "m", text, "\033[0m")
  message(styled, appendLF = newline)
  invisible(NULL)
}

#' Print gray dots, then "Done" in `color_code`
#'
#' The dots stay gray so the eye follows the "Done" markers.
#' @noRd
mark_done <- function(n = 3, color_code = ansi_teal) {
  # one message, printed at once
  message(
    "\033[",
    ansi_gray,
    "m",
    strrep(".", n),
    "\033[0m \033[",
    color_code,
    "mDone\033[0m"
  )
}

#' Open a gray progress bar on stderr
#' @noRd
open_progress <- function(n_steps) {
  # stdout would mix the bar into the results, and plain stderr is red in
  # RStudio; close_progress() resets the style
  message("\033[", ansi_gray, "m", appendLF = FALSE)
  utils::txtProgressBar(min = 0, max = n_steps, style = 3, file = stderr())
}

#' Reset the progress style and close a half-finished line
#'
#' Runs from on.exit(), which unlike tryCatch() also runs on a user interrupt.
#' @noRd
close_progress <- function(progress, finished) {
  if (!is.null(progress)) {
    cat("\033[0m", file = stderr())
  }
  if (!finished) {
    # closing the bar breaks the line; without one, break it directly
    if (!is.null(progress)) close(progress) else message("")
  }
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
