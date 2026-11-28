#' Map outcome labels onto 0/1
#' @param actual The true labels: a two-level factor (the second level is the
#'   positive class), 0/1, -1/1, or logical.
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
  # no silent coercion: it would turn a 1/2 vector into a wrong answer
  stop(
    "`actual` must be a two-level factor, 0/1, -1/1, or logical.",
    call. = FALSE
  )
}

#' Count the missing values of `x`, a factor's NA level included
#' @noRd
count_missing <- function(x) {
  # is.na() misses an NA level, such as addNA() makes, which as.character()
  # turns into NA; as.character() alone would miss NaN
  sum(is.na(x) | is.na(as.character(x)))
}

#' Report progress on the console
#' @noRd
NULL
# stderr, so progress stays out of results

# fixed colors, legible on light and dark terminals; not the faint code (2),
# which Positron ignores
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
#' @noRd
mark_done <- function(n = 3, color_code = ansi_teal) {
  # one message, printed at once; gray dots, so the eye follows the "Done"
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
#' @noRd
close_progress <- function(progress, finished) {
  # called from on.exit(), which unlike tryCatch() also runs on an interrupt
  if (!is.null(progress)) {
    cat("\033[0m", file = stderr())
  }
  if (!finished) {
    # closing the bar breaks the line; without one, break it directly
    if (!is.null(progress)) close(progress) else message("")
  }
}

#' Decide whether printed output may carry color
#' @noRd
use_ansi <- function() {
  # a live console only: knitr and captured output show escape codes as garbage
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
