#' @title Utility Functions for Console Feedback and Styling
#'
#' @description A set of utility functions to enhance the console feedback
#'   during processes. These functions include animated feedback, colored
#'   messages, and combinations of both for a better user experience in the
#'   terminal.
#'
#' @return All functions are called for their side effects. They do not return
#'   values, but produce visual effects in the console, such as animations or
#'   colored text.
#'
#' @param n An integer specifying the number of iterations for dots or
#'   animations. Defaults to 3.
  #'
#' @param delay A numeric value specifying the delay (in seconds) between
#'   iterations or dots. Defaults to 0.2.
#'
#' @param color_code An integer specifying the ANSI color code for the text.
#'   Defaults to 32 (green).
#'
#' @param text A character string representing the message to be displayed. Used
#'   in \code{color_message()}.
#'
#' @param newline A logical value indicating whether to append a newline after
#'   the message. Defaults to \code{FALSE}.
#'
#' @name feedback
#'
NULL

#' @rdname feedback
#'
#' @description \code{walking_dots()} creates a sequence of animated dots (e.g.,
#'   "...") followed by a "Done" message. The number of dots and the delay can
#'   be customized.
#'
#' @examples
#' # Example usage:
#' \dontrun{walking_dots(n = 5, delay = 0.1)}
#'
walking_dots <- function(n = 3, delay = 0.2) {
  for (i in seq_len(n)) {
      message(".", appendLF = FALSE)
      Sys.sleep(delay)
    }
  message(" Done\n", appendLF = FALSE)
}

#' @rdname feedback
#'
#' @description \code{color_message()} prints a message in a specified color
#'   using ANSI color codes. The text and color can be customized.
#'
#' @examples
#' # Example usage:
#' \dontrun{color_message("A green message", color_code = 32)}
#'
color_message <- function(text, color_code = 32, newline = FALSE) {
  msg <- paste0("\033[", color_code, "m", text, "\033[0m")
  cat(msg)
}

#' @rdname feedback
#'
#' @description \code{walking_colordots()} is a combination of
#'   \code{walking_dots()} and \code{color_message()}. It prints animated dots
#'   in a specified color, with a delay between each, and ends with a colored
#'   "Done" message.
#'
#' @examples
#' # Example usage:
#' \dontrun{walking_colordots(n = 4, delay = 0.15, color_code = 34)}
#'
walking_colordots <- function(n = 3, delay = 0.1, color_code = 32) {
  for (i in seq_len(n)) {
      color_message(".", color_code = 30)
      Sys.sleep(delay)
    }
  color_message(" Done\n", color_code = 32, newline = TRUE)
}
