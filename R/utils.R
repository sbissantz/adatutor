#' @title Map Outcome Labels Onto 0/1
#'
#' @description Not exported. Converts any of the label encodings the package
#'   accepts into a 0/1 integer vector, so every function that needs a binary
#'   outcome can take whatever form the caller happens to have.
#'
#' @details Four encodings are recognized:
#'
#' \describe{
#'   \item{factor}{Exactly two levels. The \emph{second} level is the positive
#'     class, which is R's own convention and makes
#'     \code{factor(c("failure", "success"))} come out the way a reader expects.
#'     A factor with any other number of levels is an error rather than a guess.}
#'   \item{logical}{\code{TRUE} is positive.}
#'   \item{0/1}{Returned as is.}
#'   \item{-1/1}{Mapped by \eqn{(y + 1) / 2}. This is what
#'     \code{\link[=predict.adaboost]{predict()}} returns for
#'     \code{type = "class"}, so a retrodiction can be handed straight back
#'     in.}
#' }
#'
#' Anything else errors. Silently coercing an unrecognized encoding is how a 1/2
#' vector becomes a wrong answer without complaint, which is exactly the class of
#' bug this package exists to warn about.
#'
#' This lives here rather than beside any one caller because four files use it:
#' \code{\link[adatutor]{assess}}, \code{\link[adatutor]{lpocv}},
#' \code{\link[adatutor]{bootCI}} and \code{\link[adatutor]{plot_adabound}}.
#'
#' @param actual The labels, in any of the encodings above.
#'
#' @return An integer vector of 0 and 1, the same length as \code{actual}.
#'
#' @name as_binary
#'
#' @keywords internal
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

#' @title Console Feedback While a Fit Runs
#'
#' @description Not exported. Progress feedback for the long-running fitters:
#'   animated dots, colored text, and the two combined.
#'   \code{\link[adatutor]{adaboost}} and
#'   \code{\link[=predict.adaboost]{predict()}} call them when
#'   \code{verbose = TRUE}.
#'
#' @details All three write to the \emph{message} stream, not to standard
#'   output. Progress is diagnostic rather than a result, so it belongs on
#'   \code{stderr} where R puts \code{message()} and \code{warning()}: that keeps
#'   it out of \code{capture.output()}, out of a shell redirect, and out of
#'   anything downstream that reads a fit's printed value. It also means a
#'   knitr chunk hides it with \code{message: false} rather than
#'   \code{results: hide}.
#'
#'   Color is set with raw ANSI escape codes rather than a dependency. The
#'   palette has to stay legible on a light \emph{and} a dark terminal, which
#'   rules out most of it: measured as contrast against white and black and
#'   taking the worse of the two, viridis's dark purple manages 1.38 on black
#'   and its yellow 1.26 on white. Only the middle of the scale survives, so:
#'
#' \describe{
#'   \item{\code{ansi_teal} (\code{38;5;30})}{Viridis teal, for a completed
#'     step. xterm-256 index 30 is where viridis 0.4 and 0.5 both quantize, and
#'     it scores 4.36 against the worse background -- better than either exact
#'     colour, and 256-colour carries much further than truecolor.}
#'   \item{\code{ansi_dim} (\code{2})}{Faint, for the running commentary and
#'     the dots. It dims the terminal's \emph{own} foreground rather than
#'     naming a colour, so it cannot be invisible on either background. This
#'     replaces \code{30}, plain black, whose worst case was 1.00 -- it
#'     disappeared completely on a dark theme.}
#'   \item{\code{ansi_bold} (\code{1})}{The opening and closing lines.}
#'   \item{\code{ansi_note} (\code{1;38;5;30})}{Bold teal, for the
#'     retrodiction notice. Distinguished by weight rather than a second hue:
#'     viridis is a sequential palette with no categorical warning slot, and the
#'     band that survives both backgrounds is too narrow to carry two hues a
#'     reader could tell apart.}
#' }
#'
#'   A terminal that understands none of this prints the escapes literally,
#'   which is why all of it is behind \code{verbose}. One that ignores
#'   \code{2} renders normal weight, which is harmless.
#'
#'   The dots in \code{walking_colordots()} are always dim. That is deliberate:
#'   the eye should follow the \dQuote{Done} markers down the transcript, not
#'   the filler between them, so \code{color_code} sets the color of
#'   \dQuote{Done} and leaves the dots faint.
#'
#'   \code{delay} exists to make the animation legible, so these functions are
#'   slower than the work they report on when \code{n} is large. They are not on
#'   any path that runs per boosting round.
#'
#' @param n The number of dots. Defaults to 3.
#'
#' @param delay Seconds between dots. Defaults to 0.2 for
#'   \code{walking_dots()} and 0.1 for \code{walking_colordots()}.
#'
#' @param color_code An ANSI code, spliced verbatim into the escape
#'   sequence, so \code{"38;5;30"} and \code{"2"} work as well as a bare
#'   number. Defaults to \code{ansi_teal}.
#'
#' @param text The string to print.
#'
#' @param newline Whether to append a newline after the message. Defaults to
#'   \code{FALSE}.
#'
#' @return All three are called for their side effect and return \code{NULL}
#'   invisibly.
#'
#' @name feedback
#'
#' @keywords internal
NULL

#' @rdname feedback
#'
#' @description The palette lives here rather than as literals at the call
#'   sites, so the transcript can be re-themed in one place.
#'
#' @format Character strings, spliced verbatim into \code{\\033[<code>m}.
#'
ansi_bold <- "1"
#' @rdname feedback
ansi_dim <- "2"
#' @rdname feedback
ansi_teal <- "38;5;30"
#' @rdname feedback
ansi_note <- "1;38;5;30"

#' @rdname feedback
#'
#' @description \code{walking_dots()} prints \code{n} dots with a pause between
#'   them, then \dQuote{Done}. Uncolored, and written to the message stream.
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
#' @description \code{color_message()} prints \code{text} wrapped in an ANSI
#'   color code, resetting the color afterwards so nothing leaks into the next
#'   line.
#'
#' @examples
#' # Example usage:
#' \dontrun{color_message("A teal message", color_code = ansi_teal)}
#'
color_message <- function(text, color_code = ansi_teal, newline = FALSE) {
  msg <- paste0("\033[", color_code, "m", text, "\033[0m")
  message(msg, appendLF = newline)
  invisible(NULL)
}

#' @rdname feedback
#'
#' @description \code{walking_colordots()} is \code{walking_dots()} in color:
#'   grey dots, then \dQuote{Done} in \code{color_code}.
#'
#' @examples
#' # Example usage:
#' \dontrun{walking_colordots(n = 4, delay = 0.15, color_code = ansi_bold)}
#'
walking_colordots <- function(n = 3, delay = 0.1, color_code = ansi_teal) {
  for (i in seq_len(n)) {
    # faint, always: the dots are filler and should not compete with "Done"
    color_message(".", color_code = ansi_dim)
    Sys.sleep(delay)
  }
  color_message(" Done", color_code = color_code, newline = TRUE)
}
