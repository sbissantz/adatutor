# What an expression writes to stderr, byte for byte: capture.output() splits
# on newlines and would hide a line left open
capture_stderr <- function(expr) {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  # binary, so Windows does not write "\n" as "\r\n"
  con <- file(path, open = "wb")
  sink(con, type = "message")
  tryCatch(force(expr), finally = {
    sink(type = "message")
    close(con)
  })
  size <- file.size(path)
  if (size == 0) "" else readChar(path, size, useBytes = TRUE)
}

# the value of an expression, with what it wrote to stdout and stderr
capture_run <- function(expr) {
  value <- NULL
  out <- NULL
  err <- capture_stderr(out <- utils::capture.output(value <- expr))
  list(value = value, out = out, err = err)
}

strip_ansi <- function(x) {
  gsub("\033\\[[0-9;]*m", "", x)
}

# the lines of a transcript, with the escape codes removed
read_lines <- function(raw) {
  strsplit(strip_ansi(raw), "\n", fixed = TRUE)[[1]]
}

# the escape codes in `x`, such as "38;5;244" for gray
read_codes <- function(x) {
  codes <- unlist(regmatches(x, gregexpr("\033\\[[0-9;]*m", x)))
  gsub("^\033\\[|m$", "", codes)
}

# bold is code 1, alone or first in a list such as "1;38;5;30"
has_bold <- function(x) {
  codes <- read_codes(x)
  any(codes == "1" | startsWith(codes, "1;"))
}

# teal starts nowhere but right before "Done"
teal_only_on_done <- function(x) {
  x <- paste(x, collapse = "\n")
  teal <- gregexpr("\033[38;5;30m", x, fixed = TRUE)[[1]]
  done <- gregexpr("\033[38;5;30mDone\033[0m", x, fixed = TRUE)[[1]]
  identical(as.integer(teal), as.integer(done))
}
