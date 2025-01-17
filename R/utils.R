walking_dots <- function(n = 3, delay = 0.2) {
  for (i in seq_len(n)) {
      message(".", appendLF = FALSE)
      Sys.sleep(delay)
    }
  message(" Done\n", appendLF = FALSE)
}

color_message <- function(text, color_code = 32, newline = FALSE) {
  msg <- paste0("\033[", color_code, "m", text, "\033[0m")
  cat(msg)
}

walking_colordots <- function(n = 3, delay = 0.2, color_code = 32) {
  for (i in seq_len(n)) {
      color_message(".", color_code = 30)
      Sys.sleep(delay)
    }
  color_message(" Done\n", color_code = 32, newline = TRUE)
}
