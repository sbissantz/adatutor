check_list <- function(x) {
  if (!is.list(x)) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp),"` is not a list. ")
    sug <- paste0("Check if", "`", deparse(x_inp),"` is valid.")
    warning(c(msg, sug), call. = FALSE)
   }
}

check_df <- function(x) {
  if (!is.data.frame(x)) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp),"` is not a data frame. ")
    sug <- paste0("Check if", "`", deparse(x_inp),"` is valid.")
    warning(c(msg, sug), call. = FALSE)
   }
}

check_empty <- function(x) {
  if (length(x) == 0) {
    x_inp <- substitute(x)
    msg <- paste0("`", deparse(x_inp),"` has no elements. ")
    sug <- paste0("Check if", "`", deparse(x_inp),"` is valid.")
    warning(c(msg, sug), call. = FALSE)
   }
}
