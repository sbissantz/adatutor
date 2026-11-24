# Read what a figure drew off the display list: each element is one drawing
# call, its C entry point first, then the evaluated arguments; `...` goes to
# pdf(), for a device of the manuscript's size
record_calls <- function(expr, ...) {
  grDevices::pdf(NULL, ...)
  on.exit(grDevices::dev.off(), add = TRUE)
  grDevices::dev.control("enable")
  value <- force(expr)
  calls <- lapply(grDevices::recordPlot()[[1]], \(item) item[[2]])
  list(value = value, calls = calls)
}

# the calls to one C entry point, such as "C_plotXY" or "C_axis"
find_calls <- function(calls, name) {
  Filter(\(call) identical(call[[1]]$name, name), calls)
}

# an ensemble that fits its data without error: two stumps carve the set, and
# a few rounds find them
fit_separable <- function() {
  set.seed(1)
  data <- data.frame(
    power.o = stats::runif(60),
    n.o = stats::runif(60, 10, 500)
  )
  data$replicate <- factor(
    ifelse(data$power.o > 0.6 & data$n.o > 200, "success", "failure"),
    levels = c("failure", "success")
  )
  fit <- rpart::rpart(
    replicate ~ power.o + n.o,
    data = data,
    method = "class",
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 10, eta = 1, verbose = FALSE, check_inputs = FALSE)
  list(fit = fit, data = data)
}
