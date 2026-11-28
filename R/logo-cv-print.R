#' Methods for `logo_cv` objects
#'
#' `print()` shows the estimates of one metric per project. `summary()`
#' summarizes each setting across the projects. `as.data.frame()` gives all
#' estimates as one long table. `plot()` draws the estimates per project,
#' with bands when `x` carries intervals from [bootstrap()].
#'
#' @param x,object A result of [logo_cv()] or [tune_logo_cv()].
#' @param metric The metric to show, one of the names [assess()] returns.
#' @param sort_by The columns of the summary to sort by, or `NULL` to keep the
#'   grid order. Rows go highest first on every column, and later columns
#'   break ties in earlier ones. Rows that tie on all of them keep their grid
#'   order, and rows with `NA` go last.
#' @param ... Ignored.
#'
#' @return `print()` and `plot()` return `x` invisibly.
#'
#'   `summary()` returns a data frame with one row per setting: the
#'   setting's hyperparameters, then the `mean`, `sd`, `lower`, `min` and
#'   `max` of its estimates across the projects, where `lower` is
#'   \eqn{\mathrm{mean} - \mathrm{sd}}{mean - sd}. A statistic is `NA` if a
#'   project is `NA`. The row names are the settings' row numbers in the
#'   grid. For a nested result, `summary()` gives one row over the outer
#'   folds, without the hyperparameters.
#'
#'   `as.data.frame()` returns a data frame with one row per estimate.
#'
#' @family cross-validation
#'
#' @name logo_cv_methods
NULL

#' @rdname logo_cv_methods
#' @export
print.logo_cv <- function(x, metric = "auroc", ...) {
  # NA or several names would print nonsense; one unknown name prints a note
  if (
    !(is.character(metric) || is.factor(metric)) ||
      length(metric) != 1L ||
      is.na(metric)
  ) {
    stop(
      "`metric` must be a single name among the metrics of the result: ",
      paste(dimnames(x$estimates)$metric, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  # a factor would print its code in the header
  metric <- as.character(metric)
  nested <- isTRUE(x$nested)
  cat(
    if (nested) {
      "Nested leave-one-group-out cross-validation\n"
    } else {
      "Leave-one-group-out cross-validation\n"
    }
  )
  holdout <- attr(x, "holdout")
  # a nested result always says which groups were outer folds
  if (nested && is.null(holdout)) {
    holdout <- "each group"
  }
  # `selected` is the result's own metric, not the one shown
  header <- c(
    model = x$model,
    settings = nrow(x$grid),
    holdout = if (!is.null(holdout)) paste(holdout, collapse = ", "),
    metric = metric,
    selected = if (nested) paste(paste(x$sort_by, collapse = ", "), x$metric)
  )
  # pad first, then style, so the escape bytes cannot shift the values
  labels <- style_ansi(formatC(names(header), width = -9), ansi_gray)
  cat(paste0("  ", labels, " ", header, "\n"), sep = "")
  cat("\n")

  if (!metric %in% dimnames(x$estimates)$metric) {
    cat("no estimates for `", metric, "`\n", sep = "")
    return(invisible(x))
  }

  # per project, with no mean or SE across them: shared training data bias
  # the naive SE down (Bengio & Grandvalet, 2004)
  if (nested) {
    print_by_fold(x, metric)
  } else {
    print_by_setting(x, metric)
  }

  if (nested) {
    # report how often the inner searches disagree: that is the point
    n_distinct <- length(unique(x$selected$setting))
    cat(sprintf(
      "\nThe %d inner searches selected %d distinct setting%s.\n",
      nrow(x$selected),
      n_distinct,
      if (n_distinct == 1L) "" else "s"
    ))
  }
  invisible(x)
}

#' @rdname logo_cv_methods
#' @export
summary.logo_cv <- function(object, metric = "auroc", sort_by = NULL, ...) {
  metrics <- dimnames(object$estimates)$metric
  if (length(metric) != 1L || !metric %in% metrics) {
    stop(
      "`metric` must be a single name among the metrics of the result: ",
      paste(metrics, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  estimates <- read_estimates(object, metric)
  # no na.rm: a project without an estimate makes all five NA, as in hypergrid
  out <- data.frame(
    mean = rowMeans(estimates),
    sd = apply(estimates, 1L, stats::sd),
    min = apply(estimates, 1L, min),
    max = apply(estimates, 1L, max)
  )
  out <- add_lower(out)
  if (!isTRUE(object$nested)) {
    out <- cbind(read_hyperparameters(object), out)
  }
  # the setting's row number, not the row names the caller's grid carried
  rownames(out) <- seq_len(nrow(out))

  if (is.null(sort_by)) {
    return(out)
  }
  sort_summary(out, sort_by)
}

#' Add `lower`, the mean minus the SD, right after `sd`
#' @noRd
add_lower <- function(out) {
  # shared by both summary() methods, so their columns match
  out$lower <- out$mean - out$sd
  columns <- setdiff(names(out), "lower")
  out[append(columns, "lower", after = match("sd", columns))]
}

#' Sort a summary's rows highest first by the columns in `sort_by`
#' @noRd
sort_summary <- function(out, sort_by) {
  # shared by both summary() methods, so they rank alike
  if (
    !is.character(sort_by) ||
      length(sort_by) == 0L ||
      anyNA(sort_by) ||
      anyDuplicated(sort_by) > 0L ||
      !all(sort_by %in% names(out))
  ) {
    stop(
      "`sort_by` must name one or more columns of the summary, each once: ",
      paste(names(out), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  # a stable sort: rows that tie on every column keep their grid order
  keys <- unname(as.list(out[sort_by]))
  sorted <- do.call(
    order,
    c(keys, decreasing = TRUE, na.last = TRUE, method = "radix")
  )
  out[sorted, , drop = FALSE]
}

#' Take one metric's estimates as a setting x project matrix
#' @noRd
read_estimates <- function(x, metric) {
  cube <- x$estimates
  if (isTRUE(x$nested)) {
    # project x metric: the outer folds make one row
    return(matrix(
      cube[, as.character(metric)],
      nrow = 1L,
      dimnames = list(NULL, rownames(cube))
    ))
  }
  # not cube[, , metric], which drops to a vector for one setting or project
  array(
    cube[,, as.character(metric)],
    dim = dim(cube)[1:2],
    dimnames = dimnames(cube)[1:2]
  )
}

#' Keep the grid columns that hold a value in at least one row
#' @noRd
read_hyperparameters <- function(x) {
  # a learner records NA for a setting it lacks, such as a glm's depth
  x$grid[vapply(x$grid, \(column) !all(is.na(column)), logical(1))]
}

#' Print the projects once, then one row of estimates per setting
#' @noRd
print_by_setting <- function(x, metric) {
  estimates <- read_estimates(x, metric)
  info <- x$projects[match(colnames(estimates), x$projects$project), ]
  projects <- rbind(
    n = format(info$n),
    `base rate` = format(round(info$base_rate, 2), nsmall = 2)
  )
  colnames(projects) <- colnames(estimates)
  print_labeled(projects)
  cat("\n")

  # by name, not by value: the counts print whole, every other metric with
  # three decimals, so 0.62 prints as 0.620 and a rate of 1 as 1.000
  estimates <- round(estimates, 3)
  counted <- metric %in% c("tp", "tn", "fp", "fn")
  # the settings unrounded, so a rate such as 0.0025 prints in full
  table <- data.frame(
    read_hyperparameters(x),
    format(estimates, nsmall = if (counted) 0L else 3L),
    check.names = FALSE
  )
  # the head of a long grid; ranking it is the job of summary()
  n_shown <- if (nrow(table) > 20L) 10L else nrow(table)
  print(table[seq_len(n_shown), , drop = FALSE], row.names = FALSE)
  if (n_shown < nrow(table)) {
    cat(
      "... and ",
      format(nrow(table) - n_shown, big.mark = ","),
      " more settings; summary() ranks them.\n",
      sep = ""
    )
  }

  if (!is.null(x$ci)) {
    intervals <- format_intervals(x, metric, colnames(estimates))
    cat("\n", intervals$label, "\n", sep = "")
    print_labeled(intervals$block)
  }
}

#' Print a character matrix with its row labels gray
#' @noRd
print_labeled <- function(block, use = use_ansi()) {
  # style the lines print() gives, so the columns never shift
  lines <- utils::capture.output(print(noquote(block), right = TRUE))
  # print() pads the row labels to one width; a wrapped table repeats them,
  # and its header lines leave that width blank
  width <- max(nchar(rownames(block)))
  label <- substr(lines, 1L, width)
  labeled <- nzchar(trimws(label))
  lines[labeled] <- paste0(
    style_ansi(label[labeled], ansi_gray, use = use),
    substring(lines[labeled], width + 1L)
  )
  cat(paste0(lines, "\n"), sep = "")
}

#' Print one row per outer fold, with the setting that fold selected
#' @noRd
print_by_fold <- function(x, metric) {
  long <- as.data.frame(x)
  rows <- long[long$metric == metric, , drop = FALSE]
  # the grid's columns, in the order as.data.frame() gives them
  fold_columns <- c("project", "n", "base_rate", "estimate")
  settings <- setdiff(
    names(long),
    c("model", "setting", "metric", fold_columns)
  )
  columns <- c(settings, fold_columns)
  columns <- columns[vapply(
    rows[columns],
    \(column) !all(is.na(column)),
    logical(1)
  )]
  table <- format(rows[, columns, drop = FALSE], digits = 3)
  if (!is.null(x$ci)) {
    intervals <- format_intervals(x, metric, rows$project)
    table <- cbind(table, t(intervals$block))
  }
  print(table, row.names = FALSE)
  if (!is.null(x$ci)) {
    cat(intervals$label, "\n", sep = "")
  }
}

#' Format each project's bootstrap interval of one metric, with its label
#' @noRd
format_intervals <- function(x, metric, projects) {
  # by name, so the columns follow the estimates whatever the order of `ci`
  cis <- x$ci[projects]
  read_column <- function(column) {
    vapply(cis, \(ci) ci[[column]][match(metric, ci$metric)], numeric(1))
  }
  # as the estimates: the counts whole, every other metric with three decimals;
  # trimmed, so one wide cell widens only its own project's column
  digits <- if (metric %in% c("tp", "tn", "fp", "fn")) 0L else 3L
  block <- rbind(
    lower = format(
      round(read_column("lower"), digits),
      nsmall = digits,
      trim = TRUE
    ),
    upper = format(
      round(read_column("upper"), digits),
      nsmall = digits,
      trim = TRUE
    ),
    # not 1e+05: a project bootstrap() gave up on drops 100 * n_resample draws
    dropped = format(read_column("drop"), scientific = FALSE, trim = TRUE)
  )
  colnames(block) <- projects
  # one bootstrap() call made every project's intervals
  first <- cis[[1L]]
  n_resample <- attr(first, "n_resample")
  label <- paste0(
    format(100 * attr(first, "conf")),
    "% bootstrap CI (",
    format(n_resample, big.mark = ",", scientific = FALSE),
    if (isTRUE(attr(first, "stratified"))) " stratified",
    if (n_resample == 1) " resample" else " resamples",
    "/project)"
  )
  list(block = block, label = label)
}

#' @rdname logo_cv_methods
#' @export
as.data.frame.logo_cv <- function(x, ...) {
  cube <- x$estimates
  dim_names <- dimnames(cube)
  core_columns <- c("depth", "n_iter", "eta")

  # aperm() then as.vector() order rows metric fastest, then project, then
  # setting, matching expand.grid() below
  if (isTRUE(x$nested)) {
    keys <- expand.grid(
      metric = dim_names$metric,
      project = dim_names$project,
      stringsAsFactors = FALSE
    )
    values <- as.vector(aperm(cube, c(2L, 1L)))
    setting <- x$selected$setting[match(keys$project, x$selected$project)]
  } else {
    keys <- expand.grid(
      metric = dim_names$metric,
      project = dim_names$project,
      setting = dim_names$setting,
      stringsAsFactors = FALSE
    )
    values <- as.vector(aperm(cube, c(3L, 2L, 1L)))
    setting <- as.integer(keys$setting)
  }

  settings <- x$grid[setting, , drop = FALSE]
  project_info <- x$projects[
    match(keys$project, x$projects$project),
    ,
    drop = FALSE
  ]
  extra_columns <- setdiff(names(settings), core_columns)

  out <- data.frame(
    model = x$model,
    setting = setting,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  out <- cbind(out, `rownames<-`(settings[, core_columns, drop = FALSE], NULL))
  if (length(extra_columns)) {
    out <- cbind(
      out,
      `rownames<-`(settings[, extra_columns, drop = FALSE], NULL)
    )
  }
  cbind(
    out,
    data.frame(
      project = keys$project,
      n = project_info$n,
      base_rate = project_info$base_rate,
      metric = keys$metric,
      estimate = values,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  )
}

#' @rdname logo_cv_methods
#'
#' @param levels The levels of the bootstrap bands, such as 0.95.
#' @param baseline The reference line. `"auto"` draws it where a useless model
#'   would score: 0.5 for `auroc` and `bacc`, 0 for `mcc`, the majority-class
#'   rate for `acc`, and each project's base rate for `auprc`, `ppv`, `f1` and
#'   the `patk` metrics. Other metrics get no line. A number sets it; `NA`
#'   leaves it out.
#'
#' @export
plot.logo_cv <- function(
  x,
  metric = "auroc",
  levels = c(0.50, 0.80, 0.95),
  baseline = "auto",
  ...
) {
  # the metric as given first: as.character() in prepare_logo_cv() stops one
  # that is no text, such as a function, with another message
  read_metric_rows(x, metric)
  # before par(), which opens a device when none is open
  figure <- prepare_logo_cv(
    x,
    metric = metric,
    levels = levels,
    baseline = baseline,
    ylab = metric,
    add_keys = TRUE
  )
  # room below for the project names only: there is no x-axis title
  old_par <- graphics::par(mar = c(3, 5, 4, 2) + 0.1)
  on.exit(graphics::par(old_par), add = TRUE)
  draw_logo_cv(figure, main = "Leave-one-group-out performance")
  invisible(x)
}
