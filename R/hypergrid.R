#' AdaBoost hyperparameter grid by leave-one-group-out cross-validation
#'
#' Every setting of an AdaBoost grid, scored by leave-one-group-out
#' cross-validation on the six tuning projects of [altmext]: each project is
#' held out in turn and scored by the model trained on the other five. HOLZ,
#' the newest project, takes no part; the tutorial keeps it for the final
#' test. The grid is one [tune_logo_cv()] call with `cores = 4`, which takes
#' about 22 minutes on four cores; the script is `data-raw/hypergrid.R`.
#'
#' Each row also holds the mean, SD, minimum and maximum of its six project
#' scores. These four columns rank settings; they are not estimates of
#' performance on a new project. The rows are in grid order, and which column
#' to rank by is the modeler's choice (see the examples).
#'
#' @format A list of 17 data frames, one per metric of [assess()], from `tp`
#'   to `patk`. Each has one row per setting, 1,320 in all, and these columns:
#' * `depth`: The depth of the tree that is boosted, 1 to 4.
#' * `n_iter`: The number of boosting rounds, 10 to 3,000.
#' * `eta`: The learning rate, 0.0025 to 1.
#' * `eerp`, `ml1`, `ml2`, `ml3`, `rpp`, `ssrp`: The metric on that project,
#'   held out.
#' * `mean`, `sd`, `min`, `max`: The mean, standard deviation, minimum and
#'   maximum of the six project columns. They are `NA` if a project is `NA`.
#'
#' @details
#' `attr(hypergrid, "projects")` gives each project's `n` and `base_rate`.
#'
#' The trees are `rpart()` trees with its defaults apart from `maxdepth`, so
#' `cp = 0.01` and `minbucket = 7`.
#'
#' `patk_5` is `NA` for ML3: it has only three successes, so precision among
#' the top five is undefined.
#'
#' @section Ranking the settings:
#' The list has class `hypergrid`, so `summary()` ranks the settings of one
#' metric's table and returns at most `n`, best first. It adds the column
#' `lower` right after `sd`: the mean across the projects minus their SD,
#' which is the lower end of the band of one SD around the mean. Like `mean`,
#' `sd`, `min` and `max`, `lower` ranks settings; it is not an estimate of
#' performance on a new project. The row names stay the settings' row numbers
#' in the table.
#'
#' By default, `summary()` ranks every setting by `lower`, highest first, so
#' the first row is the tutorial's choice: the setting with the highest
#' `lower`. It sorts as the `summary()` of a [tune_logo_cv()] result does with
#' the same `sort_by`.
#'
#' Two arguments narrow the ranking. They act in this order, `by` on the
#' settings that `depth` kept:
#' * `depth` keeps the settings of those tree depths.
#' * `by` keeps the best setting of each value of one hyperparameter, so
#'   `by = "depth"` gives one row per depth. The best is the first in the
#'   `sort_by` order; of settings that tie on every column of `sort_by`, it
#'   is the first in grid order.
#'
#' `summary()` then sorts the settings that are left by `sort_by` and returns
#' the first `n`.
#'
#' @param object The grid, `hypergrid`, or another list of class `hypergrid`.
#' @param metric The metric whose table to rank. Defaults to `"auroc"`.
#' @param sort_by The columns of the table to sort by, as a character vector,
#'   or `NULL` to keep the grid order. Rows go highest first on every column,
#'   and later columns break ties in earlier ones. Rows that tie on all of
#'   them keep their grid order, and rows with `NA` go last. Defaults to
#'   `"lower"`, which puts first the setting with the highest mean minus SD.
#' @param n The most settings to return, one whole number of 1 or more.
#'   Defaults to 5, or with `by` to `Inf`, which returns the best setting of
#'   every value. There are fewer when `depth` or `by` leaves fewer. A number
#'   larger than the grid, or `Inf`, returns every setting that is left.
#' @param depth The tree depths whose settings to keep, as a numeric vector of
#'   values in the table's `depth` column, or `NULL` to keep every depth.
#'   Defaults to `NULL`.
#' @param by The hyperparameter to group by, `"depth"`, `"n_iter"` or `"eta"`:
#'   each of its values keeps only its best setting. `NULL`, the default,
#'   keeps every setting. It needs a `sort_by`, which decides the best.
#' @param ... Must be empty. A misspelled argument stops with an error
#'   instead of being ignored.
#'
#' @return `summary()` returns a data frame: at most `n` rows of the table of
#'   `metric`, best first, with the column `lower` added right after `sd`.
#'   With `depth` or `by`, the rows are only those they keep.
#'
#' @family datasets
#'
#' @examples
#' data(hypergrid)
#' auc <- hypergrid$auroc
#'
#' # one setting, project by project
#' auc[auc$depth == 1 & auc$n_iter == 1000 & auc$eta == 0.1, ]
#'
#' # lower: the mean across the projects minus their SD
#' auc$lower <- auc$mean - auc$sd
#' head(auc[order(-auc$lower), ], 3)
#'
#' # the same ranking from summary(): the first is the tutorial's choice
#' summary(hypergrid, n = 3)
#'
#' # the five best settings of depth 1, the stumps
#' summary(hypergrid, depth = 1)
#'
#' # the best setting of each depth
#' summary(hypergrid, by = "depth")
#'
#' @name hypergrid
#' @docType data
#'
# @name, not a trailing "hypergrid": the string form looks the object up,
# which fails until data-raw/hypergrid.R has written the .rda
NULL

#' @rdname hypergrid
#' @export
summary.hypergrid <- function(
  object,
  metric = "auroc",
  sort_by = "lower",
  n = if (is.null(by)) 5 else Inf,
  depth = NULL,
  by = NULL,
  ...
) {
  # a misspelled argument, such as n_top, would otherwise be ignored
  if (...length() > 0L) {
    labels <- ...names() %||% rep("", ...length())
    labels <- ifelse(
      nzchar(labels),
      paste0("`", labels, "`"),
      "an unnamed value"
    )
    stop(
      "Unused argument(s): ",
      paste(labels, collapse = ", "),
      ". summary() for a hypergrid takes `metric`, `sort_by`, `n`, ",
      "`depth` and `by`.",
      call. = FALSE
    )
  }
  metrics <- names(object)
  if (length(metric) != 1L || !metric %in% metrics) {
    stop(
      "`metric` must be a single name among the metrics of the grid: ",
      paste(metrics, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  # Inf is allowed: it returns every row
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1 || n != round(n)) {
    stop("`n` must be one whole number, 1 or more, or `Inf`.", call. = FALSE)
  }
  # a factor would pick the table by its code
  out <- object[[as.character(metric)]]
  depths <- sort(unique(out$depth))
  if (
    !is.null(depth) &&
      (!is.numeric(depth) ||
        length(depth) == 0L ||
        anyNA(depth) ||
        !all(depth %in% depths))
  ) {
    stop(
      "`depth` must be `NULL` or one or more depths of the grid: ",
      paste(depths, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  # a factor would pick the column by its code
  hyperparameters <- intersect(c("depth", "n_iter", "eta"), names(out))
  if (
    !is.null(by) &&
      (!is.character(by) || length(by) != 1L || !by %in% hyperparameters)
  ) {
    stop(
      "`by` must be `NULL` or one hyperparameter of the grid: ",
      paste(hyperparameters, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!is.null(by) && is.null(sort_by)) {
    stop("`by` needs a `sort_by` to tell the best setting.", call. = FALSE)
  }

  out <- add_lower(out)
  if (!is.null(depth)) {
    out <- out[out$depth %in% depth, , drop = FALSE]
  }
  if (!is.null(sort_by)) {
    out <- sort_summary(out, sort_by)
  }
  if (!is.null(by)) {
    # the first row of each value is its best; the rows kept stay sorted, as
    # if picked first and then sorted
    out <- out[!duplicated(out[[by]]), , drop = FALSE]
  }
  out[seq_len(min(n, nrow(out))), , drop = FALSE]
}
