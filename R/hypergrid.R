#' AdaBoost hyperparameter grid by leave-one-group-out cross-validation
#'
#' Every setting of an AdaBoost grid, scored by leave-one-group-out
#' cross-validation on the six projects of [altmext] other than HOLZ. Each
#' project is held out in turn and scored by the model trained on the other
#' five. `summary()` ranks the settings of one metric.
#'
#' @format A list of class `hypergrid` with 17 data frames, one per metric of
#'   [assess()], from `tp` to `patk`. Each has one row per setting, 1,320 in
#'   all, and these columns:
#' * `depth`: The depth of the tree that is boosted, 1 to 4.
#' * `n_iter`: The number of boosting rounds, 10 to 3,000.
#' * `eta`: The learning rate, 0.0025 to 1.
#' * `eerp`, `ml1`, `ml2`, `ml3`, `rpp`, `ssrp`: The metric on that project,
#'   held out. `NA` marks an undefined metric, such as `patk_5` on ML3 (see
#'   [patk()]).
#' * `mean`, `sd`, `min`, `max`: The mean, standard deviation, minimum and
#'   maximum of the six project columns. They are `NA` if a project is `NA`.
#'
#' `attr(hypergrid, "projects")` gives each project's `n` and `base_rate`.
#'
#' @param object The grid, `hypergrid`, or another list of class `hypergrid`.
#' @param metric The metric whose table to rank, one of the names of
#'   `object`.
#' @param sort_by The columns of the summary to sort by, or `NULL` to keep the
#'   grid order. Rows go highest first on every column, and later columns
#'   break ties in earlier ones. Rows that tie on all of them keep their grid
#'   order, and rows with `NA` go last.
#' @param n The most settings to return, one whole number of 1 or more, or
#'   `Inf` for all. Defaults to 5, or to `Inf` with `by`.
#' @param depth The tree depths whose settings to keep, or `NULL` to keep
#'   every depth.
#' @param by The hyperparameter to group by, `"depth"`, `"n_iter"` or `"eta"`,
#'   or `NULL` to keep every setting. Each of its values keeps only its best
#'   setting: the first in `sort_by` order, or in grid order on a tie. It acts
#'   on the settings `depth` kept and needs a `sort_by`.
#' @param ... Must be empty; any argument is an error.
#'
#' @return `summary()` returns a data frame: at most `n` rows of the table of
#'   `metric`, sorted by `sort_by`, with the column `lower`,
#'   \eqn{\mathrm{mean} - \mathrm{sd}}{mean - sd}, added after `sd`. The row
#'   names are the settings' row numbers in the table.
#'
#' @family datasets
#' @seealso [tune_logo_cv()] to score a grid of settings.
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
#' # the same ranking from summary()
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
# which fails until the .rda exists
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
