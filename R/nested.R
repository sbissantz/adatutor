#' Nested leave-one-group-out cross-validation of the AdaBoost grid
#'
#' The procedure "search the grid of [hypergrid], then fit the winner", scored
#' by nested leave-one-group-out cross-validation on the seven projects of
#' [altmext]. Each project is held out in turn and scored by the setting with
#' the highest `lower` in a search of the grid on the other six.
#'
#' @format An object of class `logo_cv`, as [tune_logo_cv()] returns it with
#'   `nested = TRUE`. The methods in [logo_cv_methods] and [bootstrap()] work
#'   on it as on any result of [logo_cv()]. It is a list with these elements:
#' * `estimates`: A matrix with one row per project and one column per
#'   metric of [assess()]. Each row is the project's score from the setting
#'   its own inner search picked. `NA` marks an undefined metric, such as
#'   `patk_5` on ML3 (see [patk()]).
#' * `grid`: The 1,320 settings, in the row order of [hypergrid].
#' * `projects`: Each project's `n` and `base_rate`.
#' * `scores`: The held-out margins, one row per study effect, with the
#'   columns `project`, `actual` and `score`. [bootstrap()] resamples them.
#' * `model`: `"adaboost"`.
#' * `nested`: `TRUE`.
#' * `metric` and `sort_by`: The rule of the inner searches, `"auroc"` and
#'   `"lower"`: the highest \eqn{\mathrm{mean} - \mathrm{sd}}{mean - sd} of
#'   the AUROC across the inner projects.
#' * `selected`: One row per project: the `setting` its inner search picked,
#'   as a row number of `grid`; that setting's `depth`, `n_iter` and `eta`;
#'   and `inner_value`, the setting's `lower` in that inner search.
#'
#' @family datasets
#'
#' @examples
#' data(nested)
#'
#' # one row per project, with the setting its inner search picked
#' nested
#'
#' # the seven estimates in one row: their mean, SD, mean minus SD, minimum
#' # and maximum
#' summary(nested)
#'
#' # the settings the seven inner searches picked
#' nested$selected
#'
#' @name nested
#' @docType data
#'
# @name, not a trailing "nested": the string form looks the object up, which
# fails until the .rda exists
NULL
