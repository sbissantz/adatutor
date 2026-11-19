#' Nested leave-one-group-out cross-validation of the AdaBoost grid
#'
#' The procedure "search the grid of [hypergrid], then fit the winner", scored
#' by nested leave-one-group-out cross-validation on all seven projects of
#' [altmext]. Each project is held out in turn. On the other six, a
#' leave-one-group-out search of the 1,320 settings of [hypergrid] picks the
#' setting with the highest `lower`. That setting is trained on the six and
#' scored once on the held-out project. The result is one estimate per project
#' for the procedure, not for any one setting. It is one [tune_logo_cv()] call
#' with `nested = TRUE` and `cores = 4`, which took 140 minutes on four cores;
#' the script is `data-raw/nested.R`.
#'
#' @format An object of class `logo_cv`, as [tune_logo_cv()] returns it with
#'   `nested = TRUE`. `print()`, `summary()`, `as.data.frame()` and `plot()`
#'   work on it as on any result of [logo_cv()] (see [logo_cv_methods]), and
#'   so does [bootstrap()]. It is a list with these elements:
#' * `estimates`: A matrix of project x metric, one row per project and one
#'   column per metric of [assess()]. Each row is the project's score from
#'   the setting its own inner search picked.
#' * `grid`: The 1,320 settings, in the row order of [hypergrid]: `n_iter`
#'   fastest, then `eta`, then `depth`.
#' * `projects`: Each project's `n` and `base_rate`.
#' * `scores`: The held-out margins, one row per study effect, with the
#'   columns `project`, `actual` and `score`. [bootstrap()] resamples them.
#' * `model`: `"adaboost"`.
#' * `nested`: `TRUE`.
#' * `metric` and `sort_by`: The rule of the inner searches, `"auroc"` and
#'   `"lower"`.
#' * `selected`: One row per project: the `setting` its inner search picked,
#'   as a row number of `grid`; that setting's `depth`, `n_iter` and `eta`;
#'   and `inner_value`, the setting's `lower` in that inner search.
#'
#' @details
#' The rule is the tutorial's: among the settings scored on the six inner
#' projects, take the one with the highest `lower`, the mean AUROC across the
#' projects minus their SD. Like `lower`, `inner_value` ranks settings; it is
#' not an estimate of performance on a new project. Each of the seven inner
#' searches picked a different setting.
#'
#' The recipe is that of [hypergrid]: `rpart()` trees with its defaults apart
#' from `maxdepth`, so `cp = 0.01` and `minbucket = 7`.
#'
#' Read the estimates as those of [logo_cv()]: one per project, with their
#' spread, and not as a mean with a standard error.
#'
#' `patk_5` is `NA` for ML3: it has only three successes, fewer than the five
#' places, and [patk()] returns `NA` when `k` exceeds the number of positives.
#'
#' @section HOLZ among the projects:
#' The tutorial keeps HOLZ back for the final test, which [benchmark]
#' records. This dataset comes after that test, which is why HOLZ can be one
#' of its projects: HOLZ's fold repeats the final test rather than adding a
#' new one. Its inner search is the search of [hypergrid] on the six tuning
#' projects, so it picks the same setting, trees of depth 2 boosted for 2,000
#' rounds at a learning rate of 0.02. Its estimates on HOLZ equal the
#' AdaBoost cells of [benchmark] on every metric, among them an AUROC of
#' 0.768. In the other six folds, HOLZ is one of the inner projects, so it
#' helps choose their settings. The tutorial's choice comes from [hypergrid],
#' which never sees HOLZ, and this dataset changes neither that choice nor
#' its final test.
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
# fails until data-raw/nested.R has written the .rda
NULL
