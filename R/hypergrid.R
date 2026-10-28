#' AdaBoost hyperparameter grid by leave-one-group-out cross-validation
#'
#' Every setting of an AdaBoost grid, scored by leave-one-group-out
#' cross-validation on the six tuning projects of [altmext]: each project is
#' held out in turn and scored by the model trained on the other five. HOLZ,
#' the newest project, takes no part; the tutorial keeps it for the final
#' test. Made by `data-raw/hypergrid.R`.
#'
#' The scores are project-level only, with no means and no rankings: which
#' summary to use is your choice (see the examples).
#'
#' @format A list of 17 data frames, one per measure of [assess()], from `tp`
#'   to `patk`. Each has one row per setting, 2,640 in all, and these columns:
#' * `depth`: The depth of the tree that is boosted, 1 to 8.
#' * `T`: The number of boosting rounds, 10 to 3,000.
#' * `eta`: The learning rate, 0.0025 to 1.
#' * `eerp`, `ml1`, `ml2`, `ml3`, `rpp`, `ssrp`: The measure on that project,
#'   held out.
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
#' @family datasets
#'
#' @examples
#' data(hypergrid)
#' auc <- hypergrid$auroc
#' projects <- attr(hypergrid, "projects")$project
#'
#' # one setting, project by project
#' auc[auc$depth == 1 & auc$T == 1000 & auc$eta == 0.1, ]
#'
#' # a summary of your choice: mean minus SD across the projects
#' x <- as.matrix(auc[projects])
#' auc$stable <- rowMeans(x) - apply(x, 1, sd)
#' head(auc[order(-auc$stable), ], 3)
#'
#' # or the worst project, ties broken by stability
#' auc$worst <- apply(x, 1, min)
#' head(auc[order(-auc$worst, -auc$stable), ], 3)
#'
#' @name hypergrid
#' @docType data
#'
# @name, not a trailing "hypergrid": the string form looks the object up,
# which fails until data-raw/hypergrid.R has written the .rda
NULL
