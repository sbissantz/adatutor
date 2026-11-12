#' AdaBoost hyperparameter grid by leave-one-group-out cross-validation
#'
#' Every setting of an AdaBoost grid, scored by leave-one-group-out
#' cross-validation on the six tuning projects of [altmext]: each project is
#' held out in turn and scored by the model trained on the other five. HOLZ,
#' the newest project, takes no part; the tutorial keeps it for the final
#' test. The grid is one [tune_logo_cv()] call with `cores = 4`, which takes
#' about 40 minutes on four cores; the script is `data-raw/hypergrid.R`.
#'
#' Each row also holds the mean, SD, minimum and maximum of its six project
#' scores. These four columns rank settings; they are not estimates of
#' performance on a new project. The rows are in grid order, and which column
#' to rank by is the modeler's choice (see the examples).
#'
#' @format A list of 17 data frames, one per metric of [assess()], from `tp`
#'   to `patk`. Each has one row per setting, 2,640 in all, and these columns:
#' * `depth`: The depth of the tree that is boosted, 1 to 8.
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
#' @family datasets
#'
#' @examples
#' data(hypergrid)
#' auc <- hypergrid$auroc
#'
#' # one setting, project by project
#' auc[auc$depth == 1 & auc$n_iter == 1000 & auc$eta == 0.1, ]
#'
#' # stability: the mean minus the SD across the projects
#' auc$stable <- auc$mean - auc$sd
#' head(auc[order(-auc$stable), ], 3)
#'
#' # or the worst project, ties broken by stability
#' head(auc[order(-auc$min, -auc$stable), ], 3)
#'
#' @name hypergrid
#' @docType data
#'
# @name, not a trailing "hypergrid": the string form looks the object up,
# which fails until data-raw/hypergrid.R has written the .rda
NULL
