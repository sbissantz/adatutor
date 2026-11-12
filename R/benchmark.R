#' Benchmark models by leave-one-group-out cross-validation and on HOLZ
#'
#' The AdaBoost ensemble of the tutorial next to four other models, all
#' trained on the four predictors of [altmext] and scored the same way. Each
#' model is scored by leave-one-group-out cross-validation on the six tuning
#' projects: each project is held out in turn and scored by the model trained
#' on the other five. Each model is then trained on all six and scored once on
#' HOLZ, the project the tutorial keeps back for the final test. The script is
#' `data-raw/benchmark.R`; it runs in about a minute.
#'
#' @format A list of 17 data frames, one per metric of [assess()], from `tp`
#'   to `patk`. Each has one row per model and these columns:
#' * `model`: The model: `"logit"`, `"tree"`, `"rf"`, `"adaboost"` or
#'   `"xgboost"`.
#' * `eerp`, `ml1`, `ml2`, `ml3`, `rpp`, `ssrp`: The metric on that project,
#'   held out and scored by the model trained on the other five.
#' * `holz`: The metric on HOLZ, scored by the model trained on all six
#'   tuning projects.
#'
#' @details
#' The models and how each was set:
#' * `logit`: A logistic regression from [stats::glm()] with the main effects
#'   of the four predictors. It has nothing to tune.
#' * `tree`: A classification tree from [rpart::rpart()]. Its depth, 5, was
#'   chosen from depths 1 to 8 by the rule below. Depths 5 to 8 score the
#'   same on every project, so the smallest was taken.
#' * `rf`: A random forest from [randomForest::randomForest()] at its
#'   defaults, 500 trees with two predictors tried per split. It is not tuned,
#'   because its choice of setting moved with the random seed. The seed is set
#'   to 112 before each call that fits it.
#' * `adaboost`: The tutorial's ensemble, trees of depth 2 boosted for 2,000
#'   rounds at a learning rate of 0.02, chosen from [hypergrid] by the rule
#'   below.
#' * `xgboost`: Gradient boosting from `xgboost::xgboost()` with the
#'   logistic loss, tuned by the rule below over 180 settings: depths 1, 2
#'   and 4; learning rates 0.1, 0.05 and 0.01; `min_child_weight` 1 and 5;
#'   `lambda` 1 and 10; and 50, 100, 200, 500 and 1,000 rounds. The choice is
#'   depth 4, 200 rounds, a rate of 0.05, `min_child_weight` 5 and `lambda` 10.
#'
#' The rule is the tutorial's: among the settings scored on the six tuning
#' projects, take the one with the best worst project, and break ties by
#' stability, the mean across the projects minus their SD. HOLZ takes no part
#' in any choice.
#'
#' Every model is scored on a continuous score: AdaBoost on its margin, the
#' others on the predicted probability of success. The metrics that need a
#' threshold cut the margin at 0 and the probabilities at 0.5.
#'
#' `attr(benchmark, "projects")` gives each project's `n` and `base_rate`.
#' `attr(benchmark, "settings")` has one row per model and hyperparameter, with
#' the columns `model`, `hyperparameter` and `value`. The values are text. The
#' logistic regression has no hyperparameters, so it has no rows. `depth`,
#' `n_iter` and `eta` are named as in the tutorial; the others carry their
#' package's argument name.
#'
#' `patk_5` is `NA` for ML3: it has only three successes, so precision among
#' the top five is undefined.
#'
#' @family datasets
#'
#' @examples
#' data(benchmark)
#'
#' # the AUROC of each model, project by project, HOLZ last
#' print(benchmark$auroc, digits = 2)
#' attr(benchmark, "settings")
#'
#' # on the six tuning projects: stability and the worst project
#' projects <- c("eerp", "ml1", "ml2", "ml3", "rpp", "ssrp")
#' x <- as.matrix(benchmark$auroc[projects])
#' data.frame(
#'   model = benchmark$auroc$model,
#'   stable = rowMeans(x) - apply(x, 1, sd),
#'   worst = apply(x, 1, min)
#' )
#'
#' @name benchmark
#' @docType data
#'
# @name, not a trailing "benchmark": the string form looks the object up,
# which fails until data-raw/benchmark.R has written the .rda
NULL
