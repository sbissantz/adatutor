#' Five models benchmarked by leave-one-group-out cross-validation and on HOLZ
#'
#' The metrics of five models trained on the four predictors of [altmext].
#' Each model is scored by leave-one-group-out cross-validation on the six
#' projects other than HOLZ (`pid == "holz"` in [altmext]), then trained on
#' those six and scored on HOLZ.
#'
#' @format A list of 17 data frames, one per metric of [assess()], from `tp`
#'   to `patk`. Each has one row per model and these columns:
#' * `model`: The model: `"logit"`, a logistic regression from
#'   [stats::glm()]; `"tree"`, a classification tree from [rpart::rpart()];
#'   `"rf"`, a random forest from [randomForest::randomForest()];
#'   `"adaboost"`, an ensemble from [adaboost()]; or `"xgboost"`, gradient
#'   boosting from `xgboost::xgboost()`.
#' * `eerp`, `ml1`, `ml2`, `ml3`, `rpp`, `ssrp`: The metric on that project,
#'   held out and scored by the model trained on the other five. `NA` marks
#'   an undefined metric, such as `patk_5` on ML3 (see [patk()]).
#' * `holz`: The metric on HOLZ, scored by the model trained on the six other
#'   projects.
#'
#' AdaBoost is scored on its margin and the other models on the predicted
#' probability of success. The metrics that need a threshold cut the margin
#' at 0 and probabilities at 0.5.
#'
#' `attr(benchmark, "projects")` gives each project's `n` and `base_rate`.
#' `attr(benchmark, "settings")` gives each model's hyperparameters as text,
#' with the columns `model`, `hyperparameter` and `value`.
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
#' # on the six tuning projects: the mean, the SD, the mean minus the SD and
#' # the worst project
#' projects <- c("eerp", "ml1", "ml2", "ml3", "rpp", "ssrp")
#' x <- as.matrix(benchmark$auroc[projects])
#' data.frame(
#'   model = benchmark$auroc$model,
#'   mean = rowMeans(x),
#'   sd = apply(x, 1, sd),
#'   lower = rowMeans(x) - apply(x, 1, sd),
#'   min = apply(x, 1, min)
#' )
#'
#' @name benchmark
#' @docType data
#'
# @name, not a trailing "benchmark": the string form looks the object up,
# which fails until the .rda exists
NULL
