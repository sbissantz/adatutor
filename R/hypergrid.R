#' AdaBoost grid search by leave-one-group-out cross-validation
#'
#' Every setting of a 480-row AdaBoost grid, scored on every replication
#' project by leave-one-group-out cross-validation (the projects are the
#' groups), under two values of the tree complexity parameter `cp`. Made by `data-raw/hypergrid.R`.
#'
#' The search is plain, not nested: the best average is chosen and graded on
#' the same five projects, which is optimistic on a grid this size. Quote
#' [hypernested] instead. The mean over projects is fine for ranking settings,
#' but it is not a performance estimate (see [logo_cv()]).
#'
#' Paired over the settings, `cp = 0` trails `cp = 0.01` by .0044 in mean
#' AUROC (95 percent CI .0021 to .0067). The whole gap is at depth 4 (-.0152,
#' 95 percent CI -.0214 to -.0090). At depth 1 there is no difference
#' (+.0001, 95 percent CI -.0012 to .0014).
#'
#' @format An object of class `logo_cv` (see [logo_cv()]):
#' * `estimates`: An array of 960 settings x 5 projects x 15 measures. `NA`
#'   marks an undefined measure, such as `ppv` when a model predicts no
#'   positives. `auroc` is never `NA`.
#' * `grid`: The 960 settings: `depth` 1 to 4, `T` 10 to 1000, `eta` 0.02 to
#'   1, and `cp` 0 or 0.01.
#' * `projects`: Each project's `n` and `base_rate`.
#' * `model`: Always `"adaboost"`.
#'
#' @family datasets
#'
#' @examples
#' data(hypergrid)
#'
#' # one measure across every setting and project: a 960 x 5 matrix
#' au <- hypergrid$estimates[, , "auroc"]
#' dim(au)
#'
#' # `grid` says what each row of that matrix is
#' g <- hypergrid$grid
#' head(cbind(g, round(au, 3)), 3)
#'
#' # one setting, sorted across projects
#' i <- which(g$depth == 1 & g$T == 1000 & g$eta == 0.15 & g$cp == 0.01)
#' sort(au[i, ], decreasing = TRUE)
#'
#' # the mean over projects ranks settings; it does not estimate performance
#' m <- rowMeans(au)
#' g[which.max(m), ]
#'
#' # the whole long table, one row per estimate
#' str(as.data.frame(hypergrid))
#'
#' @name hypergrid
#' @docType data
#'
# @name, not a trailing "hypergrid": the string form looks the object up,
# which fails until data-raw/hypergrid.R has written the .rda
NULL

#' Nested leave-one-group-out search for AdaBoost
#'
#' The grid of [hypergrid], run nested: each project is held out in turn, the
#' grid search is repeated on the other four, and the chosen setting is refit
#' and scored on the held-out project. Made by `data-raw/hypergrid.R`.
#'
#' This is the figure to quote for AdaBoost. It estimates the procedure
#' "search the grid, fit the winner", not any one setting. The settings the
#' outer folds chose are a diagnostic, and they disagree with each other. Only
#' `cp = 0.01` is nested.
#'
#' @format An object of class `logo_cv` with `nested = TRUE`:
#' * `estimates`: An array of 5 projects x 15 measures.
#' * `selected`: The `depth`, `T` and `eta` each outer fold chose. Its
#'   `inner_criterion` is `NA` because the run predates it; rerunning
#'   `data-raw/hypergrid.R` fills it in.
#' * `projects`, `model`: As in [hypergrid].
#'
#' @family datasets
#'
#' @examples
#' data(hypernested)
#'
#' # the five estimates, each with the setting its outer fold chose
#' au <- hypernested$estimates[, "auroc"]
#' cbind(
#'   hypernested$projects,
#'   hypernested$selected[, c("depth", "T", "eta")],
#'   auroc = round(au, 3)
#' )
#'
#' # the folds hold 10 to 90 studies, so the plain and weighted means differ
#' mean(au)
#' weighted.mean(au, hypernested$projects$n)
#'
#' @name hypernested
#' @docType data
#'
NULL
