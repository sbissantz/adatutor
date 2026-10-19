#' Stratified 70/15/15 split of the extended Altmejd data
#'
#' The training, validation and test sets of [altmext]. Made by
#' `data-raw/altmext-splits.R` with the same [partition()] call as
#' [altmejd_splits], run on `altmext`.
#'
#' The sets are shipped because a seed is not enough: R changed `sample()` in
#' version 3.6.0, which moved every seeded split.
#'
#' The split is stratified on `replicate`, so each set stays close to the
#' whole data's share of successes (.483): .483, .477 and .489. It is not
#' stratified on `pid`, so the project mix varies between the sets. SCORE, for
#' example, makes up 31 percent of the whole data but 35, 25 and 20 percent of
#' the three sets.
#'
#' @format A list of three data frames with the columns of [altmext]: `train`
#'   (205 rows), `valid` (44 rows) and `test` (45 rows).
#'
#' @family datasets
#' @seealso [partition()] to make your own split.
#'
#' @name altmext_splits
#'
#' @docType data
#'
NULL
