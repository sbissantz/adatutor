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
#' whole data's share of successes (.475): .475, .467 and .484. It is not
#' stratified on `pid`, so the project mix varies between the sets.
#'
#' @format A list of three data frames with the columns of [altmext]: `train`
#'   (141 rows), `valid` (30 rows) and `test` (31 rows).
#'
#' @section License:
#' The same as [altmext].
#'
#' @family datasets
#' @seealso [partition()] to make your own split.
#'
#' @name altmext_splits
#'
#' @docType data
#'
NULL
