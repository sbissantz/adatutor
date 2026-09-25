#' Stratified 70/15/15 split of the Altmejd data
#'
#' The training, validation and test sets the tutorial uses, shipped so your
#' numbers match the text. Made by `data-raw/altmejd-splits.R` with the
#' [partition()] call that Listings 7 to 9 build up to.
#'
#' The sets are shipped because a seed is not enough: R changed `sample()` in
#' version 3.6.0, which moved every seeded split. Use them from Module 2 on.
#' Later modules fit on `train`, and Module 3 evaluates on `test`. The
#' tutorial never uses `valid`.
#'
#' The split is stratified on `replicate`, so each set stays close to the
#' whole data's share of successes (.447): .449, .435 and .455.
#'
#' @format A list of three data frames with the columns of [altmejd]: `train`
#'   (107 rows), `valid` (23 rows) and `test` (22 rows).
#'
#' @family datasets
#' @seealso [partition()] to make your own split.
#'
#' @name altmejd_splits
#'
#' @docType data
#'
NULL
