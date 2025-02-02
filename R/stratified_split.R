#' Stratified Split of a Dataset into Two Subsets
#'
#' The function takes a dataset and splits it into two subsets while preserving
#' the distribution of a specified grouping variable.
#'
#' @param set A data frame to be split into subsets.
#' @param group A character string specifying the name of the outcome variable
#'   in `set`.
#' @param id A character string specifying the name of the (categorical) stratum
#'   variable in `set`.
#' @param prop A numeric value between 0 and 1 indicating the proportion of rows
#'   to include in the first subset. Defaults to 0.7.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{set1}}{The first subset of the dataset, containing
#'     approximately \code{prop} proportion of the rows, stratified by the grouping variable.}
#'     \item{\code{set2}}{The second subset of the dataset, containing the
#'     remaining rows.}
#'   }
#'
#' @examples
#' # Load a dataset
#' data(altmejd)
#'
#' # Perform a 70-30 stratified split by
#' split <- stratified_split(altmejd, group = "replicate", id = "eid", prop = 0.7)
#'
#' # Access the subsets
#' set1 <- split$set1
#' set2 <- split$set2
#'
#'@export
stratified_split <- function(set, group, id, prop = 0.7) {
  check_numeric(prop)
  check_prop(prop)
  set <- as.data.frame(set)
  group <- as.character(group)
  id <- as.character(id)
  set_split <- split(set, set[,group])
  n_set <- lapply(set_split, function(set) round(nrow(set) * prop))
  strat_idx <- mapply(function(set, n) sample(set[,id], n),
                      set_split, n_set)
  set_idx <- set[,id] %in% unlist(strat_idx)
  list("set1" = set[set_idx, ], "set2" = set[!set_idx, ])
}
