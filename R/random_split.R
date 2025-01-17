#' Randomly Split a Dataset into Two Subsets
#'
#' The function takes a dataset and splits it into two subsets randomly, based
#' on a specified proportion.
#'
#' @param set A data frame or a matrix to be split into subsets.
#' @param prop A numeric value between 0 and 1 indicating the proportion of rows
#'   to include in the first subset. Defaults to 0.7.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{set1}}{The first subset of the dataset, containing
#'     \code{prop} proportion of the rows.}
#'     \item{\code{set2}}{The second subset of the dataset, containing the
#'     remaining rows.}
#'   }
#'
#' @examples
#' # Load a dataset
#' data(altmejd)
#'
#' # A 70-30 simple random split
#' split <- random_split(altmejd, prop = 0.7)
#'
#' # Access the subsets
#' set1 <- split$set1
#' set2 <- split$set2
#'
#' @export
random_split <- function(set, prop = 0.7) {
  check_numeric(prop)
  check_prop(prop)
  set <- as.data.frame(set)
  n_fx <- nrow(set)
  n_split <- round(n_fx * prop)
  split_idx <- sample(seq_len(n_fx), n_split)
  # Training and testing set
  list("set1" = set[split_idx, ], "set2" = set[-split_idx, ])
}
