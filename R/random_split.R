#'@export
random_split <- function(set, sprop = 0.7, seed = 112) {
  set <- as.data.frame(set) ; set.seed(seed)
  n_effect <- nrow(set)
  n_split <- round(n_effect * sprop)
  split_idx <- sample(seq_len(n_effect), n_split)
  # Training and testing set
  list("set1" = set[split_idx,], "set2" = set[-split_idx,] )
}
