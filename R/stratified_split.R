#'@export
stratified_split <- function(set, group, id, prop = 0.7, seed = 112) {
  set <- as.data.frame(set) ; group <- as.character(group)
  id <- as.character(id) ; set.seed(seed)
  set_split <- split(set, set[,group])
  n_set <- lapply(set_split, function(set) round(nrow(set) * prop))
  strat_idx <- mapply(function(set, n) sample(set[,id], n),
                      set_split, n_set)
  set_idx <- set[,id] %in% unlist(strat_idx)
  list("set1" = set[set_idx, ], "set2" = set[!set_idx, ])
}
