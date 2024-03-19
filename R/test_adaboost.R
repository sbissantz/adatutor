#'@export
test_adaboost <- function(X, ensemble) {
  pred <- rep(0, nrow(X))
  for (tree in ensemble) {
    h <- tree[["learner"]]
    a <- tree[["learner_weight"]]
    # Get the object as integer
    y_pred <- predict(h, X, type = "vector")
    # Transform from 1/2 coding to -1/1
    # TODO: DANIEL: Shouldn't the next parameter be called "a" ?
    # TODO: And why -1/+1 coding and not 0/1 coding?!
    y_num <- 2 * y_pred - 3
    # Eta: learning rate (contribution of each weak learner)
    pred <- pred + a * y_num
  }
  list("class" = sign(pred), "raw" = pred)
}
