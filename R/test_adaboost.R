#'@export
test_adaboost <- function(X, ensemble) {
  pred <- rep(0, nrow(X))
  for (tree in ensemble) {
    h <- tree$learner
    a <- tree$learner_weight
    # Get the object as integer
    y_pred <- predict(h, X, type = "vector")
    # Transform from 1/2 coding to -1/1
    y_num <- 2 * y_pred - 3
    # Eta: learning rate (contribution of each weak learner)
    pred <- pred + a * y_num
    # pred <- pred + a * eta * (2*as.numeric(y_class)-3)
    # Outcome: y
  }
  list("class" = sign(pred), "raw" = pred)
}
