# Train a Set of Classification Trees Using AdaBoost

The function trains a set of classification trees using AdaBoost. It is
built on top of the `rpart` package, so the full range of tree
hyperparameters can be used to fine-tune the trees (see, e.g.,
[`rpart.control`](https://rdrr.io/pkg/rpart/man/rpart.control.html)). In
addition, the implementation allows customization of the number of trees
and the learning rate. It also provides verbose output to track the
training progress.

## Usage

``` r
trainAda(formula, data, T, eta, treehypar, input_checks = TRUE, verbose = TRUE)
```

## Arguments

- formula:

  A [`formula`](https://rdrr.io/r/stats/formula.html) specifying the
  relationship between the outcome and the predictors:
  `outcome ~ predictors`. The predictors should be included as additive
  terms (e.g., `X1 + X2 + ...`), Interactions are not supported (see the
  `formula` argument in
  [`rpart`](https://rdrr.io/pkg/rpart/man/rpart.html)).

- data:

  A data frame containing the variables in the model.

- T:

  An integer specifying the number of trees.

- eta:

  A numeric value representing the learning rate of the algorithm.

- treehypar:

  A list of control parameters for decision trees (passed to
  [`rpart.control`](https://rdrr.io/pkg/rpart/man/rpart.control.html)).

- input_checks:

  A logical value indicating whether to perform input validation checks.
  Defaults to `TRUE`.

- verbose:

  A logical value indicating whether to display verbose output during
  the training process. Defaults to `TRUE`.

## Value

A list containing the trained weak learners (`h`) and their associated
weights (`a`). Additional attributes may be included for model tracking
purposes.

## Details

The function implements the AdaBoost algorithm with a specified number
of iterations (`T`). It initializes observation weights, trains a
sequence of decision trees, and updates the weights at each iteration
based on prediction errors. A final ensemble of weak learners is
produced.

Key steps in the algorithm:

1.  Initialize observation weights.

2.  Train a decision tree using the current weights.

3.  Compute the weighted classification error and update the observation
    weights.

4.  Store the weak learner and its associated weight.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage:
library(rpart)
data(iris)
# Prepare binary classification data
irisbin <- iris[iris$Species != "setosa", ]
irisbin$Species <- factor(irisbin$Species)

# Set tree hyperparameters
treehypar <- rpart::rpart.control(maxdepth = 1, cp = 0)

# Train AdaBoost model
model <- trainAda(Species ~ ., data = irisbin, T = 10, eta = 1,
treehypar = treehypar)
} # }
```
