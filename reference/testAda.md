# Generate Predictions or Retrodictions From a Set of (Adaptively) Boosted Trees and Model Weights

This function implements the testing phase of the AdaBoost algorithm. It
extracts the adaptively boosted weak learners (e.g. classification
stumps) and their corresponding weights to combine them into the
weighted sum \$\$H(\mathbf{x}) = \sum\_{i=1}^Ta_th_t(\mathbf x),\$\$
which yields AdaBoost's predictions or retrodictions on a given set.

## Usage

``` r
testAda(fit, data, input_checks = TRUE, verbose = TRUE)
```

## Arguments

- fit:

  A model fitted with
  [`trainAda`](https://sbissantz.github.io/adatutor/reference/trainAda.md).
  This is a list of boosted trees and theirs weights. More specifically,
  each element of the list contains a weak learner (e.g., a decision
  stump) and its corresponding weight.

- data:

  A data frame containing the test set for predictions. The structure
  should match the training set used to generate `h`.

- input_checks:

  A logical value indicating whether to perform input validation checks.
  Defaults to `TRUE`.

- verbose:

  A logical value specifying whether to display progress messages and
  animations. Defaults to `TRUE`.

## Value

A numeric vector containing the final predictions from the AdaBoost
model. The predictions are in the form of `-1` or `1`, corresponding to
binary classification outcomes.

## Details

The function proceeds as follows:

1.  If `input_checks` is `TRUE`, basic input validation is performed to
    ensure that `fit` is a valid AdaBoost model and `data` is
    appropriate for predictions.

2.  Progress messages and animations are shown if `verbose` is `TRUE`.

3.  Boosted weak learners (`h`) and their weights (`a`) are extracted
    from `fit`.

4.  Predictions or retrodictions are generated for each weak learner
    using the test set.

5.  Individual predictions are combined using the weights from the
    AdaBoost model to produce the final predictions.

## Examples

``` r
# Example usage:
# Assume `fit` is a trained model and `test` is a data frame.
# ypred <- testAda(fit, test)
```
