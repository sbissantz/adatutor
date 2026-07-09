# A Short Overview

## Introduction

This vignette provides a short walk through of how to use adatutor for
classification tasks using Adaptive Boosting (AdaBoost). AdaBoost is an
ensemble learning method that improves weak classifiers by iteratively
adjusting their weights based on classification errors. Here, we apply
AdaBoost to predict the replication success of social science lab
experiments using data from Altmejd et al. (2019).

## Setup

Before we start, make sure the `adatutor` package is installed and
loaded.

``` r

# Load package
library(adatutor)
#> adatutor: Introducing the Boosting Framework with AdaBoost
#> Version: 1.0.0
```

We will use a short version of the `altmejd` dataset (see the R
Documentation for more information). Altmejd et al. (2019) pooled the
data from five large-scale replication projects, which we will use to
classify the replication outcomes of social science lab experiment. The
dataset can be loaded with:

``` r

# Load data
data(altmejd)
```

### The Names of the Predictors and the Outcome Variables

We will use only a subset of the predictors used by Altmejd et al.
(2019) – the *reviewer metrics*. Here are the names of the reviewer
metrics:

``` r

# Reviewer metrics (names)
prednms <- c("power.o","effect_size.o", "n.o","p_value.o")
```

Our outcome variable is a binary measure of replication success with the
name:

``` r

# Outcome variable (name)
outnme <- "replicate"
```

### The Variables of Interest

The predictors and outcomes are the variables of interest in our
prediction task.

``` r

# Variables of interest
voinms <- c(prednms, outnme)
```

For convenience, we abbreviate the original name of the dataset.

``` r

# Shorten 'altmejd'
df <- altmejd
```

Setting a seed ensures reproducible results:

``` r

# Reproducibility
set.seed(112)
```

## Splitting the Data

To evaluate model performance, we split the data into training (70%) and
test (30%) sets.

### Simple Random Split

A standard random split:

``` r

# Simple random split
randsplit <- random_split(df, prop = 0.7)
train <- randsplit$set1
test <- randsplit$set2
```

### Stratified Split

Stratified sampling ensures proportional representation of the outcome
variable:

``` r

# Stratified split
stratsplit <- stratified_split(df, group = "replicate", id = "eid", prop = 0.7)
train <- stratsplit$set1
test <- stratsplit$set2
```

## Adaptive Boosting

The AdaBoost algorithm combines multiple base models into a strong
overall model – the *ensemble*. We use classification stumps as base
learners, which require the following hyperparameter setup:

``` r

# Tree hyperparameter setup
treehypar <- rpart::rpart.control(
  # Maximum depth
  maxdepth = 1,
  # Gini impurity (default)
  split = "gini")
```

### Training Phase

In addition to the tree hyperparameters, we need to set the boosting
hyperparameters. The key hyperparameters are:

- `T`: Number of base models

- `eta`: Learning rate

Here, we develop an ensemble with 100 stumps and a learning rate of 1:

``` r

# Boosted stumps and weights
fit <- trainAda(replicate ~ ., data = train[, voinms],
  # Boosting hyperparameter
  T = 10, eta = 1,
  # Tree hyperparameter
  treehypar = treehypar)
```

### Test Phase

Once the model is trained, we can use it to make predictions on the data
in the test set:

``` r

# Predictions on test set
ypred <- testAda(fit, data = test[, prednms])
```

### Model Evaluation

A confusion matrix can help us evaluate the prediction accuracy:

``` r

# Confusion matrix
table(ypred, test[, outnme])
#>      
#> ypred failure success
#>    -1      20      11
#>    1        5       9
```

## References

Altmejd, Adam, Anna Dreber, Eskil Forsell, et al. 2019. “Predicting the
Replicability of Social Science Lab Experiments.” *PLOS ONE* 14 (12):
1–18. <https://doi.org/10.1371/journal.pone.0225826>.
