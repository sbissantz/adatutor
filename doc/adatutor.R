## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# Load package
library(adatutor)

## -----------------------------------------------------------------------------
# Load data
data(altmejd)

## -----------------------------------------------------------------------------
# Reviewer metrics (names)
prednms <- c("power.o","effect_size.o", "n.o","p_value.o")

## -----------------------------------------------------------------------------
# Outcome variable (name)
outnme <- "replicate"

## -----------------------------------------------------------------------------
# Variables of interest
voinms <- c(prednms, outnme)

## -----------------------------------------------------------------------------
# Shorten 'altmejd'
df <- altmejd

## -----------------------------------------------------------------------------
# Reproducibility
set.seed(112)

## -----------------------------------------------------------------------------
# Simple random split
randsplit <- random_split(df, prop = 0.7)
train <- randsplit$set1
test <- randsplit$set2

## -----------------------------------------------------------------------------
# Stratified split
stratsplit <- stratified_split(df, group = "replicate", id = "eid", prop = 0.7)
train <- stratsplit$set1
test <- stratsplit$set2

## -----------------------------------------------------------------------------
# Tree hyperparameter setup
treehypar <- rpart::rpart.control(
  # Maximum depth
  maxdepth = 1,
  # Gini impurity (default)
  split = "gini")

## ----results='hide'-----------------------------------------------------------
# Boosted stumps and weights
fit <- trainAda(replicate ~ ., data = train[, voinms],
  # Boosting hyperparameter
  T = 10, eta = 1,
  # Tree hyperparameter
  treehypar = treehypar)

## ----results='hide'-----------------------------------------------------------
# Predictions on test set
ypred <- testAda(fit, data = test[, prednms])

## -----------------------------------------------------------------------------
# Confusion matrix
table(ypred, test[, outnme])

