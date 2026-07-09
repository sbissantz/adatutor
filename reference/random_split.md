# Randomly Split a Dataset into Two Subsets

The function takes a dataset and splits it into two subsets randomly,
based on a specified proportion.

## Usage

``` r
random_split(set, prop = 0.7)
```

## Arguments

- set:

  A data frame or a matrix to be split into subsets.

- prop:

  A numeric value between 0 and 1 indicating the proportion of rows to
  include in the first subset. Defaults to 0.7.

## Value

A list with two elements:

- `set1`:

  The first subset of the dataset, containing `prop` proportion of the
  rows.

- `set2`:

  The second subset of the dataset, containing the remaining rows.

## Examples

``` r
# Load a dataset
data(altmejd)

# A 70-30 simple random split
split <- random_split(altmejd, prop = 0.7)

# Access the subsets
set1 <- split$set1
set2 <- split$set2
```
