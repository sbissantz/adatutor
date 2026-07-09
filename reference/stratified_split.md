# Stratified Split of a Dataset into Two Subsets

The function takes a dataset and splits it into two subsets while
preserving the distribution of a specified grouping variable.

## Usage

``` r
stratified_split(set, group, id, prop = 0.7)
```

## Arguments

- set:

  A data frame to be split into subsets.

- group:

  A character string specifying the name of the outcome variable in
  `set`.

- id:

  A character string specifying the name of the (categorical) stratum
  variable in `set`.

- prop:

  A numeric value between 0 and 1 indicating the proportion of rows to
  include in the first subset. Defaults to 0.7.

## Value

A list with two elements:

- `set1`:

  The first subset of the dataset, containing approximately `prop`
  proportion of the rows, stratified by the grouping variable.

- `set2`:

  The second subset of the dataset, containing the remaining rows.

## Examples

``` r
# Load a dataset
data(altmejd)

# Perform a 70-30 stratified split by
split <- stratified_split(altmejd, group = "replicate", id = "eid", prop = 0.7)

# Access the subsets
set1 <- split$set1
set2 <- split$set2
```
