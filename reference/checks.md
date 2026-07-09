# Miscellaneous Functions for Input Validation

A set of input validation functions for internal use that ensure the
passed arguments meet the expected criteria (e.g., type, length, and
value ranges). These functions catch common input errors, provide
meaningful error messages and guidance for correction.

`check_df()` ensures that `x` is a data frame. If it is not, an error is
thrown with a message indicating the issue.

`check_eta()` validates that the input is a numeric value and, if it is
greater than 1, provides a warning about potential overfitting.

`check_length()` ensures that `x` contains at least one element.

`check_list()` validates that the input is a list.

`check_numeric()` validates that the input is a numeric value.

`check_train()` ensures that the training and test set names are not the
same.

`check_prop()` validates that the input is a proportion (a numeric value
between 0 and 1).

## Usage

``` r
check_df(x)

check_eta(x)

check_length(x)

check_list(x)

check_numeric(x)

check_train(trainnme, testnme)

check_prop(x)
```

## Arguments

- x:

  A generic input to be validated. The expected type depends on the
  specific function.

- trainnme:

  A character string representing the name of the training set.

- testnme:

  A character string representing the name of the test set.

## Value

All functions are called for their side effects. If the input is
invalid, an error, or warning is generated with helpful suggestions for
troubleshooting.

## Details

Ensures that the input is not empty. If the input has a length of 0, an
error message is raised with a suggestion to check the input.

Ensures that the input is a valid list. If not, an error message is
raised.

Ensures that the input is numeric. If not, an error message is raised
with a suggestion to validate the input.

## Examples

``` r
# Example usage:
if (FALSE) check_df(5) # \dontrun{}  # Fails with an error message

# Example usage:
if (FALSE) check_eta("a") # \dontrun{}  # Fails with an error

# Example usage:
if (FALSE) check_length(character(0)) # \dontrun{}  # Fails with an error

# Example usage:
if (FALSE) check_list(5) # \dontrun{}  # Fails with an error

# Example usage:
if (FALSE) check_numeric("a") # \dontrun{}  # Fails with an error

# Example usage:
if (FALSE) check_train("train", "train") # \dontrun{}  # Fails with an error

# Example usage:
if (FALSE) check_prop(0) # \dontrun{}  # Issues a warning
```
