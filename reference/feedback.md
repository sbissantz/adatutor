# Utility Functions for Console Feedback and Styling

A set of utility functions to enhance the console feedback during
processes. These functions include animated feedback, colored messages,
and combinations of both for a better user experience in the terminal.

`walking_dots()` creates a sequence of animated dots (e.g., "...")
followed by a "Done" message. The number of dots and the delay can be
customized.

`color_message()` prints a message in a specified color using ANSI color
codes. The text and color can be customized.

`walking_colordots()` is a combination of `walking_dots()` and
`color_message()`. It prints animated dots in a specified color, with a
delay between each, and ends with a colored "Done" message.

## Usage

``` r
walking_dots(n = 3, delay = 0.2)

color_message(text, color_code = 32, newline = FALSE)

walking_colordots(n = 3, delay = 0.2, color_code = 32)
```

## Arguments

- n:

  An integer specifying the number of iterations for dots or animations.
  Defaults to 3.

- delay:

  A numeric value specifying the delay (in seconds) between iterations
  or dots. Defaults to 0.2.

- text:

  A character string representing the message to be displayed. Used in
  `color_message()`.

- color_code:

  An integer specifying the ANSI color code for the text. Defaults to 32
  (green).

- newline:

  A logical value indicating whether to append a newline after the
  message. Defaults to `FALSE`.

## Value

All functions are called for their side effects. They do not return
values, but produce visual effects in the console, such as animations or
colored text.

## Examples

``` r
# Example usage:
if (FALSE) walking_dots(n = 5, delay = 0.1) # \dontrun{}

# Example usage:
if (FALSE) color_message("A green message", color_code = 32) # \dontrun{}

# Example usage:
if (FALSE) walking_colordots(n = 4, delay = 0.15, color_code = 34) # \dontrun{}
```
