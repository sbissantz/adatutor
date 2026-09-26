#' Split a data set into parts, optionally within strata
#'
#' Draws the given proportions of the rows into one set each and leaves the
#' rest in the last set. With `strata`, the draw happens separately within
#' each level of that column, so its distribution is kept in every part.
#' Stratify on the outcome to keep each class's share close to that of the
#' whole data; this matters most for small data sets.
#'
#' Rows keep their original order. Proportions are of the whole data, counts
#' are rounded, and the last set takes whatever was not drawn.
#'
#' @param data A data frame to split.
#' @param prop One proportion per set, each between 0 and 1. A single number
#'   gives two sets, `prop` and the rest. Several must sum to 1, e.g.
#'   `c(0.7, 0.15, 0.15)`. Defaults to 0.7.
#' @param strata The name of a column to stratify on, usually the outcome.
#'   The default, `NULL`, draws from the whole data set. The name matches
#'   `strata` in `rsample::initial_split()`.
#'
#' @return A list of data frames named `set1`, `set2`, and so on, one per
#'   proportion. A proportion of 0 gives a data frame with no rows.
#'
#' @family cross-validation
#' @seealso [altmejd_splits], the split used in the tutorial.
#'
#' @examples
#' data(altmejd)
#'
#' # a simple 70/30 split
#' set.seed(112)
#' plain <- partition(altmejd, prop = 0.7)
#' nrow(plain$set1)
#' nrow(plain$set2)
#'
#' # the same, keeping the outcome's distribution in both parts
#' set.seed(112)
#' strat <- partition(altmejd, prop = 0.7, strata = "replicate")
#' round(proportions(table(altmejd$replicate)), 3)
#' round(proportions(table(strat$set1$replicate)), 3)
#' round(proportions(table(strat$set2$replicate)), 3)
#'
#' # one proportion per set: 70/15/15 in a single call
#' set.seed(112)
#' three <- partition(altmejd, prop = c(0.7, 0.15, 0.15), strata = "replicate")
#' vapply(three, nrow, integer(1))
#'
#' @export
partition <- function(data, prop = 0.7, strata = NULL) {
  check_numeric(prop)
  check_prop(prop)
  check_df(data)
  data <- as.data.frame(data)

  # one proportion is shorthand for two: `prop` and the rest
  if (length(prop) == 1L) {
    prop <- c(prop, 1 - prop)
  } else if (!isTRUE(all.equal(sum(prop), 1))) {
    stop(
      "`prop` must sum to 1 when it names more than one set. Got ",
      paste(format(prop), collapse = " + "),
      " = ",
      format(sum(prop)),
      ". The last set takes whatever is left, so a shortfall would be handed ",
      "to it silently.",
      call. = FALSE
    )
  }

  if (!is.null(strata)) {
    strata <- as.character(strata)
    if (length(strata) != 1L || !strata %in% names(data)) {
      stop(
        "`strata` must name one column of `data`. Got ",
        paste0("`", paste(strata, collapse = "`, `"), "`"),
        "; `data` has ",
        paste0("`", paste(names(data), collapse = "`, `"), "`"),
        ".",
        call. = FALSE
      )
    }
  }

  # no strata means one stratum holding everything
  stratum <- if (is.null(strata)) rep(1L, nrow(data)) else data[[strata]]
  stratum_rows <- split(seq_len(nrow(data)), stratum, drop = TRUE)
  sizes <- vapply(stratum_rows, length, integer(1))

  assignment <- integer(nrow(data))
  leftovers <- stratum_rows

  # split-major: every stratum's first draw, then its second
  for (j in seq_len(length(prop) - 1L)) {
    for (g in seq_along(leftovers)) {
      # count from the stratum's original size: proportions of the whole
      n_draw <- min(round(sizes[g] * prop[j]), length(leftovers[[g]]))
      if (n_draw > 0L) {
        # sample.int, then index: sample(rows, k) reads a one-row stratum as 1:n
        picks <- sample.int(length(leftovers[[g]]), n_draw)
        assignment[leftovers[[g]][picks]] <- j
        leftovers[[g]] <- leftovers[[g]][-picks]
      }
    }
  }
  # the last set takes what nobody drew, so every row lands exactly once
  assignment[unlist(leftovers, use.names = FALSE)] <- length(prop)

  # numbered, not train/test: which set is which is the caller's call;
  # drop = FALSE: a one-column data frame would otherwise come back a vector
  stats::setNames(
    lapply(seq_along(prop), \(j) data[assignment == j, , drop = FALSE]),
    paste0("set", seq_along(prop))
  )
}
