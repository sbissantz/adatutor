#' @title Split a Data Set into Two or More Parts, Optionally Within Strata
#'
#' @description Draws the given proportions of the rows into one set each and
#'   leaves the remainder in the last. With \code{strata}, the draw happens
#'   separately inside each level of that variable, so its distribution is
#'   preserved in every part.
#'
#' @details \strong{A simple random split is a stratified split with one
#'   stratum.} That is not a slogan here, it is how the function works: with
#'   \code{strata = NULL} every row is put in the same stratum and the ordinary
#'   code path runs. There is no separate branch for the ungrouped case, which
#'   is the clearest way to show that the two splits people talk about as
#'   different procedures are the same procedure with different grouping.
#'
#'   \strong{Why stratify.} A simple random split can land more of a rare
#'   outcome on one side by chance, and with a small data set that shifts the
#'   base rate the model trains against. Stratifying on the outcome holds each
#'   class's share close to what it is in the whole data.
#'
#'   \strong{Rows keep their original order} in every part. A split is a
#'   choice of rows, not a shuffle, and preserving the order means the parts can
#'   be read against the source.
#'
#'   \strong{Proportions are of the whole data, and the last part is the
#'   remainder.} \code{k} proportions give \code{k} sets: the first
#'   \code{k - 1} are drawn at \code{round(n * prop[j])} of each stratum's
#'   original size, and whatever nobody drew becomes the last. Rounding each
#'   count independently would not add up -- for \code{n = 10},
#'   \code{round(10 * c(.7, .15, .15))} asks for 11 rows out of 10 -- so making
#'   the last set the remainder is what keeps every row landing exactly once.
#'
#'   \strong{Positions are sampled, never values.} The obvious way to draw
#'   within a stratum, \code{sample(ids, k)}, walks into R's length-one trap:
#'   when a stratum holds a single row, \code{sample()} reads its argument as
#'   \code{1:n} and returns some other number entirely. Drawing with
#'   \code{sample.int(length(rows), k)} and indexing cannot do that, so a
#'   stratum of one behaves like any other. This is also why no identifier
#'   column is needed: row positions identify rows already, and asking for an id
#'   would quietly assume it holds no duplicates.
#'
#' @param data A data frame to split.
#'
#' @param prop One proportion per set, each between 0 and 1. A single number is
#'   shorthand for two sets, \code{prop} and the rest, and is the only case that
#'   need not sum to one; \code{c(0.7, 0.15, 0.15)} gives three and must.
#'   Defaults to 0.7. Applied within each stratum, and rounded, so a total can
#'   differ from \code{prop * nrow(data)} by a row per stratum.
#'
#' @param strata Optional. The name of a column to stratify on -- usually the
#'   outcome. The default \code{NULL} draws from the whole data set at once.
#'   Named to match \code{strata} in \code{rsample::initial_split()}, so the
#'   idea carries over unchanged.
#'
#' @return A list of as many data frames as \code{prop} has entries, named
#'   \code{set1} to \code{setk}, the last holding whatever was not drawn. They
#'   are numbered rather than named train and test because which set is which is
#'   the caller's decision, not the function's -- a 70/15/15 split can be one
#'   call or two, and either way \code{set2} is only the validation set because
#'   you say so. A proportion of zero gives a zero-row data frame, not
#'   \code{NULL}, so \code{k} proportions always give \code{k} data frames.
#'
#' @seealso \code{\link[adatutor]{altmejd_splits}}, the split this package ships
#'   so a reader's numbers match the tutorial's.
#'
#' @examples
#' data(altmejd)
#'
#' # A simple 70/30 split
#' set.seed(112)
#' plain <- partition(altmejd, prop = 0.7)
#' nrow(plain$set1)
#' nrow(plain$set2)
#'
#' # The same, holding the outcome's distribution steady in both halves
#' set.seed(112)
#' strat <- partition(altmejd, prop = 0.7, strata = "replicate")
#' round(proportions(table(altmejd$replicate)), 3)
#' round(proportions(table(strat$set1$replicate)), 3)
#' round(proportions(table(strat$set2$replicate)), 3)
#'
#' # One proportion per set: 70/15/15 in a single call
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
    msg <- "`prop` must sum to 1 when it names more than one set. "
    sug <- paste0(
      "Got ",
      paste(format(prop), collapse = " + "),
      " = ",
      format(sum(prop)),
      ". The last set takes whatever is left, so a shortfall would be handed ",
      "to it silently."
    )
    stop(c(msg, sug), call. = FALSE)
  }

  if (!is.null(strata)) {
    strata <- as.character(strata)
    if (length(strata) != 1L || !strata %in% names(data)) {
      msg <- paste0("`strata` must name one column of `data`. ")
      sug <- paste0(
        "Got ",
        paste0("`", paste(strata, collapse = "`, `"), "`"),
        "; `data` has ",
        paste0("`", paste(names(data), collapse = "`, `"), "`"),
        "."
      )
      stop(c(msg, sug), call. = FALSE)
    }
  }

  # no strata means one stratum holding everything
  key <- if (is.null(strata)) rep(1L, nrow(data)) else data[[strata]]
  rows <- split(seq_len(nrow(data)), key, drop = TRUE)
  sizes <- vapply(rows, length, integer(1))

  set_of <- integer(nrow(data))
  left <- rows

  # split-major: every stratum's first draw, then its second
  for (j in seq_len(length(prop) - 1L)) {
    for (g in seq_along(left)) {
      # count from the stratum's original size: proportions of the whole
      k <- min(round(sizes[g] * prop[j]), length(left[[g]]))
      if (k > 0L) {
        # sample.int, then index: sample(rows, k) reads a one-row stratum as 1:n
        take <- sample.int(length(left[[g]]), k)
        set_of[left[[g]][take]] <- j
        left[[g]] <- left[[g]][-take]
      }
    }
  }
  # the last set takes what nobody drew, so every row lands exactly once
  set_of[unlist(left, use.names = FALSE)] <- length(prop)

  # drop = FALSE: a one-column data frame would otherwise come back a vector
  stats::setNames(
    lapply(seq_along(prop), function(j) data[set_of == j, , drop = FALSE]),
    paste0("set", seq_along(prop))
  )
}
