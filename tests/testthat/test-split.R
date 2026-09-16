test_that("partition() splits without strata", {
  set.seed(112)
  split <- partition(altmejd, prop = 0.7)
  expect_length(split, 2)
  expect_equal(dim(split$set1), c(106, 7))
  expect_equal(dim(split$set2), c(46, 7))
})

test_that("partition() splits within strata", {
  set.seed(112)
  split <- partition(altmejd, prop = 0.7, strata = "replicate")
  expect_length(split, 2)
  expect_equal(dim(split$set1), c(107, 7))
  expect_equal(dim(split$set2), c(45, 7))

  # the point of stratifying: each class keeps its share
  whole <- proportions(table(altmejd$replicate))
  drawn <- proportions(table(split$set1$replicate))
  expect_lt(max(abs(whole - drawn)), 0.02)
})

test_that("a one-row stratum is handled", {
  # sample(x, k) reads a length-one x as 1:x, so the old implementation drew a
  # number that was not a row at all: the lone row landed in set1 on 1 of 200
  # seeds where it should land there every time.
  tiny <- data.frame(
    eid = c(101L, 102L, 103L, 104L, 999L),
    grp = c("a", "a", "a", "a", "b"),
    stringsAsFactors = FALSE
  )
  landed <- vapply(
    1:200,
    function(s) {
      set.seed(s)
      999L %in% partition(tiny, prop = 0.7, strata = "grp")$set1$eid
    },
    logical(1)
  )
  expect_true(all(landed))
})

test_that("partition() is the hand-written listings", {
  # this is what the tutorial claims, so it is worth asserting rather than
  # asserting something weaker about sizes
  d <- altmejd

  set.seed(112)
  n_train <- round(nrow(d) * 0.7)
  by_hand <- d[seq_len(nrow(d)) %in% sample(seq_len(nrow(d)), n_train), ]
  set.seed(112)
  expect_identical(partition(d, prop = 0.7)$set1, by_hand)

  set.seed(112)
  sp <- split(seq_len(nrow(d)), d$replicate)
  drawn <- unlist(
    lapply(sp, function(i) i[sample.int(length(i), round(length(i) * 0.7))]),
    use.names = FALSE
  )
  strat_hand <- d[seq_len(nrow(d)) %in% drawn, ]
  set.seed(112)
  expect_identical(
    partition(d, prop = 0.7, strata = "replicate")$set1,
    strat_hand
  )
})

test_that("partition() reproduces the split the package ships", {
  # Listing 9 re-seeds before the stratified call, so it now lands on the
  # canonical split rather than one shifted by the preceding draw
  set.seed(112)
  expect_identical(
    partition(altmejd, prop = 0.7, strata = "replicate")$set1,
    altmejd_splits$train
  )
})

test_that("both halves keep the data's row order and lose nothing", {
  set.seed(112)
  p <- partition(altmejd, prop = 0.7, strata = "replicate")
  for (half in list(p$set1, p$set2)) {
    expect_false(is.unsorted(match(rownames(half), rownames(altmejd))))
  }
  expect_equal(nrow(p$set1) + nrow(p$set2), nrow(altmejd))
  expect_setequal(c(p$set1$eid, p$set2$eid), altmejd$eid)
})

test_that("partition() does not consult an id column", {
  # the old implementation matched sampled ids with %in%, which silently
  # assumed they were unique
  dup <- altmejd
  dup$eid <- 1L
  set.seed(112)
  p <- partition(dup, prop = 0.7, strata = "replicate")
  expect_equal(nrow(p$set1), 107)
  expect_equal(nrow(p$set1) + nrow(p$set2), nrow(dup))
})

test_that("partition() validates its arguments", {
  expect_error(partition(altmejd, prop = 0.7, strata = "nope"), "strata")
  expect_error(partition("not a data frame", prop = 0.7))
  expect_error(partition(altmejd, prop = 3))
})

# ---- prop as a vector -------------------------------------------------------

test_that("one proportion is shorthand for two", {
  set.seed(112)
  one <- partition(altmejd, prop = 0.7)
  set.seed(112)
  two <- partition(altmejd, prop = c(0.7, 0.3))
  expect_identical(one, two)
})

test_that("a vector of proportions gives that many sets", {
  set.seed(112)
  p <- partition(altmejd, prop = c(0.7, 0.15, 0.15), strata = "replicate")

  expect_identical(names(p), c("set1", "set2", "set3"))
  expect_identical(
    vapply(p, nrow, integer(1)),
    c(set1 = 107L, set2 = 23L, set3 = 22L)
  )
  # every row lands exactly once, which is what making the last set the
  # remainder guarantees -- round(n * prop) three times would not
  ids <- unlist(lapply(p, `[[`, "eid"), use.names = FALSE)
  expect_setequal(ids, altmejd$eid)
  expect_false(as.logical(anyDuplicated(ids)))
})

test_that("set1 does not move when more proportions are added", {
  # split-major draw order is what buys this: drawing stratum-major would put
  # each later stratum's first draw at a different point in the stream
  set.seed(112)
  two <- partition(altmejd, prop = 0.7, strata = "replicate")
  set.seed(112)
  three <- partition(altmejd, prop = c(0.7, 0.15, 0.15), strata = "replicate")

  expect_identical(three$set1, two$set1)
  expect_identical(three$set1, altmejd_splits$train)
})

test_that("the rows always add up, where naive rounding would not", {
  # round(n * c(.7, .15, .15)) sums to 11 for n = 10 and to 29 for n = 30
  for (n in c(10L, 17L, 30L)) {
    set.seed(1)
    s <- partition(data.frame(x = seq_len(n)), prop = c(0.7, 0.15, 0.15))
    expect_equal(sum(vapply(s, nrow, integer(1))), n)
  }
})

test_that("a vector prop must be a simplex", {
  # the last set is the remainder, so a shortfall would be handed to it in
  # silence rather than reported
  expect_error(partition(altmejd, prop = c(0.7, 0.15)), "sum to 1")
  expect_error(partition(altmejd, prop = c(0.7, 0.4)), "sum to 1")
  expect_error(partition(altmejd, prop = c(-0.1, 1.1)), "between 0 and 1")

  # and it no longer leaks R's "the condition has length > 1"
  msg <- tryCatch(
    partition(altmejd, prop = c(0.7, 0.15)),
    error = conditionMessage
  )
  expect_false(grepl("condition has length", msg))
})

test_that("a zero proportion gives an empty set, silently", {
  # deliberate: k proportions always give k data frames, so a programmatically
  # built `prop` needs no pre-filtering. A message could not tell a typo from
  # an intended zero, and nrow(set3) already says it.
  set.seed(112)
  expect_no_message(p <- partition(altmejd, prop = c(0.7, 0.3, 0)))

  expect_true(is.data.frame(p$set3))
  expect_equal(nrow(p$set3), 0L)
  expect_identical(names(p$set3), names(altmejd))
  expect_equal(sum(vapply(p, nrow, integer(1))), nrow(altmejd))
})

test_that("proportions that only look like floating-point trouble are fine", {
  # sum() accumulates in long double and rounds once, so this is exactly 1
  expect_no_error(partition(altmejd, prop = c(0.1, 0.2, 0.7)))
})
