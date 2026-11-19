test_that("hypergrid holds one table per metric, a row per setting", {
  data(hypergrid)
  projects <- c("eerp", "ml1", "ml2", "ml3", "rpp", "ssrp")
  expect_named(hypergrid, list_metrics())
  for (table in hypergrid) {
    expect_named(
      table,
      c("depth", "n_iter", "eta", projects, "mean", "sd", "min", "max")
    )
    expect_equal(nrow(table), 1320)
    expect_false(anyDuplicated(table[c("depth", "n_iter", "eta")]) > 0)
  }
  expect_identical(hypergrid$auroc[1:3], hypergrid$patk[1:3])
})

test_that("hypergrid holds depths 1 to 4, 15 rates and 22 round counts", {
  data(hypergrid)
  auc <- hypergrid$auroc
  expect_equal(sort(unique(auc$depth)), 1:4)
  expect_equal(length(unique(auc$eta)), 15)
  expect_equal(range(auc$eta), c(0.0025, 1))
  expect_equal(length(unique(auc$n_iter)), 22)
  expect_equal(range(auc$n_iter), c(10, 3000))
  expect_equal(as.vector(table(auc$depth)), rep(15 * 22, 4))
})

test_that("hypergrid's rows run n_iter fastest, then eta, then depth", {
  data(hypergrid)
  for (metric in names(hypergrid)) {
    table <- hypergrid[[metric]]
    # the grid of data-raw/hypergrid.R, rebuilt from the table's own values
    grid <- expand.grid(
      n_iter = sort(unique(table$n_iter)),
      eta = sort(unique(table$eta)),
      depth = sort(unique(table$depth))
    )
    for (column in c("depth", "n_iter", "eta")) {
      expect_identical(
        table[[column]],
        grid[[column]],
        label = paste(metric, column)
      )
    }
    expect_identical(
      rownames(table),
      as.character(seq_len(nrow(table))),
      label = paste(metric, "row names")
    )
  }
})

test_that("hypergrid's projects are altmext without HOLZ", {
  data(hypergrid)
  data(altmext)
  tune <- altmext[altmext$pid != "holz", ]
  info <- attr(hypergrid, "projects")
  expect_identical(info$project, sort(unique(tune$pid)))
  expect_equal(info$n, as.vector(table(tune$pid)[info$project]))
  expect_equal(
    info$base_rate,
    as.vector(tapply(tune$replicate == "success", tune$pid, mean)[info$project])
  )
})

test_that("hypergrid's mean, sd, min and max summarize the projects", {
  data(hypergrid)
  projects <- attr(hypergrid, "projects")$project
  for (metric in names(hypergrid)) {
    table <- hypergrid[[metric]]
    scores <- as.matrix(table[projects])
    expect_equal(table$mean, apply(scores, 1, mean), label = metric)
    expect_equal(table$sd, apply(scores, 1, sd), label = metric)
    expect_equal(table$min, apply(scores, 1, min), label = metric)
    expect_equal(table$max, apply(scores, 1, max), label = metric)
  }

  # An NA project makes all four NA: ML3 in every row of patk_5, and only there
  summaries <- c("mean", "sd", "min", "max")
  expect_true(all(is.na(hypergrid$patk_5$ml3)))
  expect_true(all(is.na(hypergrid$patk_5[summaries])))
  for (metric in setdiff(names(hypergrid), "patk_5")) {
    expect_false(anyNA(hypergrid[[metric]][summaries]), label = metric)
  }
  expect_length(hypergrid, 17)
})

test_that("hypergrid's AUROC is complete and between 0 and 1", {
  data(hypergrid)
  scores <- as.matrix(hypergrid$auroc[attr(hypergrid, "projects")$project])
  expect_false(anyNA(scores))
  expect_true(all(scores >= 0 & scores <= 1))
})

test_that("a stored hypergrid score matches a direct fit", {
  data(hypergrid)
  data(altmext)
  predictors <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  tune <- altmext[altmext$pid != "holz", ]
  train <- tune[tune$pid != "ml3", c(predictors, "replicate")]
  test <- tune[tune$pid == "ml3", ]
  h <- rpart::rpart(replicate ~ ., data = train, maxdepth = 1, model = TRUE)
  fit <- adaboost(h, n_iter = 10, eta = 1, verbose = FALSE)
  margin <- predict(fit, test[, predictors], type = "margin", verbose = FALSE)
  auc <- hypergrid$auroc
  stored <- auc$ml3[auc$depth == 1 & auc$n_iter == 10 & auc$eta == 1]
  expect_equal(stored, auroc(test$replicate, margin))
})

test_that("tune_logo_cv() reproduces stored settings on every metric", {
  data(hypergrid)
  data(altmext)
  tune <- altmext[altmext$pid != "holz", ]
  projects <- attr(hypergrid, "projects")$project
  recipe <- rpart::rpart(
    replicate ~ power.o + effect_size.o + n.o + p_value.o,
    data = tune,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 10, eta = 1, verbose = FALSE)

  # three settings drawn among those up to 100 rounds, which keeps it fast
  settings <- hypergrid$auroc[c("depth", "n_iter", "eta")]
  set.seed(2026)
  rows <- sample(which(settings$n_iter <= 100), 3)
  res <- tune_logo_cv(recipe, settings[rows, ], data = tune, group = "pid")

  for (metric in names(hypergrid)) {
    expect_equal(
      unname(res$estimates[, projects, metric]),
      unname(as.matrix(hypergrid[[metric]][rows, projects])),
      label = metric
    )
  }
})

# ---- summary() ---------------------------------------------------------------

# A table with lower, the mean minus the SD, right after sd, as summary()
# returns it
add_lower_by_hand <- function(table) {
  at <- match("sd", names(table))
  cbind(
    table[seq_len(at)],
    lower = table$mean - table$sd,
    table[-seq_len(at)]
  )
}

test_that("hypergrid is a list of class hypergrid around the 17 tables", {
  data(hypergrid)
  expect_identical(class(hypergrid), c("hypergrid", "list"))

  plain <- unclass(hypergrid)
  expect_identical(class(plain), "list")
  expect_named(plain, list_metrics())
  expect_true(all(vapply(plain, is.data.frame, logical(1))))
  expect_identical(names(attributes(plain)), c("names", "projects"))
  expect_named(attr(plain, "projects"), c("project", "n", "base_rate"))
  expect_equal(nrow(attr(plain, "projects")), 6)
})

test_that("summary() over every setting gives the five highest lower", {
  data(hypergrid)
  top <- summary(hypergrid)
  expect_s3_class(top, "data.frame")
  expect_equal(nrow(top), 5)
  expect_named(
    top,
    c(
      "depth",
      "n_iter",
      "eta",
      "eerp",
      "ml1",
      "ml2",
      "ml3",
      "rpp",
      "ssrp",
      "mean",
      "sd",
      "lower",
      "min",
      "max"
    )
  )
  expect_identical(top$lower, top$mean - top$sd)

  # the rows are the grid's own, under their row numbers
  expect_identical(rownames(top), c("438", "275", "153", "174", "194"))
  auc <- hypergrid$auroc
  expect_identical(top[names(auc)], auc[rownames(top), ])

  # the first row is the tutorial's choice
  expect_equal(top$depth[1], 2)
  expect_equal(top$n_iter[1], 2000)
  expect_equal(top$eta[1], 0.02)
  # no setting has a higher lower, and none ties it
  lower <- auc$mean - auc$sd
  expect_identical(top$lower[1], max(lower))
  expect_equal(sum(lower == max(lower)), 1)
  expect_true(all(diff(top$lower) <= 0))
})

test_that("summary() over every setting equals order() on -lower", {
  data(hypergrid)
  ranked <- add_lower_by_hand(hypergrid$auroc)
  ranked <- ranked[order(-ranked$lower), ]

  expect_identical(summary(hypergrid), ranked[1:5, ])
  expect_identical(summary(hypergrid, n = 1), ranked[1, ])
  expect_identical(summary(hypergrid, n = 3), ranked[1:3, ])
  expect_identical(summary(hypergrid, n = 3L), ranked[1:3, ])
  # more rows than the grid has, or Inf, give every row
  expect_identical(summary(hypergrid, n = nrow(ranked)), ranked)
  expect_identical(summary(hypergrid, n = 1e6), ranked)
  expect_identical(summary(hypergrid, n = Inf), ranked)

  # rows 153 and 174 tie exactly on lower and keep their grid order
  expect_identical(ranked["153", "lower"], ranked["174", "lower"])
  expect_identical(rownames(ranked)[3:4], c("153", "174"))
})

test_that("summary() ranks any metric, by any columns of its table", {
  data(hypergrid)
  bacc <- add_lower_by_hand(hypergrid$bacc)
  expect_identical(
    summary(hypergrid, "bacc"),
    bacc[order(-bacc$lower), ][1:5, ]
  )
  expect_identical(
    summary(hypergrid, "bacc", sort_by = "mean", n = 4),
    bacc[order(-bacc$mean), ][1:4, ]
  )
  # a factor acts as its name; its code, 2, would pick tn
  expect_identical(
    summary(hypergrid, factor("bacc", levels = c("auroc", "bacc"))),
    summary(hypergrid, "bacc")
  )

  auc <- add_lower_by_hand(hypergrid$auroc)
  expect_identical(
    summary(hypergrid, sort_by = c("min", "lower")),
    auc[order(-auc$min, -auc$lower), ][1:5, ]
  )
  # a project, as any column of the table
  expect_identical(
    summary(hypergrid, sort_by = "rpp", n = 3),
    auc[order(-auc$rpp), ][1:3, ]
  )
  # NULL keeps the grid order
  expect_identical(summary(hypergrid, sort_by = NULL, n = 3), auc[1:3, ])

  # patk_5 has no summaries, so nothing moves
  patk <- summary(hypergrid, "patk_5")
  expect_identical(rownames(patk), as.character(1:5))
  expect_true(all(is.na(patk$lower)))
})

test_that("summary() keeps grid order among ties and puts NA last", {
  # rows 2, 3 and 5 tie on every column, row 6 ties them on lower only, and
  # row 4 lacks its sd; binary fractions, so equal differences are equal
  grid <- structure(
    list(
      auroc = data.frame(
        depth = 1:6,
        mean = c(0.625, 0.75, 0.75, 0.875, 0.75, 0.875),
        sd = c(0.125, 0.125, 0.125, NA, 0.125, 0.25),
        min = c(0.5, 0.5, 0.5, NA, 0.5, 0.375),
        max = 1
      )
    ),
    class = c("hypergrid", "list")
  )

  expect_identical(
    rownames(summary(grid, n = Inf)),
    c("2", "3", "5", "6", "1", "4")
  )
  expect_identical(
    rownames(summary(grid, sort_by = c("lower", "mean"), n = Inf)),
    c("6", "2", "3", "5", "1", "4")
  )
  expect_identical(
    rownames(summary(grid, sort_by = "min", n = Inf)),
    c("1", "2", "3", "5", "6", "4")
  )
  expect_identical(rownames(summary(grid, n = 3)), c("2", "3", "5"))
  expect_identical(summary(grid, n = 1)$depth, 2L)
})

test_that("summary() puts lower right after sd in a hand-made grid", {
  grid <- structure(
    list(
      auroc = data.frame(depth = 1, mean = 0.75, sd = 0.125, min = 0.5, max = 1)
    ),
    class = c("hypergrid", "list")
  )
  top <- summary(grid)
  expect_named(top, c("depth", "mean", "sd", "lower", "min", "max"))
  expect_identical(rownames(top), "1")
  expect_identical(top$lower, 0.625)
})

test_that("summary() needs one metric of the grid", {
  data(hypergrid)
  for (bad in list("nope", c("auroc", "bacc"), NA_character_, NULL, 1)) {
    expect_error(
      summary(hypergrid, metric = bad),
      "`metric` must be a single name among the metrics of the grid: tp, tn,",
      fixed = TRUE
    )
  }
})

test_that("summary() rejects a sort_by it cannot use and names the columns", {
  data(hypergrid)
  columns <- paste(
    "depth, n_iter, eta, eerp, ml1, ml2, ml3, rpp, ssrp,",
    "mean, sd, lower, min, max."
  )
  # a factor would pick a column by its code
  bad_sort_by <- list(
    "nope",
    NA_character_,
    c("lower", "lower"),
    character(0),
    1,
    factor("lower")
  )
  for (bad in bad_sort_by) {
    expect_error(summary(hypergrid, sort_by = bad), columns, fixed = TRUE)
  }
})

test_that("summary() needs n to be one whole number of 1 or more", {
  data(hypergrid)
  bad_n <- list(0, -1, 2.5, -Inf, NaN, NA_real_, NA, "5", TRUE, c(1, 2), NULL)
  for (bad in bad_n) {
    expect_error(
      summary(hypergrid, n = bad),
      "`n` must be one whole number, 1 or more, or `Inf`.",
      fixed = TRUE
    )
  }
})

test_that("summary() stops on an argument it does not take", {
  data(hypergrid)
  takes <- paste(
    " summary() for a hypergrid takes `metric`, `sort_by`, `n`,",
    "`depth` and `by`."
  )
  expect_error(
    summary(hypergrid, n_top = 3),
    paste0("Unused argument(s): `n_top`.", takes),
    fixed = TRUE
  )
  expect_error(
    summary(hypergrid, metrc = "bacc"),
    paste0("Unused argument(s): `metrc`.", takes),
    fixed = TRUE
  )
  expect_error(
    summary(hypergrid, N = 3),
    paste0("Unused argument(s): `N`.", takes),
    fixed = TRUE
  )
  # the argument that left with the old rule is now unknown
  expect_error(
    summary(hypergrid, unbeaten = TRUE),
    paste0("Unused argument(s): `unbeaten`.", takes),
    fixed = TRUE
  )
  # the sixth value by position is by, so the extra one is the seventh
  expect_error(
    summary(hypergrid, "auroc", "lower", 3, 1, "depth", "x"),
    paste0("Unused argument(s): an unnamed value.", takes),
    fixed = TRUE
  )
  expect_error(
    summary(hypergrid, "auroc", "lower", 3, NULL, NULL, "x", n_top = 3),
    paste0("Unused argument(s): an unnamed value, `n_top`.", takes),
    fixed = TRUE
  )
  # the five arguments the message names are the ones the method takes
  expect_identical(
    names(formals(summary.hypergrid)),
    c("object", "metric", "sort_by", "n", "depth", "by", "...")
  )
})

# ---- summary(): depth and by -------------------------------------------------

# A hypergrid of one table built from mean, sd and min; lower = mean - sd.
# The values are binary fractions, so equal differences are exactly equal.
make_grid <- function(mean, sd, min) {
  structure(
    list(
      auroc = data.frame(
        depth = seq_along(mean),
        mean = mean,
        sd = sd,
        min = min,
        max = 1
      )
    ),
    class = c("hypergrid", "list")
  )
}

# As make_grid(), with the depth of each row given
make_depth_grid <- function(depth, mean, sd, min) {
  grid <- make_grid(mean, sd, min)
  grid$auroc$depth <- depth
  grid
}

# The best row of each value of `by` in base R alone: split the rows by value,
# take the first of each in the order of `sort_by`, and sort those, kept in
# grid order, the same way
pick_best <- function(table, by, sort_by) {
  rank_rows <- function(rows) {
    keys <- unname(lapply(table[rows, sort_by, drop = FALSE], \(x) -x))
    rows[do.call(order, keys)]
  }
  groups <- split(seq_len(nrow(table)), table[[by]])
  best <- sort(vapply(groups, \(rows) rank_rows(rows)[1], integer(1)))
  table[rank_rows(unname(best)), ]
}

test_that("summary() with depth ranks the settings of those depths", {
  data(hypergrid)
  auc <- add_lower_by_hand(hypergrid$auroc)
  ranked <- auc[order(-auc$lower), ]

  stumps <- summary(hypergrid, depth = 1)
  expect_identical(rownames(stumps), c("275", "153", "174", "194", "214"))
  expect_identical(stumps, ranked[ranked$depth == 1, ][1:5, ])
  expect_identical(summary(hypergrid, depth = 1L), stumps)

  # several depths share one ranking, whatever the order they are given in
  some <- summary(hypergrid, depth = c(1, 3), n = Inf)
  expect_identical(some, ranked[ranked$depth %in% c(1, 3), ])
  expect_true(is.unsorted(some$depth) && is.unsorted(rev(some$depth)))
  expect_identical(summary(hypergrid, depth = c(3, 1), n = Inf), some)
  expect_identical(summary(hypergrid, depth = c(1, 3, 1), n = Inf), some)
  expect_identical(summary(hypergrid, depth = c(1, 3), n = 4), some[1:4, ])

  # every depth is no filter at all
  expect_identical(summary(hypergrid, depth = 1:4, n = Inf), ranked)
  expect_identical(summary(hypergrid, depth = 1:4), summary(hypergrid))

  # NULL keeps the grid order within the depths
  expect_identical(
    summary(hypergrid, depth = 2, sort_by = NULL, n = 3),
    auc[auc$depth == 2, ][1:3, ]
  )

  # another metric and sort_by
  bacc <- add_lower_by_hand(hypergrid$bacc)
  kept <- bacc[bacc$depth %in% c(2, 4), ]
  expect_identical(
    summary(hypergrid, "bacc", sort_by = "mean", depth = c(2, 4), n = Inf),
    kept[order(-kept$mean), ]
  )
})

test_that("summary() needs depth to be depths of the grid", {
  data(hypergrid)
  message <- paste(
    "`depth` must be `NULL` or one or more depths of the grid:",
    "1, 2, 3, 4."
  )
  bad_depth <- list(
    0,
    5,
    8,
    -1,
    1.5,
    Inf,
    c(1, 2, 9),
    NA,
    NA_real_,
    NaN,
    c(1, NA),
    numeric(0),
    integer(0),
    "1",
    TRUE,
    factor(1),
    list(1)
  )
  for (bad in bad_depth) {
    expect_error(summary(hypergrid, depth = bad), message, fixed = TRUE)
  }
  # a hand-made grid names its own depths
  grid <- make_grid(mean = c(0.75, 0.875), sd = 0.125, min = 0.5)
  expect_error(
    summary(grid, depth = 3),
    "`depth` must be `NULL` or one or more depths of the grid: 1, 2.",
    fixed = TRUE
  )
})

test_that("summary() with by keeps the best setting of each value", {
  data(hypergrid)
  for (metric in c("auroc", "tp", "bacc", "patk_5")) {
    table <- add_lower_by_hand(hypergrid[[metric]])
    for (by in c("depth", "n_iter", "eta")) {
      label <- paste(metric, by)
      best <- pick_best(table, by, "lower")
      expect_identical(summary(hypergrid, metric, by = by), best, label = label)
      expect_equal(nrow(best), length(unique(table[[by]])), label = label)
      expect_identical(
        summary(hypergrid, metric, sort_by = c("mean", "sd"), by = by),
        pick_best(table, by, c("mean", "sd")),
        label = label
      )
      # a project ties often, so grid order decides many groups
      expect_identical(
        summary(hypergrid, metric, sort_by = "ml3", by = by),
        pick_best(table, by, "ml3"),
        label = label
      )
    }
  }
})

test_that("summary() gives the best setting of each depth", {
  data(hypergrid)
  best <- summary(hypergrid, depth = 1:4, by = "depth")
  expect_identical(rownames(best), c("438", "275", "691", "1015"))
  expect_equal(best$depth, c(2, 1, 3, 4))
  expect_equal(round(best$lower, 3), c(0.674, 0.672, 0.662, 0.658))
  expect_true(all(diff(best$lower) < 0))
  expect_identical(summary(hypergrid, by = "depth"), best)
  # the first row is the tutorial's choice
  expect_identical(best[1, ], summary(hypergrid)[1, ])
  expect_equal(best$n_iter[1], 2000)
  expect_equal(best$eta[1], 0.02)
})

test_that("summary() applies by to the depths it keeps", {
  data(hypergrid)
  auc <- add_lower_by_hand(hypergrid$auroc)
  kept <- auc[auc$depth %in% c(1, 4), ]
  expect_identical(
    summary(hypergrid, depth = c(1, 4), by = "n_iter"),
    pick_best(kept, "n_iter", "lower")
  )
  expect_identical(
    summary(hypergrid, depth = c(1, 4), sort_by = "mean", by = "eta"),
    pick_best(kept, "eta", "mean")
  )
  # one row for each depth kept
  expect_equal(
    sort(summary(hypergrid, depth = c(1, 3, 4), by = "depth")$depth),
    c(1, 3, 4)
  )
  expect_identical(
    summary(hypergrid, depth = c(1, 3, 4), by = "depth"),
    summary(hypergrid, by = "depth")[c("275", "691", "1015"), ]
  )
})

test_that("summary() returns every value with by unless n is given", {
  data(hypergrid)
  auc <- hypergrid$auroc
  expect_equal(nrow(summary(hypergrid)), 5)
  expect_equal(nrow(summary(hypergrid, depth = 3)), 5)
  expect_equal(nrow(summary(hypergrid, sort_by = NULL)), 5)
  for (by in c("depth", "n_iter", "eta")) {
    every <- summary(hypergrid, by = by)
    expect_equal(nrow(every), length(unique(auc[[by]])), label = by)
    expect_identical(summary(hypergrid, by = by, n = Inf), every)
    expect_identical(summary(hypergrid, by = by, n = 100), every)
    expect_identical(summary(hypergrid, by = by, n = 3), every[1:3, ])
    expect_identical(summary(hypergrid, by = by, n = 1), every[1, ])
  }
  expect_equal(nrow(summary(hypergrid, by = "depth")), 4)
  expect_equal(nrow(summary(hypergrid, by = "n_iter")), 22)
  expect_equal(nrow(summary(hypergrid, by = "eta")), 15)

  # the three best depths, not the three best settings, two of which are
  # stumps
  top <- summary(hypergrid, by = "depth", n = 3)
  expect_identical(rownames(top), c("438", "275", "691"))
  expect_equal(top$depth, c(2, 1, 3))
  expect_equal(summary(hypergrid, n = 3)$depth, c(2, 1, 1))
})

test_that("summary() with by keeps the first of tied settings in grid order", {
  # lower is mean - sd: rows 2 to 5 and 8 tie on it; rows 3 and 5 tie on
  # every column, as do rows 2 and 4; rows 7 and 8 tie on min only
  grid <- make_depth_grid(
    depth = c(1, 2, 1, 2, 1, 2, 3, 3),
    mean = c(0.75, 0.875, 0.875, 0.875, 0.875, 0.75, 0.75, 0.875),
    sd = 0.125,
    min = c(0.5, 0.625, 0.625, 0.625, 0.625, 0.625, 0.75, 0.75)
  )
  expect_identical(rownames(summary(grid, by = "depth")), c("2", "3", "8"))
  expect_identical(
    rownames(summary(grid, sort_by = "min", by = "depth")),
    c("7", "2", "3")
  )
  expect_identical(
    rownames(summary(grid, sort_by = c("min", "lower"), by = "depth")),
    c("8", "2", "3")
  )
  expect_identical(
    rownames(summary(grid, by = "depth", n = 2)),
    c("2", "3")
  )
})

test_that("summary() with by puts a value whose rows are all NA last", {
  # row 1 lacks min, rows 3 and 4 lack everything
  grid <- make_depth_grid(
    depth = c(1, 1, 2, 2, 3),
    mean = c(0.875, 0.875, NA, NA, 0.75),
    sd = c(0.125, 0.125, NA, NA, 0.125),
    min = c(NA, 0.5, NA, NA, 0.625)
  )
  expect_no_warning(top <- summary(grid, by = "depth"))
  # on lower, rows 1 and 2 tie, so row 1 is the best of depth 1
  expect_identical(rownames(top), c("1", "5", "3"))
  expect_identical(
    rownames(summary(grid, sort_by = "min", by = "depth")),
    c("5", "2", "3")
  )
})

test_that("summary() needs by to be one hyperparameter and a sort_by", {
  data(hypergrid)
  expect_error(
    summary(hypergrid, by = "depth", sort_by = NULL),
    "`by` needs a `sort_by` to tell the best setting.",
    fixed = TRUE
  )
  message <- paste(
    "`by` must be `NULL` or one hyperparameter of the grid:",
    "depth, n_iter, eta."
  )
  bad_by <- list(
    "nope",
    "min",
    "lower",
    "ml3",
    "Depth",
    NA_character_,
    NA,
    c("depth", "eta"),
    character(0),
    1,
    TRUE,
    factor("depth"),
    list("depth")
  )
  for (bad in bad_by) {
    expect_error(summary(hypergrid, by = bad), message, fixed = TRUE)
  }
  # a hand-made grid has depth only
  grid <- make_grid(mean = c(0.75, 0.875), sd = 0.125, min = 0.5)
  expect_error(
    summary(grid, by = "n_iter"),
    "`by` must be `NULL` or one hyperparameter of the grid: depth.",
    fixed = TRUE
  )
})
