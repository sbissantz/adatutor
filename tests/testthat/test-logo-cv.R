fml <- replicate ~ power.o + effect_size.o + n.o + p_value.o

# a boosted tree on altmejd; `...` reaches the rpart controls, e.g. cp
fit_ada <- function(n_iter = 5, eta = 1, depth = 1, ...) {
  h <- rpart::rpart(
    fml,
    data = altmejd,
    method = "class",
    model = TRUE,
    maxdepth = depth,
    ...
  )
  adaboost(h, n_iter = n_iter, eta = eta, verbose = FALSE)
}

test_that("logo_cv() returns a setting x project x metric cube", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")

  expect_s3_class(res, "logo_cv")
  expect_named(
    res,
    c(
      "estimates",
      "grid",
      "projects",
      "scores",
      "model",
      "nested",
      "criterion",
      "selected"
    )
  )

  n_measures <- length(assess(c(1, 0), c(1, -1)))
  expect_equal(dim(res$estimates), c(1L, 5L, n_measures))
  expect_named(dimnames(res$estimates), c("setting", "project", "metric"))
  expect_setequal(
    dimnames(res$estimates)$project,
    c("eerp", "ml1", "ml3", "rpp", "ssrp")
  )
  expect_equal(dimnames(res$estimates)$metric, names(assess(c(1, 0), c(1, -1))))

  # every fold is held out exactly once, so the fold sizes must total the data
  expect_equal(sum(res$projects$n), nrow(altmejd))
  expect_setequal(res$projects$project, dimnames(res$estimates)$project)
})

test_that("as.data.frame() flattens the cube without losing a cell", {
  data(altmejd)
  grid <- expand.grid(n_iter = c(5, 10), eta = c(1, 0.5))
  res <- tune_logo_cv(fit_ada(), grid, data = altmejd, group = "pid")

  d <- as.data.frame(res)
  expect_equal(nrow(d), length(res$estimates))
  expect_named(
    d,
    c(
      "model",
      "setting",
      "depth",
      "n_iter",
      "eta",
      "project",
      "n",
      "base_rate",
      "metric",
      "estimate"
    )
  )

  # every long row must point back at the cell it came from
  cell <- cbind(
    match(as.character(d$setting), dimnames(res$estimates)$setting),
    match(d$project, dimnames(res$estimates)$project),
    match(d$metric, dimnames(res$estimates)$metric)
  )
  expect_equal(d$estimate, res$estimates[cell])
})

test_that("logo_cv() reproduces the published leave-project-out figures", {
  # Pins the fold logic against notes 2: AdaBoost stumps, n_iter = 10, eta = 1.
  # A mismatch here means the folds are wrong, not that the table is.
  # eerp is 0.734 rather than the 0.714 of notes 2 because the base learners
  # now carry rpart's cp = 0.01; the other four folds are unmoved.
  data(altmejd)
  res <- logo_cv(fit_ada(n_iter = 10), data = altmejd, group = "pid")
  got <- res$estimates[1, , "auroc"]

  want <- c(eerp = 0.734, ml1 = 0.818, ml3 = 0.619, rpp = 0.620, ssrp = 0.676)
  expect_equal(
    round(got[names(want)], 3),
    want,
    tolerance = 1e-3
  )
})

test_that("AdaBoost is scored on the margin, not on class labels", {
  # The submitted manuscript's bug: hard labels make auroc collapse onto
  # balanced accuracy. logo_cv() must never reintroduce it.
  data(altmejd)
  res <- logo_cv(fit_ada(n_iter = 10), data = altmejd, group = "pid")
  d <- as.data.frame(res)

  auroc <- d$estimate[d$metric == "auroc"]
  bacc <- d$estimate[d$metric == "bacc"]
  expect_false(isTRUE(all.equal(auroc, bacc)))
})

test_that("a grid is crossed with every fold", {
  data(altmejd)
  grid <- expand.grid(n_iter = c(5, 10), eta = c(1, 0.5))
  res <- tune_logo_cv(fit_ada(), grid, data = altmejd, group = "pid")

  n_measures <- length(assess(c(1, 0), c(1, -1)))
  expect_equal(dim(res$estimates), c(nrow(grid), 5L, n_measures))

  d <- as.data.frame(res)
  auroc <- d[d$metric == "auroc", ]
  expect_equal(nrow(auroc), nrow(grid) * 5L)
  # each setting appears on each fold exactly once
  expect_true(all(table(auroc$n_iter, auroc$eta, auroc$project) == 1))
})

test_that("a grid may omit columns, which fall back to the fit's values", {
  data(altmejd)
  res <- tune_logo_cv(
    fit_ada(eta = 0.5, depth = 2),
    grid = data.frame(n_iter = c(5, 10)),
    data = altmejd,
    group = "pid"
  )
  expect_equal(res$grid$n_iter, c(5, 10))
  expect_true(all(res$grid$eta == 0.5))
  expect_true(all(res$grid$depth == 2))
})

test_that("a grid column may set an rpart control, per row", {
  data(altmejd)
  small <- expand.grid(n_iter = c(5, 10), eta = 1, depth = 1)

  # one call with cp in the grid has to mean exactly what two fits with cp in
  # their control mean. Nothing else can catch a wrong merge order: the cached
  # search results predate the change.
  both <- tune_logo_cv(
    fit_ada(),
    grid = rbind(cbind(small, cp = 0), cbind(small, cp = 0.01)),
    data = altmejd,
    group = "pid"
  )
  expect_true("cp" %in% names(both$grid))
  expect_true("cp" %in% names(as.data.frame(both)))

  d <- as.data.frame(both)
  key <- function(x) paste(x$depth, x$n_iter, x$eta, x$project, x$metric)
  for (cpv in c(0, 0.01)) {
    arm <- tune_logo_cv(fit_ada(cp = cpv), small, data = altmejd, group = "pid")
    ref <- as.data.frame(arm)
    got <- d[d$cp == cpv, ]
    expect_equal(ref$estimate, got$estimate[match(key(ref), key(got))])
  }

  # the row is the more specific statement, so it overrides the fit's control
  clash <- tune_logo_cv(
    fit_ada(cp = 0.5),
    grid = cbind(small, cp = 0),
    data = altmejd,
    group = "pid"
  )
  only_row <- tune_logo_cv(
    fit_ada(),
    grid = cbind(small, cp = 0),
    data = altmejd,
    group = "pid"
  )
  expect_equal(clash$estimates, only_row$estimates)
})

test_that("a grid column that sets nothing is an error that names it", {
  data(altmejd)
  fit <- fit_ada()
  # a grid built to hold results as well as settings is a common shape, and
  # silence there would hide a column meant to tune something
  noisy <- data.frame(
    n_iter = 5,
    eta = 1,
    depth = 1,
    auc.mean = NA,
    runtime = NA
  )
  expect_error(
    tune_logo_cv(fit, grid = noisy, data = altmejd, group = "pid"),
    "auc\\.mean, runtime"
  )
  # and the message says what a grid may hold
  expect_error(
    tune_logo_cv(fit, grid = noisy, data = altmejd, group = "pid"),
    "`depth`, `n_iter`, `eta` and the rpart controls"
  )
  expect_silent(
    suppressMessages(tune_logo_cv(
      fit,
      grid = data.frame(n_iter = 5, eta = 1, depth = 1, minbucket = 4),
      data = altmejd,
      group = "pid"
    ))
  )
})

test_that("the other learners run and report NA for settings they lack", {
  data(altmejd)

  # a learner records NA in the grid for a setting it has no use for
  logit <- glm(fml, data = altmejd, family = binomial) |>
    logo_cv(data = altmejd, group = "pid")
  expect_equal(logit$model, "glm")
  expect_true(all(is.na(logit$grid$n_iter)))
  expect_true(all(is.na(logit$grid$eta)))
  expect_true(all(is.na(logit$grid$depth)))
  expect_true(all(is.finite(logit$estimates[1, , "auroc"])))
  # and the flattened view still shows them, since it reads the grid
  expect_true(all(is.na(as.data.frame(logit)$n_iter)))

  stump <- rpart::rpart(fml, data = altmejd, method = "class", maxdepth = 1) |>
    logo_cv(data = altmejd, group = "pid")
  expect_equal(stump$model, "rpart")
  expect_true(all(is.na(stump$grid$n_iter)))
  expect_equal(unique(stump$grid$depth), 1)

  tree <- rpart::rpart(fml, data = altmejd, method = "class", maxdepth = 4) |>
    logo_cv(data = altmejd, group = "pid")
  expect_equal(unique(tree$grid$depth), 4)

  # probability models are thresholded at 0.5, so a fitted logit must not
  # predict every case into one class on every fold
  expect_false(all(logit$estimates[1, , "spec"] == 0))
})

test_that("the other learners refit their own call on each fold", {
  data(altmejd)
  train <- altmejd[altmejd$pid != "eerp", ]
  test <- altmejd[altmejd$pid == "eerp", ]
  in_eerp <- function(res) res$scores$score[res$scores$project == "eerp"]

  logit <- logo_cv(
    glm(fml, data = altmejd, family = binomial),
    data = altmejd,
    group = "pid"
  )
  by_hand <- glm(fml, data = train, family = binomial) |>
    predict(test, type = "response")
  expect_equal(in_eerp(logit), unname(by_hand))

  # a `.` formula must not reach the group column of the full data
  dotted <- logo_cv(
    glm(
      replicate ~ .,
      data = altmejd[, all.vars(fml)],
      family = binomial
    ),
    data = altmejd,
    group = "pid"
  )
  expect_identical(dotted$scores, logit$scores)
})

test_that("random forest runs when the suggested package is available", {
  skip_if_not_installed("randomForest")
  data(altmejd)
  set.seed(1)
  # only the fit: randomForest() can warn about its own argument matching
  forest <- suppressWarnings(randomForest::randomForest(fml, data = altmejd))
  rf <- logo_cv(forest, data = altmejd, group = "pid")
  expect_equal(rf$model, "rf")
  expect_true(all(is.finite(rf$estimates[1, , "auroc"])))
})

test_that("logo_cv() rejects malformed input", {
  data(altmejd)
  fit <- fit_ada()
  expect_error(logo_cv(fit, data = altmejd, group = "nope"), "not found")
  expect_error(
    logo_cv(fit, data = altmejd, group = c("pid", "eid")),
    "single column"
  )
  expect_error(
    logo_cv(fit, data = altmejd[altmejd$pid == "rpp", ], group = "pid"),
    "at least two levels"
  )
  expect_error(
    tune_logo_cv(fit, grid = data.frame(), data = altmejd, group = "pid"),
    "no rows"
  )

  # a formula is not a fit: the recipe lives on the fitted model
  expect_error(
    logo_cv(fml, data = altmejd, group = "pid"),
    "must be a fitted model"
  )
  # a setting passed here would be dropped without a word
  expect_error(
    logo_cv(fit, data = altmejd, group = "pid", n_iter = 10, grid = 1),
    "`n_iter`, `grid`.*tune_logo_cv"
  )
  # learners that give no probability of the second class
  expect_error(
    logo_cv(glm(n.o ~ power.o, data = altmejd), data = altmejd, group = "pid"),
    "family = binomial"
  )
  expect_error(
    logo_cv(
      rpart::rpart(n.o ~ power.o, data = altmejd),
      data = altmejd,
      group = "pid"
    ),
    "classification tree"
  )
})

test_that("tune_logo_cv() refuses a glm fit, which has nothing to tune", {
  data(altmejd)
  logit <- glm(fml, data = altmejd, family = binomial)
  expect_error(
    tune_logo_cv(
      logit,
      grid = data.frame(n_iter = 5),
      data = altmejd,
      group = "pid"
    ),
    "no settings to tune. Cross-validate it with logo_cv().",
    fixed = TRUE
  )
  # any other object is told which fits the tuner takes
  expect_error(
    tune_logo_cv(
      fml,
      grid = data.frame(depth = 1),
      data = altmejd,
      group = "pid"
    ),
    "must be a fit from adaboost\\(\\)"
  )
})

test_that("the tuners reject arguments they do not take", {
  data(altmejd)
  tree <- rpart::rpart(fml, data = altmejd, method = "class")
  for (fit in list(fit_ada(), tree)) {
    expect_error(
      tune_logo_cv(
        fit,
        data.frame(depth = 1),
        data = altmejd,
        group = "pid",
        n_iter = 5
      ),
      "Unused argument(s): `n_iter`.",
      fixed = TRUE
    )
  }
})

# the lines of a printed result between its blank lines: the projects block,
# then the table of estimates
read_printed_parts <- function(out) {
  blanks <- which(out == "")
  list(
    projects = out[(blanks[1] + 1L):(blanks[2] - 1L)],
    table = out[(blanks[2] + 1L):(blanks[3] - 1L)]
  )
}

read_printed_table <- function(out) {
  read.table(
    text = read_printed_parts(out)$table,
    header = TRUE,
    check.names = FALSE
  )
}

test_that("the methods behave", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")

  expect_true(is.array(res$estimates))
  expect_s3_class(as.data.frame(res), "data.frame")

  # one setting, so one row, summarizing the metric asked for
  s <- summary(res, metric = "auprc")
  expect_equal(nrow(s), 1L)
  expect_equal(s$mean, mean(res$estimates[1, , "auprc"]))

  out <- capture.output(print(res))
  expect_true(any(grepl("Leave-one-group-out", out)))
  expect_true(any(grepl("adaboost", out)))
  # the spread is shown, never a mean or standard error across folds
  expect_true(any(grepl("spread", out)))
  expect_false(any(grepl("\\bmean\\b|\\bSE\\b|\\bsd\\b", out)))
  # and the table carries a header and exactly one row per setting, so there
  # is no aggregate row hiding in it
  expect_length(read_printed_parts(out)$table, 2L)

  expect_true(any(grepl(
    "no estimates",
    capture.output(print(res, metric = "nope"))
  )))
})

# ---- print() and summary() ---------------------------------------------------

# out of grid order on purpose, with an rpart control that print() must show
cp_grid <- data.frame(
  depth = c(2, 1, 2),
  n_iter = c(5, 5, 3),
  eta = 1,
  cp = c(0.01, 0, 0)
)

test_that("print() shows the projects once, then one row per setting", {
  # wide enough that the table does not wrap
  local_reproducible_output(width = 200)
  data(altmejd)
  res <- tune_logo_cv(fit_ada(), cp_grid, data = altmejd, group = "pid")
  projects <- dimnames(res$estimates)$project
  out <- capture.output(print(res))

  expect_true("  metric: auroc" %in% out)
  expect_false(any(grepl("measure", out)))
  # the spread across the projects, never a mean or SD of it
  expect_false(any(grepl("\\bmean\\b|\\bSE\\b|\\bsd\\b", out)))
  expect_true(
    "Estimates are per project; read the spread, not an average." %in% out
  )

  # each project's size and base rate once, in the order of the table
  block <- read_printed_parts(out)$projects
  info <- res$projects[match(projects, res$projects$project), ]
  expect_length(block, 3L)
  expect_equal(strsplit(trimws(block[1]), " +")[[1]], projects)
  expect_equal(strsplit(block[2], " +")[[1]], c("n", as.character(info$n)))
  expect_equal(
    strsplit(sub("^base rate +", "", block[3]), " +")[[1]],
    sprintf("%.2f", info$base_rate)
  )

  # one row per setting, in grid order, every grid column first
  table <- read_printed_table(out)
  expect_named(table, c("depth", "n_iter", "eta", "cp", projects))
  expect_equal(table[names(cp_grid)], cp_grid)
  expect_equal(
    as.matrix(table[projects]),
    round(res$estimates[, projects, "auroc"], 3),
    ignore_attr = TRUE
  )
  # three decimals in every estimate, so 0.62 shows as 0.620; counts such as
  # tp stay whole
  read_cells <- function(out) {
    rows <- strsplit(trimws(read_printed_parts(out)$table[-1]), " +")
    unlist(lapply(rows, \(row) tail(row, length(projects))))
  }
  expect_true(all(grepl("^[01]\\.[0-9]{3}$", read_cells(out))))
  # also in a column whose values need only two
  planted <- res
  planted$estimates[, "rpp", "auroc"] <- 0.62
  planted_out <- capture.output(print(planted))
  expect_true(all(grepl("^[01]\\.[0-9]{3}$", read_cells(planted_out))))
  tp <- capture.output(print(res, metric = "tp"))
  expect_true(all(grepl("^[0-9]+$", read_cells(tp))))
})

test_that("print() shows one column per held-out group", {
  local_reproducible_output(width = 200)
  data(altmejd)
  res <- logo_cv(
    fit_ada(),
    data = altmejd,
    group = "pid",
    holdout = c("ssrp", "eerp")
  )
  out <- capture.output(print(res))

  expect_true("  holdout: ssrp, eerp" %in% out)
  block <- read_printed_parts(out)$projects
  expect_equal(strsplit(trimws(block[1]), " +")[[1]], c("ssrp", "eerp"))
  table <- read_printed_table(out)
  expect_named(table, c("depth", "n_iter", "eta", "ssrp", "eerp"))
  expect_equal(nrow(table), 1L)
})

test_that("a learner without settings prints and summarizes the projects only", {
  local_reproducible_output(width = 200)
  data(altmejd)
  logit <- glm(fml, data = altmejd, family = binomial) |>
    logo_cv(data = altmejd, group = "pid")
  projects <- dimnames(logit$estimates)$project

  table <- read_printed_table(capture.output(print(logit)))
  expect_named(table, projects)
  expect_equal(
    unlist(table),
    round(logit$estimates[1, , "auroc"], 3),
    ignore_attr = TRUE
  )
  expect_named(summary(logit), c("mean", "sd", "min", "max", "stable"))
})

test_that("print() shows the first 10 of more than 20 settings", {
  local_reproducible_output(width = 200)
  data(altmejd)
  # rows that differ only in n_iter share one fit per fold, so this is cheap
  long <- tune_logo_cv(
    fit_ada(),
    data.frame(n_iter = 1:21),
    data = altmejd,
    group = "pid"
  )
  lines <- read_printed_parts(capture.output(print(long)))$table
  # the header, ten rows in grid order, and the note
  expect_length(lines, 12L)
  expect_equal(read.table(text = lines[1:11], header = TRUE)$n_iter, 1:10)
  expect_equal(lines[12], "... and 11 more settings; summary() ranks them.")

  # 20 settings still print in full
  twenty <- tune_logo_cv(
    fit_ada(),
    data.frame(n_iter = 1:20),
    data = altmejd,
    group = "pid"
  )
  expect_equal(read_printed_table(capture.output(print(twenty)))$n_iter, 1:20)
})

test_that("summary() gives each setting's statistics across the projects", {
  data(altmejd)
  # the caller's row names must not reach the summary
  grid <- cp_grid[c(3, 1, 2), ]
  res <- tune_logo_cv(fit_ada(), grid, data = altmejd, group = "pid")
  expect_equal(rownames(res$grid), c("3", "1", "2"))

  s <- summary(res, "auroc")
  expect_named(
    s,
    c("depth", "n_iter", "eta", "cp", "mean", "sd", "min", "max", "stable")
  )
  expect_equal(rownames(s), c("1", "2", "3"))
  expect_equal(s[names(grid)], grid, ignore_attr = TRUE)

  auroc <- res$estimates[,, "auroc"]
  expect_identical(s$mean, unname(rowMeans(auroc)))
  expect_identical(s$sd, unname(apply(auroc, 1, sd)))
  expect_identical(s$min, unname(apply(auroc, 1, min)))
  expect_identical(s$max, unname(apply(auroc, 1, max)))
  expect_identical(s$stable, s$mean - s$sd)
})

test_that("summary() reproduces hypergrid's mean, sd, min and max exactly", {
  data(hypergrid)
  data(altmext)
  tune <- altmext[altmext$pid != "holz", ]
  recipe <- rpart::rpart(
    replicate ~ power.o + effect_size.o + n.o + p_value.o,
    data = tune,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 10, eta = 1, verbose = FALSE)

  # the three settings of test-hypergrid.R
  settings <- hypergrid$auroc[c("depth", "n_iter", "eta")]
  set.seed(2026)
  rows <- sample(which(settings$n_iter <= 100), 3)
  res <- tune_logo_cv(recipe, settings[rows, ], data = tune, group = "pid")

  for (metric in names(hypergrid)) {
    s <- summary(res, metric)
    for (stat in c("mean", "sd", "min", "max")) {
      expect_identical(
        s[[stat]],
        hypergrid[[metric]][[stat]][rows],
        label = paste(metric, stat)
      )
    }
  }
  # ML3 has no patk_5, so all five statistics are NA
  stats <- c("mean", "sd", "min", "max", "stable")
  expect_true(all(is.na(summary(res, "patk_5")[stats])))
  # the setting's number in this grid, not its row in hypergrid
  expect_equal(rownames(summary(res)), c("1", "2", "3"))
})

test_that("summary() of one held-out group has no sd", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid", holdout = "ssrp")
  s <- summary(res)
  estimate <- res$estimates[1, "ssrp", "auroc"]

  expect_equal(nrow(s), 1L)
  expect_identical(c(s$mean, s$min, s$max), rep(estimate, 3))
  expect_true(is.na(s$sd))
  expect_true(is.na(s$stable))
})

test_that("summary() needs one metric of the result", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")
  for (bad in list("nope", c("auroc", "bacc"), NA_character_, NULL)) {
    expect_error(
      summary(res, metric = bad),
      "single name among the metrics of the result"
    )
  }
})

test_that("summary() sorts highest first, ties in grid order, NA last", {
  data(altmejd)
  res <- tune_logo_cv(
    fit_ada(),
    data.frame(n_iter = 1:6),
    data = altmejd,
    group = "pid"
  )
  # planted estimates: rows 2, 3 and 5 tie exactly on every statistic, row 6
  # ties them on min only, and row 4 lacks a project
  flat <- rep(0.7, 5)
  res$estimates[,, "auroc"] <- rbind(
    c(0.6, 0.7, 0.8, 0.9, 1.0),
    flat,
    flat,
    c(0.5, NA, 0.9, 0.9, 0.9),
    flat,
    c(0.7, 0.8, 0.9, 0.9, 0.9)
  )

  s <- summary(res, "auroc")
  expect_equal(rownames(s), as.character(1:6))
  expect_true(all(is.na(s[4, c("mean", "sd", "min", "max", "stable")])))

  by_min <- summary(res, "auroc", sort_by = "min")
  expect_equal(rownames(by_min), c("2", "3", "5", "6", "1", "4"))
  # the tutorial's rule: the best worst project, ties broken by stability
  ranked <- summary(res, "auroc", sort_by = c("min", "stable"))
  expect_equal(rownames(ranked), c("6", "2", "3", "5", "1", "4"))
  # rows keep their names, so a name still finds its setting
  expect_identical(ranked["6", ], s["6", ])
  expect_equal(
    rownames(summary(res, "auroc", sort_by = "n_iter")),
    as.character(6:1)
  )
})

test_that("summary() rejects a sort_by it cannot use and names the columns", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")
  for (bad in list("nope", NA_character_, c("min", "min"), character(0), 1)) {
    expect_error(
      summary(res, sort_by = bad),
      "depth, n_iter, eta, mean, sd, min, max, stable",
      fixed = TRUE
    )
  }
})

# ---- holdout ---------------------------------------------------------------

test_that("holdout keeps only the named levels, in the order given", {
  data(altmejd)
  fit <- fit_ada()
  every <- logo_cv(fit, data = altmejd, group = "pid")
  some <- logo_cv(
    fit,
    data = altmejd,
    group = "pid",
    holdout = c("ssrp", "eerp")
  )

  expect_named(some, names(every))
  expect_equal(dimnames(some$estimates)$project, c("ssrp", "eerp"))
  expect_equal(some$projects$project, c("ssrp", "eerp"))
  expect_equal(unique(some$scores$project), c("ssrp", "eerp"))
  # a held-out project is the same fold either way
  expect_equal(
    some$estimates[, c("ssrp", "eerp"), , drop = FALSE],
    every$estimates[, c("ssrp", "eerp"), , drop = FALSE]
  )

  expect_equal(attr(some, "holdout"), c("ssrp", "eerp"))
  expect_null(attr(every, "holdout"))
  expect_true(any(grepl("holdout: ssrp, eerp", capture.output(print(some)))))
  expect_false(any(grepl("holdout", capture.output(print(every)))))
})

test_that("holdout rejects a level the group does not have", {
  data(altmejd)
  fit <- fit_ada()
  expect_error(
    logo_cv(fit, data = altmejd, group = "pid", holdout = c("eerp", "holz")),
    "not found in `pid`: holz"
  )
  expect_error(
    logo_cv(fit, data = altmejd, group = "pid", holdout = c("eerp", "eerp")),
    "more than once"
  )
})

test_that("holding out HOLZ equals a fit on the other projects, scored on HOLZ", {
  data(altmext)
  predictors <- all.vars(fml)[-1L]
  six <- altmext[altmext$pid != "holz", ]
  holz <- altmext[altmext$pid == "holz", ]

  res <- logo_cv(
    fit_ada(n_iter = 10),
    data = altmext,
    group = "pid",
    holdout = "holz"
  )
  direct <- rpart::rpart(
    fml,
    data = six,
    method = "class",
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 10, eta = 1, verbose = FALSE) |>
    predict(holz[, predictors], type = "margin", verbose = FALSE)

  expect_equal(dimnames(res$estimates)$project, "holz")
  expect_equal(res$scores$score, unname(direct))
  expect_equal(
    unname(res$estimates[1, "holz", "auroc"]),
    auroc(holz$replicate, direct)
  )
})

test_that("a Monte Carlo draw through holdout = \"test\" has one row", {
  data(altmejd)
  draw <- altmejd
  set.seed(7)
  draw$part <- sample(
    c("train", "test"),
    nrow(draw),
    replace = TRUE,
    prob = c(0.7, 0.3)
  )
  res <- logo_cv(fit_ada(), data = draw, group = "part", holdout = "test")

  expect_equal(dimnames(res$estimates)$project, "test")
  d <- as.data.frame(res)
  expect_equal(nrow(d[d$metric == "auroc", ]), 1L)
  expect_equal(res$projects$n, sum(draw$part == "test"))
})

# ---- one fit per group of rows, and cores ------------------------------------

test_that("rows that differ only in n_iter score as refits would", {
  # the grid's rows share one fit per fold, boosted to n_iter = 10; the
  # 5-round row is read off its first five rounds
  data(altmejd)
  rounds <- c(5, 10)
  grouped <- tune_logo_cv(
    fit_ada(),
    data.frame(depth = 1, n_iter = rounds, eta = 1),
    data = altmejd,
    group = "pid"
  )
  for (k in seq_along(rounds)) {
    alone <- tune_logo_cv(
      fit_ada(),
      data.frame(depth = 1, n_iter = rounds[k], eta = 1),
      data = altmejd,
      group = "pid"
    )
    from_fit <- logo_cv(
      fit_ada(n_iter = rounds[k]),
      data = altmejd,
      group = "pid"
    )
    in_row <- grouped$scores$setting == k
    expect_identical(grouped$estimates[k, , ], alone$estimates[1, , ])
    expect_identical(grouped$scores$score[in_row], alone$scores$score)
    expect_identical(grouped$scores$actual[in_row], alone$scores$actual)
    expect_identical(grouped$estimates[k, , ], from_fit$estimates[1, , ])
  }

  # and a refit by hand, scored by predict(), gives the same margins exactly
  train <- altmejd[altmejd$pid != "ml1", ]
  test <- altmejd[altmejd$pid == "ml1", ]
  by_hand <- rpart::rpart(fml, data = train, maxdepth = 1, model = TRUE) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE) |>
    predict(test, type = "margin", verbose = FALSE)
  expect_identical(
    grouped$scores$score[
      grouped$scores$setting == 1 & grouped$scores$project == "ml1"
    ],
    by_hand
  )
})

test_that("rows group by every setting but n_iter, whatever their order", {
  # interleaved rows and an rpart control: each row must still get its own
  # depth, rate and cp
  data(altmejd)
  grid <- data.frame(
    depth = c(1, 2, 1, 2, 1),
    n_iter = c(6, 3, 3, 6, 4),
    eta = 1,
    cp = c(0.01, 0.01, 0.01, 0.01, 0)
  )
  res <- tune_logo_cv(fit_ada(), grid, data = altmejd, group = "pid")
  for (i in seq_len(nrow(grid))) {
    alone <- tune_logo_cv(
      fit_ada(),
      grid[i, ],
      data = altmejd,
      group = "pid"
    )
    expect_identical(res$estimates[i, , ], alone$estimates[1, , ])
  }
})

test_that("cores = 2 gives what one core gives", {
  skip_on_os("windows")
  data(altmejd)
  grid <- expand.grid(depth = 1:2, n_iter = c(3, 6), eta = c(1, 0.5))
  one <- tune_logo_cv(fit_ada(), grid, data = altmejd, group = "pid")
  two <- tune_logo_cv(fit_ada(), grid, data = altmejd, group = "pid", cores = 2)
  expect_identical(two, one)

  nested_one <- tune_logo_cv(
    fit_ada(),
    grid[1:2, ],
    data = altmejd,
    group = "pid",
    nested = TRUE
  )
  nested_two <- tune_logo_cv(
    fit_ada(),
    grid[1:2, ],
    data = altmejd,
    group = "pid",
    nested = TRUE,
    cores = 2
  )
  expect_identical(nested_two, nested_one)
})

test_that("on Windows, cores above 1 run on one core and say so once", {
  local_mocked_bindings(read_os_type = function() "windows")
  data(altmejd)
  grid <- data.frame(n_iter = c(3, 6))
  expect_message(
    on_windows <- tune_logo_cv(
      fit_ada(),
      grid,
      data = altmejd,
      group = "pid",
      cores = 2
    ),
    "one core"
  )
  expect_identical(
    on_windows,
    tune_logo_cv(fit_ada(), grid, data = altmejd, group = "pid")
  )
  # once, not again for every inner search of a nested run
  messages <- capture_messages(tune_logo_cv(
    fit_ada(),
    grid,
    data = altmejd,
    group = "pid",
    nested = TRUE,
    cores = 2
  ))
  expect_length(messages, 1L)
})

test_that("a grid row without rounds is an error, not a score", {
  # sharing a fit with longer rows, a 0-round row would score a margin of 0
  data(altmejd)
  for (bad in list(c(0, 5), c(NA, 5), c("5", "10"))) {
    expect_error(
      tune_logo_cv(
        fit_ada(),
        data.frame(n_iter = bad),
        data = altmejd,
        group = "pid"
      ),
      "`n_iter` must be numbers of 1 or more"
    )
  }
})

test_that("cores must be one whole number, 1 or more", {
  data(altmejd)
  fit <- fit_ada()
  for (bad in list(0, 1.5, "4", NA, c(1, 2), Inf)) {
    expect_error(
      tune_logo_cv(
        fit,
        data.frame(n_iter = 3),
        data = altmejd,
        group = "pid",
        cores = bad
      ),
      "`cores` must be one whole number"
    )
  }
})

test_that("progress names the settings a fit serves", {
  data(altmejd)
  expect_message(
    tune_logo_cv(
      fit_ada(),
      data.frame(n_iter = c(3, 6)),
      data = altmejd,
      group = "pid",
      verbose = TRUE
    ),
    "settings 1-2/2 | fold eerp",
    fixed = TRUE
  )
})

# ---- nested LOGO-CV --------------------------------------------------------

small_grid <- expand.grid(depth = 1:2, n_iter = c(5, 10), eta = 1)

test_that("nested tune_logo_cv() reports the setting each outer fold chose", {
  data(altmejd)
  res <- tune_logo_cv(
    fit_ada(),
    small_grid,
    data = altmejd,
    group = "pid",
    nested = TRUE
  )

  expect_s3_class(res, "logo_cv")
  expect_true(res$nested)
  expect_equal(res$criterion, "auroc")
  expect_equal(nrow(res$selected), 5L)
  expect_setequal(res$selected$project, c("eerp", "ml1", "ml3", "rpp", "ssrp"))
  expect_true(all(res$selected$setting %in% seq_len(nrow(small_grid))))

  # the outer estimates carry the selected setting, not the grid position
  d <- as.data.frame(res)
  auroc <- d[d$metric == "auroc", ]
  expect_equal(nrow(auroc), 5L)
  key <- merge(
    auroc[, c("project", "setting")],
    res$selected[, c("project", "setting")],
    by = "project"
  )
  expect_equal(key$setting.x, key$setting.y)

  # the summary is one row over the outer folds, with no setting: each fold
  # chose its own
  s <- summary(res, metric = "auroc")
  expect_named(s, c("mean", "sd", "min", "max", "stable"))
  expect_equal(nrow(s), 1L)
  expect_equal(s$mean, mean(res$estimates[, "auroc"]))
  expect_equal(s$sd, sd(res$estimates[, "auroc"]))
  expect_equal(s$min, min(res$estimates[, "auroc"]))
})

test_that("the inner search never sees the held-out project", {
  # The correctness property: for each outer fold, the selected setting must be
  # the argmax of a grid search run on the remaining projects only. Recomputing
  # that independently is the real check that no leakage occurs.
  data(altmejd)
  res <- tune_logo_cv(
    fit_ada(),
    small_grid,
    data = altmejd,
    group = "pid",
    nested = TRUE
  )

  for (p in res$selected$project) {
    train <- altmejd[altmejd$pid != p, ]
    inner <- tune_logo_cv(fit_ada(), small_grid, data = train, group = "pid")
    s <- summary(inner, metric = "auroc")
    expect_equal(
      res$selected$setting[res$selected$project == p],
      as.integer(rownames(s)[which.max(s$mean)])
    )
  }
})

test_that("a nested search with a holdout selects on the other groups", {
  data(altmext)
  res <- tune_logo_cv(
    fit_ada(),
    small_grid,
    data = altmext,
    group = "pid",
    holdout = "holz",
    nested = TRUE
  )
  expect_equal(res$selected$project, "holz")
  expect_equal(dimnames(res$estimates)$project, "holz")
  expect_equal(attr(res, "holdout"), "holz")

  # the inner search ran on all six other projects
  inner <- tune_logo_cv(
    fit_ada(),
    small_grid,
    data = altmext[altmext$pid != "holz", ],
    group = "pid"
  )
  s <- summary(inner, metric = "auroc")
  expect_equal(
    res$selected$setting,
    as.integer(rownames(s)[which.max(s$mean)])
  )
})

test_that("nesting removes the optimism of scoring the winner on its own folds", {
  # Needs a grid with something to select over. With only a handful of settings
  # there is almost nothing to overfit, and the nested estimate can land above
  # the naive one by chance -- measured at -0.003 on a four-setting grid.
  #
  # The gap scales with how much there is to choose from. Measured on this data
  # with the shared cp = 0.01 control: 0.017 at 12 settings, 0.054 at 27, 0.093
  # at 48. Everything here is deterministic, so 0.017 is exact and the threshold
  # below only has to sit under it. The larger grids cost 38 s and 127 s, which
  # is not worth it for a sign test -- the magnitude belongs in the vignette.
  data(altmejd)
  grid <- expand.grid(depth = 1:3, n_iter = c(10, 30), eta = c(1, 0.1))

  plain <- tune_logo_cv(fit_ada(), grid, data = altmejd, group = "pid")
  s <- summary(plain, metric = "auroc")
  naive <- max(s$mean)

  nst <- tune_logo_cv(
    fit_ada(),
    grid,
    data = altmejd,
    group = "pid",
    nested = TRUE
  )
  nested <- summary(nst, metric = "auroc")$mean

  # the naive best-average was selected and graded on the same five projects
  expect_gt(naive - nested, 0.01)

  # the winner of the full-grid search is not what the inner searches choose
  best <- as.integer(rownames(s)[which.max(s$mean)])
  expect_gt(length(unique(nst$selected$setting)), 1L)
})

test_that("the criterion decides what the inner search maximizes", {
  data(altmejd)
  by_auroc <- tune_logo_cv(
    fit_ada(),
    small_grid,
    data = altmejd,
    group = "pid",
    nested = TRUE,
    criterion = "auroc"
  )
  by_bacc <- tune_logo_cv(
    fit_ada(),
    small_grid,
    data = altmejd,
    group = "pid",
    nested = TRUE,
    criterion = "bacc"
  )

  expect_equal(by_auroc$criterion, "auroc")
  expect_equal(by_bacc$criterion, "bacc")
  # both are valid runs; the criterion is recorded so a reader can see which
  # decision rule produced the selections
  expect_equal(nrow(by_bacc$selected), 5L)
})

test_that("nested tune_logo_cv() rejects input it cannot honor", {
  data(altmejd)
  fit <- fit_ada()
  expect_error(
    tune_logo_cv(
      fit,
      grid = data.frame(n_iter = 5),
      data = altmejd,
      group = "pid",
      nested = TRUE
    ),
    "needs a `grid` with two or more rows"
  )
  expect_error(
    tune_logo_cv(
      fit,
      small_grid,
      data = altmejd[altmejd$pid %in% c("ml1", "ml3"), ],
      group = "pid",
      nested = TRUE
    ),
    "at least three groups"
  )
  expect_error(
    tune_logo_cv(
      fit,
      small_grid,
      data = altmejd,
      group = "pid",
      nested = TRUE,
      criterion = "not_a_measure"
    ),
    "not one of the metrics"
  )
})

test_that("print() surfaces the selection instability", {
  data(altmejd)
  res <- tune_logo_cv(
    fit_ada(),
    small_grid,
    data = altmejd,
    group = "pid",
    nested = TRUE
  )
  out <- capture.output(print(res))

  expect_true(any(grepl("Nested leave-one-group-out", out)))
  expect_true(any(grepl("selected on: auroc", out)))
  expect_true(any(grepl("inner searches selected", out)))
  expect_true(any(grepl("diagnostic, not a selection rule", out)))
  # still no aggregate row and no cross-project SE
  expect_false(any(grepl("\\bmean\\b|\\bSE\\b|\\bsd\\b", out)))

  # a nested result keeps its long layout, one row per outer fold with the
  # setting it chose; only the header word changed from "measure:"
  expect_equal(
    out,
    c(
      "Nested leave-one-group-out cross-validation",
      "  model:   adaboost",
      "  settings: 4 ",
      "  selected on: auroc (inner loop)",
      "  metric: auroc",
      "",
      " depth n_iter eta project  n base_rate estimate",
      "     1      5   1    eerp 18     0.611    0.825",
      "     1      5   1     ml1 13     0.846    0.955",
      "     1     10   1     ml3 10     0.300    0.619",
      "     1     10   1     rpp 90     0.344    0.620",
      "     1     10   1    ssrp 21     0.571    0.676",
      "",
      "Estimates are per project; read the spread, not an average.",
      "The 5 inner searches selected 2 distinct settings.",
      "These are a diagnostic, not a selection rule: each rests on a subset",
      "of the projects, and no model built here is the one you would ship."
    )
  )
})

test_that("a plain run records that it was not nested", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")
  expect_false(res$nested)
  expect_null(res$selected)
  expect_false(any(grepl("Nested", capture.output(print(res)))))
})

# ---- tuning a tree and a forest ----------------------------------------------

# randomForest() matches an argument partially, which warns under
# options(warnPartialMatchArgs = TRUE); any other warning still shows
muffle_partial_matching <- function(code) {
  withCallingHandlers(
    code,
    warning = function(w) {
      if (grepl("partial argument match", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# a small classification forest on altmejd
fit_forest <- function() {
  muffle_partial_matching(
    randomForest::randomForest(fml, data = altmejd, ntree = 25)
  )
}

test_that("the tree tuner equals one logo_cv() per depth, on every metric", {
  # the depth search data-raw/benchmark.R once looped by hand
  data(altmext)
  voi <- c(all.vars(fml)[-1L], "replicate")
  tune <- altmext[altmext$pid != "holz", ]
  res <- rpart::rpart(replicate ~ ., data = tune[, voi], method = "class") |>
    tune_logo_cv(data.frame(depth = 1:8), data = tune, group = "pid")

  expect_equal(res$model, "rpart")
  for (depth in 1:8) {
    one <- rpart::rpart(
      replicate ~ .,
      data = tune[, voi],
      method = "class",
      maxdepth = depth
    ) |>
      logo_cv(data = tune, group = "pid")
    expect_identical(res$estimates[depth, , ], one$estimates[1, , ])
    expect_identical(
      res$scores$score[res$scores$setting == depth],
      one$scores$score
    )
  }
})

test_that("the tutorial's tree code picks depth 5, ties in grid order", {
  data(altmext)
  df <- altmext
  pred_nms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  out_nme <- "replicate"
  voi_nms <- c(pred_nms, out_nme)
  df_noholz <- df[df$pid != "holz", ]

  hgrid_tree <- data.frame(depth = 1:8)
  logo_tree <- rpart::rpart(
    replicate ~ .,
    data = df_noholz[, voi_nms],
    method = "class"
  ) |>
    tune_logo_cv(hgrid_tree, data = df_noholz, group = "pid")
  best_tree <- summary(logo_tree, "auroc", sort_by = c("min", "stable"))[1, ]

  expect_equal(best_tree$depth, 5)
  # depths 5 to 8 tie exactly, so the grid's order decides
  ranked <- summary(logo_tree, "auroc", sort_by = c("min", "stable"))
  expect_equal(rownames(ranked)[1:4], c("5", "6", "7", "8"))
  for (depth in 6:8) {
    expect_identical(
      logo_tree$estimates[depth, , "auroc"],
      logo_tree$estimates[5, , "auroc"]
    )
  }
})

test_that("a tree's grid row sets its rpart controls, over the fit's own", {
  data(altmejd)
  # the fit's cp of 0.5 stops at the root; each row's cp replaces it
  fit <- rpart::rpart(fml, data = altmejd, method = "class", cp = 0.5)
  grid <- data.frame(
    depth = 4,
    cp = c(0, 0.02),
    minsplit = c(10, 30),
    minbucket = c(3, 10)
  )
  res <- tune_logo_cv(fit, grid, data = altmejd, group = "pid")

  direct <- list(
    rpart::rpart(
      fml,
      data = altmejd,
      method = "class",
      maxdepth = 4,
      cp = 0,
      minsplit = 10,
      minbucket = 3
    ),
    rpart::rpart(
      fml,
      data = altmejd,
      method = "class",
      maxdepth = 4,
      cp = 0.02,
      minsplit = 30,
      minbucket = 10
    )
  )
  for (i in seq_along(direct)) {
    one <- logo_cv(direct[[i]], data = altmejd, group = "pid")
    expect_identical(res$estimates[i, , ], one$estimates[1, , ])
  }
  expect_false(identical(res$estimates[1, , ], res$estimates[2, , ]))
})

test_that("a tree's depth reaches its refits, however the fit set its controls", {
  # rpart() lets a control list override its direct arguments, so a depth set
  # beside `control` would not take
  data(altmejd)
  folds <- unique(altmejd$pid)
  by_hand <- function(depth) {
    unlist(lapply(folds, function(fold) {
      tree <- rpart::rpart(
        fml,
        data = altmejd[altmejd$pid != fold, ],
        method = "class",
        maxdepth = depth,
        cp = 0.001
      )
      unname(predict(tree, altmejd[altmejd$pid == fold, ], type = "prob")[, 2])
    }))
  }
  shapes <- list(
    direct = rpart::rpart(
      fml,
      data = altmejd,
      method = "class",
      maxdepth = 2,
      cp = 0.001
    ),
    control = rpart::rpart(
      fml,
      data = altmejd,
      method = "class",
      control = rpart::rpart.control(maxdepth = 2, cp = 0.001)
    )
  )
  for (fit in shapes) {
    res <- tune_logo_cv(
      fit,
      data.frame(depth = c(1, 3)),
      data = altmejd,
      group = "pid"
    )
    expect_identical(res$scores$score[res$scores$setting == 1], by_hand(1))
    expect_identical(res$scores$score[res$scores$setting == 2], by_hand(3))

    # the fit's own depth gives what logo_cv() gives
    own <- tune_logo_cv(
      fit,
      data.frame(depth = 2),
      data = altmejd,
      group = "pid"
    )
    plain <- logo_cv(fit, data = altmejd, group = "pid")
    expect_identical(own$estimates, plain$estimates)
    expect_identical(own$scores, plain$scores)
  }
})

test_that("a tree's or a forest's grid may hold only its own columns", {
  skip_if_not_installed("randomForest")
  data(altmejd)
  tree <- rpart::rpart(fml, data = altmejd, method = "class")
  expect_error(
    tune_logo_cv(
      tree,
      data.frame(depth = 1, n_iter = 5),
      data = altmejd,
      group = "pid"
    ),
    "n_iter set nothing. A grid may hold `depth` and the rpart controls (",
    fixed = TRUE
  )
  set.seed(1)
  forest <- fit_forest()
  expect_error(
    tune_logo_cv(forest, data.frame(depth = 2), data = altmejd, group = "pid"),
    "depth set nothing. A grid may hold `mtry`, `nodesize`, `ntree` and `maxnodes`.",
    fixed = TRUE
  )
})

test_that("nested = TRUE is for adaboost fits only", {
  skip_if_not_installed("randomForest")
  data(altmejd)
  tree <- rpart::rpart(fml, data = altmejd, method = "class")
  set.seed(1)
  forest <- fit_forest()
  grids <- list(data.frame(depth = 1:2), data.frame(mtry = 1:2))
  fits <- list(tree, forest)
  for (k in seq_along(fits)) {
    expect_error(
      tune_logo_cv(
        fits[[k]],
        grids[[k]],
        data = altmejd,
        group = "pid",
        nested = TRUE
      ),
      "`nested = TRUE` is for adaboost() fits only.",
      fixed = TRUE
    )
  }
})

test_that("the forest tuner sets one seed per fold, drawn from the session", {
  skip_if_not_installed("randomForest")
  data(altmejd)
  set.seed(1)
  forest <- fit_forest()
  # an NA leaves the argument as the fit's call had it
  grid <- data.frame(mtry = c(1, 3), maxnodes = c(4, NA))
  set.seed(5)
  res <- muffle_partial_matching(
    tune_logo_cv(forest, grid, data = altmejd, group = "pid")
  )
  expect_equal(res$model, "rf")

  # by hand: the seeds first, then each fold's forest after its fold's seed
  folds <- unique(altmejd$pid)
  set.seed(5)
  seeds <- sample.int(.Machine$integer.max, length(folds))
  by_hand <- function(...) {
    unlist(lapply(seq_along(folds), function(j) {
      set.seed(seeds[j])
      refit <- muffle_partial_matching(randomForest::randomForest(
        fml,
        data = altmejd[altmejd$pid != folds[j], ],
        ntree = 25,
        ...
      ))
      test <- altmejd[altmejd$pid == folds[j], ]
      unname(predict(refit, test, type = "prob")[, 2])
    }))
  }
  expect_identical(
    res$scores$score[res$scores$setting == 1],
    by_hand(mtry = 1, maxnodes = 4)
  )
  expect_identical(res$scores$score[res$scores$setting == 2], by_hand(mtry = 3))

  # so a row's result does not depend on the rows before it
  set.seed(5)
  alone <- muffle_partial_matching(
    tune_logo_cv(forest, grid[2, ], data = altmejd, group = "pid")
  )
  expect_identical(alone$estimates[1, , ], res$estimates[2, , ])
})

test_that("the forest tuner repeats after set.seed(), at any number of cores", {
  skip_on_os("windows")
  skip_if_not_installed("randomForest")
  data(altmejd)
  set.seed(1)
  forest <- fit_forest()
  grid <- data.frame(mtry = 1:2)
  run <- function(cores) {
    set.seed(5)
    res <- muffle_partial_matching(tune_logo_cv(
      forest,
      grid,
      data = altmejd,
      group = "pid",
      cores = cores
    ))
    list(res = res, stream = get(".Random.seed", envir = globalenv()))
  }
  one <- run(1)
  two <- run(2)
  expect_identical(two$res, one$res)
  expect_identical(run(1)$res, one$res)
  # the session's stream moves by the seeds drawn, whatever the cores
  expect_identical(two$stream, one$stream)
})

test_that("logo_cv() on a forest still draws from the session's stream", {
  # no seed rule here: the shipped benchmark's forest row was built with
  # set.seed(112) before each logo_cv() call
  skip_if_not_installed("randomForest")
  data(altmejd)
  set.seed(1)
  forest <- fit_forest()
  set.seed(3)
  res <- muffle_partial_matching(logo_cv(forest, data = altmejd, group = "pid"))
  after_res <- get(".Random.seed", envir = globalenv())

  set.seed(3)
  by_hand <- unlist(lapply(unique(altmejd$pid), function(fold) {
    refit <- muffle_partial_matching(randomForest::randomForest(
      fml,
      data = altmejd[altmejd$pid != fold, ],
      ntree = 25
    ))
    unname(predict(refit, altmejd[altmejd$pid == fold, ], type = "prob")[, 2])
  }))
  expect_identical(res$scores$score, by_hand)
  expect_identical(get(".Random.seed", envir = globalenv()), after_res)

  # and it rebuilds the forest row of the shipped benchmark
  data(altmext)
  data(benchmark)
  voi <- c(all.vars(fml)[-1L], "replicate")
  tune <- altmext[altmext$pid != "holz", ]
  set.seed(112)
  fit_rf <- muffle_partial_matching(
    randomForest::randomForest(replicate ~ ., data = tune[, voi])
  )
  set.seed(112)
  logo_rf <- muffle_partial_matching(logo_cv(
    fit_rf,
    data = tune,
    group = "pid"
  ))
  rf_row <- benchmark$auroc[benchmark$auroc$model == "rf", ]
  projects <- dimnames(logo_rf$estimates)$project
  expect_identical(
    unname(logo_rf$estimates[1, , "auroc"]),
    unname(unlist(rf_row[projects]))
  )
})

test_that("print() and summary() show a tree's or a forest's own settings", {
  local_reproducible_output(width = 200)
  skip_if_not_installed("randomForest")
  data(altmejd)
  statistics <- c("mean", "sd", "min", "max", "stable")

  # the grid's column, then the fit's depth; n_iter and eta are all NA
  tree <- rpart::rpart(fml, data = altmejd, method = "class", maxdepth = 3)
  res_tree <- tune_logo_cv(
    tree,
    data.frame(cp = c(0, 0.01)),
    data = altmejd,
    group = "pid"
  )
  projects <- dimnames(res_tree$estimates)$project
  expect_equal(res_tree$grid$depth, c(3, 3))
  expect_named(
    read_printed_table(capture.output(print(res_tree))),
    c("cp", "depth", projects)
  )
  expect_named(summary(res_tree), c("cp", "depth", statistics))

  # a forest stores mtry and ntree only, so the grid takes no other column
  # from the fit
  set.seed(1)
  forest <- fit_forest()
  set.seed(5)
  res_rf <- muffle_partial_matching(tune_logo_cv(
    forest,
    data.frame(mtry = 1:2),
    data = altmejd,
    group = "pid"
  ))
  expect_equal(res_rf$grid$ntree, c(25, 25))
  expect_false(any(c("nodesize", "maxnodes") %in% names(res_rf$grid)))
  expect_named(
    read_printed_table(capture.output(print(res_rf))),
    c("mtry", "ntree", projects)
  )
  expect_named(summary(res_rf), c("mtry", "ntree", statistics))
  expect_true(all(is.na(as.data.frame(res_rf)[c("depth", "n_iter", "eta")])))
  set.seed(3)
  boot <- suppressWarnings(bootstrap(res_rf, n_resample = 20, setting = 2))
  expect_identical(boot$estimates[1, , ], res_rf$estimates[2, , ])
})

# ---- XGBoost -----------------------------------------------------------------

xgb_predictors <- all.vars(fml)[-1L]

# a small XGBoost fit on altmejd; `...` reaches xgboost()
fit_xgb <- function(nrounds = 5, ...) {
  xgboost::xgboost(
    x = altmejd[, xgb_predictors],
    y = altmejd$replicate,
    nrounds = nrounds,
    nthreads = 1,
    ...
  )
}

# LOGO-CV by hand: one xgboost() fit per project, scored on P(success)
xgb_by_hand <- function(data, ...) {
  unlist(lapply(unique(data$pid), function(fold) {
    train <- data[data$pid != fold, ]
    refit <- xgboost::xgboost(
      x = train[, xgb_predictors],
      y = train$replicate,
      nthreads = 1,
      ...
    )
    unname(predict(refit, data[data$pid == fold, xgb_predictors]))
  }))
}

test_that("logo_cv() on an XGBoost fit equals LOGO-CV by hand", {
  skip_if_not_installed("xgboost")
  data(altmejd)
  fit <- fit_xgb(nrounds = 4, max_depth = 2, learning_rate = 0.5)
  res <- logo_cv(fit, data = altmejd, group = "pid", outcome = "replicate")

  expect_equal(res$model, "xgboost")
  expect_identical(
    res$scores$score,
    xgb_by_hand(altmejd, nrounds = 4, max_depth = 2, learning_rate = 0.5)
  )
  # the fit's own setting, read off its parameters and xgboost's defaults
  expect_equal(
    res$grid,
    data.frame(
      depth = 2,
      n_iter = 4,
      eta = 0.5,
      min_child_weight = 1,
      lambda = 1
    )
  )
  # scored on the probability, thresholded at 0.5
  rpp <- res$scores[res$scores$project == "rpp", ]
  expect_identical(
    res$estimates[1, "rpp", ],
    assess(rpp$actual, rpp$score, threshold = 0.5)[list_measures()]
  )
})

test_that("each XGBoost grid column reaches its xgboost() argument", {
  skip_if_not_installed("xgboost")
  data(altmejd)
  fit <- fit_xgb()
  base <- data.frame(
    depth = 2,
    eta = 0.3,
    min_child_weight = 1,
    lambda = 1,
    n_iter = 3
  )
  base_args <- list(
    max_depth = 2,
    learning_rate = 0.3,
    min_child_weight = 1,
    reg_lambda = 1,
    nrounds = 3
  )
  base_scores <- do.call(xgb_by_hand, c(list(altmejd), base_args))
  changes <- list(
    depth = list(max_depth = 4),
    eta = list(learning_rate = 0.05),
    min_child_weight = list(min_child_weight = 6),
    lambda = list(reg_lambda = 20),
    n_iter = list(nrounds = 6)
  )
  for (column in names(changes)) {
    row <- base
    row[[column]] <- changes[[column]][[1L]]
    # an alias such as `eta` would still run, with a warning
    expect_no_warning(
      res <- tune_logo_cv(
        fit,
        row,
        data = altmejd,
        group = "pid",
        outcome = "replicate"
      )
    )
    by_hand <- do.call(
      xgb_by_hand,
      c(list(altmejd), utils::modifyList(base_args, changes[[column]]))
    )
    expect_identical(res$scores$score, by_hand, label = column)
    # so the setting changed the fit
    expect_false(identical(by_hand, base_scores), label = column)
  }
})

test_that("the XGBoost tuner equals one logo_cv() per setting", {
  skip_if_not_installed("xgboost")
  data(altmejd)
  grid <- data.frame(
    depth = c(1, 3),
    eta = c(0.5, 0.1),
    min_child_weight = c(1, 4),
    lambda = c(1, 5),
    n_iter = c(3, 4)
  )
  res <- tune_logo_cv(
    fit_xgb(),
    grid,
    data = altmejd,
    group = "pid",
    outcome = "replicate"
  )
  for (i in seq_len(nrow(grid))) {
    own <- fit_xgb(
      nrounds = grid$n_iter[i],
      max_depth = grid$depth[i],
      learning_rate = grid$eta[i],
      min_child_weight = grid$min_child_weight[i],
      reg_lambda = grid$lambda[i]
    )
    one <- logo_cv(own, data = altmejd, group = "pid", outcome = "replicate")
    expect_identical(res$estimates[i, , ], one$estimates[1, , ])
    expect_identical(
      res$scores$score[res$scores$setting == i],
      one$scores$score
    )
  }
})

test_that("XGBoost rows that differ only in n_iter read one fit's rounds", {
  skip_if_not_installed("xgboost")
  data(altmejd)
  fit <- fit_xgb()
  # one fit of 6 rounds per fold serves both rows
  messages <- capture_messages(
    res <- tune_logo_cv(
      fit,
      data.frame(n_iter = c(2, 6)),
      data = altmejd,
      group = "pid",
      verbose = TRUE,
      outcome = "replicate"
    )
  )
  expect_identical(
    messages,
    paste0("settings 1-2/2 | fold ", unique(altmejd$pid), "\n")
  )
  short <- res$scores$score[res$scores$setting == 1]
  long <- res$scores$score[res$scores$setting == 2]
  # 2 rounds read off the 6-round fit equal a 2-round fit
  alone <- tune_logo_cv(
    fit,
    data.frame(n_iter = 2),
    data = altmejd,
    group = "pid",
    outcome = "replicate"
  )
  expect_identical(short, alone$scores$score)
  expect_identical(short, xgb_by_hand(altmejd, nrounds = 2))
  expect_identical(long, xgb_by_hand(altmejd, nrounds = 6))
  expect_false(identical(short, long))
})

test_that("an XGBoost grid's missing columns take the fit's own values", {
  skip_if_not_installed("xgboost")
  data(altmejd)
  # the fit sets depth and rounds and leaves the rest at xgboost's defaults
  fit <- fit_xgb(nrounds = 3, max_depth = 2)
  res <- tune_logo_cv(
    fit,
    data.frame(lambda = c(1, 5)),
    data = altmejd,
    group = "pid",
    outcome = "replicate"
  )
  expect_equal(
    res$grid,
    data.frame(
      lambda = c(1, 5),
      depth = 2,
      n_iter = 3,
      eta = 0.3,
      min_child_weight = 1
    )
  )
  # the config holds the default rate as "0.300000012"
  expect_identical(res$grid$eta, c(0.3, 0.3))
  expect_identical(
    res$scores$score[res$scores$setting == 1],
    xgb_by_hand(altmejd, nrounds = 3, max_depth = 2)
  )
  expect_identical(
    res$scores$score[res$scores$setting == 2],
    xgb_by_hand(altmejd, nrounds = 3, max_depth = 2, reg_lambda = 5)
  )
  # a rate the fit was given is kept as given
  given <- logo_cv(
    fit_xgb(learning_rate = 0.05),
    data = altmejd,
    group = "pid",
    outcome = "replicate"
  )
  expect_identical(given$grid$eta, 0.05)
})

test_that("an XGBoost fit needs `outcome`, naming the fit's two-level factor", {
  skip_if_not_installed("xgboost")
  data(altmejd)
  fit <- fit_xgb()
  expect_error(
    logo_cv(fit, data = altmejd, group = "pid"),
    "`outcome` is missing. An XGBoost fit keeps no formula",
    fixed = TRUE
  )
  expect_error(
    tune_logo_cv(fit, data.frame(n_iter = 2), data = altmejd, group = "pid"),
    "`outcome` is missing.",
    fixed = TRUE
  )
  expect_error(
    logo_cv(
      fit,
      data = altmejd,
      group = "pid",
      outcome = c("replicate", "pid")
    ),
    "`outcome` must be a single column name.",
    fixed = TRUE
  )
  expect_error(
    logo_cv(fit, data = altmejd, group = "pid", outcome = "nope"),
    "`outcome` column `nope` not found in `data`.",
    fixed = TRUE
  )
  levels_message <- paste0(
    "must be a factor with the levels the fit was trained on, ",
    "\"failure\" and \"success\", in that order."
  )
  expect_error(
    logo_cv(fit, data = altmejd, group = "pid", outcome = "n.o"),
    levels_message,
    fixed = TRUE
  )
  reversed <- altmejd
  reversed$replicate <- factor(
    reversed$replicate,
    levels = c("success", "failure")
  )
  expect_error(
    logo_cv(fit, data = reversed, group = "pid", outcome = "replicate"),
    levels_message,
    fixed = TRUE
  )
})

test_that("each check of an XGBoost fit gives its message", {
  skip_if_not_installed("xgboost")
  data(altmejd)
  run <- function(fit, data = altmejd) {
    logo_cv(fit, data = data, group = "pid", outcome = "replicate")
  }
  # three levels: multi:softprob, not a probability of the second level
  three <- xgboost::xgboost(
    x = altmejd[, xgb_predictors],
    y = factor(rep(c("a", "b", "c"), length.out = nrow(altmejd))),
    nrounds = 2,
    nthreads = 1
  )
  expect_error(
    run(three),
    "`x` must be fitted with the objective \"binary:logistic\"",
    fixed = TRUE
  )
  unnamed <- xgboost::xgboost(
    x = unname(as.matrix(altmejd[, xgb_predictors])),
    y = altmejd$replicate,
    nrounds = 2,
    nthreads = 1
  )
  expect_error(run(unnamed), "`x` has no feature names.", fixed = TRUE)
  expect_error(
    run(fit_xgb(), data = altmejd[setdiff(names(altmejd), "n.o")]),
    "`data` lacks the fit's predictor column(s): n.o.",
    fixed = TRUE
  )

  # arguments a refit from the stored parameters would drop
  n <- nrow(altmejd)
  expect_error(
    run(fit_xgb(weights = rep(1, n))),
    "`x` was fitted with `weights`, which its stored parameters do not keep",
    fixed = TRUE
  )
  expect_error(
    run(fit_xgb(base_margin = rep(0, n))),
    "`x` was fitted with `base_margin`,",
    fixed = TRUE
  )
  expect_error(
    run(fit_xgb(feature_weights = rep(1, 4))),
    "`x` was fitted with `feature_weights`,",
    fixed = TRUE
  )
  # the evaluation log goes to the console
  capture.output(
    stopping <- fit_xgb(eval_set = 0.2, early_stopping_rounds = 2)
  )
  expect_error(
    run(stopping),
    "`x` was fitted with `eval_set`, `early_stopping_rounds`,",
    fixed = TRUE
  )
})

test_that("an XGBoost fit refuses what it cannot honor", {
  skip_if_not_installed("xgboost")
  data(altmejd)
  fit <- fit_xgb()
  expect_error(
    tune_logo_cv(
      fit,
      data.frame(n_iter = 2:3),
      data = altmejd,
      group = "pid",
      nested = TRUE,
      outcome = "replicate"
    ),
    "`nested = TRUE` is for adaboost() fits only.",
    fixed = TRUE
  )
  expect_error(
    tune_logo_cv(
      fit,
      data.frame(mtry = 1),
      data = altmejd,
      group = "pid",
      outcome = "replicate"
    ),
    paste0(
      "mtry set nothing. A grid may hold `depth`, `n_iter`, `eta`, ",
      "`min_child_weight` and `lambda`."
    ),
    fixed = TRUE
  )
  expect_error(
    tune_logo_cv(
      fit,
      data.frame(n_iter = 2),
      data = altmejd,
      group = "pid",
      outcome = "replicate",
      n_iter = 5
    ),
    "Unused argument(s): `n_iter`.",
    fixed = TRUE
  )
  expect_error(
    logo_cv(
      fit,
      data = altmejd,
      group = "pid",
      outcome = "replicate",
      grid = 1
    ),
    "Unused argument(s): `grid`.",
    fixed = TRUE
  )

  # xgb.train() returns an xgb.Booster without the class "xgboost"
  booster <- xgboost::xgb.train(
    params = list(objective = "binary:logistic", nthread = 1),
    data = xgboost::xgb.DMatrix(
      as.matrix(altmejd[, xgb_predictors]),
      label = as.integer(altmejd$replicate == "success")
    ),
    nrounds = 2
  )
  expect_error(
    logo_cv(booster, data = altmejd, group = "pid", outcome = "replicate"),
    "Fit the model with xgboost::xgboost(x = <data frame>, y = <factor>)",
    fixed = TRUE
  )
  expect_error(
    tune_logo_cv(
      booster,
      data.frame(n_iter = 2),
      data = altmejd,
      group = "pid",
      outcome = "replicate"
    ),
    "Fit the model with xgboost::xgboost(x = <data frame>, y = <factor>)",
    fixed = TRUE
  )
})

test_that("an XGBoost held-out group of one row keeps a column per setting", {
  skip_if_not_installed("xgboost")
  data(altmejd)
  data <- altmejd
  data$part <- "train"
  data$part[1] <- "solo"
  fit <- fit_xgb()
  grid <- data.frame(n_iter = c(2, 4))
  res <- tune_logo_cv(
    fit,
    grid,
    data = data,
    group = "part",
    holdout = "solo",
    outcome = "replicate"
  )
  expect_identical(res$projects$n, 1L)
  by_hand <- vapply(
    grid$n_iter,
    function(n) {
      refit <- xgboost::xgboost(
        x = data[-1, xgb_predictors],
        y = data$replicate[-1],
        nrounds = n,
        nthreads = 1
      )
      unname(predict(refit, data[1, xgb_predictors]))
    },
    numeric(1)
  )
  expect_identical(res$scores$score, by_hand)

  # the refit itself returns one row, one column per setting
  score <- refit_score(
    fit,
    train = data[-1, ],
    test = data[1, ],
    setting = res$grid,
    outcome = "replicate"
  )
  expect_identical(dim(score), c(1L, 2L))
})

test_that("the XGBoost tuner gives the same at any number of cores", {
  skip_on_os("windows")
  skip_if_not_installed("xgboost")
  data(altmejd)
  run <- function(cores) {
    tune_logo_cv(
      fit_xgb(),
      expand.grid(depth = 1:2, n_iter = c(2, 4)),
      data = altmejd,
      group = "pid",
      cores = cores,
      outcome = "replicate"
    )
  }
  expect_identical(run(2), run(1))
})

test_that("print() and summary() show an XGBoost fit's five settings", {
  local_reproducible_output(width = 200)
  skip_if_not_installed("xgboost")
  data(altmejd)
  statistics <- c("mean", "sd", "min", "max", "stable")
  settings <- c("depth", "n_iter", "eta", "min_child_weight", "lambda")
  res <- tune_logo_cv(
    fit_xgb(),
    data.frame(depth = 1:2),
    data = altmejd,
    group = "pid",
    outcome = "replicate"
  )
  projects <- dimnames(res$estimates)$project
  expect_named(
    read_printed_table(capture.output(print(res))),
    c(settings, projects)
  )
  expect_named(summary(res), c(settings, statistics))
  set.seed(3)
  boot <- suppressWarnings(bootstrap(res, n_resample = 20, setting = 2))
  expect_identical(boot$estimates[1, , ], res$estimates[2, , ])
})

test_that("the tutorial's XGBoost pick rebuilds the benchmark's xgboost row", {
  skip_if_not_installed("xgboost")
  data(altmext)
  data(benchmark)
  df <- altmext
  pred_nms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  out_nme <- "replicate"
  df_noholz <- df[df$pid != "holz", ]
  xgb_fit <- xgboost::xgboost(
    x = df_noholz[, pred_nms],
    y = df_noholz[, out_nme],
    nthreads = 1
  )
  # row 105 of the tutorial's grid of 180
  best <- data.frame(
    depth = 4,
    eta = 0.05,
    min_child_weight = 5,
    lambda = 10,
    n_iter = 200
  )
  tuning <- tune_logo_cv(
    xgb_fit,
    best,
    data = df_noholz,
    group = "pid",
    outcome = out_nme
  )
  final <- tune_logo_cv(
    xgb_fit,
    best,
    data = df,
    group = "pid",
    holdout = "holz",
    outcome = out_nme
  )
  projects <- dimnames(tuning$estimates)$project
  for (metric in names(benchmark)) {
    row <- benchmark[[metric]][benchmark[[metric]]$model == "xgboost", ]
    expect_identical(
      unname(unlist(row[projects])),
      unname(tuning$estimates[1, projects, metric]),
      label = metric
    )
    expect_identical(
      row$holz,
      final$estimates[1, "holz", metric],
      label = metric
    )
  }
})

# ---- plot.logo_cv ------------------------------------------------------------

boot_by_project <- function(n_resample = 200) {
  data(altmejd)
  prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  voinms <- c(prednms, "replicate")
  out <- list()
  for (rp in c("eerp", "ml1", "ml3", "rpp", "ssrp")) {
    tr <- altmejd[altmejd$pid != rp, ]
    te <- altmejd[altmejd$pid == rp, ]
    fit <- rpart::rpart(
      replicate ~ .,
      data = tr[, voinms],
      maxdepth = 1,
      model = TRUE
    ) |>
      adaboost(n_iter = 5, eta = 1, verbose = FALSE, check_inputs = FALSE)
    m <- predict(
      fit,
      te[, prednms],
      type = "margin",
      verbose = FALSE,
      check_inputs = FALSE
    )
    set.seed(112)
    out[[rp]] <- suppressWarnings(bootstrap(
      te$replicate,
      m,
      n_resample = n_resample
    ))
  }
  out
}

test_that("bootstrap() keeps the draws so any level can be asked for later", {
  f <- c(rep(1L, 20), rep(0L, 20))
  set.seed(1)
  ci <- bootstrap(f, rnorm(40) + f, n_resample = 150)
  d <- attr(ci, "draws")

  expect_true(is.matrix(d))
  expect_equal(nrow(d), 150L)
  expect_true(all(c("auroc", "auprc", "bacc") %in% colnames(d)))
  # the stored interval is recoverable from the stored draws
  q <- unname(quantile(d[, "auroc"], c(0.025, 0.975), na.rm = TRUE))
  expect_equal(unname(ci$lower[ci$metric == "auroc"]), q[1])
  expect_equal(unname(ci$upper[ci$metric == "auroc"]), q[2])
})

test_that("plot.logo_cv() draws with and without intervals", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")

  pf <- tempfile(fileext = ".png")
  png(pf)
  expect_silent(plot(res))
  dev.off()
  expect_gt(file.size(pf), 0)

  boot <- res
  boot$ci <- boot_by_project()
  png(pf)
  expect_silent(plot(boot))
  expect_silent(plot(boot, metric = "auprc"))
  expect_silent(plot(boot, levels = c(0.5, 0.9)))
  expect_silent(plot(boot, baseline = NA))
  expect_silent(plot(boot, baseline = 0.6))
  dev.off()

  # attached intervals add bands: more drawing calls than the bare plot
  calls <- function(x) {
    pdf(NULL)
    on.exit(dev.off())
    dev.control("enable")
    plot(x)
    length(recordPlot()[[1]])
  }
  expect_gt(calls(boot), calls(res))

  expect_error(plot(res, metric = "not_a_measure"), "no estimates")
})

test_that("plot.logo_cv() returns its input invisibly", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")
  png(tempfile(fileext = ".png"))
  out <- withVisible(plot(res))
  dev.off()
  expect_false(out$visible)
  expect_identical(out$value, res)
})

test_that("the fit's control reaches every refit", {
  # minsplit and minbucket are what actually govern how the trees grow, so
  # loosening them on the fit has to change the result -- for a plain tree and
  # for adaboost's base learners alike.
  data(altmejd)
  au <- function(fit) {
    res <- logo_cv(fit, data = altmejd, group = "pid")
    summary(res, metric = "auroc")$mean
  }

  tree <- rpart::rpart(fml, data = altmejd, method = "class", maxdepth = 4)
  loose_tree <- rpart::rpart(
    fml,
    data = altmejd,
    method = "class",
    maxdepth = 4,
    minsplit = 5,
    minbucket = 2
  )
  expect_false(isTRUE(all.equal(au(tree), au(loose_tree))))
  expect_false(isTRUE(all.equal(
    au(fit_ada(n_iter = 10)),
    au(fit_ada(n_iter = 10, minsplit = 5, minbucket = 2))
  )))

  # and the tree's refit is its own call run again on the training rows
  train <- altmejd[altmejd$pid != "eerp", ]
  test <- altmejd[altmejd$pid == "eerp", ]
  res <- logo_cv(loose_tree, data = altmejd, group = "pid")
  by_hand <- rpart::rpart(
    fml,
    data = train,
    method = "class",
    maxdepth = 4,
    minsplit = 5,
    minbucket = 2
  ) |>
    predict(test, type = "prob")
  expect_equal(
    res$scores$score[res$scores$project == "eerp"],
    unname(by_hand[, "success"])
  )
})

test_that("adaboost's base learners match a direct adaboost() fit", {
  # the default control is shared, so going through logo_cv() and calling
  # adaboost() by hand must fit the same trees on the same fold
  data(altmejd)
  voi <- c("replicate", "power.o", "effect_size.o", "n.o", "p_value.o")
  train <- altmejd[altmejd$pid != "eerp", voi]
  test <- altmejd[altmejd$pid == "eerp", voi]

  by_hand <- rpart::rpart(
    fml,
    data = train,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 10, eta = 1, verbose = FALSE, check_inputs = FALSE)
  want <- auroc(
    test$replicate,
    predict(
      by_hand,
      test[, -1],
      type = "margin",
      verbose = FALSE,
      check_inputs = FALSE
    )
  )

  res <- logo_cv(fit_ada(n_iter = 10), data = altmejd, group = "pid")
  expect_equal(res$estimates[1, "eerp", "auroc"], want, tolerance = 1e-12)
})
test_that("the learner's control reaches every boosted tree", {
  # the point of taking a fitted tree: what the caller set is what gets boosted,
  # and only the two speed controls are overridden
  data(altmejd)
  voi <- c("replicate", "power.o", "effect_size.o", "n.o", "p_value.o")
  d <- altmejd[, voi]

  h <- rpart::rpart(
    fml,
    data = d,
    maxdepth = 2,
    minsplit = 40,
    cp = 0,
    model = TRUE
  )
  fit <- adaboost(
    h,
    n_iter = 20,
    eta = 0.55,
    verbose = FALSE,
    check_inputs = FALSE
  )

  ctrl <- attr(fit, "control")
  expect_equal(ctrl$maxdepth, 2)
  expect_equal(ctrl$minsplit, 40)
  expect_equal(ctrl$cp, 0)
  # speed, not structure: the only two adaboost() overrides
  expect_equal(ctrl$xval, 0)
  expect_equal(ctrl$maxsurrogate, 0)

  # and it reaches the trees themselves, not only the attribute
  expect_true(all(
    vapply(fit, function(z) z$h$control$maxdepth, numeric(1)) == 2
  ))
  expect_true(all(
    vapply(fit, function(z) z$h$control$minsplit, numeric(1)) == 40
  ))

  # a different learner is a different ensemble, so the control is not ignored
  stumps <- rpart::rpart(fml, data = d, maxdepth = 1, cp = 0, model = TRUE) |>
    adaboost(n_iter = 20, eta = 0.55, verbose = FALSE, check_inputs = FALSE)
  expect_false(isTRUE(all.equal(
    vapply(fit, function(z) z$alpha, numeric(1)),
    vapply(stumps, function(z) z$alpha, numeric(1))
  )))
})

test_that("model weights stay finite when a base learner refuses to split", {
  # cp = 0.01 lets a weak learner come back as a bare root once the weights
  # concentrate. That is allowed; an infinite or negative alpha is not.
  data(altmejd)
  voi <- c("replicate", "power.o", "effect_size.o", "n.o", "p_value.o")
  fit <- rpart::rpart(
    fml,
    data = altmejd[, voi],
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 300, eta = 0.55, verbose = FALSE, check_inputs = FALSE)
  alphas <- vapply(fit, function(z) z$alpha, numeric(1))
  rootonly <- vapply(fit, function(z) nrow(z$h$frame) == 1L, logical(1))

  expect_true(any(rootonly))
  expect_true(all(is.finite(alphas)))
  expect_true(all(alphas > 0))
})
# ---- the pipeline: h |> adaboost() |> logo_cv() |> bootstrap() |> plot() ----

test_that("logo_cv() on an adaboost fit equals tune_logo_cv() with the fit's own row", {
  data(altmejd)
  prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  h <- rpart::rpart(
    replicate ~ .,
    data = altmejd[, c(prednms, "replicate")],
    maxdepth = 1,
    model = TRUE
  )
  fit <- adaboost(h, n_iter = 5, eta = 0.5, verbose = FALSE)

  from_fit <- logo_cv(fit, data = altmejd, group = "pid")
  from_grid <- tune_logo_cv(
    fit,
    grid = data.frame(depth = 1, n_iter = 5, eta = 0.5),
    data = altmejd,
    group = "pid"
  )

  expect_identical(from_fit$estimates, from_grid$estimates)
  expect_identical(from_fit$scores, from_grid$scores)
  expect_equal(
    from_fit$grid[, c("n_iter", "eta", "depth")],
    data.frame(n_iter = 5, eta = 0.5, depth = 1)
  )

  # the `.` of the learner's formula reads the same columns as the names
  spelled_out <- logo_cv(fit_ada(eta = 0.5), data = altmejd, group = "pid")
  expect_identical(from_fit$estimates, spelled_out$estimates)
})

test_that("logo_cv() refits the fit's split rule, not the default", {
  data(altmejd)
  prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  fit_split <- function(rule) {
    rpart::rpart(
      replicate ~ .,
      data = altmejd[, c(prednms, "replicate")],
      maxdepth = 2,
      parms = list(split = rule),
      model = TRUE
    ) |>
      adaboost(n_iter = 5, eta = 1, verbose = FALSE)
  }
  info <- logo_cv(fit_split("information"), data = altmejd, group = "pid")
  gini <- logo_cv(fit_split("gini"), data = altmejd, group = "pid")
  expect_identical(attr(fit_split("information"), "split"), "information")
  expect_false(identical(info$scores$score, gini$scores$score))
})

test_that("logo_cv() keeps the held-out scores of every fold", {
  data(altmejd)
  prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")

  expect_equal(nrow(res$scores), nrow(altmejd))
  expect_setequal(unique(res$scores$project), res$projects$project)

  # the ssrp scores are what a manual refit on the other projects predicts
  tr <- altmejd[altmejd$pid != "ssrp", ]
  te <- altmejd[altmejd$pid == "ssrp", ]
  manual <- rpart::rpart(
    replicate ~ .,
    data = tr[, c(prednms, "replicate")],
    maxdepth = 1,
    cp = 0.01,
    xval = 0,
    maxsurrogate = 0,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE) |>
    predict(te[, prednms], type = "margin", verbose = FALSE)
  expect_equal(res$scores$score[res$scores$project == "ssrp"], unname(manual))
})

test_that("bootstrap() on logo_cv equals the per-project loop", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")

  set.seed(3)
  boot <- suppressWarnings(bootstrap(res, n_resample = 50))

  set.seed(3)
  manual <- lapply(res$projects$project, function(p) {
    rows <- res$scores$project == p
    suppressWarnings(bootstrap(
      res$scores$actual[rows],
      res$scores$score[rows],
      n_resample = 50
    ))
  })
  names(manual) <- res$projects$project

  expect_s3_class(boot, "logo_cv")
  expect_identical(boot$ci, manual)
  expect_identical(boot$estimates, res$estimates)
})

test_that("bootstrap() names the project in its warnings", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")
  set.seed(3)
  warning_texts <- capture_warnings(
    bootstrap(res, n_resample = 50, stratified = TRUE)
  )
  # one per project, in project order
  expect_identical(
    sub(": stratified.*", "", warning_texts),
    paste("project", res$projects$project)
  )
})

test_that("bootstrap() on logo_cv needs one setting and stored scores", {
  data(altmejd)
  grid <- expand.grid(n_iter = c(3, 5), eta = 1, depth = 1)
  res <- tune_logo_cv(fit_ada(), grid, data = altmejd, group = "pid")

  expect_error(bootstrap(res, n_resample = 20), "Choose one with `setting`")
  expect_error(bootstrap(res, n_resample = 20, setting = 3), "one row number")

  set.seed(3)
  one <- suppressWarnings(bootstrap(res, n_resample = 20, setting = 2))
  expect_equal(dim(one$estimates)[1], 1L)
  expect_identical(one$estimates[1, , ], res$estimates[2, , ])
  expect_equal(one$grid$n_iter, 5)

  no_scores <- res
  no_scores$scores <- NULL
  expect_error(bootstrap(no_scores, setting = 2), "Rerun logo_cv")
})

test_that("bootstrap() keeps the holdout of a logo_cv result", {
  data(altmejd)
  res <- logo_cv(
    fit_ada(),
    data = altmejd,
    group = "pid",
    holdout = c("rpp", "ssrp")
  )
  set.seed(3)
  boot <- suppressWarnings(bootstrap(res, n_resample = 20))
  expect_named(boot$ci, c("rpp", "ssrp"))
  expect_equal(attr(boot, "holdout"), c("rpp", "ssrp"))
})

test_that("nested tune_logo_cv keeps the chosen setting's scores and bootstraps", {
  data(altmejd)
  grid <- expand.grid(n_iter = c(3, 5), eta = 1, depth = 1)
  res <- tune_logo_cv(
    fit_ada(),
    grid,
    data = altmejd,
    group = "pid",
    nested = TRUE
  )
  expect_equal(nrow(res$scores), nrow(altmejd))
  set.seed(3)
  boot <- suppressWarnings(bootstrap(res, n_resample = 20))
  expect_named(boot$ci, res$projects$project)
})
test_that("accuracy's reference line is the majority-class rate", {
  # always predicting the larger class scores max(p, 1 - p), not p
  reference <- find_reference("acc", "auto", c(0.3, 0.8))
  expect_equal(reference$values, c(0.7, 0.8))
  expect_match(reference$label, "majority-class rate")
})

test_that("every patk budget gets the base-rate floor, not only the default", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")
  calls <- function(...) {
    pdf(NULL)
    on.exit(dev.off())
    dev.control("enable")
    plot(res, ...)
    length(recordPlot()[[1]])
  }
  for (m in c("patk_3", "patk_5", "patk", "auprc")) {
    expect_gt(calls(metric = m), calls(metric = m, baseline = NA))
  }
})

test_that("the y-axis keeps its tick at 1 and draws none above", {
  data(altmejd)
  res <- logo_cv(fit_ada(n_iter = 50), data = altmejd, group = "pid")
  y_ticks <- function(metric) {
    pdf(NULL)
    on.exit(dev.off())
    dev.control("enable")
    plot(res, metric = metric)
    items <- recordPlot()[[1]]
    fn <- vapply(
      items,
      function(e) {
        x <- e[[2]][[1]]
        if (is.list(x) && !is.null(x$name)) x$name else ""
      },
      character(1)
    )
    # the last side-2 axis: plot() records its own suppressed one first
    left <- Filter(function(e) isTRUE(e[[2]][[2]] == 2), items[fn == "C_axis"])
    left[[length(left)]][[2]][[3]]
  }
  # three projects score 1 at the top 3, so the axis must reach 1
  ticks <- y_ticks("patk_3")
  expect_true(1 %in% ticks)
  expect_true(all(ticks <= 1))
})
