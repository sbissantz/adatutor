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
      "metric",
      "sort_by",
      "selected"
    )
  )
  # a plain run uses neither
  expect_identical(res$metric, NA_character_)
  expect_identical(res$sort_by, NA_character_)

  n_metrics <- length(assess(c(1, 0), c(1, -1)))
  expect_equal(dim(res$estimates), c(1L, 5L, n_metrics))
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

  n_metrics <- length(assess(c(1, 0), c(1, -1)))
  expect_equal(dim(res$estimates), c(nrow(grid), 5L, n_metrics))

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
# then the table of estimates. The end of the output closes the last part.
read_printed_parts <- function(out) {
  blanks <- c(which(out == ""), length(out) + 1L)
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
  # the estimates per project, never a mean or standard error across folds
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

  expect_true("  metric:   auroc" %in% out)
  expect_false(any(grepl("measure", out)))
  # the spread across the projects, never a mean or SD of it
  expect_false(any(grepl("\\bmean\\b|\\bSE\\b|\\bsd\\b", out)))
  # the table is the last thing printed
  expect_identical(
    tail(out, 1L),
    tail(read_printed_parts(out)$table, 1L)
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

test_that("print() decides decimals by the metric's name, not its values", {
  local_reproducible_output(width = 200)
  data(altmejd)
  res <- tune_logo_cv(fit_ada(), cp_grid, data = altmejd, group = "pid")
  read_cells <- function(metric) {
    out <- capture.output(print(res, metric = metric))
    rows <- strsplit(trimws(read_printed_parts(out)$table[-1]), " +")
    unlist(lapply(rows, \(row) tail(row, 5L)))
  }
  # a rate whose cells are all 0 or 1 still shows three decimals
  res$estimates[,, "sens"] <- rep(c(0, 1), length.out = 15L)
  expect_setequal(read_cells("sens"), c("0.000", "1.000"))
  # and each count stays whole
  for (count in c("tp", "tn", "fp", "fn")) {
    expect_true(all(grepl("^[0-9]+$", read_cells(count))), label = count)
  }
})

test_that("print() shows the settings unrounded", {
  local_reproducible_output(width = 200)
  data(altmejd)
  res <- tune_logo_cv(
    fit_ada(),
    data.frame(n_iter = 3, eta = 0.0025, cp = 0.0125),
    data = altmejd,
    group = "pid"
  )
  table <- read_printed_parts(capture.output(print(res)))$table
  expect_identical(
    head(strsplit(trimws(table[2]), " +")[[1]], 4L),
    c("3", "0.0025", "0.0125", "1")
  )
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

  expect_true("  holdout:  ssrp, eerp" %in% out)
  block <- read_printed_parts(out)$projects
  expect_equal(strsplit(trimws(block[1]), " +")[[1]], c("ssrp", "eerp"))
  table <- read_printed_table(out)
  expect_named(table, c("depth", "n_iter", "eta", "ssrp", "eerp"))
  expect_equal(nrow(table), 1L)
})

test_that("print() gives each held-out project its own n and base rate", {
  local_reproducible_output(width = 200)
  data(altmejd)
  # not the data's order, which is alphabetical
  holdout <- c("ssrp", "eerp", "ml3")
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid", holdout = holdout)
  block <- read_printed_parts(capture.output(print(res)))$projects
  in_project <- lapply(holdout, \(project) altmejd$pid == project)

  expect_equal(strsplit(trimws(block[1]), " +")[[1]], holdout)
  expect_equal(
    strsplit(block[2], " +")[[1]],
    c("n", vapply(in_project, \(rows) as.character(sum(rows)), ""))
  )
  expect_equal(
    strsplit(sub("^base rate +", "", block[3]), " +")[[1]],
    vapply(
      in_project,
      \(rows) sprintf("%.2f", mean(altmejd$replicate[rows] == "success")),
      ""
    )
  )
  # each column finds its project by name, not by its row in `projects`
  res$projects <- res$projects[c(3, 1, 2), ]
  expect_identical(
    read_printed_parts(capture.output(print(res)))$projects,
    block
  )
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
  expect_named(summary(logit), c("mean", "sd", "lower", "min", "max"))
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

test_that("print() needs one metric name, and notes one without estimates", {
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")
  # summary()'s error, raised before the header prints
  for (bad in list(NA, NA_character_, c("auroc", "acc"), character(0), 1)) {
    printed <- capture.output(
      error <- tryCatch(print(res, metric = bad), error = identity)
    )
    expect_s3_class(error, "error")
    expect_identical(
      conditionMessage(error),
      tryCatch(summary(res, metric = bad), error = conditionMessage)
    )
    expect_match(
      conditionMessage(error),
      "single name among the metrics of the result"
    )
    expect_identical(printed, character(0))
  }

  # one name the result lacks gets the header and a note
  printed <- capture.output(shown <- withVisible(print(res, metric = "nope")))
  expect_identical(
    printed,
    c(
      "Leave-one-group-out cross-validation",
      "  model:    adaboost",
      "  settings: 1",
      "  metric:   nope",
      "",
      "no estimates for `nope`"
    )
  )
  expect_false(shown$visible)
  expect_identical(shown$value, res)
})

# the lines a bootstrapped result prints between the table of estimates and
# the footer: the line naming the intervals, then the block of projects
read_printed_intervals <- function(out) {
  blanks <- c(which(out == ""), length(out) + 1L)
  out[(blanks[3] + 1L):(blanks[4] - 1L)]
}

test_that("print() shows each project's interval of the printed metric", {
  local_reproducible_output(width = 200)
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")
  set.seed(3)
  boot <- suppressWarnings(bootstrap(res, n_resample = 200))
  projects <- dimnames(boot$estimates)$project
  # each column finds its project by name, not by its place in `ci`
  boot$ci <- rev(boot$ci)
  # the cells as printed
  read_block <- function(metric) {
    out <- capture.output(print(boot, metric = metric))
    read.table(
      text = read_printed_intervals(out)[-1],
      header = TRUE,
      colClasses = "character",
      na.strings = character()
    )
  }
  read_ci <- function(metric, column) {
    vapply(
      boot$ci[projects],
      \(ci) ci[[column]][ci$metric == metric],
      numeric(1)
    )
  }

  for (metric in c("auroc", "auprc")) {
    block <- read_block(metric)
    expect_named(block, projects)
    expect_identical(rownames(block), c("lower", "upper", "dropped"))
    for (bound in c("lower", "upper")) {
      cells <- unlist(block[bound, ])
      # three decimals, as the estimates
      expect_true(all(grepl("^[01]\\.[0-9]{3}$", cells)), label = metric)
      expect_equal(
        as.numeric(cells),
        round(read_ci(metric, bound), 3),
        ignore_attr = TRUE,
        label = paste(metric, bound)
      )
    }
    expect_equal(
      as.numeric(unlist(block["dropped", ])),
      read_ci(metric, "drop"),
      ignore_attr = TRUE
    )
  }

  # a count prints whole, though its bounds fall between two draws
  bounds <- c(read_ci("tp", "lower"), read_ci("tp", "upper"))
  expect_false(all(bounds == round(bounds)))
  block <- read_block("tp")
  for (row in c("lower", "upper", "dropped")) {
    expect_true(all(grepl("^[0-9]+$", unlist(block[row, ]))), label = row)
  }
  expect_equal(
    as.numeric(unlist(block["lower", ])),
    round(read_ci("tp", "lower")),
    ignore_attr = TRUE
  )
  expect_equal(
    as.numeric(unlist(block["upper", ])),
    round(read_ci("tp", "upper")),
    ignore_attr = TRUE
  )

  # a negative bound widens only its own project's column
  block <- read_block("mcc")
  lower <- unlist(block["lower", ])
  expect_true(any(startsWith(lower, "-")) && !all(startsWith(lower, "-")))
  widths <- pmax(
    nchar(names(block)),
    vapply(block, \(cells) max(nchar(cells)), integer(1))
  )
  lines <- read_printed_intervals(capture.output(print(boot, metric = "mcc")))
  expect_identical(
    unique(nchar(lines[-1])),
    nchar("dropped") + sum(1L + widths)
  )

  # a project whose bootstrap gave up prints NA
  boot$ci$ml3[c("lower", "upper")] <- NA_real_
  block <- read_block("auroc")
  expect_identical(
    unname(unlist(block[c("lower", "upper"), "ml3"])),
    c("NA", "NA")
  )
})

test_that("print() shows a given-up project's dropped draws in full", {
  local_reproducible_output(width = 200)
  data(altmejd)
  # ml3 without its successes: every resample has one class
  one_class <- altmejd[
    !(altmejd$pid == "ml3" & altmejd$replicate == "success"),
  ]
  h <- rpart::rpart(
    fml,
    data = one_class,
    method = "class",
    model = TRUE,
    maxdepth = 1
  )
  fit <- adaboost(h, n_iter = 5, eta = 1, verbose = FALSE)
  res <- logo_cv(
    fit,
    data = one_class,
    group = "pid",
    holdout = c("ml3", "ssrp")
  )
  # the default n_resample: ml3 drops 100,000 draws, which format() would print
  # as 1e+05; stratified, so ssrp drops none
  set.seed(3)
  boot <- suppressWarnings(bootstrap(res, stratified = TRUE))
  expect_identical(
    vapply(boot$ci, \(ci) ci$drop[1], integer(1)),
    c(ml3 = 100000L, ssrp = 0L)
  )

  lines <- read_printed_intervals(capture.output(print(boot)))[-1]
  block <- read.table(
    text = lines,
    header = TRUE,
    colClasses = "character",
    na.strings = character()
  )
  expect_identical(unname(unlist(block[, "ml3"])), c("NA", "NA", "100000"))
  expect_identical(block["dropped", "ssrp"], "0")
  # the wide count widens only its own column: ssrp stays as wide as 0.xxx
  expect_identical(lines[4], "dropped 100000     0")
})

test_that("print() names the level and the resamples of the intervals", {
  local_reproducible_output(width = 200)
  data(altmejd)
  read_line <- function(x) read_printed_intervals(capture.output(print(x)))[1]

  # one held-out project keeps 1,000 resamples quick
  one <- logo_cv(fit_ada(), data = altmejd, group = "pid", holdout = "ssrp")
  set.seed(3)
  expect_identical(
    read_line(suppressWarnings(bootstrap(one, n_resample = 1000))),
    "95% bootstrap CI (1,000 resamples/project)"
  )
  # one resample is singular
  set.seed(3)
  expect_identical(
    read_line(suppressWarnings(bootstrap(one, n_resample = 1))),
    "95% bootstrap CI (1 resample/project)"
  )
  set.seed(3)
  expect_identical(
    read_line(
      suppressWarnings(bootstrap(one, n_resample = 1, stratified = TRUE))
    ),
    "95% bootstrap CI (1 stratified resample/project)"
  )

  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")
  set.seed(3)
  stratified <- suppressWarnings(
    bootstrap(res, n_resample = 200, stratified = TRUE, conf = 0.9)
  )
  line <- read_line(stratified)
  expect_identical(line, "90% bootstrap CI (200 stratified resamples/project)")
  expect_lt(nchar(line), 72)
  set.seed(3)
  expect_identical(
    read_line(suppressWarnings(bootstrap(res, n_resample = 20, conf = 0.995))),
    "99.5% bootstrap CI (20 resamples/project)"
  )

  # the longest line in sight, set by hand: a million draws take too long
  long <- stratified
  long$ci <- lapply(long$ci, \(ci) {
    attr(ci, "conf") <- 0.999
    attr(ci, "n_resample") <- 1000000
    ci
  })
  line <- read_line(long)
  expect_identical(
    line,
    "99.9% bootstrap CI (1,000,000 stratified resamples/project)"
  )
  expect_lt(nchar(line), 72)
})

test_that("print() adds the intervals only when bootstrap() attached them", {
  local_reproducible_output(width = 200)
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")
  set.seed(3)
  boot <- suppressWarnings(bootstrap(res, n_resample = 20))

  plain <- capture.output(print(res))
  expect_false(any(grepl("bootstrap CI", plain)))
  # a blank line, the line naming the intervals and the block of four lines
  # are all a bootstrapped result adds
  printed <- capture.output(shown <- withVisible(print(boot)))
  added <- which(printed == "")[3] + 0:5
  expect_match(printed[added[2]], "bootstrap CI")
  expect_identical(printed[-added], plain)

  expect_false(shown$visible)
  expect_identical(shown$value, boot)
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
    c("depth", "n_iter", "eta", "cp", "mean", "sd", "lower", "min", "max")
  )
  expect_equal(rownames(s), c("1", "2", "3"))
  expect_equal(s[names(grid)], grid, ignore_attr = TRUE)

  auroc <- res$estimates[,, "auroc"]
  expect_identical(s$mean, unname(rowMeans(auroc)))
  expect_identical(s$sd, unname(apply(auroc, 1, sd)))
  expect_identical(s$min, unname(apply(auroc, 1, min)))
  expect_identical(s$max, unname(apply(auroc, 1, max)))
  expect_identical(s$lower, s$mean - s$sd)
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
  stats <- c("mean", "sd", "lower", "min", "max")
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
  expect_true(is.na(s$lower))
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

test_that("a factor metric acts as its name in print(), summary() and plot()", {
  local_reproducible_output(width = 200)
  data(altmejd)
  res <- logo_cv(fit_ada(), data = altmejd, group = "pid")
  # the bands come from the bootstrap draws, so plot() has some to pick from
  set.seed(3)
  res <- suppressWarnings(bootstrap(res, n_resample = 20))
  # its code, 2, would pick the second metric, tn
  acc <- factor("acc", levels = c("auroc", "acc"))

  expect_identical(
    capture.output(print(res, metric = acc)),
    capture.output(print(res, metric = "acc"))
  )
  expect_identical(summary(res, metric = acc), summary(res, metric = "acc"))
  record <- function(metric) {
    pdf(NULL)
    on.exit(dev.off())
    dev.control("enable")
    plot(res, metric = metric)
    recordPlot()[[1]]
  }
  expect_identical(record(acc), record("acc"))

  # a nested result reads its estimates from a project x metric matrix
  nested <- tune_logo_cv(
    fit_ada(),
    data.frame(n_iter = c(3, 5)),
    data = altmejd,
    group = "pid",
    nested = TRUE
  )
  expect_identical(
    capture.output(print(nested, metric = acc)),
    capture.output(print(nested, metric = "acc"))
  )
  expect_identical(
    summary(nested, metric = acc),
    summary(nested, metric = "acc")
  )
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
  expect_true(all(is.na(s[4, c("mean", "sd", "lower", "min", "max")])))

  by_min <- summary(res, "auroc", sort_by = "min")
  expect_equal(rownames(by_min), c("2", "3", "5", "6", "1", "4"))
  # the tutorial's rule: the highest mean minus SD
  ranked <- summary(res, "auroc", sort_by = "lower")
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
      "depth, n_iter, eta, mean, sd, lower, min, max",
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
  expect_true("  holdout:  ssrp, eerp" %in% capture.output(print(some)))
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

test_that("on Windows, cores = 2 runs a socket cluster and gives what one core gives", {
  local_mocked_bindings(read_os_type = function() "windows")
  data(altmejd)
  grid <- expand.grid(depth = 1:2, n_iter = c(3, 6))
  expect_no_message(
    on_windows <- tune_logo_cv(
      fit_ada(),
      grid,
      data = altmejd,
      group = "pid",
      cores = 2
    )
  )
  expect_identical(
    on_windows,
    tune_logo_cv(fit_ada(), grid, data = altmejd, group = "pid")
  )

  # each inner search starts a cluster, so two outer folds keep it short
  run_nested <- function(cores) {
    tune_logo_cv(
      fit_ada(),
      grid,
      data = altmejd,
      group = "pid",
      holdout = c("eerp", "ml3"),
      nested = TRUE,
      cores = cores
    )
  }
  expect_identical(run_nested(2), run_nested(1))
})

test_that("on Windows, the workers run this session's adatutor and options", {
  local_mocked_bindings(read_os_type = function() "windows")
  # `scipen` changes how the code check deparses numbers
  old <- options(warn = 1, na.action = "na.exclude", scipen = 999)
  on.exit(options(old), add = TRUE)
  # under devtools::test() the source, not an installed copy
  read_worker <- function(k) {
    list(
      path = getNamespaceInfo("adatutor", "path"),
      warn = getOption("warn"),
      na_action = getOption("na.action")
    )
  }
  on_workers <- run_tasks(2, read_worker, cores = 2)
  expect_identical(on_workers, list(read_worker(1), read_worker(2)))
})

test_that("a worker that loaded another adatutor, or failed to load, stops", {
  session <- list(paths = c(adatutor = "/session/adatutor", rpart = "/r/rpart"))
  moved <- list(paths = c(adatutor = "/other/adatutor", rpart = "/r/rpart"))
  # Windows puts a drive letter in front
  expect_error(
    check_workers(list(session, moved), session),
    "loaded adatutor from .*/other/adatutor, not from .*/session/adatutor"
  )
  expect_error(
    check_workers(list(list(error = "no package called 'pkgload'")), session),
    "could not load what the tasks need: no package called 'pkgload'",
    fixed = TRUE
  )
  expect_silent(check_workers(list(session, session), session))

  # the same folder, but its code changed after this session loaded it
  session$code <- list(f = "function () 1")
  changed <- session
  changed$code <- list(f = "function () 2")
  expect_error(
    check_workers(list(session, changed), session),
    "adatutor changed since this session loaded it"
  )
})

test_that("on Windows, a worker stops if adatutor changed on disk since loading", {
  skip_if_not_installed("pkgload")
  session <- read_session()
  # only under devtools::test(), which loads the source: an installed copy
  # would need a reinstall
  skip_if(session$installed)
  # a copy of the source stands in for the folder this session loaded
  copy <- tempfile()
  dir.create(copy)
  on.exit(unlink(copy, recursive = TRUE), add = TRUE)
  file.copy(
    file.path(session$paths[["adatutor"]], c("DESCRIPTION", "NAMESPACE", "R")),
    copy,
    recursive = TRUE
  )
  session$paths[["adatutor"]] <- copy
  local_mocked_bindings(
    read_os_type = function() "windows",
    read_session = function() session
  )
  expect_identical(run_tasks(2, identity, cores = 2), list(1L, 2L))

  # the author changes a function and does not reload; the later definition
  # in the file wins
  cat(
    "\ndescribe_settings <- function(rows, n_settings) \"changed\"\n",
    file = file.path(copy, "R", "logo-cv.R"),
    append = TRUE
  )
  expect_error(
    run_tasks(2, identity, cores = 2),
    "adatutor changed since this session loaded it"
  )
})

test_that("on Windows, a task's error and a dead worker reach the user", {
  local_mocked_bindings(read_os_type = function() "windows")
  # the task's own message, as on one core
  fail_second <- function(k) if (k == 2) stop("task 2 failed") else k
  expect_error(run_tasks(3, fail_second), "^task 2 failed$")
  expect_error(run_tasks(3, fail_second, cores = 2), "^task 2 failed$")

  # a worker that dies, e.g. for lack of memory; the other one stops as well
  session <- Sys.getpid()
  # one file per worker, named by its PID: writes of two workers to one file
  # can interleave
  pids_dir <- tempfile()
  dir.create(pids_dir)
  on.exit(unlink(pids_dir, recursive = TRUE), add = TRUE)
  kill_second <- function(k) {
    file.create(file.path(pids_dir, Sys.getpid()))
    if (k == 2 && Sys.getpid() != session) {
      tools::pskill(Sys.getpid())
    }
    k
  }
  expect_error(
    run_tasks(4, kill_second, cores = 2),
    "A parallel worker returned no result"
  )
  # the worker that lives can reach its first task after the error
  for (i in 1:50) {
    if (length(list.files(pids_dir)) == 2L) {
      break
    }
    Sys.sleep(0.1)
  }
  pids <- as.integer(list.files(pids_dir))
  expect_length(pids, 2L)
  # psnice() is NA for a process that no longer exists
  for (i in 1:50) {
    if (all(is.na(tools::psnice(pids)))) {
      break
    }
    Sys.sleep(0.1)
  }
  expect_true(all(is.na(tools::psnice(pids))))

  # a worker that dies while it loads adatutor, before any task
  local_mocked_bindings(
    prepare_worker = function(session, read_code) tools::pskill(Sys.getpid())
  )
  expect_error(
    run_tasks(4, identity, cores = 2),
    "A parallel worker returned no result"
  )
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

test_that("cores above what the computer has are lowered, with a message", {
  data(altmejd)
  fit <- fit_ada()
  grid <- data.frame(n_iter = 2:3)
  local_mocked_bindings(count_cores = function() 2L)
  expect_message(
    lowered <- tune_logo_cv(
      fit,
      grid,
      data = altmejd,
      group = "pid",
      cores = 8
    ),
    "Specified `cores = 8`, but this computer has only 2 cores. The run continues with `cores = 2`.",
    fixed = TRUE
  )
  expect_no_message(
    asked <- tune_logo_cv(fit, grid, data = altmejd, group = "pid", cores = 2)
  )
  expect_identical(lowered$estimates, asked$estimates)

  # an unknown count leaves the request as it is
  local_mocked_bindings(count_cores = function() NA_integer_)
  expect_no_message(
    tune_logo_cv(fit, grid, data = altmejd, group = "pid", cores = 2)
  )
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
  expect_identical(res$metric, "auroc")
  expect_identical(res$sort_by, "lower")
  expect_named(
    res$selected,
    c("project", "setting", "depth", "n_iter", "eta", "inner_value")
  )
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
  expect_named(s, c("mean", "sd", "lower", "min", "max"))
  expect_equal(nrow(s), 1L)
  expect_equal(s$mean, mean(res$estimates[, "auroc"]))
  expect_equal(s$sd, sd(res$estimates[, "auroc"]))
  expect_equal(s$min, min(res$estimates[, "auroc"]))
})

test_that("the inner search never sees the held-out project", {
  # The correctness property: for each outer fold, the selected setting must be
  # the first row of summary() of a grid search run on the remaining projects
  # only. Recomputing that independently is the real check that no leakage
  # occurs.
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
    s <- summary(inner, metric = "auroc", sort_by = "lower")
    expect_equal(
      res$selected$setting[res$selected$project == p],
      as.integer(rownames(s)[1])
    )
  }
})

# for each outer fold of a nested result, the row summary() puts first in a
# search on the other groups, by hand, with its row number as `setting`
pick_by_hand <- function(res, fit, grid, data) {
  rows <- lapply(res$selected$project, function(p) {
    inner <- tune_logo_cv(
      fit,
      grid,
      data = data[data$pid != p, ],
      group = "pid"
    )
    first <- summary(inner, metric = res$metric, sort_by = res$sort_by)[1, ]
    cbind(setting = as.integer(rownames(first)), first)
  })
  do.call(rbind, rows)
}

test_that("each outer fold picks summary()'s first row for any sort_by", {
  data(altmejd)
  fit <- fit_ada()
  for (sort_by in list("lower", c("min", "lower"), "max")) {
    res <- tune_logo_cv(
      fit,
      small_grid,
      data = altmejd,
      group = "pid",
      nested = TRUE,
      metric = "bacc",
      sort_by = sort_by
    )
    by_hand <- pick_by_hand(res, fit, small_grid, altmejd)
    label <- paste(sort_by, collapse = ", ")
    expect_identical(res$sort_by, sort_by)
    expect_identical(res$metric, "bacc")
    expect_identical(res$selected$setting, by_hand$setting, label = label)
    # the winner's value of the first statistic in `sort_by`
    expect_identical(
      res$selected$inner_value,
      by_hand[[sort_by[1]]],
      label = label
    )
  }
})

test_that("sort_by = \"mean\" picks as the nested search did before sort_by", {
  # the rule before sort_by: the highest mean over the inner projects, ties to
  # the first row
  data(altmejd)
  fit <- fit_ada()
  res <- tune_logo_cv(
    fit,
    small_grid,
    data = altmejd,
    group = "pid",
    nested = TRUE,
    sort_by = "mean"
  )
  for (p in res$selected$project) {
    inner <- tune_logo_cv(
      fit,
      small_grid,
      data = altmejd[altmejd$pid != p, ],
      group = "pid"
    )
    means <- rowMeans(inner$estimates[,, "auroc", drop = FALSE], na.rm = TRUE)
    chosen <- res$selected[res$selected$project == p, ]
    expect_identical(chosen$setting, unname(which.max(means)), label = p)
    expect_identical(chosen$inner_value, max(means), label = p)
  }
  # on this grid, the mean and the mean minus SD disagree on some fold
  by_lower <- tune_logo_cv(
    fit,
    small_grid,
    data = altmejd,
    group = "pid",
    nested = TRUE
  )
  expect_false(identical(by_lower$selected$setting, res$selected$setting))
})

test_that("a setting with an NA statistic cannot be picked", {
  data(altmejd)
  inner <- tune_logo_cv(
    fit_ada(),
    data.frame(n_iter = 1:3),
    data = altmejd,
    group = "pid"
  )
  # planted: row 1 has the best projects but lacks one, so its statistics are
  # NA, as in summary(); the old rule, rowMeans(na.rm = TRUE), would pick it
  inner$estimates[,, "auroc"] <- rbind(
    c(0.9, 0.9, NA, 0.9, 0.9),
    c(0.6, 0.6, 0.6, 0.6, 0.6),
    c(0.8, 0.7, 0.6, 0.7, 0.8)
  )
  expect_true(all(is.na(summary(inner)[1, c("mean", "lower")])))
  for (sort_by in list("mean", "lower", c("min", "lower"))) {
    pick <- pick_setting(inner, "auroc", sort_by, fold = "x")
    expect_identical(pick$setting, 3L)
    expect_identical(
      pick$value,
      summary(inner, sort_by = sort_by)[[sort_by[1]]][1]
    )
  }

  # no setting with a value: an error, not a pick
  inner$estimates[, 2, "auroc"] <- NA
  expect_error(
    pick_setting(inner, "auroc", "lower", fold = "ssrp"),
    paste0(
      "No setting can be picked in the inner search without `ssrp`: every ",
      "setting's `lower` of `auroc` is NA"
    ),
    fixed = TRUE
  )

  # patk_5 is undefined on ML3, so every inner search that keeps ML3 stops
  expect_error(
    tune_logo_cv(
      fit_ada(),
      small_grid,
      data = altmejd,
      group = "pid",
      nested = TRUE,
      metric = "patk_5"
    ),
    "inner search without `eerp`: every setting's `lower` of `patk_5` is NA",
    fixed = TRUE
  )
})

test_that("a nested search with a holdout selects on the other groups", {
  # the six tuning projects: HOLZ is the locked final test and is never an
  # outer fold
  data(altmext)
  tune <- altmext[altmext$pid != "holz", ]
  res <- tune_logo_cv(
    fit_ada(),
    small_grid,
    data = tune,
    group = "pid",
    holdout = "ml2",
    nested = TRUE
  )
  expect_equal(res$selected$project, "ml2")
  expect_equal(dimnames(res$estimates)$project, "ml2")
  expect_equal(attr(res, "holdout"), "ml2")

  # the inner search ran on all five other projects
  inner <- tune_logo_cv(
    fit_ada(),
    small_grid,
    data = tune[tune$pid != "ml2", ],
    group = "pid"
  )
  s <- summary(inner, metric = "auroc", sort_by = "lower")
  expect_equal(res$selected$setting, as.integer(rownames(s)[1]))
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

  # the naive rule, the highest mean, inside the loop too; the measured gaps
  # above are for this rule
  nst <- tune_logo_cv(
    fit_ada(),
    grid,
    data = altmejd,
    group = "pid",
    nested = TRUE,
    sort_by = "mean"
  )
  nested <- summary(nst, metric = "auroc")$mean

  # the naive best-average was selected and graded on the same five projects
  expect_gt(naive - nested, 0.01)

  # the winner of the full-grid search is not what the inner searches choose
  best <- as.integer(rownames(s)[which.max(s$mean)])
  expect_gt(length(unique(nst$selected$setting)), 1L)
})

test_that("the metric decides what the inner search ranks by", {
  data(altmejd)
  by_auroc <- tune_logo_cv(
    fit_ada(),
    small_grid,
    data = altmejd,
    group = "pid",
    nested = TRUE,
    metric = "auroc"
  )
  by_bacc <- tune_logo_cv(
    fit_ada(),
    small_grid,
    data = altmejd,
    group = "pid",
    nested = TRUE,
    metric = "bacc"
  )

  expect_equal(by_auroc$metric, "auroc")
  expect_equal(by_bacc$metric, "bacc")
  # both are valid runs; the metric is recorded so a reader can see which
  # decision rule produced the selections
  expect_equal(nrow(by_bacc$selected), 5L)

  # the old name is an error, not ignored
  expect_error(
    tune_logo_cv(
      fit_ada(),
      small_grid,
      data = altmejd,
      group = "pid",
      nested = TRUE,
      criterion = "bacc"
    ),
    "Unused argument(s): `criterion`.",
    fixed = TRUE
  )
})

# bad `metric` and `sort_by` stop a nested search before any fit; `...`
# carries `outcome` for an XGBoost fit. Call it where cross_validate() is
# mocked to stop.
expect_checked_before_fit <- function(fit, grid, ...) {
  metric_message <- paste0(
    "`metric` must be one of the metrics assess() returns: ",
    paste(list_metrics(), collapse = ", "),
    "."
  )
  sort_by_message <- paste0(
    "`sort_by` must name one or more of the summary statistics, each once: ",
    "mean, sd, lower, min, max."
  )
  run <- function(...) {
    tryCatch(
      tune_logo_cv(fit, grid, data = altmejd, group = "pid", ...),
      error = conditionMessage
    )
  }
  for (bad in list("not_a_metric", c("auroc", "bacc"), NA, NULL)) {
    expect_identical(
      run(nested = TRUE, metric = bad, ...),
      metric_message,
      label = deparse1(bad)
    )
  }
  # `depth` is a column of summary(), but not a statistic
  bad_sort_by <- list(
    "depth",
    "median",
    c("mean", "mean"),
    NA_character_,
    character(0),
    1,
    NULL
  )
  for (bad in bad_sort_by) {
    expect_identical(
      run(nested = TRUE, sort_by = bad, ...),
      sort_by_message,
      label = deparse1(bad)
    )
  }
  # not used, so not checked, when the search is not nested
  expect_identical(
    run(metric = "not_a_metric", sort_by = "depth", ...),
    "a fit started"
  )
}

test_that("bad `metric` and `sort_by` stop an AdaBoost search before any fit", {
  data(altmejd)
  fit <- fit_ada()
  # a fit would reach cross_validate() first
  local_mocked_bindings(cross_validate = function(...) stop("a fit started"))
  expect_checked_before_fit(fit, small_grid)
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
      metric = "not_a_metric"
    ),
    "`metric` must be one of the metrics assess() returns",
    fixed = TRUE
  )
})

# the message tune_logo_cv() stops with for each value of `nested` that is
# neither TRUE nor FALSE
read_nested_errors <- function(fit, grid, ...) {
  read_error <- function(nested) {
    tryCatch(
      {
        tune_logo_cv(
          fit,
          grid,
          data = altmejd,
          group = "pid",
          nested = nested,
          ...
        )
        "no error"
      },
      error = conditionMessage
    )
  }
  bad <- list(1, 0, NA, "yes", c(TRUE, FALSE), NULL)
  vapply(bad, read_error, character(1))
}

test_that("an adaboost fit's `nested` must be TRUE or FALSE", {
  data(altmejd)
  fit <- fit_ada()
  grid <- data.frame(n_iter = c(3, 5))
  expect_identical(
    read_nested_errors(fit, grid),
    rep("`nested` must be `TRUE` or `FALSE`.", 6L)
  )

  # TRUE and FALSE work as before
  plain <- tune_logo_cv(fit, grid, data = altmejd, group = "pid")
  expect_identical(
    tune_logo_cv(fit, grid, data = altmejd, group = "pid", nested = FALSE),
    plain
  )
  expect_false(plain$nested)
  nested <- tune_logo_cv(
    fit,
    grid,
    data = altmejd,
    group = "pid",
    nested = TRUE
  )
  expect_true(nested$nested)
  expect_equal(nrow(nested$selected), 5L)
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

  # still no aggregate row and no cross-project SE
  expect_false(any(grepl("\\bmean\\b|\\bSE\\b|\\bsd\\b", out)))
  expect_false(any(grepl("read the spread|diagnostic|inner loop", out)))

  # a nested result keeps its long layout, one row per outer fold with the
  # setting it chose; the picks are summary()'s first rows (see above)
  expect_equal(
    out,
    c(
      "Nested leave-one-group-out cross-validation",
      "  model:    adaboost",
      "  settings: 4",
      "  holdout:  each group",
      "  metric:   auroc",
      "  selected: lower auroc",
      "",
      " depth n_iter eta project  n base_rate estimate",
      "     1      5   1    eerp 18     0.611    0.825",
      "     2      5   1     ml1 13     0.846    0.773",
      "     2      5   1     ml3 10     0.300    0.643",
      "     1     10   1     rpp 90     0.344    0.620",
      "     1     10   1    ssrp 21     0.571    0.676",
      "",
      "The 5 inner searches selected 3 distinct settings."
    )
  )
})

test_that("a nested header names the metric shown and the one selected by", {
  data(altmejd)
  res <- tune_logo_cv(
    fit_ada(),
    small_grid,
    data = altmejd,
    group = "pid",
    holdout = c("ssrp", "ml1"),
    nested = TRUE,
    sort_by = c("min", "lower")
  )
  read_header <- function(...) {
    out <- capture.output(print(res, ...))
    out[seq_len(which(out == "")[1] - 1L)]
  }
  expected <- c(
    "Nested leave-one-group-out cross-validation",
    "  model:    adaboost",
    "  settings: 4",
    "  holdout:  ssrp, ml1",
    "  metric:   auroc",
    "  selected: min, lower auroc"
  )
  expect_identical(read_header(), expected)
  # `metric:` follows print(); `selected:` stays the result's own
  expected[5] <- "  metric:   acc"
  expect_identical(read_header(metric = "acc"), expected)
  n_distinct <- length(unique(res$selected$setting))
  expect_identical(
    tail(capture.output(print(res)), 2L),
    c(
      "",
      sprintf(
        "The 2 inner searches selected %d distinct setting%s.",
        n_distinct,
        if (n_distinct == 1L) "" else "s"
      )
    )
  )
})

test_that("print() of a bootstrapped nested result adds three columns", {
  local_reproducible_output(width = 200)
  data(altmejd)
  res <- tune_logo_cv(
    fit_ada(),
    data.frame(n_iter = c(3, 5)),
    data = altmejd,
    group = "pid",
    nested = TRUE
  )
  set.seed(3)
  boot <- suppressWarnings(bootstrap(res, n_resample = 20))
  # each row finds its project by name, not by its place in `ci`
  boot$ci <- rev(boot$ci)
  printed <- capture.output(shown <- withVisible(print(boot)))
  plain <- capture.output(print(res))

  # the fold table, then the line naming the intervals right under it
  blanks <- which(printed == "")
  plain_blanks <- which(plain == "")
  expect_identical(
    printed[blanks[2] - 1L],
    "95% bootstrap CI (20 resamples/project)"
  )
  table <- read.table(
    text = printed[(blanks[1] + 1L):(blanks[2] - 2L)],
    header = TRUE,
    colClasses = "character"
  )
  plain_table <- read.table(
    text = plain[(plain_blanks[1] + 1L):(plain_blanks[2] - 1L)],
    header = TRUE,
    colClasses = "character"
  )
  expect_named(table, c(names(plain_table), "lower", "upper", "dropped"))
  expect_identical(table[names(plain_table)], plain_table)

  read_ci <- function(column, metric = "auroc") {
    vapply(
      boot$ci[table$project],
      \(ci) ci[[column]][ci$metric == metric],
      numeric(1)
    )
  }
  for (bound in c("lower", "upper")) {
    expect_true(all(grepl("^[01]\\.[0-9]{3}$", table[[bound]])), label = bound)
    expect_equal(
      as.numeric(table[[bound]]),
      round(read_ci(bound), 3),
      ignore_attr = TRUE,
      label = bound
    )
  }
  expect_equal(as.numeric(table$dropped), read_ci("drop"), ignore_attr = TRUE)

  # the columns follow `metric`, and a count prints whole, though its bounds
  # fall between two draws
  bounds <- c(read_ci("lower", "tp"), read_ci("upper", "tp"))
  expect_false(all(bounds == round(bounds)))
  printed_tp <- capture.output(print(boot, metric = "tp"))
  blanks_tp <- which(printed_tp == "")
  table_tp <- read.table(
    text = printed_tp[(blanks_tp[1] + 1L):(blanks_tp[2] - 2L)],
    header = TRUE,
    colClasses = "character"
  )
  expect_identical(table_tp$project, table$project)
  for (bound in c("lower", "upper")) {
    expect_true(all(grepl("^[0-9]+$", table_tp[[bound]])), label = bound)
    expect_equal(
      as.numeric(table_tp[[bound]]),
      round(read_ci(bound, "tp")),
      ignore_attr = TRUE,
      label = bound
    )
  }

  # the header and the footer stay as they were
  expect_identical(printed[seq_len(blanks[1])], plain[seq_len(plain_blanks[1])])
  expect_identical(
    printed[blanks[2]:length(printed)],
    plain[plain_blanks[2]:length(plain)]
  )
  expect_false(shown$visible)
  expect_identical(shown$value, boot)
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

test_that("the tutorial's tree code picks depth 4 by the highest lower", {
  data(altmext)
  df <- altmext
  pred_nms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  out_nme <- "replicate"
  voi_nms <- c(pred_nms, out_nme)
  df_noholz <- df[df$pid != "holz", ]

  tree_hgrid <- data.frame(depth = 1:4)
  tree_tune <- rpart::rpart(
    replicate ~ .,
    data = df_noholz[, voi_nms],
    method = "class"
  ) |>
    tune_logo_cv(tree_hgrid, data = df_noholz, group = "pid")
  tree_best <- summary(tree_tune, "auroc", sort_by = "lower")[1, ]

  expect_equal(tree_best$depth, 4)
  # depth 4 wins outright; depths 1 and 2 tie exactly and keep grid order
  ranked <- summary(tree_tune, "auroc", sort_by = "lower")
  expect_equal(rownames(ranked), c("4", "3", "1", "2"))
  expect_gt(ranked$lower[1], ranked$lower[2])
  expect_identical(
    tree_tune$estimates[2, , "auroc"],
    tree_tune$estimates[1, , "auroc"]
  )
})

test_that("on Windows, the tutorial's AdaBoost recipe gives what one core gives", {
  local_mocked_bindings(read_os_type = function() "windows")
  data(altmext)
  pred_nms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  voi_nms <- c(pred_nms, "replicate")
  df_noholz <- altmext[altmext$pid != "holz", ]
  ada_fit <- rpart::rpart(
    replicate ~ .,
    data = df_noholz[, voi_nms],
    method = "class",
    maxdepth = 1,
    parms = list(split = "gini"),
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE)
  grid <- expand.grid(depth = 1:2, n_iter = c(3, 5), eta = c(1, 0.5))
  run <- function(cores, ...) {
    tune_logo_cv(
      ada_fit,
      grid,
      data = df_noholz,
      group = "pid",
      cores = cores,
      ...
    )
  }
  expect_identical(run(2), run(1))
  # each inner search starts a cluster, so two outer folds keep it short
  run_nested <- function(cores) {
    run(cores, nested = TRUE, holdout = c("eerp", "ml3"))
  }
  expect_identical(run_nested(2), run_nested(1))
})

test_that("on Windows, an AdaBoost recipe needs nothing of its tree's call", {
  local_mocked_bindings(read_os_type = function() "windows")
  data(altmejd)
  # as typed at the top level: rpart attached and called bare, and a depth
  # from a one-row data frame or a number of the session. A refit grows its
  # trees from the stored formula and control alone.
  if (!"package:rpart" %in% search()) {
    library(rpart)
    on.exit(detach("package:rpart"), add = TRUE)
  }
  assign("best", data.frame(depth = 2), envir = globalenv())
  assign("d", 2, envir = globalenv())
  on.exit(rm(list = c("best", "d"), envir = globalenv()), add = TRUE)
  stumps <- list(
    rpart(fml, data = altmejd, method = "class", maxdepth = 1, model = TRUE),
    rpart::rpart(
      fml,
      data = altmejd,
      method = "class",
      maxdepth = best$depth,
      model = TRUE
    ),
    rpart::rpart(
      fml,
      data = altmejd,
      method = "class",
      maxdepth = d,
      model = TRUE
    )
  )
  grid <- data.frame(n_iter = c(2, 4))
  for (stump in stumps) {
    fit <- adaboost(stump, n_iter = 4, eta = 1, verbose = FALSE)
    expect_identical(list_globals(fit, altmejd), list())
    expect_identical(
      tune_logo_cv(fit, grid, data = altmejd, group = "pid", cores = 2),
      tune_logo_cv(fit, grid, data = altmejd, group = "pid")
    )
  }
})

test_that("on Windows, the tutorial's XGBoost fit gives what one core gives", {
  skip_if_not_installed("xgboost", "3.0.0")
  local_mocked_bindings(read_os_type = function() "windows")
  data(altmext)
  pred_nms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  out_nme <- "replicate"
  df_noholz <- altmext[altmext$pid != "holz", ]
  xgb_fit <- xgboost::xgboost(
    x = df_noholz[, pred_nms],
    y = df_noholz[, out_nme],
    nthreads = 1
  )
  run <- function(cores, ...) {
    tune_logo_cv(
      xgb_fit,
      expand.grid(depth = 1:2, n_iter = c(2, 4)),
      data = df_noholz,
      group = "pid",
      outcome = out_nme,
      cores = cores,
      ...
    )
  }
  expect_identical(run(2), run(1))
  run_nested <- function(cores) {
    run(
      cores,
      nested = TRUE,
      sort_by = c("min", "lower"),
      holdout = c("eerp", "ml3")
    )
  }
  expect_identical(run_nested(2), run_nested(1))
})

test_that("on Windows, the vignette's tree and forest give what one core gives", {
  local_mocked_bindings(read_os_type = function() "windows")
  data(altmejd)
  df <- altmejd
  voi_nms <- c("power.o", "effect_size.o", "n.o", "p_value.o", "replicate")
  tree_fit <- rpart::rpart(
    replicate ~ .,
    data = df[, voi_nms],
    method = "class",
    maxdepth = 2
  )
  run_tree <- function(cores) {
    tune_logo_cv(
      tree_fit,
      data.frame(depth = 1:4),
      data = df,
      group = "pid",
      cores = cores
    )
  }
  expect_identical(run_tree(2), run_tree(1))

  skip_if_not_installed("randomForest")
  set.seed(112)
  rf_fit <- muffle_partial_matching(
    randomForest::randomForest(replicate ~ ., data = df[, voi_nms])
  )
  rf_hgrid <- expand.grid(mtry = 1:2, nodesize = c(1, 10))
  run_forest <- function(cores) {
    set.seed(112)
    muffle_partial_matching(
      tune_logo_cv(rf_fit, rf_hgrid, data = df, group = "pid", cores = cores)
    )
  }
  expect_identical(run_forest(2), run_forest(1))
})

test_that("on Windows, the session's plain values reach the workers", {
  local_mocked_bindings(read_os_type = function() "windows")
  data(altmejd)
  # as typed at the top level: numbers `sd` and `var`, which the formula
  # reads while it calls stats' sd() and var(), a number it reads by a
  # string, and a one-row data frame and a list the call reads. A new R
  # process has none of them.
  local(
    {
      sd <- 0.2
      var <- 2
      n_scale <- 3
      best <- data.frame(depth = 3)
      tree_parms <- list(split = "information")
    },
    envir = globalenv()
  )
  on.exit(
    rm(
      list = c("sd", "var", "n_scale", "best", "tree_parms"),
      envir = globalenv()
    ),
    add = TRUE
  )
  formula <- replicate ~
    power.o +
    I(n.o / sd(n.o) * sd) +
    I(n.o / var + 0 * var(n.o)) +
    I(n.o %% (get("n_scale") * 10))
  environment(formula) <- globalenv()
  tree <- rpart::rpart(
    formula,
    data = altmejd,
    method = "class",
    maxdepth = best$depth,
    parms = tree_parms
  )
  expect_identical(
    list_globals(tree, altmejd),
    # the call's order, which match.call() sets by rpart()'s arguments
    list(
      sd = 0.2,
      var = 2,
      n_scale = 3,
      tree_parms = list(split = "information"),
      best = data.frame(depth = 3)
    )
  )
  grid <- data.frame(depth = 2:3)
  expect_identical(
    tune_logo_cv(tree, grid, data = altmejd, group = "pid", cores = 2),
    tune_logo_cv(tree, grid, data = altmejd, group = "pid")
  )

  # do.call() puts rpart() itself in the call, which a worker has as well
  tree <- do.call(
    rpart::rpart,
    list(formula = formula, data = altmejd, method = "class")
  )
  expect_identical(
    tune_logo_cv(tree, grid, data = altmejd, group = "pid", cores = 2),
    tune_logo_cv(tree, grid, data = altmejd, group = "pid")
  )
})

test_that("on Windows, an argument left out where the fit was made needs nothing", {
  local_mocked_bindings(read_os_type = function() "windows")
  data(altmejd)
  # `p` holds no value, here and in the frame a worker gets with the formula
  make_tree <- function(p) {
    rpart::rpart(
      replicate ~ power.o + n.o,
      data = altmejd,
      method = "class",
      parms = p
    )
  }
  # and handed on, missing, from an outer frame
  make_outer <- function(p) make_tree(p)
  grid <- data.frame(depth = 1:2)
  for (tree in list(make_tree(), make_outer())) {
    expect_identical(list_globals(tree, altmejd), list())
    expect_identical(
      tune_logo_cv(tree, grid, data = altmejd, group = "pid", cores = 2),
      tune_logo_cv(tree, grid, data = altmejd, group = "pid")
    )
  }
})

test_that("on Windows, a fit that uses code of the session stops before any worker starts", {
  local_mocked_bindings(
    read_os_type = function() "windows",
    run_socket_tasks = function(...) stop("a worker started")
  )
  data(altmejd)
  # as typed at the top level; a new R process has none of these
  local(
    {
      bin_power <- function(x) round(10 * x)
      round <- function(x, digits = 0) x %% 7
      calib <- lm(n.o ~ power.o, data = altmejd)
      adjust_n <- function(x) predict(calib, newdata = data.frame(power.o = x))
      rules <- list(bin = function(x) x %/% 10)
      tool_box <- new.env()
      tool_box$f <- function(x) x %/% 10
      holder <- structure(1, fn = function(x) x %/% 10)
      make_parms <- function() list(split = "information")
      my_na <- function(object, ...) object
      gini <- function(p) 1 - (p^2 + (1 - p)^2)
    },
    envir = globalenv()
  )
  on.exit(
    rm(
      list = c(
        "bin_power",
        "round",
        "calib",
        "adjust_n",
        "rules",
        "tool_box",
        "holder",
        "make_parms",
        "my_na",
        "gini"
      ),
      envir = globalenv()
    ),
    add = TRUE
  )
  grid <- data.frame(depth = 1:2)
  tune <- function(fit) {
    tune_logo_cv(fit, grid, data = altmejd, group = "pid", cores = 2)
  }
  tree_of <- function(term) {
    formula <- stats::as.formula(
      paste("replicate ~ power.o +", term),
      env = globalenv()
    )
    rpart::rpart(formula, data = altmejd, method = "class")
  }

  expect_error(
    tune(tree_of("bin_power(n.o)")),
    paste0(
      "The fit uses `bin_power` from your session. On Windows, parallel ",
      "workers start empty and may not get it. Set `cores = 1`, or compute ",
      "the column first and use it in the formula."
    ),
    fixed = TRUE
  )
  # a global round() that masks base's, and a helper that predicts from a
  # global model
  expect_error(tune(tree_of("round(n.o)")), "`round` from your session")
  expect_error(tune(tree_of("adjust_n(power.o)")), "`adjust_n` from your")
  # objects that hold functions
  holds_code <- "from your session, an object that holds code. On Windows"
  expect_error(tune(tree_of("rules$bin(n.o)")), paste0("`rules` ", holds_code))
  expect_error(
    tune(tree_of("tool_box$f(n.o)")),
    paste0("`tool_box` ", holds_code)
  )
  expect_error(
    tune(tree_of("attr(holder, \"fn\")(n.o)")),
    paste0("`holder` ", holds_code)
  )
  # a lambda's argument named round: the call inside skips the number it
  # holds and finds the session's round()
  expect_error(
    tune(tree_of("I(sapply(n.o, function(round) round(round)))")),
    "`round` from your session"
  )
  # a session function named in a string, which do.call() and sapply() look
  # up by that name
  named <- paste0(
    "The fit names `bin_power` in a string, and your session has a function ",
    "of that name. On Windows, parallel workers start empty and may not get ",
    "it. Set `cores = 1`, or rename the function in your session."
  )
  expect_error(
    tune(tree_of("I(do.call(\"bin_power\", list(power.o)))")),
    named,
    fixed = TRUE
  )
  expect_error(
    tune(tree_of("I(sapply(power.o, \"bin_power\"))")),
    named,
    fixed = TRUE
  )

  # a helper in an argument of the call
  formula <- fml
  environment(formula) <- globalenv()
  tree <- rpart::rpart(
    formula,
    data = altmejd,
    method = "class",
    parms = make_parms()
  )
  expect_error(
    tune(tree),
    paste0(
      "The fit uses `make_parms` from your session. On Windows, parallel ",
      "workers start empty and may not get it. Set `cores = 1`, or compute ",
      "the value first and use it in the call."
    ),
    fixed = TRUE
  )
  # a session function the call names in a string, which model.frame() looks
  # up by that name
  tree <- rpart::rpart(
    formula,
    data = altmejd,
    method = "class",
    na.action = "my_na"
  )
  expect_error(tune(tree), "`my_na` in a string, and your session has")
  # rpart() reads split = "gini" as its own rule, not as the tutorial's
  # gini(), but no check can tell the two apart
  tree <- rpart::rpart(
    formula,
    data = altmejd,
    method = "class",
    parms = list(split = "gini")
  )
  expect_error(tune(tree), "`gini` in a string, and your session has")

  # a helper made where the fit was made
  make_tree <- function() {
    shift_n <- function(x) x + 1
    rpart::rpart(
      replicate ~ power.o + shift_n(n.o),
      data = altmejd,
      method = "class"
    )
  }
  expect_error(tune(make_tree()), "`shift_n` from your session")
  # a session function as the default of an argument of that frame
  make_binned <- function(bin = bin_power) {
    rpart::rpart(
      replicate ~ power.o + bin(n.o),
      data = altmejd,
      method = "class"
    )
  }
  expect_error(tune(make_binned()), "`bin` from your session")

  # a function do.call() puts in the call itself
  tree <- do.call(
    rpart::rpart,
    list(
      formula = formula,
      data = altmejd,
      method = "class",
      na.action = function(x) x
    )
  )
  expect_error(
    tune(tree),
    "The fit's call holds a function or other code itself",
    fixed = TRUE
  )

  # attached lists, one under a package's name
  attach(
    list(tool_n = function(x) x %/% 10),
    name = "adatutor_test_tools",
    warn.conflicts = FALSE
  )
  on.exit(detach("adatutor_test_tools"), add = TRUE)
  attach(
    list(fake_n = function(x) x %/% 10),
    name = "package:adatutor_fake",
    warn.conflicts = FALSE
  )
  on.exit(detach("package:adatutor_fake"), add = TRUE)
  expect_error(tune(tree_of("tool_n(n.o)")), "`tool_n` from your session")
  expect_error(tune(tree_of("fake_n(n.o)")), "`fake_n` from your session")

  # a fit made by a function sourced into an attached environment, as
  # sys.source(file, envir = attach(NULL, name = ...)) makes it: a worker
  # would get that environment without its objects
  tools <- attach(NULL, name = "package:adatutor_tools", warn.conflicts = FALSE)
  on.exit(detach("package:adatutor_tools"), add = TRUE)
  assign("n_cap", 150, envir = tools)
  make_capped <- function(rows) {
    rpart::rpart(
      replicate ~ power.o + I(pmin(n.o, n_cap)),
      data = rows,
      method = "class"
    )
  }
  environment(make_capped) <- tools
  expect_error(
    tune(make_capped(altmejd)),
    paste0(
      "The fit's formula looks up its objects through the attached ",
      "environment `package:adatutor_tools`. On Windows, parallel workers ",
      "get attached environments without their objects. Set `cores = 1`, or ",
      "make the fit at the top level."
    ),
    fixed = TRUE
  )

  # rpart() called bare, which needs rpart attached
  if (!"package:rpart" %in% search()) {
    library(rpart)
    on.exit(detach("package:rpart"), add = TRUE)
  }
  tree <- rpart(formula, data = altmejd, method = "class")
  bare <- paste0(
    "The fit uses `rpart` from the attached package rpart. On Windows, ",
    "parallel workers load packages but do not attach them. Set ",
    "`cores = 1`, or write `rpart::rpart()`."
  )
  expect_error(tune(tree), bare, fixed = TRUE)
  # and past a session object named rpart, which the call skips
  assign("rpart", "a label", envir = globalenv())
  on.exit(rm("rpart", envir = globalenv()), add = TRUE)
  expect_error(tune(tree), bare, fixed = TRUE)

  # a helper in a forest's call
  skip_if_not_installed("randomForest")
  local(pick_size <- function() 5, envir = globalenv())
  on.exit(rm("pick_size", envir = globalenv()), add = TRUE)
  forest <- muffle_partial_matching(randomForest::randomForest(
    formula,
    data = altmejd,
    ntree = 25,
    nodesize = pick_size()
  ))
  expect_error(
    muffle_partial_matching(tune_logo_cv(
      forest,
      data.frame(mtry = 1:2),
      data = altmejd,
      group = "pid",
      cores = 2
    )),
    "`pick_size` from your session"
  )
})

test_that("the check runs once per call on Windows, and never off it", {
  data(altmejd)
  grid <- expand.grid(depth = 1:2, n_iter = c(3, 6))
  calls <- 0L
  check <- list_globals
  local_mocked_bindings(
    list_globals = function(fit, data) {
      calls <<- calls + 1L
      check(fit, data)
    },
    # the tasks run here, so no cluster starts
    run_socket_tasks = function(n_tasks, run_task, n_workers, globals) {
      lapply(seq_len(n_tasks), run_task)
    }
  )
  run <- function(cores, ...) {
    tune_logo_cv(
      fit_ada(),
      grid,
      data = altmejd,
      group = "pid",
      cores = cores,
      ...
    )
  }
  one <- run(1)
  nested_one <- run(1, nested = TRUE)
  local_mocked_bindings(read_os_type = function() "unix")
  expect_identical(run(2), one)
  expect_identical(calls, 0L)

  local_mocked_bindings(read_os_type = function() "windows")
  expect_identical(run(2), one)
  expect_identical(calls, 1L)
  # not once per inner search
  expect_identical(run(2, nested = TRUE), nested_one)
  expect_identical(calls, 2L)
})

test_that("list_globals() reads a call with an empty argument and classed lists", {
  data(altmejd)
  # `prior_table[1, ]` holds the empty name of a missing argument; the
  # as.list() method of numeric_version and POSIXlt returns the object again
  local(
    {
      prior_table <- rbind(c(0.5, 0.5))
      stamps <- list(getRversion(), as.POSIXlt("2026-01-01", tz = "UTC"))
    },
    envir = globalenv()
  )
  on.exit(
    rm(list = c("prior_table", "stamps"), envir = globalenv()),
    add = TRUE
  )
  formula <- replicate ~ power.o + I(n.o + length(stamps))
  environment(formula) <- globalenv()
  tree <- rpart::rpart(
    formula,
    data = altmejd,
    method = "class",
    parms = list(prior = prior_table[1, ])
  )
  globals <- list_globals(tree, altmejd)
  expect_identical(globals$prior_table, rbind(c(0.5, 0.5)))
  expect_identical(globals$stamps, get("stamps", envir = globalenv()))
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

test_that("a tree, a forest and other objects refuse nested = TRUE", {
  skip_if_not_installed("randomForest")
  data(altmejd)
  tree <- rpart::rpart(fml, data = altmejd, method = "class")
  set.seed(1)
  forest <- fit_forest()
  # the default method: a fit the package does not tune, and no fit at all
  linear <- lm(n.o ~ power.o, data = altmejd)
  grids <- list(
    data.frame(depth = 1:2),
    data.frame(mtry = 1:2),
    data.frame(n_iter = 1:2),
    data.frame(n_iter = 1:2)
  )
  fits <- list(tree, forest, linear, altmejd)
  for (k in seq_along(fits)) {
    expect_error(
      tune_logo_cv(
        fits[[k]],
        grids[[k]],
        data = altmejd,
        group = "pid",
        nested = TRUE
      ),
      paste0(
        "^`nested = TRUE` is currently only supported with fits from ",
        "adaboost\\(\\) and xgboost\\(\\)\\.$"
      ),
      label = class(fits[[k]])[1]
    )
  }
  # without nesting, the default method says what it needs
  expect_error(
    tune_logo_cv(linear, grids[[3]], data = altmejd, group = "pid"),
    "`x` must be a fit from adaboost(), rpart(), randomForest() or xgboost()",
    fixed = TRUE
  )
})

test_that("a tree's or a forest's `nested` must be TRUE or FALSE", {
  data(altmejd)
  flag <- rep("`nested` must be `TRUE` or `FALSE`.", 6L)
  # the whole message: one sentence
  not_nestable <- paste0(
    "^`nested = TRUE` is currently only supported with fits from ",
    "adaboost\\(\\) and xgboost\\(\\)\\.$"
  )
  tree <- rpart::rpart(fml, data = altmejd, method = "class")
  expect_identical(read_nested_errors(tree, data.frame(depth = 1:2)), flag)
  expect_error(
    tune_logo_cv(
      tree,
      data.frame(depth = 1:2),
      data = altmejd,
      group = "pid",
      nested = TRUE
    ),
    not_nestable
  )

  skip_if_not_installed("randomForest")
  set.seed(1)
  forest <- fit_forest()
  expect_identical(read_nested_errors(forest, data.frame(mtry = 1:2)), flag)
  expect_error(
    tune_logo_cv(
      forest,
      data.frame(mtry = 1:2),
      data = altmejd,
      group = "pid",
      nested = TRUE
    ),
    not_nestable
  )
})

test_that("the forest tuner refuses an argument it does not take", {
  skip_if_not_installed("randomForest")
  data(altmejd)
  set.seed(1)
  forest <- fit_forest()
  expect_error(
    tune_logo_cv(
      forest,
      data.frame(mtry = 1:2),
      data = altmejd,
      group = "pid",
      ntree = 50
    ),
    "Unused argument(s): `ntree`.",
    fixed = TRUE
  )
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

test_that("nodesize and ntree in a forest's grid reach the refit", {
  # mtry and maxnodes are pinned by the test above
  skip_if_not_installed("randomForest")
  data(altmejd)
  set.seed(1)
  forest <- fit_forest()
  folds <- unique(altmejd$pid)
  # by hand, with the tuner's seeds: one per fold, drawn after set.seed(5)
  by_hand <- function(...) {
    set.seed(5)
    seeds <- sample.int(.Machine$integer.max, length(folds))
    unlist(lapply(seq_along(folds), function(j) {
      set.seed(seeds[j])
      refit <- muffle_partial_matching(randomForest::randomForest(
        fml,
        data = altmejd[altmejd$pid != folds[j], ],
        ...
      ))
      test <- altmejd[altmejd$pid == folds[j], ]
      unname(predict(refit, test, type = "prob")[, 2])
    }))
  }
  base <- data.frame(mtry = 2, nodesize = 1, ntree = 25)
  base_args <- list(mtry = 2, nodesize = 1, ntree = 25)
  base_scores <- do.call(by_hand, base_args)
  changes <- list(nodesize = list(nodesize = 20), ntree = list(ntree = 60))
  for (column in names(changes)) {
    row <- base
    row[[column]] <- changes[[column]][[1L]]
    set.seed(5)
    res <- muffle_partial_matching(
      tune_logo_cv(forest, row, data = altmejd, group = "pid")
    )
    want <- do.call(by_hand, utils::modifyList(base_args, changes[[column]]))
    expect_identical(res$scores$score, want, label = column)
    # so the setting changed the fit
    expect_false(identical(want, base_scores), label = column)
  }
})

test_that("the forest tuner repeats after set.seed(), at any number of cores", {
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

test_that("on Windows, the forest tuner repeats one core, whatever the RNG kind", {
  skip_if_not_installed("randomForest")
  data(altmejd)
  set.seed(1)
  forest <- fit_forest()
  run <- function(cores) {
    set.seed(5)
    res <- muffle_partial_matching(tune_logo_cv(
      forest,
      data.frame(mtry = 1:2),
      data = altmejd,
      group = "pid",
      cores = cores
    ))
    list(res = res, stream = get(".Random.seed", envir = globalenv()))
  }
  run_socket <- function() {
    local_mocked_bindings(read_os_type = function() "windows")
    run(2)
  }
  # the result, and the session's stream just past the seeds
  default_kind <- run(1)
  expect_identical(run_socket(), default_kind)

  # a kind a new R process does not start with, which set.seed() on a worker
  # must use as well
  kinds <- RNGkind()
  on.exit(do.call(RNGkind, as.list(kinds)), add = TRUE)
  RNGkind("L'Ecuyer-CMRG")
  one <- run(1)
  expect_false(identical(one$res, default_kind$res))
  expect_identical(run_socket(), one)
})

test_that("the forest tuner leaves the session's stream just past its seeds", {
  skip_if_not_installed("randomForest")
  data(altmejd)
  set.seed(1)
  forest <- fit_forest()
  # the stream right after the tuner's draw of one seed per fold
  set.seed(5)
  sample.int(.Machine$integer.max, length(unique(altmejd$pid)))
  past_seeds <- get(".Random.seed", envir = globalenv())
  run <- function(cores) {
    set.seed(5)
    muffle_partial_matching(tune_logo_cv(
      forest,
      data.frame(mtry = 1:2),
      data = altmejd,
      group = "pid",
      cores = cores
    ))
    get(".Random.seed", envir = globalenv())
  }
  expect_identical(run(1), past_seeds)
  expect_identical(run(2), past_seeds)
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

test_that("a forest's refits mute randomForest's own partial match", {
  skip_if_not_installed("randomForest")
  data(altmejd)
  set.seed(1)
  forest <- fit_forest()
  old <- options(warnPartialMatchArgs = TRUE)
  on.exit(options(old), add = TRUE)
  # the warning randomForest() gives from its own code
  expect_warning(
    randomForest::randomForest(fml, data = altmejd, ntree = 5),
    "partial argument match of 'along' to 'along.with'",
    fixed = TRUE
  )
  expect_no_warning(logo_cv(forest, data = altmejd, group = "pid"))
  expect_no_warning(
    tune_logo_cv(forest, data.frame(mtry = 1:2), data = altmejd, group = "pid")
  )
})

test_that("another warning in a forest's refit still reaches the user", {
  skip_if_not_installed("randomForest")
  data(altmejd)
  # an argument of the call that matches partially as well, but not `along`;
  # each refit runs it again
  choose_nodesize <- function() length(seq(len = 1))
  formula <- replicate ~ power.o + effect_size.o + n.o + p_value.o
  set.seed(1)
  forest <- muffle_partial_matching(randomForest::randomForest(
    formula,
    data = altmejd,
    ntree = 25,
    nodesize = choose_nodesize()
  ))
  old <- options(warnPartialMatchArgs = TRUE)
  on.exit(options(old), add = TRUE)
  warned <- character()
  collect <- function(w) {
    warned <<- c(warned, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
  withCallingHandlers(
    {
      logo_cv(forest, data = altmejd, group = "pid")
      tune_logo_cv(
        forest,
        data.frame(mtry = 1:2),
        data = altmejd,
        group = "pid"
      )
    },
    warning = collect
  )
  # once per refit: one per project, then two per project
  expect_identical(
    warned,
    rep(
      "partial argument match of 'len' to 'length.out'",
      3L * length(unique(altmejd$pid))
    )
  )
})

test_that("print() and summary() show a tree's or a forest's own settings", {
  local_reproducible_output(width = 200)
  skip_if_not_installed("randomForest")
  data(altmejd)
  statistics <- c("mean", "sd", "lower", "min", "max")

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
  skip_if_not_installed("xgboost", "3.0.0")
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
    assess(rpp$actual, rpp$score, threshold = 0.5)[list_metrics()]
  )
})

test_that("each XGBoost grid column reaches its xgboost() argument", {
  skip_if_not_installed("xgboost", "3.0.0")
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
  skip_if_not_installed("xgboost", "3.0.0")
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
  skip_if_not_installed("xgboost", "3.0.0")
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
  skip_if_not_installed("xgboost", "3.0.0")
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
  skip_if_not_installed("xgboost", "3.0.0")
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
  skip_if_not_installed("xgboost", "3.0.0")
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
  skip_if_not_installed("xgboost", "3.0.0")
  data(altmejd)
  fit <- fit_xgb()
  # a nested search, as for AdaBoost
  expect_error(
    tune_logo_cv(
      fit,
      data.frame(n_iter = 2),
      data = altmejd,
      group = "pid",
      nested = TRUE,
      outcome = "replicate"
    ),
    "needs a `grid` with two or more rows"
  )
  expect_error(
    tune_logo_cv(
      fit,
      data.frame(n_iter = 2:3),
      data = altmejd[altmejd$pid %in% c("ml1", "ml3"), ],
      group = "pid",
      nested = TRUE,
      outcome = "replicate"
    ),
    "at least three groups"
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

test_that("an XGBoost fit's `nested` must be TRUE or FALSE", {
  skip_if_not_installed("xgboost", "3.0.0")
  data(altmejd)
  fit <- fit_xgb()
  grid <- data.frame(n_iter = 2:3)
  expect_identical(
    read_nested_errors(fit, grid, outcome = "replicate"),
    rep("`nested` must be `TRUE` or `FALSE`.", 6L)
  )

  # TRUE and FALSE both work
  plain <- tune_logo_cv(
    fit,
    grid,
    data = altmejd,
    group = "pid",
    nested = FALSE,
    outcome = "replicate"
  )
  expect_false(plain$nested)
  nested <- tune_logo_cv(
    fit,
    grid,
    data = altmejd,
    group = "pid",
    nested = TRUE,
    outcome = "replicate"
  )
  expect_true(nested$nested)
  expect_equal(nrow(nested$selected), 5L)
})

test_that("bad `metric` and `sort_by` stop an XGBoost search before any fit", {
  skip_if_not_installed("xgboost", "3.0.0")
  data(altmejd)
  fit <- fit_xgb()
  local_mocked_bindings(cross_validate = function(...) stop("a fit started"))
  expect_checked_before_fit(fit, small_grid, outcome = "replicate")
})

# the six tuning projects: HOLZ is the locked final test and is never an
# outer fold
read_tuning_projects <- function() {
  data(altmext, envir = environment())
  tune <- altmext[altmext$pid != "holz", ]
  stopifnot(!"holz" %in% tune$pid)
  tune
}

xgb_nested_grid <- expand.grid(
  depth = c(1, 2),
  eta = c(0.1, 0.3),
  n_iter = c(5, 20)
)

test_that("a nested XGBoost fold equals the by-hand route of the tutorial", {
  skip_if_not_installed("xgboost", "3.0.0")
  tune <- read_tuning_projects()
  xgb_fit <- xgboost::xgboost(
    x = tune[, xgb_predictors],
    y = tune$replicate,
    nthreads = 1
  )
  folds <- c("ml2", "rpp")
  res <- tune_logo_cv(
    xgb_fit,
    xgb_nested_grid,
    data = tune,
    group = "pid",
    holdout = folds,
    nested = TRUE,
    outcome = "replicate"
  )
  expect_identical(res$model, "xgboost")
  expect_identical(res$selected$project, folds)
  expect_named(
    res$selected,
    c(
      "project",
      "setting",
      "depth",
      "n_iter",
      "eta",
      "min_child_weight",
      "lambda",
      "inner_value"
    )
  )

  for (fold in folds) {
    # a search on the outer training part, its pick by summary(), then the
    # pick refitted on that part and scored on the fold (data-raw/benchmark.R)
    inner <- tune_logo_cv(
      xgb_fit,
      xgb_nested_grid,
      data = tune[tune$pid != fold, ],
      group = "pid",
      outcome = "replicate"
    )
    pick <- summary(inner, "auroc", sort_by = "lower")[1, ]
    final <- tune_logo_cv(
      xgb_fit,
      pick[names(xgb_nested_grid)],
      data = tune,
      group = "pid",
      holdout = fold,
      outcome = "replicate"
    )
    chosen <- res$selected[res$selected$project == fold, ]
    expect_identical(chosen$setting, as.integer(rownames(pick)), label = fold)
    expect_identical(chosen$inner_value, pick$lower, label = fold)
    expect_identical(
      res$estimates[fold, ],
      final$estimates[1, fold, ],
      label = fold
    )
    expect_identical(
      res$scores$score[res$scores$project == fold],
      final$scores$score,
      label = fold
    )
  }
})

test_that("the methods work on a nested XGBoost result", {
  local_reproducible_output(width = 200)
  skip_if_not_installed("xgboost", "3.0.0")
  data(altmejd)
  res <- tune_logo_cv(
    fit_xgb(),
    xgb_nested_grid,
    data = altmejd,
    group = "pid",
    nested = TRUE,
    outcome = "replicate"
  )
  projects <- unique(as.character(altmejd$pid))

  # the header, then the grid's columns: the five settings of an XGBoost fit
  out <- capture.output(print(res))
  expect_identical(
    out[1:7],
    c(
      "Nested leave-one-group-out cross-validation",
      "  model:    xgboost",
      "  settings: 8",
      "  holdout:  each group",
      "  metric:   auroc",
      "  selected: lower auroc",
      ""
    )
  )
  table <- read.table(text = out[8:13], header = TRUE)
  expect_named(
    table,
    c(
      "depth",
      "n_iter",
      "eta",
      "min_child_weight",
      "lambda",
      "project",
      "n",
      "base_rate",
      "estimate"
    )
  )
  expect_equal(table$project, projects)
  expect_equal(table$n_iter, res$selected$n_iter)
  expect_equal(table$eta, res$selected$eta)
  n_distinct <- length(unique(res$selected$setting))
  expect_identical(
    out[14:15],
    c(
      "",
      sprintf(
        "The 5 inner searches selected %d distinct setting%s.",
        n_distinct,
        if (n_distinct == 1L) "" else "s"
      )
    )
  )
  expect_length(out, 15L)
  expect_false(any(grepl("read the spread|diagnostic|inner loop", out)))

  s <- summary(res)
  expect_named(s, c("mean", "sd", "lower", "min", "max"))
  expect_equal(s$mean, mean(res$estimates[, "auroc"]))

  d <- as.data.frame(res)
  expect_equal(nrow(d), length(res$estimates))
  expect_equal(unique(d$model), "xgboost")
  auroc <- d[d$metric == "auroc", ]
  expect_equal(auroc$setting, res$selected$setting)
  expect_equal(auroc$lambda, res$selected$lambda)

  # scored on the probability, thresholded at 0.5
  rpp <- res$scores[res$scores$project == "rpp", ]
  expect_identical(
    res$estimates["rpp", ],
    assess(rpp$actual, rpp$score, threshold = 0.5)[list_metrics()]
  )
  set.seed(3)
  boot <- suppressWarnings(bootstrap(res, n_resample = 20))
  expect_named(boot$ci, projects)
  # right under the table
  expect_identical(
    capture.output(print(boot))[14],
    "95% bootstrap CI (20 resamples/project)"
  )
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_identical(withVisible(plot(boot))$value, boot)
})

test_that("a nested XGBoost search gives the same at any number of cores", {
  skip_if_not_installed("xgboost", "3.0.0")
  data(altmejd)
  run <- function(cores) {
    tune_logo_cv(
      fit_xgb(),
      expand.grid(depth = 1:2, n_iter = c(2, 4)),
      data = altmejd,
      group = "pid",
      nested = TRUE,
      cores = cores,
      outcome = "replicate"
    )
  }
  expect_identical(run(2), run(1))
})

test_that("an XGBoost held-out group of one row keeps a column per setting", {
  skip_if_not_installed("xgboost", "3.0.0")
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
  skip_if_not_installed("xgboost", "3.0.0")
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
  one <- run(1)
  expect_identical(run(2), one)
  # on the socket cluster of Windows, which gets the fit serialized
  local_mocked_bindings(read_os_type = function() "windows")
  expect_identical(run(2), one)
})

test_that("print() and summary() show an XGBoost fit's five settings", {
  local_reproducible_output(width = 200)
  skip_if_not_installed("xgboost", "3.0.0")
  data(altmejd)
  statistics <- c("mean", "sd", "lower", "min", "max")
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
  skip_if_not_installed("xgboost", "3.0.0")
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

  expect_error(plot(res, metric = "not_a_metric"), "no estimates")
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
