# Three levels, one meaning each: plain text informs, gray marks labels and
# steps, teal marks "Done" only. Nothing is bold.

console_fit <- function(n_iter = 3) {
  h <- rpart::rpart(
    replicate ~ power.o + n.o,
    data = altmejd,
    maxdepth = 1,
    model = TRUE
  )
  adaboost(h, n_iter = n_iter, eta = 1, verbose = FALSE)
}

# two depths, two round counts: 4 settings in 2 fits per fold
console_grid <- expand.grid(depth = 1:2, n_iter = c(2, 4), eta = 1)

# the codes the transcripts may use: reset, gray and teal
allowed_codes <- c("0", "38;5;244", "38;5;30")

# record the size of every progress bar opened while `code` runs
record_steps <- function(code) {
  steps <- integer()
  real_open <- open_progress
  local_mocked_bindings(open_progress = function(n_steps) {
    steps <<- c(steps, n_steps)
    real_open(n_steps)
  })
  value <- capture_run(code)
  value$steps <- steps
  value
}

# ---- adaboost() and predict() ------------------------------------------------

test_that("adaboost() prints plain start and closing lines, gray steps", {
  data(altmejd)
  h <- rpart::rpart(
    replicate ~ power.o + n.o,
    data = altmejd,
    maxdepth = 1,
    model = TRUE
  )
  run <- capture_run(adaboost(h, n_iter = 3, eta = 1))
  err <- run$err

  expect_identical(run$out, character(0))
  expect_false(has_bold(err))
  expect_true(teal_only_on_done(err))
  expect_true(all(read_codes(err) %in% allowed_codes))
  expect_false(grepl("successfully", err))
  # plain: no code before the start line or the closing line
  expect_true(startsWith(err, "Start the AdaBoost training process:\n"))
  expect_true(grepl("\nTraining process completed.\n\n", err, fixed = TRUE))
  expect_true(grepl(
    "\033[38;5;244mRun mild input checks\033[0m\033[38;5;244m...\033[0m \033[38;5;30mDone\033[0m\n",
    err,
    fixed = TRUE
  ))
})

test_that("predict() prints plain start, closing and conclusion lines", {
  data(altmejd)
  fit <- console_fit()
  run <- capture_run(predict(fit, altmejd, type = "margin"))
  err <- run$err

  expect_false(has_bold(err))
  expect_true(teal_only_on_done(err))
  expect_true(all(read_codes(err) %in% allowed_codes))
  expect_false(grepl("successfully", err))
  expect_true(startsWith(err, "Start the AdaBoost test process:\n"))
  expect_true(grepl("\nTest process completed.\n", err, fixed = TRUE))
  # the reason gray, the conclusion plain
  expect_true(grepl(
    "\033[38;5;244m  `altmejd` was also used for training.\033[0m\n",
    err,
    fixed = TRUE
  ))
  expect_true(endsWith(
    err,
    "\n! Outputs are retrodictions, not predictions.\n"
  ))
})

test_that("verbose adaboost() and predict() return what quiet ones do", {
  h <- rpart::rpart(
    replicate ~ power.o + n.o,
    data = altmejd_splits$train,
    maxdepth = 1,
    model = TRUE
  )
  loud <- capture_run(adaboost(h, n_iter = 3, eta = 1))$value
  quiet <- adaboost(h, n_iter = 3, eta = 1, verbose = FALSE)
  expect_identical(loud, quiet)

  # held-out rows, so no retrodiction notice
  nd <- altmejd_splits$test
  expect_identical(
    capture_run(predict(quiet, nd, type = "margin"))$value,
    predict(quiet, nd, type = "margin", verbose = FALSE)
  )
})

# ---- logo_cv() ---------------------------------------------------------------

test_that("logo_cv(verbose = TRUE) prints a line per fold", {
  data(altmejd)
  fit <- console_fit()
  folds <- unique(as.character(altmejd$pid))
  run <- record_steps(
    logo_cv(fit, data = altmejd, group = "pid", verbose = TRUE)
  )
  err <- run$err

  expect_identical(
    read_lines(err),
    c(
      "Start leave-one-group-out cross-validation:",
      sprintf(
        "Fold %d/%d, test: %s... Done",
        seq_along(folds),
        length(folds),
        folds
      ),
      "Cross-validation completed."
    )
  )
  # a gray fold line, a teal Done; no bar
  expect_true(grepl(
    paste0(
      "\033[38;5;244mFold 1/",
      length(folds),
      ", test: eerp\033[0m\033[38;5;244m...\033[0m \033[38;5;30mDone\033[0m\n"
    ),
    err,
    fixed = TRUE
  ))
  expect_true(startsWith(err, "Start leave-one-group-out cross-validation:\n"))
  expect_true(endsWith(err, "\nCross-validation completed.\n"))
  expect_identical(run$steps, integer())
  expect_identical(run$out, character(0))
  expect_false(has_bold(err))
  expect_true(teal_only_on_done(err))
  expect_true(all(read_codes(err) %in% allowed_codes))

  # the inner fits stay silent, and verbose = FALSE prints nothing
  quiet <- capture_run(logo_cv(
    fit,
    data = altmejd,
    group = "pid",
    verbose = FALSE
  ))
  expect_identical(quiet$err, "")
  expect_identical(quiet$out, character(0))
  expect_identical(run$value, quiet$value)
})

test_that("logo_cv() of any learner reports its folds alike", {
  data(altmejd)
  glm_fit <- glm(
    replicate ~ power.o + n.o,
    data = altmejd,
    family = binomial
  )
  run <- capture_run(
    logo_cv(
      glm_fit,
      data = altmejd,
      group = "pid",
      holdout = "ssrp",
      verbose = TRUE
    )
  )
  expect_identical(
    read_lines(run$err),
    c(
      "Start leave-one-group-out cross-validation:",
      "Fold 1/1, test: ssrp... Done",
      "Cross-validation completed."
    )
  )
  quiet <- logo_cv(
    glm_fit,
    data = altmejd,
    group = "pid",
    holdout = "ssrp",
    verbose = FALSE
  )
  expect_identical(run$value, quiet)
})

# ---- tune_logo_cv() ----------------------------------------------------------

test_that("tune_logo_cv(verbose = TRUE) prints one bar over the fits", {
  data(altmejd)
  fit <- console_fit()
  n_folds <- length(unique(altmejd$pid))
  run <- record_steps(
    tune_logo_cv(
      fit,
      console_grid,
      data = altmejd,
      group = "pid",
      verbose = TRUE
    )
  )
  err <- run$err
  lines <- read_lines(err)

  # 4 settings share 2 fits per fold, and the start line says so
  expect_identical(
    lines[1],
    sprintf(
      "Start the search: 4 settings, %d folds, %d fits",
      n_folds,
      2L * n_folds
    )
  )
  expect_identical(run$steps, 2L * n_folds)
  expect_length(lines, 3L)
  expect_match(lines[2], "100%$")
  expect_identical(lines[3], "Search completed.")
  # the bar is gray, and its style ends before the plain closing line
  expect_true(grepl("\033[38;5;244m\r", err, fixed = TRUE))
  expect_true(endsWith(err, "100%\n\033[0mSearch completed.\n"))
  expect_false(has_bold(err))
  expect_true(teal_only_on_done(err))
  expect_true(all(read_codes(err) %in% allowed_codes))
  expect_identical(run$out, character(0))

  quiet <- capture_run(
    tune_logo_cv(
      fit,
      console_grid,
      data = altmejd,
      group = "pid",
      verbose = FALSE
    )
  )
  expect_identical(quiet$err, "")
  expect_identical(quiet$out, character(0))
  expect_identical(run$value, quiet$value)
})

test_that("a search whose settings each need a fit names no fits", {
  data(altmejd)
  tree <- rpart::rpart(
    replicate ~ power.o + n.o,
    data = altmejd,
    method = "class"
  )
  n_folds <- length(unique(altmejd$pid))
  run <- record_steps(
    tune_logo_cv(
      tree,
      data.frame(depth = 1:3),
      data = altmejd,
      group = "pid",
      verbose = TRUE
    )
  )
  expect_identical(
    read_lines(run$err)[1],
    sprintf("Start the search: 3 settings, %d folds", n_folds)
  )
  expect_identical(run$steps, 3L * n_folds)
})

test_that("the start line names the cores only when there are several", {
  expect_identical(
    describe_search(4, 5, 10, 1L),
    "Start the search: 4 settings, 5 folds, 10 fits"
  )
  expect_identical(
    describe_search(4, 5, 10, 4L),
    "Start the search: 4 settings, 5 folds, 10 fits, 4 cores"
  )
  expect_identical(
    describe_search(3, 5, 15, 2L),
    "Start the search: 3 settings, 5 folds, 2 cores"
  )
})

# ---- several cores -----------------------------------------------------------

# mocks change the code that socket workers check, so these tests fork, and
# a mocked count of 2 cores keeps choose_cores() quiet on any computer

test_that("on several cores, the bar moves as each batch of fits comes back", {
  skip_on_os("windows")
  data(altmejd)
  fit <- console_fit()
  real_split <- split_batches
  local_mocked_bindings(
    count_cores = function() 2L,
    # 2 fits per worker, so the 10 fits come back in batches of 4, 4 and 2
    split_batches = function(n_tasks, n_workers, batched) {
      real_split(n_tasks, n_workers, batched, per_worker = 2L)
    }
  )
  run <- record_steps(
    tune_logo_cv(fit, console_grid, data = altmejd, group = "pid", cores = 2)
  )
  err <- run$err
  lines <- read_lines(err)

  expect_identical(
    lines[1],
    "Start the search: 4 settings, 5 folds, 10 fits, 2 cores"
  )
  expect_length(lines, 3L)
  expect_identical(lines[3], "Search completed.")
  expect_identical(run$steps, 10L)
  # the bar redraws after each batch: 40, 80 and 100 in 100
  shown <- regmatches(err, gregexpr("[0-9]+%", err))[[1]]
  expect_identical(unique(shown), c("0%", "40%", "80%", "100%"))
  expect_true(endsWith(err, "100%\n\033[0mSearch completed.\n"))
  expect_false(has_bold(err))
  expect_true(teal_only_on_done(err))
  expect_true(all(read_codes(err) %in% allowed_codes))
  expect_identical(run$out, character(0))
})

test_that("verbose is TRUE by default in every method", {
  namespace <- asNamespace("adatutor")
  methods <- ls(namespace, pattern = "^(tune_)?logo_cv(\\.[A-Za-z]+)?$")
  # the generics, six logo_cv() methods and five tune_logo_cv() methods
  expect_length(methods, 13L)
  for (method in methods) {
    expect_identical(
      formals(get(method, envir = namespace))$verbose,
      TRUE,
      info = method
    )
  }
})

test_that("verbose = FALSE prints nothing, on one core and on two", {
  skip_on_os("windows")
  data(altmejd)
  fit <- console_fit()
  local_mocked_bindings(count_cores = function() 2L)
  quiet_runs <- list(
    capture_run(logo_cv(fit, data = altmejd, group = "pid", verbose = FALSE))
  )
  for (cores in 1:2) {
    quiet_runs <- c(
      quiet_runs,
      list(
        capture_run(tune_logo_cv(
          fit,
          console_grid,
          data = altmejd,
          group = "pid",
          cores = cores,
          verbose = FALSE
        )),
        capture_run(tune_logo_cv(
          fit,
          console_grid,
          data = altmejd,
          group = "pid",
          holdout = c("ssrp", "ml1"),
          nested = TRUE,
          cores = cores,
          verbose = FALSE
        ))
      )
    )
  }
  for (run in quiet_runs) {
    expect_identical(run$err, "")
    expect_identical(run$out, character(0))
  }
})

test_that("on two cores, verbose changes no result", {
  skip_on_os("windows")
  data(altmejd)
  fit <- console_fit()
  local_mocked_bindings(count_cores = function() 2L)
  search <- function(cores, verbose) {
    capture_run(tune_logo_cv(
      fit,
      console_grid,
      data = altmejd,
      group = "pid",
      cores = cores,
      verbose = verbose
    ))$value
  }
  nested <- function(cores, verbose) {
    capture_run(tune_logo_cv(
      fit,
      console_grid,
      data = altmejd,
      group = "pid",
      holdout = c("ssrp", "ml1"),
      nested = TRUE,
      cores = cores,
      verbose = verbose
    ))$value
  }
  expect_identical(search(2, TRUE), search(2, FALSE))
  expect_identical(search(2, TRUE), search(1, FALSE))
  expect_identical(nested(2, TRUE), nested(2, FALSE))
  expect_identical(nested(2, TRUE), nested(1, FALSE))
})

test_that("batches keep each task's random-number stream", {
  skip_on_os("windows")
  kinds <- RNGkind()
  on.exit(do.call(RNGkind, as.list(kinds)), add = TRUE)
  RNGkind("L'Ecuyer-CMRG")
  set.seed(1)
  seed <- .Random.seed
  draw <- function(k) c(k, stats::runif(2))
  one_batch <- run_tasks(11, draw, cores = 2)
  expect_identical(.Random.seed, seed)

  done <- integer()
  real_split <- split_batches
  local_mocked_bindings(
    split_batches = function(n_tasks, n_workers, batched) {
      real_split(n_tasks, n_workers, batched, per_worker = 1L)
    }
  )
  batched <- run_tasks(11, draw, cores = 2, report = \(n) done <<- c(done, n))
  # batches of 2, the lone last task joined to the one before
  expect_identical(done, c(2L, 4L, 6L, 8L, 11L))
  expect_identical(batched, one_batch)
  expect_identical(.Random.seed, seed)
  # each task drew from a stream of its own
  expect_length(unique(vapply(batched, `[`, numeric(1), 2L)), 11L)
})

test_that("split_batches() joins a lone last task and can keep one batch", {
  expect_identical(split_batches(5, 2, batched = FALSE), list(1:5))
  expect_identical(
    split_batches(9, 2, batched = TRUE, per_worker = 2L),
    list(1:4, 5:9)
  )
  expect_identical(
    split_batches(10, 2, batched = TRUE, per_worker = 2L),
    list(1:4, 5:8, 9:10)
  )
  expect_identical(split_batches(3, 2, batched = TRUE), list(1:3))
})

test_that("on the socket path, the bar shows and no result changes", {
  # count_cores() cannot be mocked here, so a one-core computer skips
  skip_if(isTRUE(count_cores() < 2L))
  # the only mock socket workers accept: their code check leaves it out
  local_mocked_bindings(read_os_type = function() "windows")
  data(altmejd)
  fit <- console_fit()
  loud <- capture_run(
    tune_logo_cv(fit, console_grid, data = altmejd, group = "pid", cores = 2)
  )
  lines <- read_lines(loud$err)
  expect_identical(
    lines[1],
    "Start the search: 4 settings, 5 folds, 10 fits, 2 cores"
  )
  expect_identical(lines[length(lines)], "Search completed.")
  expect_true(endsWith(loud$err, "100%\n\033[0mSearch completed.\n"))
  quiet <- tune_logo_cv(
    fit,
    console_grid,
    data = altmejd,
    group = "pid",
    cores = 2,
    verbose = FALSE
  )
  expect_identical(loud$value, quiet)
})

test_that("on the socket path, a task's error reads the same with progress", {
  local_mocked_bindings(read_os_type = function() "windows")
  fail_second <- function(k) if (k == 2) stop("task 2 failed") else k
  done <- integer()
  expect_error(
    run_tasks(3, fail_second, cores = 2, report = \(n) done <<- c(done, n)),
    "^task 2 failed$"
  )
  # the error stops the run only after its batch came back
  expect_identical(done, 3L)
})

# ---- the nested search -------------------------------------------------------

test_that("the nested search prints one line per outer fold, and no bar", {
  data(altmejd)
  fit <- console_fit()
  run <- record_steps(
    tune_logo_cv(
      fit,
      console_grid,
      data = altmejd,
      group = "pid",
      holdout = c("ssrp", "ml1"),
      nested = TRUE,
      verbose = TRUE
    )
  )
  err <- run$err
  lines <- read_lines(err)

  expect_identical(
    lines,
    c(
      "Start the nested search: 2 outer folds, 4 settings each",
      "Outer fold 1/2, test: ssrp... Done",
      "Outer fold 2/2, test: ml1... Done",
      "Nested search completed."
    )
  )
  # the inner searches open no bar
  expect_identical(run$steps, integer())
  # a gray fold line, a teal Done
  expect_true(grepl(
    paste0(
      "\033[38;5;244mOuter fold 1/2, test: ssrp\033[0m",
      "\033[38;5;244m...\033[0m \033[38;5;30mDone\033[0m\n"
    ),
    err,
    fixed = TRUE
  ))
  expect_true(startsWith(err, "Start the nested search:"))
  expect_true(endsWith(err, "\nNested search completed.\n"))
  expect_false(has_bold(err))
  expect_true(teal_only_on_done(err))
  expect_true(all(read_codes(err) %in% allowed_codes))
  expect_identical(run$out, character(0))

  quiet <- capture_run(
    tune_logo_cv(
      fit,
      console_grid,
      data = altmejd,
      group = "pid",
      holdout = c("ssrp", "ml1"),
      nested = TRUE,
      verbose = FALSE
    )
  )
  expect_identical(quiet$err, "")
  expect_identical(quiet$out, character(0))
  expect_identical(run$value, quiet$value)
})

test_that("on several cores, the nested search names them, same lines", {
  skip_on_os("windows")
  data(altmejd)
  local_mocked_bindings(count_cores = function() 2L)
  run <- record_steps(
    tune_logo_cv(
      console_fit(),
      console_grid,
      data = altmejd,
      group = "pid",
      holdout = c("ssrp", "ml1"),
      nested = TRUE,
      cores = 2
    )
  )
  expect_identical(
    read_lines(run$err),
    c(
      "Start the nested search: 2 outer folds, 4 settings each, 2 cores",
      "Outer fold 1/2, test: ssrp... Done",
      "Outer fold 2/2, test: ml1... Done",
      "Nested search completed."
    )
  )
  expect_identical(run$steps, integer())
  expect_true(teal_only_on_done(run$err))
  expect_true(all(read_codes(run$err) %in% allowed_codes))
})

# ---- an error in mid-run -----------------------------------------------------

# make the `n`th refit fail
fail_refit <- function(n, env = parent.frame()) {
  calls <- 0L
  real_refit <- refit_score
  local_mocked_bindings(
    refit_score = function(fit, ...) {
      calls <<- calls + 1L
      if (calls == n) {
        stop("forced failure", call. = FALSE)
      }
      real_refit(fit, ...)
    },
    .env = env
  )
}

test_that("an error closes the open fold line", {
  data(altmejd)
  fit <- console_fit()
  fail_refit(2L)
  err <- capture_stderr(
    expect_error(
      logo_cv(fit, data = altmejd, group = "pid", verbose = TRUE),
      "forced failure"
    )
  )
  expect_true(endsWith(err, "\n"))
  expect_identical(tail(read_lines(err), 1L), "Fold 2/5, test: ml1")
})

test_that("an error closes the bar and resets its style", {
  data(altmejd)
  fit <- console_fit()
  fail_refit(3L)
  err <- capture_stderr(
    expect_error(
      tune_logo_cv(
        fit,
        console_grid,
        data = altmejd,
        group = "pid",
        verbose = TRUE
      ),
      "forced failure"
    )
  )
  expect_true(endsWith(err, "\n"))
  expect_identical(tail(read_codes(err), 1L), "0")
})

test_that("an error in a nested search closes its outer fold's line", {
  data(altmejd)
  fit <- console_fit()
  nested_err <- function() {
    capture_stderr(
      expect_error(
        tune_logo_cv(
          fit,
          console_grid,
          data = altmejd,
          group = "pid",
          holdout = c("ssrp", "ml1"),
          nested = TRUE,
          verbose = TRUE
        ),
        "forced failure"
      )
    )
  }
  # the inner search of the first outer fold makes 8 fits; the 9th is its
  # refit, and the 3rd is in its inner search
  for (n in c(9L, 3L)) {
    local({
      fail_refit(n)
      err <- nested_err()
      expect_true(endsWith(err, "\n"))
      expect_identical(
        tail(read_lines(err), 1L),
        "Outer fold 1/2, test: ssrp"
      )
    })
  }
})

test_that("a worker's error reads the same with verbose and closes the bar", {
  skip_on_os("windows")
  data(altmejd)
  fit <- console_fit()
  # each fork counts its own refits from 0, so every fork's first refit fails
  fail_refit(1L)
  local_mocked_bindings(count_cores = function() 2L)
  run <- function(verbose) {
    text <- NULL
    err <- capture_stderr(
      text <- tryCatch(
        # mclapply() also warns that function calls failed
        suppressWarnings(tune_logo_cv(
          fit,
          console_grid,
          data = altmejd,
          group = "pid",
          cores = 2,
          verbose = verbose
        )),
        error = conditionMessage
      )
    )
    list(text = text, err = err)
  }
  loud <- run(TRUE)
  quiet <- run(FALSE)
  expect_identical(loud$text, "forced failure")
  expect_identical(loud$text, quiet$text)
  expect_identical(quiet$err, "")
  # the bar ends its line and resets the style
  expect_true(endsWith(loud$err, "\n"))
  expect_identical(tail(read_codes(loud$err), 1L), "0")
  expect_match(read_lines(loud$err)[1], ", 2 cores$")
})

# ---- print methods -----------------------------------------------------------

# a print, plain and with styling forced on
print_both <- function(x, ...) {
  plain <- utils::capture.output(print(x, ...))
  local_mocked_bindings(use_ansi = function() TRUE)
  styled <- utils::capture.output(print(x, ...))
  list(plain = plain, styled = styled)
}

expect_gray_labels_only <- function(both) {
  expect_false(any(grepl("\033", both$plain, fixed = TRUE)))
  # padding first, styling after: stripped, the styled print is the plain one
  expect_identical(strip_ansi(both$styled), both$plain)
  expect_true(any(grepl("\033[38;5;244m", both$styled, fixed = TRUE)))
  # no bold and no teal: a print has no "Done"
  expect_true(all(read_codes(both$styled) %in% c("0", "38;5;244")))
}

test_that("assess() prints a plain title, gray labels and plain values", {
  y <- c(rep(1, 12), rep(0, 9))
  set.seed(41)
  s <- rnorm(21) + y
  both <- print_both(assess(y, s))
  expect_gray_labels_only(both)
  expect_identical(both$plain[1], "Performance metrics")
  expect_identical(both$styled[1], "Performance metrics")
  expect_true(startsWith(both$styled[3], "\033[38;5;244mpredicted 1"))
  expect_true(any(startsWith(both$styled, "\033[38;5;244mAt threshold 0")))
  expect_true(any(startsWith(both$styled, "\033[38;5;244mAmong top k")))
  expect_true(any(startsWith(both$styled, "\033[38;5;244mOver thresholds")))
  # metric names and values stay plain
  expect_false(any(grepl("\033\\[38;5;244m *(sens|auroc|tp)", both$styled)))

  cm <- confusion(y, s)
  both <- print_both(assess(cm))
  expect_gray_labels_only(both)
  expect_identical(both$plain[1], "Performance metrics")

  both <- print_both(cm)
  expect_gray_labels_only(both)
  expect_identical(both$plain[1:2], c("Confusion matrix", "At threshold 0"))
  expect_identical(both$styled[2], "\033[38;5;244mAt threshold 0\033[0m")
})

test_that("logo_cv results print gray labels without colons", {
  data(altmejd)
  fit <- console_fit()
  res <- logo_cv(fit, data = altmejd, group = "pid", verbose = FALSE)
  both <- print_both(res)
  expect_gray_labels_only(both)
  expect_identical(both$plain[1], "Leave-one-group-out cross-validation")
  expect_true("  model     adaboost" %in% both$plain)
  expect_false(any(grepl("model:", both$plain, fixed = TRUE)))
  expect_true("  \033[38;5;244mmodel    \033[0m adaboost" %in% both$styled)
  expect_true(any(startsWith(both$styled, "\033[38;5;244mbase rate\033[0m")))

  set.seed(1)
  both <- print_both(bootstrap(res, n_resample = 20))
  expect_gray_labels_only(both)
  expect_true(any(startsWith(both$styled, "\033[38;5;244mdropped\033[0m")))

  # a narrow console wraps the projects block, and each part keeps its labels
  local_reproducible_output(width = 30)
  both <- print_both(res)
  expect_gray_labels_only(both)
  expect_gt(sum(startsWith(both$plain, "base rate")), 1L)

  # the head of a long grid, and a nested result
  local_reproducible_output(width = 80)
  long <- tune_logo_cv(
    fit,
    data.frame(n_iter = 1:21),
    data = altmejd,
    group = "pid",
    verbose = FALSE
  )
  both <- print_both(long)
  expect_gray_labels_only(both)
  expect_true(any(startsWith(both$plain, "... and 11 more settings")))

  data(nested)
  both <- print_both(nested)
  expect_gray_labels_only(both)
  expect_true("  selected  lower auroc" %in% both$plain)
})
