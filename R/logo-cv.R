#' Leave-one-group-out cross-validation
#'
#' Holds out each level of `group` in turn, refits the model on the rest and
#' scores the held-out rows, so the result is one performance estimate per
#' group rather than a single number: leave-one-group-out cross-validation
#' (LOGO-CV). In the tutorial the groups are the replication projects. Whole
#' projects are held out because the study effects in a project share a design
#' and a base rate.
#'
#' Pass a fitted model. `logo_cv()` reads the recipe off the fit and refits it
#' once per group. The fit's own data are not used, because cross-validation
#' evaluates the procedure, not one fitted model. To compare AdaBoost settings,
#' pass the fit and a grid to [tune_logo_cv()].
#'
#' @section What each method reads off the fit:
#' * An [adaboost()] fit: the formula, the tree controls, the splitting rule,
#'   `n_iter` and `eta`. Each refit grows the tree and boosts it, and is
#'   scored on the margin.
#' * A classification tree from [rpart::rpart()], a binomial [stats::glm()],
#'   or a classification forest from [randomForest::randomForest()] fitted
#'   with a formula: the call that made the fit. The call runs again with
#'   `data` replaced by the training rows, so settings such as `control`,
#'   `parms`, `family`, `ntree` or `mtry` carry over. Each refit is scored on
#'   its predicted probability of the second class.
#'
#' Every model is scored on a continuous score, never on class labels.
#'
#' The call runs where its formula was made, so any other object it names,
#' such as `weights = w` or `control = ctrl`, must still exist there. A
#' variable named as a column of `data`, such as `weights = n.o`, travels with
#' the rows. A random forest draws at random: set a seed to repeat a result.
#'
#' @section Holding out some groups only:
#' `holdout` names the groups to hold out, in that order. The other groups
#' are only ever trained on. One name gives a single split into training and
#' test data: the final test on a project kept back from tuning, or one Monte
#' Carlo draw whose test part is a level of a random column (see the
#' examples).
#'
#' @section Reading the estimates:
#' The estimates are returned per project and are not averaged. Describe their
#' spread, but do not turn it into a standard error or a confidence interval.
#' The folds differ in size (10 to 90 study effects in `altmejd`), and they
#' share most of their training data, so the usual standard error is too small
#' (Bengio & Grandvalet, 2004).
#'
#' @param x A fitted model: a fit from [adaboost()], a classification tree
#'   from [rpart::rpart()], a binomial [stats::glm()], or a classification
#'   forest from [randomForest::randomForest()] fitted with a formula.
#' @param data A data frame with the model's variables and the `group`
#'   column, holding every group.
#' @param group The name of the column that defines the folds, e.g. `"pid"`.
#' @param holdout The levels of `group` to hold out, one fold each. Defaults
#'   to `NULL`, which holds out every level once.
#' @param verbose Whether to report progress. Defaults to `FALSE`.
#' @param ... Must be empty. The settings belong to the fit, so an argument
#'   such as `n_iter` or `grid` is an error.
#'
#' @return An object of class `logo_cv`.
#'   * `estimates`: an array of setting x project x metric, e.g.
#'     `res$estimates[, , "auroc"]`. `as.data.frame()` gives the same values
#'     as one long table. An `NA` cell is an undefined measure, not a failed
#'     fit.
#'   * `grid`: the settings, one row per setting. `depth`, `n_iter` and `eta`
#'     are `NA` where the learner has no such setting.
#'   * `projects`: each fold's `n` and `base_rate`.
#'   * `scores`: the held-out scores, one row per study and setting, which
#'     [bootstrap()] resamples.
#'   * `model`: the learner, `"adaboost"`, `"rpart"`, `"glm"` or `"rf"`.
#'   * `nested`, `criterion` and `selected`: set by [tune_logo_cv()] when it
#'     nests the search.
#'
#' @family cross-validation
#'
#' @references Bengio, Y., & Grandvalet, Y. (2004). No unbiased estimator of the
#'   variance of k-fold cross-validation. *Journal of Machine Learning Research,
#'   5*, 1089-1105.
#'
#' @examples
#' data(altmejd)
#' prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
#'
#' # cross-validate the recipe of a fitted ensemble
#' h <- rpart::rpart(
#'   replicate ~ .,
#'   data = altmejd[, c(prednms, "replicate")],
#'   maxdepth = 1,
#'   model = TRUE
#' )
#' fit <- adaboost(h, n_iter = 10, eta = 1, verbose = FALSE)
#' res <- logo_cv(fit, data = altmejd, group = "pid")
#' res
#' plot(res)
#'
#' # another learner: pass its fit
#' glm(
#'   replicate ~ power.o + effect_size.o + n.o + p_value.o,
#'   data = altmejd,
#'   family = binomial
#' ) |>
#'   logo_cv(data = altmejd, group = "pid")
#'
#' # random five-fold cross-validation: random folds as the groups
#' set.seed(1)
#' altmejd$fold <- sample(rep(1:5, length.out = nrow(altmejd)))
#' logo_cv(fit, data = altmejd, group = "fold")
#'
#' # one Monte Carlo draw: a random split, holding out the test part only
#' altmejd$part <- sample(
#'   c("train", "test"),
#'   nrow(altmejd),
#'   replace = TRUE,
#'   prob = c(0.7, 0.3)
#' )
#' logo_cv(fit, data = altmejd, group = "part", holdout = "test")
#'
#' # the final test on HOLZ, the project kept back from tuning
#' data(altmext)
#' logo_cv(fit, data = altmext, group = "pid", holdout = "holz")
#'
#' @export
logo_cv <- function(x, data, group, holdout = NULL, verbose = FALSE, ...) {
  UseMethod("logo_cv")
}

#' @rdname logo_cv
#' @export
logo_cv.adaboost <- function(
  x,
  data,
  group,
  holdout = NULL,
  verbose = FALSE,
  ...
) {
  check_dots_empty(...)
  # one recipe is a grid of one row, the fit's own
  tune_logo_cv(
    x,
    grid = read_setting(x),
    data = data,
    group = group,
    holdout = holdout,
    verbose = verbose
  )
}

#' @rdname logo_cv
#' @export
logo_cv.rpart <- function(
  x,
  data,
  group,
  holdout = NULL,
  verbose = FALSE,
  ...
) {
  check_dots_empty(...)
  if (!identical(x$method, "class")) {
    stop(
      "`x` must be a classification tree, fitted with `method = \"class\"`: ",
      "logo_cv() scores its predicted probabilities.",
      call. = FALSE
    )
  }
  cross_validate(
    x,
    data = data,
    group = group,
    # depth is the one setting of the grid a tree has
    grid = data.frame(
      depth = x$control$maxdepth,
      n_iter = NA_real_,
      eta = NA_real_
    ),
    folds = read_folds(data, group, holdout),
    model = "rpart",
    holdout = holdout,
    verbose = verbose
  )
}

#' @rdname logo_cv
#' @export
logo_cv.glm <- function(
  x,
  data,
  group,
  holdout = NULL,
  verbose = FALSE,
  ...
) {
  check_dots_empty(...)
  if (!x$family$family %in% c("binomial", "quasibinomial")) {
    stop(
      "`x` must be a logistic regression, fitted with `family = binomial`: ",
      "logo_cv() scores its predicted probabilities.",
      call. = FALSE
    )
  }
  cross_validate(
    x,
    data = data,
    group = group,
    grid = data.frame(depth = NA_real_, n_iter = NA_real_, eta = NA_real_),
    folds = read_folds(data, group, holdout),
    model = "glm",
    holdout = holdout,
    verbose = verbose
  )
}

#' @rdname logo_cv
#' @export
logo_cv.randomForest <- function(
  x,
  data,
  group,
  holdout = NULL,
  verbose = FALSE,
  ...
) {
  check_dots_empty(...)
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop(
      "A random forest needs the randomForest package. ",
      "Install it with install.packages(\"randomForest\").",
      call. = FALSE
    )
  }
  if (!identical(x$type, "classification")) {
    stop(
      "`x` must be a classification forest, fitted on a factor outcome: ",
      "logo_cv() scores its predicted probabilities.",
      call. = FALSE
    )
  }
  if (is.null(x$terms)) {
    stop(
      "`x` must be fitted with a formula, as in ",
      "randomForest(replicate ~ ., data = d): logo_cv() refits its call on ",
      "each fold's rows.",
      call. = FALSE
    )
  }
  cross_validate(
    x,
    data = data,
    group = group,
    grid = data.frame(depth = NA_real_, n_iter = NA_real_, eta = NA_real_),
    folds = read_folds(data, group, holdout),
    model = "rf",
    holdout = holdout,
    verbose = verbose
  )
}

#' @rdname logo_cv
#' @export
logo_cv.default <- function(
  x,
  data,
  group,
  holdout = NULL,
  verbose = FALSE,
  ...
) {
  stop(
    "`x` must be a fitted model: a fit from adaboost(), rpart(), glm() or ",
    "randomForest(), not an object of class `",
    class(x)[1L],
    "`. Fit the model first, then pass the fit; to search a grid of AdaBoost ",
    "settings, use tune_logo_cv().",
    call. = FALSE
  )
}

#' Tune AdaBoost settings by leave-one-group-out cross-validation
#'
#' Cross-validates an [adaboost()] fit at every row of a grid of settings,
#' with the folds of [logo_cv()]: each level of `group` is held out in turn,
#' or only the levels in `holdout`. Each row changes the recipe read off the
#' fit, so the result has one estimate per setting and group.
#'
#' @section Grid columns:
#' `depth`, `n_iter` and `eta` set the depth of the boosted tree, the number
#' of rounds and the learning rate. A column named after an
#' [rpart::rpart.control()] setting, such as `cp` or `minbucket`, sets that
#' control for its row. A column the grid lacks takes the fit's own value, so
#' `data.frame(n_iter = c(10, 100))` varies the rounds and keeps the fit's
#' depth and rate. Any other column is an error.
#'
#' @section Nested cross-validation:
#' Picking the best row of a grid and reporting its score is optimistic,
#' because the same folds chose the winner and graded it. With
#' `nested = TRUE`, each held-out group takes no part in the choice: a search
#' on the other groups picks the row with the highest mean `criterion`, and
#' only that row is scored on the held-out group. The result estimates the
#' procedure "search the grid, then fit the winner", not any one setting. It
#' is slightly pessimistic, because each model trains on one group fewer.
#' Expect the outer folds to choose different settings.
#'
#' @section Speed:
#' Rows of `grid` that differ only in `n_iter` share one fit per fold. The
#' tree is boosted once, to the largest `n_iter` among them, and each row is
#' scored on the margin after its own number of rounds. The first rounds of a
#' longer run are the shorter run, so the result equals refitting each row. A
#' grid of many round counts therefore costs about as much as its largest one;
#' each value of `depth`, `eta` or an rpart control costs a fit of its own.
#'
#' @param x A fit from [adaboost()]. The grid varies its settings; its
#'   formula, other tree controls and splitting rule stay.
#' @param grid A data frame of settings, one row per setting. See the section
#'   on grid columns.
#' @inheritParams logo_cv
#' @param nested Whether to nest the search. The default, `FALSE`, scores
#'   every row of `grid` on every fold. `TRUE` needs two or more rows and at
#'   least three groups.
#' @param criterion The measure the inner search maximizes when
#'   `nested = TRUE`. Defaults to `"auroc"`. Fix it in advance: it decides
#'   which setting is chosen.
#' @param cores The number of cores to use. Defaults to 1. On macOS and
#'   Linux, the work is spread over that many forked R processes with
#'   [parallel::mclapply()]. Windows cannot fork, so there a value above 1
#'   runs on one core and says so. The results are the same at any number of
#'   cores. Progress messages from `verbose` appear only on one core.
#'
#' @return An object of class `logo_cv`, as from [logo_cv()], with one
#'   setting per row of `grid`. When nested, `estimates` is project x metric,
#'   and `selected` gives the setting each outer fold chose, with the inner
#'   mean of `criterion`.
#'
#' @family cross-validation
#' @seealso [hypergrid], a grid of 2,640 settings already scored on the six
#'   tuning projects of [altmext].
#'
#' @examples
#' data(altmejd)
#' fit <- rpart::rpart(
#'   replicate ~ power.o + effect_size.o + n.o + p_value.o,
#'   data = altmejd,
#'   maxdepth = 1,
#'   model = TRUE
#' ) |>
#'   adaboost(n_iter = 10, eta = 1, verbose = FALSE)
#'
#' # the rounds vary; depth and rate stay the fit's
#' res <- tune_logo_cv(
#'   fit,
#'   grid = data.frame(n_iter = c(5, 10)),
#'   data = altmejd,
#'   group = "pid"
#' )
#' summary(res)
#'
#' # nested: each held-out project takes no part in the choice
#' grid <- expand.grid(depth = 1:2, n_iter = c(5, 10))
#' tune_logo_cv(fit, grid, data = altmejd, group = "pid", nested = TRUE)
#'
#' @export
tune_logo_cv <- function(
  x,
  grid,
  data,
  group,
  holdout = NULL,
  nested = FALSE,
  criterion = "auroc",
  cores = 1,
  verbose = FALSE
) {
  if (!inherits(x, "adaboost")) {
    stop(
      "`x` must be a fit from adaboost(): tuning grids are for adaboost fits. ",
      "Pass another learner's fit to logo_cv().",
      call. = FALSE
    )
  }
  check_ada_fit(x)
  grid <- normalize_grid(grid, x)
  folds <- read_folds(data, group, holdout)
  cores <- choose_cores(cores)
  # messages from forked workers would interleave
  verbose <- verbose && cores == 1L

  if (!nested) {
    return(cross_validate(
      x,
      data = data,
      group = group,
      grid = grid,
      folds = folds,
      model = "adaboost",
      holdout = holdout,
      cores = cores,
      verbose = verbose
    ))
  }

  if (nrow(grid) < 2L) {
    stop(
      "`nested = TRUE` needs a `grid` with two or more rows: with nothing to ",
      "choose between, there is no selection step to protect against.",
      call. = FALSE
    )
  }
  # the inner search runs on every group but the held-out one, not only on
  # those in `holdout`
  if (length(unique(as.character(data[[group]]))) < 3L) {
    stop(
      "`nested = TRUE` needs at least three groups: one is held out and the ",
      "inner search needs two or more of the rest.",
      call. = FALSE
    )
  }
  if (length(criterion) != 1L || !criterion %in% list_measures()) {
    stop(
      "`criterion` \"",
      paste(criterion, collapse = ", "),
      "\" is not one of the measures returned by assess().",
      call. = FALSE
    )
  }
  cross_validate_nested(
    x,
    data = data,
    group = group,
    grid = grid,
    folds = folds,
    criterion = criterion,
    holdout = holdout,
    cores = cores,
    verbose = verbose
  )
}

#' Check `cores` and return how many to use
#' @noRd
choose_cores <- function(cores) {
  if (
    !is.numeric(cores) ||
      length(cores) != 1L ||
      !is.finite(cores) ||
      cores < 1 ||
      cores != round(cores)
  ) {
    stop("`cores` must be one whole number, 1 or more.", call. = FALSE)
  }
  # forking, not a socket cluster: the workers share this session, so they
  # need no installed copy of the package and nothing exported to them
  if (cores > 1 && read_os_type() == "windows") {
    message(
      "Running on one core: `cores` above 1 needs forked processes, which ",
      "Windows does not have."
    )
    return(1L)
  }
  as.integer(cores)
}

#' Read the operating system type, a seam for tests
#' @noRd
read_os_type <- function() {
  .Platform$OS.type
}

#' Run tasks, forked over `cores` on Unix-like systems; keep their order
#' @noRd
run_tasks <- function(n_tasks, run_task, cores = 1L) {
  if (cores == 1L || n_tasks == 1L) {
    return(lapply(seq_len(n_tasks), run_task))
  }
  # a fresh fork per task: tasks differ in cost (depth, largest n_iter), and
  # handing them out as cores free up balances the load
  results <- parallel::mclapply(
    seq_len(n_tasks),
    run_task,
    mc.cores = min(cores, n_tasks),
    mc.preschedule = FALSE
  )
  for (result in results) {
    if (inherits(result, "try-error")) {
      stop(conditionMessage(attr(result, "condition")), call. = FALSE)
    }
  }
  # a worker that was killed, e.g. for lack of memory, returns NULL
  if (any(vapply(results, is.null, logical(1)))) {
    stop(
      "A parallel worker returned no result. Rerun with fewer `cores`.",
      call. = FALSE
    )
  }
  results
}

#' Group the grid rows that one fit scores: those that differ only in
#' `n_iter`
#' @noRd
group_settings <- function(grid, model) {
  rows <- seq_len(nrow(grid))
  # only AdaBoost has rounds to read off a longer fit
  if (model != "adaboost") {
    return(as.list(rows))
  }
  shared <- grid[setdiff(names(grid), "n_iter")]
  # match() compares values exactly; factor levels or pasted numbers round
  codes <- lapply(shared, \(column) match(column, unique(column)))
  key <- do.call(paste, c(unname(codes), sep = "-"))
  unname(split(rows, factor(key, levels = unique(key))))
}

#' Describe a task's settings for a progress message
#' @noRd
describe_settings <- function(rows, n_settings) {
  if (length(rows) == 1L) {
    return(paste0("setting ", rows, "/", n_settings))
  }
  contiguous <- identical(rows, seq(rows[1L], rows[length(rows)]))
  paste0(
    "settings ",
    if (contiguous) {
      paste0(rows[1L], "-", rows[length(rows)])
    } else {
      paste(rows, collapse = ",")
    },
    "/",
    n_settings
  )
}

#' Check `data`, `group` and `holdout`, and return the folds
#' @noRd
read_folds <- function(data, group, holdout = NULL) {
  check_df(data)
  check_length(data)
  if (!is.character(group) || length(group) != 1L) {
    stop("`group` must be a single column name.", call. = FALSE)
  }
  if (!group %in% names(data)) {
    stop("`group` column `", group, "` not found in `data`.", call. = FALSE)
  }
  folds <- unique(as.character(data[[group]]))
  if (length(folds) < 2L) {
    stop(
      "`group` must have at least two levels to leave one out.",
      call. = FALSE
    )
  }
  if (is.null(holdout)) {
    return(folds)
  }

  if (!is.atomic(holdout) || length(holdout) == 0L) {
    stop("`holdout` must name one or more levels of `group`.", call. = FALSE)
  }
  holdout <- as.character(holdout)
  if (anyDuplicated(holdout)) {
    stop("`holdout` names a level more than once.", call. = FALSE)
  }
  unknown <- setdiff(holdout, folds)
  if (length(unknown)) {
    stop(
      "`holdout` names level(s) not found in `",
      group,
      "`: ",
      paste(unknown, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  # the order given, not the order in the data
  holdout
}

#' Read the fit's own setting as a one-row grid
#' @noRd
read_setting <- function(fit) {
  data.frame(
    depth = attr(fit, "control")$maxdepth,
    n_iter = attr(fit, "n_iter"),
    eta = attr(fit, "eta")
  )
}

#' Read the formula of a fit, with any `.` expanded
#' @noRd
read_formula <- function(fit) {
  if (inherits(fit, "adaboost")) {
    return(attr(fit, "formula"))
  }
  # through terms(), which strips the attributes a refit must not inherit,
  # such as `predvars` computed on the fit's own data
  stats::formula(stats::terms(fit))
}

#' Score every setting on every fold, keeping the held-out scores
#' @noRd
cross_validate <- function(
  fit,
  data,
  group,
  grid,
  folds,
  model,
  holdout = NULL,
  cores = 1L,
  verbose = FALSE
) {
  outcome <- all.vars(read_formula(fit))[1L]
  n_settings <- nrow(grid)
  held_out <- lapply(folds, \(fold) as.character(data[[group]]) == fold)
  groups <- group_settings(grid, model)

  # one task per group of settings and fold, the fold varying fastest; each
  # returns the held-out scores, one column per setting of its group
  tasks <- expand.grid(fold = seq_along(folds), group = seq_along(groups))
  run_task <- function(k) {
    rows <- groups[[tasks$group[k]]]
    in_fold <- held_out[[tasks$fold[k]]]
    if (verbose) {
      message(
        describe_settings(rows, n_settings),
        " | fold ",
        folds[tasks$fold[k]]
      )
    }
    score <- refit_score(
      fit,
      train = data[!in_fold, , drop = FALSE],
      test = data[in_fold, , drop = FALSE],
      setting = grid[rows, , drop = FALSE]
    )
    # the other learners score one setting, as a vector
    matrix(score, ncol = length(rows))
  }
  results <- run_tasks(nrow(tasks), run_task, cores)

  # where each setting's scores sit: the task of its group on a fold, and the
  # column within that task
  task_of <- matrix(seq_len(nrow(tasks)), nrow = length(folds))
  group_of <- rep(seq_along(groups), lengths(groups))[order(unlist(groups))]
  column_of <- sequence(lengths(groups))[order(unlist(groups))]

  # allocate the setting x project x metric cube up front
  measure_names <- list_measures()
  estimates <- array(
    NA_real_,
    dim = c(n_settings, length(folds), length(measure_names)),
    dimnames = list(
      setting = as.character(seq_len(n_settings)),
      project = folds,
      metric = measure_names
    )
  )
  actuals <- lapply(held_out, \(in_fold) data[[outcome]][in_fold])
  # n and base_rate describe a project, so store them once per project
  projects <- data.frame(
    project = folds,
    n = vapply(held_out, sum, integer(1)),
    base_rate = vapply(actuals, \(actual) mean(as_binary(actual)), numeric(1)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  # setting, then project, then the held-out rows in data order
  score_parts <- vector("list", n_settings * length(folds))
  for (i in seq_len(n_settings)) {
    for (j in seq_along(folds)) {
      score <- results[[task_of[j, group_of[i]]]][, column_of[i]]
      measures <- assess(
        actuals[[j]],
        score,
        threshold = choose_threshold(model)
      )
      estimates[i, j, names(measures)] <- unname(measures)
      score_parts[[(i - 1L) * length(folds) + j]] <- score
    }
  }
  # keep the held-out scores, so bootstrap() needs no refit
  scores <- data.frame(
    setting = rep(seq_len(n_settings), each = sum(projects$n)),
    project = rep(rep(folds, projects$n), times = n_settings),
    actual = data[[outcome]][rep(
      unlist(lapply(held_out, which)),
      times = n_settings
    )],
    score = unlist(score_parts),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      estimates = estimates,
      grid = grid,
      projects = projects,
      scores = scores,
      model = model,
      nested = FALSE,
      criterion = NA_character_,
      selected = NULL
    ),
    class = "logo_cv",
    # an attribute, not an element, so the elements stay as bootstrap() and
    # the plots read them
    holdout = if (!is.null(holdout)) folds
  )
}

#' Run the outer loop of nested LOGO-CV, calling tune_logo_cv() for each inner
#' search
#' @noRd
cross_validate_nested <- function(
  fit,
  data,
  group,
  grid,
  folds,
  criterion,
  holdout = NULL,
  cores = 1L,
  verbose = FALSE
) {
  outcome <- all.vars(read_formula(fit))[1L]
  measure_names <- list_measures()

  # no setting dimension: each outer fold selects its own (see `selected`)
  estimates <- array(
    NA_real_,
    dim = c(length(folds), length(measure_names)),
    dimnames = list(project = folds, metric = measure_names)
  )
  projects <- data.frame(
    project = folds,
    n = NA_integer_,
    base_rate = NA_real_,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  selected <- vector("list", length(folds))
  scores <- vector("list", length(folds))

  for (k in seq_along(folds)) {
    fold <- folds[k]
    if (verbose) {
      message("outer fold ", k, "/", length(folds), ": ", fold)
    }

    # drop the held-out project entirely, which keeps the selection honest
    held_out <- as.character(data[[group]]) == fold
    train <- data[!held_out, , drop = FALSE]
    test <- data[held_out, , drop = FALSE]

    inner_cv <- tune_logo_cv(
      fit,
      grid = grid,
      data = train,
      group = group,
      cores = cores,
      verbose = FALSE
    )

    # the mean over the inner folds ranks settings; it is not an estimate
    # (see ?tune_logo_cv)
    inner_means <- rowMeans(
      inner_cv$estimates[,, criterion, drop = FALSE],
      na.rm = TRUE
    )
    winner <- unname(which.max(inner_means))

    score <- refit_score(
      fit,
      train = train,
      test = test,
      setting = grid[winner, , drop = FALSE]
    )[, 1L]

    actual <- test[[outcome]]
    measures <- assess(actual, score, threshold = choose_threshold("adaboost"))

    # keep the whole grid row, so a control tuned in the grid travels along;
    # depth, n_iter and eta keep fixed positions
    choice <- grid[winner, , drop = FALSE]
    choice <- choice[,
      union(c("depth", "n_iter", "eta"), names(choice)),
      drop = FALSE
    ]
    selected[[k]] <- data.frame(
      project = fold,
      setting = winner,
      `rownames<-`(choice, NULL),
      inner_criterion = max(inner_means, na.rm = TRUE),
      row.names = NULL,
      stringsAsFactors = FALSE
    )

    estimates[fold, names(measures)] <- unname(measures)
    scores[[k]] <- data.frame(
      project = fold,
      actual = actual,
      score = unname(score),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    projects$n[k] <- nrow(test)
    projects$base_rate[k] <- mean(as_binary(actual))
  }

  structure(
    list(
      estimates = estimates,
      grid = grid,
      projects = projects,
      scores = do.call(rbind, scores),
      model = "adaboost",
      nested = TRUE,
      criterion = criterion,
      selected = do.call(rbind, selected)
    ),
    class = "logo_cv",
    holdout = if (!is.null(holdout)) folds
  )
}

#' List the measures assess() returns
#' @noRd
list_measures <- function() {
  names(assess(c(1, 0), c(1, -1)))
}

#' Choose the classification threshold for a learner's score
#' @noRd
choose_threshold <- function(model) {
  # 0 for the AdaBoost margin, 0.5 for probabilities
  if (model == "adaboost") 0 else 0.5
}

#' List the tree controls a grid row may set
#' @noRd
list_tree_controls <- function() {
  # maxdepth is excluded: depth is tuned
  setdiff(names(formals(rpart::rpart.control)), c("maxdepth", "..."))
}

#' Check the grid's columns and fill in missing ones from the fit
#' @noRd
normalize_grid <- function(grid, fit) {
  grid <- as.data.frame(grid)
  if (nrow(grid) == 0L) {
    stop("`grid` has no rows.", call. = FALSE)
  }
  # an error, not a warning: a grid that also holds results would otherwise
  # hide a column meant to tune something
  allowed <- c("depth", "n_iter", "eta", list_tree_controls())
  unknown <- setdiff(names(grid), allowed)
  if (length(unknown)) {
    stop(
      "grid column(s) ",
      paste(unknown, collapse = ", "),
      " set nothing. A grid may hold `depth`, `n_iter`, `eta` and the rpart ",
      "controls (",
      paste(list_tree_controls(), collapse = ", "),
      ").",
      call. = FALSE
    )
  }
  own <- read_setting(fit)
  for (column in setdiff(names(own), names(grid))) {
    grid[[column]] <- own[[column]]
  }
  grid
}

#' Refit a model on one fold and score the held-out rows
#'
#' The adaboost method scores a group of grid rows and returns one column per
#' row; the other methods score one row and return a vector.
#' @noRd
refit_score <- function(fit, train, test, setting) {
  UseMethod("refit_score")
}

#' Boost the fit's recipe once for grid rows that differ only in `n_iter`,
#' and score the margin after each row's `n_iter`
#' @exportS3Method NULL
#' @noRd
refit_score.adaboost <- function(fit, train, test, setting) {
  formula <- attr(fit, "formula")
  outcome <- all.vars(formula)[1L]
  predictors <- all.vars(formula)[-1L]
  if (identical(predictors, ".")) {
    predictors <- setdiff(names(train), outcome)
  }
  variables <- c(predictors, outcome)

  # the fit's control, then the rows' rpart controls, then their depth; the
  # rows share all three, so the first row speaks for the group
  shared <- setting[1L, , drop = FALSE]
  control <- attr(fit, "control")
  row_controls <- intersect(names(shared), list_tree_controls())
  control <- utils::modifyList(
    control,
    as.list(shared[, row_controls, drop = FALSE])
  )
  control$maxdepth <- shared[["depth"]]

  # model = TRUE carries the data adaboost() needs
  h <- rpart::rpart(
    formula,
    data = train[, variables, drop = FALSE],
    method = "class",
    control = control,
    # fits made before the split attribute existed used gini
    parms = list(split = attr(fit, "split") %||% "gini"),
    model = TRUE
  )
  # round t depends only on the rounds before it, so the first n rounds of
  # the longest run are the n-round run
  n_iter <- setting[["n_iter"]]
  refit <- adaboost(
    h,
    n_iter = max(n_iter),
    eta = shared[["eta"]],
    # each fit is thrown away, so keep no data for retrodictions
    keep_data = FALSE,
    verbose = FALSE,
    check_inputs = FALSE
  )

  # each tree's vote on each held-out row, coded as predict.adaboost() codes
  # it; matrix() keeps a single held-out row a row
  newdata <- test[, predictors, drop = FALSE]
  n_rows <- nrow(newdata)
  codes <- matrix(
    vapply(
      refit,
      \(round) stats::predict(round$h, newdata = newdata, type = "vector"),
      numeric(n_rows)
    ),
    nrow = n_rows
  )
  votes <- 2 * codes - 3
  alpha <- vapply(refit, `[[`, numeric(1), "alpha")

  # the margin, not labels: labels collapse auroc onto balanced accuracy.
  # The product predict.adaboost() takes, not a running sum, so each column
  # is identical to a refit's margin: cumsum() accumulates in extended
  # precision on some platforms and differs in the last digits.
  margins <- vapply(
    n_iter,
    \(n) {
      used <- seq_len(min(n, length(alpha)))
      as.vector(alpha[used] %*% t(votes[, used, drop = FALSE]))
    },
    numeric(n_rows)
  )
  matrix(margins, nrow = n_rows)
}

#' Refit a tree's call and score the probability of the second class
#' @exportS3Method NULL
#' @noRd
refit_score.rpart <- function(fit, train, test, setting) {
  refit <- refit_call(fit, train)
  prob <- stats::predict(refit, newdata = test, type = "prob")
  prob[, ncol(prob)]
}

#' Refit a logistic regression's call and score its probability
#' @exportS3Method NULL
#' @noRd
refit_score.glm <- function(fit, train, test, setting) {
  # code the outcome 0/1 as for the other learners; an expression such as
  # I(y == "a") is left for glm() to read
  outcome <- read_formula(fit)[[2L]]
  if (is.name(outcome)) {
    outcome <- as.character(outcome)
    train[[outcome]] <- as_binary(train[[outcome]])
  }
  refit <- refit_call(fit, train)
  stats::predict(refit, newdata = test, type = "response")
}

#' Refit a forest's call and score the probability of the second class
#' @exportS3Method NULL
#' @noRd
refit_score.randomForest <- function(fit, train, test, setting) {
  # the stored call names randomForest() bare, which needs it attached
  refit <- refit_call(fit, train, fun = quote(randomForest::randomForest))
  prob <- stats::predict(refit, newdata = test, type = "prob")
  prob[, ncol(prob)]
}

#' Run a fit's own call again on other rows
#' @noRd
refit_call <- function(fit, rows, fun = NULL) {
  formula <- read_formula(fit)
  call <- stats::getCall(fit)
  if (!is.null(fun)) {
    call[[1L]] <- fun
  }
  # the expanded formula, so a `.` cannot reach the group column; the rows
  # themselves, so the call needs no name for them
  call$formula <- formula
  call$data <- rows
  # not update(), which runs the call in this package: here the call's other
  # names resolve where they did for the original fit
  eval(call, environment(formula))
}

#' Methods for logo_cv objects
#'
#' `print()` shows the per-project estimates of one measure, `summary()`
#' returns them as a data frame, and `as.data.frame()` flattens all estimates
#' into one long table. `plot()` draws the estimates per project, with bands
#' when `x` carries intervals from [bootstrap()].
#'
#' @param x,object An object of class `logo_cv`.
#' @param metric The measure to show. Defaults to `"auroc"`.
#' @param ... Ignored.
#'
#' @return `print()` and `plot()` return `x` invisibly; `summary()` and
#'   `as.data.frame()` return data frames.
#'
#' @family cross-validation
#'
#' @name logo_cv_methods
NULL

#' @rdname logo_cv_methods
#' @export
print.logo_cv <- function(x, metric = "auroc", ...) {
  nested <- isTRUE(x$nested)
  cat(
    if (nested) {
      "Nested leave-one-group-out cross-validation\n"
    } else {
      "Leave-one-group-out cross-validation\n"
    }
  )
  cat("  model:   ", x$model, "\n", sep = "")
  cat("  settings:", nrow(x$grid), "\n")
  holdout <- attr(x, "holdout")
  if (!is.null(holdout)) {
    cat("  holdout: ", paste(holdout, collapse = ", "), "\n", sep = "")
  }
  if (nested) {
    cat("  selected on: ", x$criterion, " (inner loop)\n", sep = "")
  }
  cat("  measure: ", metric, "\n\n", sep = "")

  long <- as.data.frame(x)
  rows <- long[long$metric == metric, , drop = FALSE]
  if (nrow(rows) == 0L) {
    cat("no estimates for `", metric, "`\n", sep = "")
    return(invisible(x))
  }

  columns <- c(
    "depth",
    "n_iter",
    "eta",
    "project",
    "n",
    "base_rate",
    "estimate"
  )
  columns <- columns[vapply(
    rows[columns],
    \(column) !all(is.na(column)),
    logical(1)
  )]
  print(format(rows[, columns, drop = FALSE], digits = 3), row.names = FALSE)

  # no mean or SE across folds: shared training data bias the naive SE down
  # (Bengio & Grandvalet, 2004)
  cat("\nEstimates are per project; read the spread, not an average.\n")

  if (nested) {
    # report how often the inner searches disagree: that is the point
    n_distinct <- length(unique(x$selected$setting))
    cat(sprintf(
      "The %d inner searches selected %d distinct setting%s.\n",
      nrow(x$selected),
      n_distinct,
      if (n_distinct == 1L) "" else "s"
    ))
    cat(
      "These are a diagnostic, not a selection rule: each rests on a subset\n"
    )
    cat("of the projects, and no model built here is the one you would ship.\n")
  }
  invisible(x)
}

#' @rdname logo_cv_methods
#' @export
summary.logo_cv <- function(object, metric = "auroc", ...) {
  long <- as.data.frame(object)
  rows <- long[long$metric == metric, , drop = FALSE]
  rows[order(rows$depth, rows$n_iter, rows$eta, rows$project), , drop = FALSE]
}

#' @rdname logo_cv_methods
#' @export
as.data.frame.logo_cv <- function(x, ...) {
  cube <- x$estimates
  dim_names <- dimnames(cube)
  core_columns <- c("depth", "n_iter", "eta")

  # aperm() then as.vector() order rows metric fastest, then project, then
  # setting, matching expand.grid() below
  if (isTRUE(x$nested)) {
    keys <- expand.grid(
      metric = dim_names$metric,
      project = dim_names$project,
      stringsAsFactors = FALSE
    )
    values <- as.vector(aperm(cube, c(2L, 1L)))
    setting <- x$selected$setting[match(keys$project, x$selected$project)]
  } else {
    keys <- expand.grid(
      metric = dim_names$metric,
      project = dim_names$project,
      setting = dim_names$setting,
      stringsAsFactors = FALSE
    )
    values <- as.vector(aperm(cube, c(3L, 2L, 1L)))
    setting <- as.integer(keys$setting)
  }

  settings <- x$grid[setting, , drop = FALSE]
  project_info <- x$projects[
    match(keys$project, x$projects$project),
    ,
    drop = FALSE
  ]
  extra_columns <- setdiff(names(settings), core_columns)

  out <- data.frame(
    model = x$model,
    setting = setting,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  out <- cbind(out, `rownames<-`(settings[, core_columns, drop = FALSE], NULL))
  if (length(extra_columns)) {
    out <- cbind(
      out,
      `rownames<-`(settings[, extra_columns, drop = FALSE], NULL)
    )
  }
  cbind(
    out,
    data.frame(
      project = keys$project,
      n = project_info$n,
      base_rate = project_info$base_rate,
      metric = keys$metric,
      estimate = values,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  )
}

#' @rdname logo_cv_methods
#'
#' @param levels The band widths, widest last. Defaults to 0.50, 0.80 and
#'   0.95.
#' @param baseline The reference line. `"auto"` draws it where a useless model
#'   would score: 0.5 for `auroc` and `bacc`, 0 for `mcc`, the majority-class
#'   rate for `acc`, and each project's base rate for the others. A number
#'   sets it; `NA` leaves it out.
#' @param max_rejected Projects whose bootstrap rejected more than this share
#'   of draws get a star and a footnote. Defaults to 0.05.
#'
#' @export
plot.logo_cv <- function(
  x,
  metric = "auroc",
  levels = c(0.50, 0.80, 0.95),
  baseline = "auto",
  max_rejected = 0.05,
  ...
) {
  old_par <- graphics::par(mar = c(5.2, 5, 4, 2) + 0.1)
  on.exit(graphics::par(old_par), add = TRUE)
  draw_logo_cv(
    x,
    metric = metric,
    levels = levels,
    baseline = baseline,
    max_rejected = max_rejected,
    main = "Leave-one-group-out performance",
    ylab = metric,
    add_footnote = TRUE
  )
  invisible(x)
}

#' Draw the per-project estimates and bands; return what was drawn
#'
#' Shared by plot.logo_cv() and tutplot_logocv(), which differ only in
#' margins, title and footnote.
#' @noRd
draw_logo_cv <- function(
  x,
  metric = "auroc",
  levels = c(0.50, 0.80, 0.95),
  baseline = "auto",
  max_rejected = 0.05,
  main = NULL,
  ylab = metric,
  add_footnote = TRUE
) {
  long <- as.data.frame(x)
  estimates <- long[long$metric == metric, , drop = FALSE]
  if (nrow(estimates) == 0L) {
    stop("no estimates for metric `", metric, "`.", call. = FALSE)
  }
  estimates <- estimates[order(estimates$project), , drop = FALSE]
  n_projects <- nrow(estimates)
  positions <- seq_len(n_projects)
  levels <- sort(levels)

  intervals <- compute_bands(x$ci, estimates$project, metric, levels)
  bands <- intervals$bands
  rejection_rate <- intervals$rejection_rate
  flagged <- rejection_rate > max_rejected
  # the star points at the footnote, so it goes where the footnote goes
  starred <- add_footnote & flagged

  reference <- find_reference(metric, baseline, estimates$base_rate)

  values <- c(estimates$estimate, unlist(bands), reference$values)
  data_range <- range(values, na.rm = TRUE)
  padding <- max(diff(data_range), 0.05) * 0.14
  colors <- viridisLite::viridis(n_projects, end = 0.85)

  ylim <- c(data_range[1] - padding, data_range[2] + padding * 2)
  graphics::plot(
    NULL,
    xlim = c(0.5, n_projects + 0.5),
    ylim = ylim,
    xaxt = "n",
    yaxt = "n",
    xlab = "",
    ylab = ylab,
    main = main
  )
  graphics::axis(
    1,
    at = positions,
    labels = paste0(estimates$project, ifelse(starred, "*", ""))
  )
  # ticks only where the measure can go: the headroom for the n labels is not
  # a value
  bounds <- if (metric == "mcc") c(-1, 1) else c(0, 1)
  # round: pretty() returns 1 as 1.0000000000000002, which the filter drops
  ticks <- round(pretty(ylim), 10)
  graphics::axis(
    2,
    at = ticks[
      ticks >= max(ylim[1], bounds[1]) & ticks <= min(ylim[2], bounds[2])
    ]
  )

  if (any(is.finite(reference$values))) {
    graphics::segments(
      positions - 0.42,
      reference$values,
      positions + 0.42,
      reference$values,
      col = "gray55",
      lty = 2,
      lwd = 2
    )
  }

  widths <- seq(9, 2.2, length.out = length(levels))
  opacities <- seq(1, 0.32, length.out = length(levels))
  for (i in positions) {
    band <- bands[[i]]
    if (is.null(band)) {
      next
    }
    for (k in seq_along(levels)) {
      graphics::segments(
        i,
        band[1, k],
        i,
        band[2, k],
        lwd = widths[k],
        col = grDevices::adjustcolor(colors[i], alpha.f = opacities[k])
      )
    }
  }

  # halo first, so the estimate stays legible against the darkest band
  graphics::points(
    positions,
    estimates$estimate,
    pch = 19,
    cex = 1.6,
    col = "white"
  )
  graphics::points(
    positions,
    estimates$estimate,
    pch = 19,
    cex = 1.15,
    col = colors
  )

  # fold size drives the band widths, so it belongs on the plot
  graphics::text(
    positions,
    data_range[2] + padding * 1.5,
    paste0("n = ", estimates$n),
    cex = 0.75,
    col = "gray30"
  )

  # keys go in the margin rather than over the data
  if (!is.null(x$ci) && any(!vapply(bands, is.null, logical(1)))) {
    graphics::mtext(
      paste0("bands: ", paste0(round(100 * levels), collapse = " / "), "%"),
      side = 3,
      line = 0.25,
      adj = 1,
      cex = 0.72,
      col = "gray35"
    )
  }
  if (nzchar(reference$label)) {
    graphics::mtext(
      reference$label,
      side = 3,
      line = 0.25,
      adj = 0,
      cex = 0.72,
      col = "gray35"
    )
  }
  if (add_footnote && any(flagged)) {
    graphics::mtext(
      paste0(
        "* resamples rejected as single-class: ",
        paste0(
          estimates$project[flagged],
          " ",
          round(100 * rejection_rate[flagged]),
          "%",
          collapse = ",  "
        ),
        " -- those intervals are conditioned and read too narrow"
      ),
      side = 1,
      line = 3.6,
      adj = 0,
      cex = 0.62,
      col = "gray35"
    )
  }

  read_widest <- function(i, bound) {
    if (is.null(bands[[i]])) NA_real_ else bands[[i]][bound, length(levels)]
  }
  data.frame(
    project = estimates$project,
    n = estimates$n,
    estimate = estimates$estimate,
    lower = vapply(positions, read_widest, numeric(1), bound = 1L),
    upper = vapply(positions, read_widest, numeric(1), bound = 2L),
    rejected = rejection_rate,
    flagged = flagged,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' Compute each project's bands from its bootstrap draws
#'
#' Also returns the share of resamples each bootstrap rejected.
#' @noRd
compute_bands <- function(ci, projects, metric, levels) {
  bands <- vector("list", length(projects))
  rejection_rate <- rep(0, length(projects))
  for (i in seq_along(projects)) {
    project_ci <- ci[[projects[i]]]
    draws <- attr(project_ci, "draws")
    if (is.null(draws) || !metric %in% colnames(draws)) {
      next
    }
    # from the draws, so any level can be asked for later
    bands[[i]] <- vapply(
      levels,
      function(level) {
        tail_prob <- (1 - level) / 2
        stats::quantile(
          draws[, metric],
          c(tail_prob, 1 - tail_prob),
          na.rm = TRUE
        )
      },
      numeric(2)
    )
    n_dropped <- project_ci$drop[1]
    rejection_rate[i] <- n_dropped /
      (n_dropped + attr(project_ci, "n_resample"))
  }
  list(bands = bands, rejection_rate = rejection_rate)
}

#' Find the reference line and its key
#' @noRd
find_reference <- function(metric, baseline, base_rate) {
  n_projects <- length(base_rate)
  if (!identical(baseline, "auto")) {
    if (length(baseline) == 1L && is.na(baseline)) {
      return(list(values = rep(NA_real_, n_projects), label = ""))
    }
    return(list(
      values = rep(as.numeric(baseline), n_projects),
      label = paste0("dashed line: ", signif(baseline, 3))
    ))
  }
  # what a useless model scores: 0.5 for auroc and bacc, 0 for mcc, always
  # predicting the larger class for acc, the base rate for the precision
  # measures (auprc, ppv, patk) and f1
  if (metric %in% c("auroc", "bacc")) {
    list(values = rep(0.5, n_projects), label = "dashed line: chance (0.5)")
  } else if (metric == "mcc") {
    list(values = rep(0, n_projects), label = "dashed line: no association (0)")
  } else if (metric == "acc") {
    list(
      values = pmax(base_rate, 1 - base_rate),
      label = "dashed line: majority-class rate"
    )
  } else if (
    metric %in% c("auprc", "ppv", "f1") || grepl("^patk(_|$)", metric)
  ) {
    list(values = base_rate, label = "dashed line: base rate of each project")
  } else {
    list(values = rep(NA_real_, n_projects), label = "")
  }
}
