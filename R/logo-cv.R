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
#' evaluates the procedure, not one fitted model. To compare settings of a
#' model, pass its fit and a grid to [tune_logo_cv()].
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
#' * An XGBoost fit from [xgboost::xgboost()], version 3.0 or later, made with
#'   a data frame of predictors and a factor outcome: its stored parameters,
#'   its number of rounds and the names of its predictors. Each refit runs
#'   `xgboost()` afresh with those parameters on the training rows, takes the
#'   outcome from the column named by `outcome`, and is scored on its
#'   predicted probability of the second level. The stored parameters do not
#'   keep `weights`, `base_margin`, `eval_set`, `early_stopping_rounds` or
#'   `feature_weights`, so a fit made with any of them is an error. Fit it
#'   with `nthreads = 1`: each refit runs on as many threads as the fit, and
#'   [tune_logo_cv()] can run several refits at once.
#'
#' Every model is scored on a continuous score, never on class labels.
#'
#' For a tree, a glm or a forest, the call runs where its formula was made,
#' so any other object it names, such as `weights = w`, must still exist
#' there. A variable named as a column of `data`, such as `weights = n.o`,
#' travels with the rows. A random forest draws at random: set a seed to
#' repeat a result.
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
#'   from [rpart::rpart()], a binomial [stats::glm()], a classification forest
#'   from [randomForest::randomForest()] fitted with a formula, or an XGBoost
#'   fit from [xgboost::xgboost()].
#' @param data A data frame with the model's variables and the `group`
#'   column, holding every group.
#' @param group The name of the column that defines the folds, e.g. `"pid"`.
#' @param holdout The levels of `group` to hold out, one fold each. Defaults
#'   to `NULL`, which holds out every level once.
#' @param verbose Whether to report progress. Defaults to `FALSE`.
#' @param outcome The name of the outcome column in `data`, e.g.
#'   `"replicate"`, for an XGBoost fit, which needs it. Such a fit keeps no
#'   formula, so its predictors are the columns named by its feature names.
#' @param ... Must be empty. The settings belong to the fit, so an argument
#'   such as `n_iter` or `grid` is an error.
#'
#' @return An object of class `logo_cv`.
#'   * `estimates`: an array of setting x project x metric, e.g.
#'     `res$estimates[, , "auroc"]`. `as.data.frame()` gives the same values
#'     as one long table. An `NA` cell is an undefined metric, not a failed
#'     fit.
#'   * `grid`: the settings, one row per setting. `depth`, `n_iter` and `eta`
#'     are `NA` where the learner has no such setting.
#'   * `projects`: each fold's `n` and `base_rate`.
#'   * `scores`: the held-out scores, one row per study and setting, which
#'     [bootstrap()] resamples.
#'   * `model`: the learner, `"adaboost"`, `"rpart"`, `"glm"`, `"rf"` or
#'     `"xgboost"`.
#'   * `nested`, `metric`, `sort_by` and `selected`: set by [tune_logo_cv()]
#'     when it nests the search.
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
  check_tree_fit(x, "logo_cv()")
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
  check_forest_fit(x, "logo_cv()")
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
logo_cv.xgboost <- function(
  x,
  data,
  group,
  holdout = NULL,
  verbose = FALSE,
  outcome,
  ...
) {
  check_dots_empty(...)
  check_xgb_fit(x, "logo_cv()")
  folds <- read_folds(data, group, holdout)
  check_xgb_data(x, data, outcome)
  cross_validate(
    x,
    data = data,
    group = group,
    grid = read_xgb_setting(x),
    folds = folds,
    model = "xgboost",
    holdout = holdout,
    verbose = verbose,
    outcome = outcome
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
  check_not_booster(x)
  stop(
    "`x` must be a fitted model: a fit from adaboost(), rpart(), glm(), ",
    "randomForest() or xgboost(), not an object of class `",
    class(x)[1L],
    "`. Fit the model first, then pass the fit; to search a grid of ",
    "settings, use tune_logo_cv().",
    call. = FALSE
  )
}

#' Group the grid rows that one fit scores: those that differ only in
#' `n_iter`
#' @noRd
group_settings <- function(grid, model) {
  rows <- seq_len(nrow(grid))
  # only boosted models have rounds to read off a longer fit
  if (!model %in% c("adaboost", "xgboost")) {
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
#'
#' `globals` is evaluated only if run_tasks() starts a socket cluster.
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
  verbose = FALSE,
  outcome = all.vars(read_formula(fit))[1L],
  seeds = NULL,
  globals = list_globals(fit, data)
) {
  # a fit without a formula passes the name of its outcome column
  force(outcome)
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
    # the fold's seed, the same in any task and on any core
    if (!is.null(seeds)) {
      set.seed(seeds[tasks$fold[k]])
    }
    score <- refit_score(
      fit,
      train = data[!in_fold, , drop = FALSE],
      test = data[in_fold, , drop = FALSE],
      setting = grid[rows, , drop = FALSE],
      outcome = outcome
    )
    # the other learners score one setting, as a vector
    matrix(score, ncol = length(rows))
  }
  results <- run_tasks(nrow(tasks), run_task, cores, globals = globals)

  # where each setting's scores sit: the task of its group on a fold, and the
  # column within that task
  task_of <- matrix(seq_len(nrow(tasks)), nrow = length(folds))
  group_of <- rep(seq_along(groups), lengths(groups))[order(unlist(groups))]
  column_of <- sequence(lengths(groups))[order(unlist(groups))]

  # allocate the setting x project x metric cube up front
  metric_names <- list_metrics()
  estimates <- array(
    NA_real_,
    dim = c(n_settings, length(folds), length(metric_names)),
    dimnames = list(
      setting = as.character(seq_len(n_settings)),
      project = folds,
      metric = metric_names
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
      metrics <- assess(
        actuals[[j]],
        score,
        threshold = choose_threshold(model)
      )
      estimates[i, j, names(metrics)] <- unname(metrics)
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
      metric = NA_character_,
      sort_by = NA_character_,
      selected = NULL
    ),
    class = "logo_cv",
    # an attribute, not an element, so the elements stay as bootstrap() and
    # the plots read them
    holdout = if (!is.null(holdout)) folds
  )
}

#' List the metrics assess() returns
#' @noRd
list_metrics <- function() {
  names(assess(c(1, 0), c(1, -1)))
}

#' Choose the classification threshold for a learner's score
#' @noRd
choose_threshold <- function(model) {
  # 0 for the AdaBoost margin, 0.5 for probabilities
  if (model == "adaboost") 0 else 0.5
}
