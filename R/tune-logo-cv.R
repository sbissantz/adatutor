#' Tune a model's settings by leave-one-group-out cross-validation
#'
#' Cross-validates a fitted model at every row of a grid of settings, with
#' the folds of [logo_cv()]: each level of `group` is held out in turn, or
#' only the levels in `holdout`. Each row changes the recipe read off the
#' fit, so the result has one estimate per setting and group.
#'
#' @section Grid columns:
#' The columns a grid may hold depend on the fit. A column the grid lacks
#' takes the fit's own value, and a column not listed for the fit is an
#' error.
#' * An [adaboost()] fit: `depth`, `n_iter` and `eta` set the depth of the
#'   boosted tree, the number of rounds and the learning rate. A column named
#'   after an [rpart::rpart.control()] setting, such as `cp` or `minbucket`,
#'   sets that control for its row. So `data.frame(n_iter = c(10, 100))`
#'   varies the rounds and keeps the fit's depth and rate.
#' * A classification tree from [rpart::rpart()]: `depth` sets the tree's
#'   `maxdepth`, and the same rpart controls set theirs.
#' * A classification forest from [randomForest::randomForest()]: `mtry`,
#'   `nodesize`, `ntree` and `maxnodes` set those arguments. A forest stores
#'   only its `mtry` and `ntree`, so a grid without `nodesize` or `maxnodes`
#'   leaves them as the fit's call set them.
#' * An XGBoost fit from [xgboost::xgboost()]: `depth`, `eta` and `n_iter` set
#'   `max_depth`, `learning_rate` and `nrounds`, and `min_child_weight` and
#'   `lambda` set `min_child_weight` and `reg_lambda`. Where the fit left a
#'   setting at its default, the grid takes xgboost's default. Fit it with
#'   `nthreads = 1`: each refit runs on as many threads as the fit, and
#'   `cores` above 1 runs several refits at once.
#'
#' @section Nested cross-validation:
#' Picking the best row of a grid and reporting its score is optimistic,
#' because the same folds chose the winner and graded it. With
#' `nested = TRUE`, each held-out group takes no part in the choice: a search
#' on the other groups picks the row that
#' `summary(inner, metric = metric, sort_by = sort_by)` puts first, and only
#' that row is scored on the held-out group. By default, that is the row with
#' the highest mean AUROC minus its SD. The result estimates the procedure
#' "search the grid, then fit the winner", not any one setting. It is slightly
#' pessimistic, because each model trains on one group fewer. Expect the outer
#' folds to choose different settings. Nesting is currently only supported
#' with fits from [adaboost()] and [xgboost::xgboost()].
#'
#' A row whose statistics are `NA`, because `metric` is undefined on one of
#' the inner projects, cannot be picked; if no row has a value, the search
#' stops with an error.
#'
#' @section Random forests:
#' A forest draws at random. Before it refits, the tuner draws one seed per
#' fold from the session's random-number stream, and it sets a fold's seed
#' before each refit on that fold. So the result repeats after [set.seed()],
#' and it is the same at any number of cores. Since [logo_cv()] draws a
#' forest's refits from the session's stream itself, a forest's result from
#' the tuner differs from its result from `logo_cv()` after the same seed,
#' even for the fit's own setting.
#'
#' @section Speed:
#' Rows of `grid` that differ only in `n_iter` share one fit per fold. The
#' model is boosted once, to the largest `n_iter` among them, and each row is
#' scored after its own number of rounds: AdaBoost on the margin, XGBoost on
#' the probability. The first rounds of a longer run are the shorter run, so
#' the result equals refitting each row. A grid of many round counts therefore
#' costs about as much as its largest one. Each distinct combination of the
#' other columns costs one fit per fold, and so does every row of a tree or a
#' forest.
#'
#' @param x A fitted model: a fit from [adaboost()], a classification tree
#'   from [rpart::rpart()], a classification forest from
#'   [randomForest::randomForest()] fitted with a formula, or an XGBoost fit
#'   from [xgboost::xgboost()]. The grid varies its settings; everything else
#'   the fit was made with stays.
#' @param grid A data frame of settings, one row per setting. See the section
#'   on grid columns.
#' @inheritParams logo_cv
#' @param nested Whether to nest the search, which is currently only supported
#'   with fits from [adaboost()] and [xgboost::xgboost()]. The default,
#'   `FALSE`, scores every row of `grid` on every fold. `TRUE` needs two or
#'   more rows and at least three groups.
#' @param metric The performance metric the inner search ranks the settings
#'   by when `nested = TRUE`, one of the metrics [assess()] returns. Defaults
#'   to `"auroc"`. Fix it in advance: it decides which setting is chosen. It is
#'   not used when `nested = FALSE`.
#' @param sort_by The statistics of the summary the inner search ranks the
#'   settings by when `nested = TRUE`: one or more of `"mean"`, `"sd"`,
#'   `"lower"`, `"min"` and `"max"`, each once. Each outer fold picks the
#'   setting that `summary()` of its inner search puts first when sorted by
#'   `sort_by`: highest first, later statistics break ties, and settings that
#'   tie on all of them go in grid order. Defaults to `"lower"`, the mean
#'   minus the SD; `"mean"` picks the highest mean. It is not used when
#'   `nested = FALSE`.
#' @param cores The number of cores to use. Defaults to 1. A number above
#'   what the computer has is lowered to that, with a message. On macOS and
#'   Linux, the work is spread over that many forked R processes with
#'   [parallel::mclapply()]. Windows cannot fork, so there it goes to that
#'   many new R processes from [parallel::makePSOCKcluster()], which first
#'   load adatutor and the packages the fit needs. The results are the same
#'   at any number of cores. On Windows, these processes are new R sessions:
#'   they get the plain values the fit reads, such as numbers and data
#'   frames, but not the functions defined in the session. So a fit whose
#'   formula or call uses such a function, even by its name in a string, or
#'   an object that holds code, such as a fitted model, stops with a
#'   message; then set `cores = 1`, or compute the column first and use it
#'   in the formula.
#' @param verbose Whether to report progress on the console. Defaults to
#'   `TRUE`, which prints a start line that counts the settings, the folds
#'   and the cores in use if more than one, then a progress bar over the fits
#'   and a closing line. On several cores, the workers hand back their fits
#'   in batches of 16 per core, and the bar moves after each batch; a search
#'   that fits in one batch jumps from start to end. A nested search prints a
#'   line per outer fold instead of the bar, which ends in "Done" when that
#'   fold's search and refit have finished. `FALSE` prints nothing, for
#'   loops, reports and scripts.
#' @param ... Must be empty. An argument the method does not take is an
#'   error, not ignored.
#'
#' @return An object of class `logo_cv`, as from [logo_cv()], with one
#'   setting per row of `grid`. When nested, `estimates` is project x metric,
#'   and `selected` gives the setting each outer fold chose. Its column
#'   `inner_value` is the chosen setting's value of the first `sort_by`
#'   statistic in the inner search.
#'
#' @family cross-validation
#' @seealso [hypergrid], a grid of 1,320 settings already scored on the six
#'   tuning projects of [altmext]. [nested], the nested search of the same grid
#'   on all seven projects.
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
#'   group = "pid",
#'   verbose = FALSE
#' )
#' # the settings, ranked by the mean across the projects minus their SD
#' summary(res, sort_by = "lower")
#'
#' # nested: each held-out project takes no part in the choice, and each
#' # inner search picks the setting with the highest mean minus SD
#' grid <- expand.grid(depth = 1:2, n_iter = c(5, 10))
#' tune_logo_cv(
#'   fit,
#'   grid,
#'   data = altmejd,
#'   group = "pid",
#'   nested = TRUE,
#'   sort_by = "lower",
#'   verbose = FALSE
#' )
#'
#' # a classification tree: the depth varies
#' tree <- rpart::rpart(
#'   replicate ~ power.o + effect_size.o + n.o + p_value.o,
#'   data = altmejd,
#'   method = "class"
#' )
#' tune_logo_cv(
#'   tree,
#'   data.frame(depth = c(1, 3, 5)),
#'   data = altmejd,
#'   group = "pid",
#'   verbose = FALSE
#' )
#'
#' @export
tune_logo_cv <- function(
  x,
  grid,
  data,
  group,
  holdout = NULL,
  nested = FALSE,
  metric = "auroc",
  sort_by = "lower",
  cores = 1,
  verbose = TRUE,
  ...
) {
  UseMethod("tune_logo_cv")
}

#' @rdname tune_logo_cv
#' @export
tune_logo_cv.adaboost <- function(
  x,
  grid,
  data,
  group,
  holdout = NULL,
  nested = FALSE,
  metric = "auroc",
  sort_by = "lower",
  cores = 1,
  verbose = TRUE,
  ...
) {
  check_dots_empty(...)
  check_ada_fit(x)
  check_nested(nested)
  grid <- normalize_grid(
    grid,
    own = read_setting(x),
    settings = c("depth", "n_iter", "eta"),
    controls = list_tree_controls()
  )
  folds <- read_folds(data, group, holdout)
  cores <- choose_cores(cores)

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

  check_nested_search(grid, data, group, metric, sort_by)
  cross_validate_nested(
    x,
    data = data,
    group = group,
    grid = grid,
    folds = folds,
    model = "adaboost",
    metric = metric,
    sort_by = sort_by,
    holdout = holdout,
    cores = cores,
    verbose = verbose
  )
}

#' @rdname tune_logo_cv
#' @export
tune_logo_cv.rpart <- function(
  x,
  grid,
  data,
  group,
  holdout = NULL,
  nested = FALSE,
  metric = "auroc",
  sort_by = "lower",
  cores = 1,
  verbose = TRUE,
  ...
) {
  check_dots_empty(...)
  check_tree_fit(x, "tune_logo_cv()")
  check_not_nested(nested)
  grid <- normalize_grid(
    grid,
    own = data.frame(
      depth = x$control$maxdepth,
      n_iter = NA_real_,
      eta = NA_real_
    ),
    settings = "depth",
    controls = list_tree_controls()
  )
  folds <- read_folds(data, group, holdout)
  cores <- choose_cores(cores)
  cross_validate(
    x,
    data = data,
    group = group,
    grid = grid,
    folds = folds,
    model = "rpart",
    holdout = holdout,
    cores = cores,
    verbose = verbose
  )
}

#' @rdname tune_logo_cv
#' @export
tune_logo_cv.randomForest <- function(
  x,
  grid,
  data,
  group,
  holdout = NULL,
  nested = FALSE,
  metric = "auroc",
  sort_by = "lower",
  cores = 1,
  verbose = TRUE,
  ...
) {
  check_dots_empty(...)
  check_forest_fit(x, "tune_logo_cv()")
  check_not_nested(nested)
  grid <- normalize_grid(
    grid,
    own = data.frame(
      depth = NA_real_,
      n_iter = NA_real_,
      eta = NA_real_,
      mtry = x$mtry,
      ntree = x$ntree
    ),
    settings = list_forest_settings()
  )
  folds <- read_folds(data, group, holdout)
  cores <- choose_cores(cores)

  # one seed per fold, drawn before any refit: a row's forests depend on the
  # session's seed, the fold and the row, not on grid order, size or `cores`
  seeds <- sample.int(.Machine$integer.max, length(folds))
  # set.seed() on one core would move the session's stream and forked
  # workers would not, so put it back where the draw left it
  stream <- get(".Random.seed", envir = globalenv())
  on.exit(assign(".Random.seed", stream, envir = globalenv()), add = TRUE)

  cross_validate(
    x,
    data = data,
    group = group,
    grid = grid,
    folds = folds,
    model = "rf",
    holdout = holdout,
    cores = cores,
    verbose = verbose,
    seeds = seeds
  )
}

#' @rdname tune_logo_cv
#' @export
tune_logo_cv.xgboost <- function(
  x,
  grid,
  data,
  group,
  holdout = NULL,
  nested = FALSE,
  metric = "auroc",
  sort_by = "lower",
  cores = 1,
  verbose = TRUE,
  outcome,
  ...
) {
  check_dots_empty(...)
  check_xgb_fit(x, "tune_logo_cv()")
  check_nested(nested)
  folds <- read_folds(data, group, holdout)
  check_xgb_data(x, data, outcome)
  grid <- normalize_grid(
    grid,
    own = read_xgb_setting(x),
    settings = names(list_xgb_settings())
  )
  cores <- choose_cores(cores)

  if (!nested) {
    return(cross_validate(
      x,
      data = data,
      group = group,
      grid = grid,
      folds = folds,
      model = "xgboost",
      holdout = holdout,
      cores = cores,
      verbose = verbose,
      outcome = outcome
    ))
  }

  check_nested_search(grid, data, group, metric, sort_by)
  cross_validate_nested(
    x,
    data = data,
    group = group,
    grid = grid,
    folds = folds,
    model = "xgboost",
    metric = metric,
    sort_by = sort_by,
    holdout = holdout,
    cores = cores,
    verbose = verbose,
    outcome = outcome
  )
}

#' @rdname tune_logo_cv
#' @export
tune_logo_cv.default <- function(
  x,
  grid,
  data,
  group,
  holdout = NULL,
  nested = FALSE,
  metric = "auroc",
  sort_by = "lower",
  cores = 1,
  verbose = TRUE,
  ...
) {
  if (inherits(x, "glm")) {
    stop(
      "`x` is a glm fit, which has no settings to tune. ",
      "Cross-validate it with logo_cv().",
      call. = FALSE
    )
  }
  check_not_booster(x)
  check_not_nested(nested)
  stop(
    "`x` must be a fit from adaboost(), rpart(), randomForest() or xgboost(), ",
    "not an object of class `",
    class(x)[1L],
    "`. Fit the model first, then pass the fit and a grid of its settings.",
    call. = FALSE
  )
}

#' Stop unless `nested` is one `TRUE` or `FALSE`
#' @noRd
check_nested <- function(nested) {
  if (!isTRUE(nested) && !isFALSE(nested)) {
    stop("`nested` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
}

#' Stop when a search that cannot be nested is asked to nest
#' @noRd
check_not_nested <- function(nested) {
  check_nested(nested)
  if (nested) {
    stop(
      "`nested = TRUE` is currently only supported with fits from adaboost() ",
      "and xgboost().",
      call. = FALSE
    )
  }
}

#' Stop unless a nested search has the grid, groups, `metric` and `sort_by`
#' it needs; called before any fit
#' @noRd
check_nested_search <- function(grid, data, group, metric, sort_by) {
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
  metrics <- list_metrics()
  if (length(metric) != 1L || !metric %in% metrics) {
    stop(
      "`metric` must be one of the metrics assess() returns: ",
      paste(metrics, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  # not left to summary(), which also sorts by grid columns and would stop
  # only after the first inner search
  statistics <- list_statistics()
  if (
    !is.character(sort_by) ||
      length(sort_by) == 0L ||
      anyNA(sort_by) ||
      anyDuplicated(sort_by) > 0L ||
      !all(sort_by %in% statistics)
  ) {
    stop(
      "`sort_by` must name one or more of the summary statistics, each once: ",
      paste(statistics, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
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

#' Read an XGBoost fit's own setting as a one-row grid
#' @noRd
read_xgb_setting <- function(fit) {
  params <- attr(fit, "params")
  # a setting left at its default is not among the stored parameters; the
  # booster's config holds it as float32 text, such as "0.300000012"
  config <- xgboost::xgb.config(fit)$learner$gradient_booster$tree_train_param
  read_value <- function(name) {
    params[[name]] %||% signif(as.numeric(config[[name]]), 7)
  }
  data.frame(
    depth = read_value("max_depth"),
    n_iter = xgboost::xgb.get.num.boosted.rounds(fit),
    eta = read_value("learning_rate"),
    min_child_weight = read_value("min_child_weight"),
    lambda = read_value("reg_lambda")
  )
}

#' Run the outer loop of nested LOGO-CV, with one inner search per outer fold
#'
#' `outcome` names the outcome column; a fit with a formula gives it.
#' `globals` is evaluated only if an inner search starts a socket cluster,
#' and then once for all of them.
#' @noRd
cross_validate_nested <- function(
  fit,
  data,
  group,
  grid,
  folds,
  model,
  metric,
  sort_by,
  holdout = NULL,
  cores = 1L,
  verbose = FALSE,
  outcome = all.vars(read_formula(fit))[1L],
  globals = list_globals(fit, data)
) {
  force(outcome)
  # a factor would print its code in the header
  metric <- as.character(metric)
  metric_names <- list_metrics()

  # no setting dimension: each outer fold selects its own (see `selected`)
  estimates <- array(
    NA_real_,
    dim = c(length(folds), length(metric_names)),
    dimnames = list(project = folds, metric = metric_names)
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

  # one line per outer fold, printed here: the outer folds run one after
  # another, and only the inner fits go to the cores. On exit, an error
  # closes the open line.
  finished <- TRUE
  if (verbose) {
    on.exit(close_progress(NULL, finished), add = TRUE)
    # each inner search holds out each other group once, and run_tasks()
    # starts no more workers than tasks
    n_inner <- length(unique(as.character(data[[group]]))) - 1L
    n_cores <- min(cores, n_inner * length(group_settings(grid, model)))
    message(
      "Start the nested search: ",
      count_noun(length(folds), "outer fold"),
      ", ",
      count_noun(nrow(grid), "setting"),
      " each",
      describe_cores(n_cores)
    )
  }

  for (k in seq_along(folds)) {
    fold <- folds[k]
    if (verbose) {
      color_message(
        paste0("Outer fold ", k, "/", length(folds), ", test: ", fold),
        color_code = ansi_gray
      )
      finished <- FALSE
    }

    # drop the held-out project entirely, which keeps the selection honest
    held_out <- as.character(data[[group]]) == fold
    train <- data[!held_out, , drop = FALSE]
    test <- data[held_out, , drop = FALSE]

    # what tune_logo_cv() runs on `train`; the checks and `cores` are done.
    # Quiet: the outer fold's line stands for it, and a quiet search sends
    # its fits to the cores in one batch.
    inner_cv <- cross_validate(
      fit,
      data = train,
      group = group,
      grid = grid,
      folds = read_folds(train, group),
      model = model,
      cores = cores,
      verbose = FALSE,
      outcome = outcome,
      globals = globals
    )
    pick <- pick_setting(inner_cv, metric, sort_by, fold)

    score <- refit_score(
      fit,
      train = train,
      test = test,
      setting = grid[pick$setting, , drop = FALSE],
      outcome = outcome
    )[, 1L]

    actual <- test[[outcome]]
    metrics <- assess(actual, score, threshold = choose_threshold(model))
    if (verbose) {
      mark_done()
      finished <- TRUE
    }

    # keep the whole grid row, so a control tuned in the grid travels along;
    # depth, n_iter and eta keep fixed positions
    choice <- grid[pick$setting, , drop = FALSE]
    choice <- choice[,
      union(c("depth", "n_iter", "eta"), names(choice)),
      drop = FALSE
    ]
    selected[[k]] <- data.frame(
      project = fold,
      setting = pick$setting,
      `rownames<-`(choice, NULL),
      inner_value = pick$value,
      row.names = NULL,
      stringsAsFactors = FALSE
    )

    estimates[fold, names(metrics)] <- unname(metrics)
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
  if (verbose) {
    message("Nested search completed.")
  }

  structure(
    list(
      estimates = estimates,
      grid = grid,
      projects = projects,
      scores = do.call(rbind, scores),
      model = model,
      nested = TRUE,
      metric = metric,
      sort_by = sort_by,
      selected = do.call(rbind, selected)
    ),
    class = "logo_cv",
    holdout = if (!is.null(holdout)) folds
  )
}

#' Pick the setting an inner search puts first, as summary() sorts it
#'
#' Returns the setting's row number in the grid and its value of the first
#' `sort_by` statistic. `fold` names the outer fold for the error message.
#' @noRd
pick_setting <- function(inner, metric, sort_by, fold) {
  # these statistics rank settings; they are not estimates (see ?tune_logo_cv)
  ranked <- summary(inner, metric = metric, sort_by = sort_by)
  value <- ranked[[sort_by[1L]]][1L]
  # summary() sorts NA last, so an NA on top means no setting has a value
  if (is.na(value)) {
    stop(
      "No setting can be picked in the inner search without `",
      fold,
      "`: every setting's `",
      sort_by[1L],
      "` of `",
      metric,
      "` is NA, because `",
      metric,
      "` is undefined on at least one of its projects. Choose another ",
      "`metric`.",
      call. = FALSE
    )
  }
  list(setting = as.integer(rownames(ranked)[1L]), value = value)
}

#' List the summary statistics a nested search may sort by
#' @noRd
list_statistics <- function() {
  c("mean", "sd", "lower", "min", "max")
}

#' List the tree controls a grid row may set
#' @noRd
list_tree_controls <- function() {
  # maxdepth is excluded: depth is tuned
  setdiff(names(formals(rpart::rpart.control)), c("maxdepth", "..."))
}

#' List the forest arguments a grid row may set
#' @noRd
list_forest_settings <- function() {
  c("mtry", "nodesize", "ntree", "maxnodes")
}

#' List the grid columns of an XGBoost fit, each named by its xgboost()
#' argument
#' @noRd
list_xgb_settings <- function() {
  c(
    depth = "max_depth",
    n_iter = "nrounds",
    eta = "learning_rate",
    min_child_weight = "min_child_weight",
    lambda = "reg_lambda"
  )
}

#' Check the grid's columns and fill in missing ones from the fit
#'
#' `own` is the fit's own setting as a one-row grid, `settings` the columns a
#' grid may hold for the model, and `controls` the rpart controls it may set.
#' @noRd
normalize_grid <- function(grid, own, settings, controls = character()) {
  grid <- as.data.frame(grid)
  if (nrow(grid) == 0L) {
    stop("`grid` has no rows.", call. = FALSE)
  }
  # an error, not a warning: a grid that also holds results would otherwise
  # hide a column meant to tune something
  unknown <- setdiff(names(grid), c(settings, controls))
  if (length(unknown)) {
    allowed <- paste0("`", settings, "`")
    if (length(controls)) {
      allowed <- c(
        allowed,
        paste0("the rpart controls (", paste(controls, collapse = ", "), ")")
      )
    }
    stop(
      "grid column(s) ",
      paste(unknown, collapse = ", "),
      " set nothing. A grid may hold ",
      paste(allowed[-length(allowed)], collapse = ", "),
      " and ",
      allowed[length(allowed)],
      ".",
      call. = FALSE
    )
  }
  for (column in setdiff(names(own), names(grid))) {
    grid[[column]] <- own[[column]]
  }
  if ("n_iter" %in% settings) {
    # rows share a fit, so a row without rounds would score 0 rather than fail
    n_iter <- grid$n_iter
    if (!is.numeric(n_iter) || anyNA(n_iter) || any(n_iter < 1)) {
      stop("grid column `n_iter` must be numbers of 1 or more.", call. = FALSE)
    }
  }
  grid
}
