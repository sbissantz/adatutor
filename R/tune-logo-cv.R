#' Tune a model's settings by leave-one-group-out cross-validation
#'
#' Cross-validates a fitted model at every row of a grid of settings, with
#' the folds of [logo_cv()]. The result has one estimate per setting and
#' group.
#'
#' @section Grid columns:
#' A column the grid lacks takes the fit's own value, and a column not listed
#' for the fit is an error.
#' * A fit from [adaboost()]: `depth`, `n_iter` and `eta` set the depth of the
#'   boosted tree, the number of rounds and the learning rate. A column named
#'   after an [rpart::rpart.control()] setting, such as `cp` or `minbucket`,
#'   sets that control for its row, except `xval` and `maxsurrogate`, which
#'   stay 0.
#' * A classification tree from [rpart::rpart()]: `depth` sets the tree's
#'   `maxdepth`, and the same rpart controls set theirs, including `xval` and
#'   `maxsurrogate`.
#' * A classification forest from [randomForest::randomForest()]: `mtry`,
#'   `nodesize`, `ntree` and `maxnodes` set those arguments. Without
#'   `nodesize` or `maxnodes`, the grid leaves them as the fit's call set
#'   them.
#' * An XGBoost fit from [xgboost::xgboost()]: `depth`, `eta` and `n_iter` set
#'   `max_depth`, `learning_rate` and `nrounds`, and `min_child_weight` and
#'   `lambda` set `min_child_weight` and `reg_lambda`. With `cores` above 1,
#'   fit it with `nthreads = 1`: each refit runs on as many threads as the
#'   fit.
#'
#' @section Nested cross-validation:
#' With `nested = TRUE`, each held-out group takes no part in the choice of
#' setting. A search on the other groups ranks the rows as `summary()` does
#' with `metric` and `sort_by`, and only the first row is scored on the
#' held-out group. A row whose statistics are `NA` cannot be chosen; if no
#' row has a value, the search stops with an error.
#'
#' @param x A fitted model: a fit from [adaboost()], a classification tree
#'   from [rpart::rpart()], a classification forest from
#'   [randomForest::randomForest()] fitted with a formula, or an XGBoost fit
#'   from [xgboost::xgboost()].
#' @param grid A data frame of settings, one row per setting. See the Grid
#'   columns section.
#' @inheritParams logo_cv
#' @param nested Whether to nest the search. Supported only for fits from
#'   [adaboost()] and [xgboost::xgboost()]. `TRUE` needs two or more rows in
#'   `grid` and at least three groups.
#' @param metric The metric the inner search ranks the settings by when
#'   `nested = TRUE`, one of the names [assess()] returns.
#' @param sort_by The statistics the inner search ranks the settings by when
#'   `nested = TRUE`: one or more of `"mean"`, `"sd"`, `"lower"`, `"min"` and
#'   `"max"`, each once, where `lower` is
#'   \eqn{\mathrm{mean} - \mathrm{sd}}{mean - sd}. Settings go highest first,
#'   later statistics break ties, and settings that tie on all of them go in
#'   grid order.
#' @param cores The number of cores to use. A number above what the computer
#'   has is lowered to that, with a message. The work runs through
#'   [parallel::mclapply()], or on Windows [parallel::makePSOCKcluster()]. The
#'   results are the same at any number of cores. On Windows with `cores`
#'   above 1, a fit whose formula or call uses code from the session, such as
#'   a function defined there, stops with a message; then set `cores = 1`.
#'
#' @return An object of class `logo_cv`, as from [logo_cv()], with one
#'   setting per row of `grid`. When nested, `estimates` is a matrix with one
#'   row per project and one column per metric, and `selected` gives the
#'   setting each outer fold chose. Its column `inner_value` is the chosen
#'   setting's value of the first `sort_by` statistic in the inner search.
#'
#' @family cross-validation
#' @seealso
#' * [hypergrid], an AdaBoost grid of 1,320 settings scored on six projects
#'   of [altmext].
#' * [nested], the nested search of that grid.
#' * [set.seed()], to repeat a forest's result.
#'
#' @examples
#' data(altmext)
#' # the six tuning projects; HOLZ is kept back for a final test
#' df_noholz <- altmext[altmext$pid != "holz", ]
#' fit <- rpart::rpart(
#'   replicate ~ power.o + effect_size.o + n.o + p_value.o,
#'   data = df_noholz,
#'   maxdepth = 1,
#'   model = TRUE
#' ) |>
#'   adaboost(n_iter = 10, eta = 1, verbose = FALSE)
#'
#' # the rounds vary; depth and rate stay the fit's
#' res <- tune_logo_cv(
#'   fit,
#'   grid = data.frame(n_iter = c(5, 10)),
#'   data = df_noholz,
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
#'   data = df_noholz,
#'   group = "pid",
#'   nested = TRUE,
#'   sort_by = "lower",
#'   verbose = FALSE
#' )
#'
#' # a classification tree: the depth varies
#' tree <- rpart::rpart(
#'   replicate ~ power.o + effect_size.o + n.o + p_value.o,
#'   data = df_noholz,
#'   method = "class"
#' )
#' tune_logo_cv(
#'   tree,
#'   data.frame(depth = c(1, 3, 5)),
#'   data = df_noholz,
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

#' Stop unless a nested search has what it needs, before any fit
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
#' @param outcome The name of the outcome column; a fit with a formula gives
#'   it.
#' @param globals Evaluated only if an inner search starts a socket cluster,
#'   and then once for all of them.
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

  # one line per outer fold, printed here: only the inner fits go to the
  # cores. On exit, an error closes the open line.
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

    # what tune_logo_cv() runs on `train`, checks done; quiet, since the outer
    # fold's line stands for it and a quiet search sends its fits in one batch
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
#' @param fold The outer fold, named in the error message.
#' @return A list of `setting`, its row number in the grid, and `value`, its
#'   value of the first `sort_by` statistic.
#' @noRd
pick_setting <- function(inner, metric, sort_by, fold) {
  # these statistics rank settings; they are not estimates of performance
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

#' List the xgboost() argument of each grid column of an XGBoost fit
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
#' @param own The fit's own setting as a one-row grid.
#' @param settings The columns a grid may hold for the model.
#' @param controls The rpart controls a grid may set.
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
