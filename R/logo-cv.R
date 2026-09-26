#' Leave-one-group-out cross-validation
#'
#' Holds out each level of `group` in turn and trains on the rest, so the
#' result is one performance estimate per group rather than a single number:
#' leave-one-group-out cross-validation (LOGO-CV). In the tutorial the groups
#' are the replication projects. Whole projects are held out because the
#' studies in a project share a design and a base rate.
#'
#' Pass a fit from [adaboost()] to cross-validate its recipe: `logo_cv()` reads
#' the formula, tree settings, splitting rule, `n_iter` and `eta` off the fit
#' and refits them once per project. The fit's own data are not used, because
#' cross-validation evaluates the procedure, not one fitted model. Pass a
#' formula instead to compare learners or search a grid.
#'
#' @section Reading the estimates:
#' The estimates are returned per project and are not averaged. Describe their
#' spread, but do not turn it into a standard error or a confidence interval.
#' The folds differ in size (10 to 90 studies in `altmejd`), and they share
#' most of their training data, so the usual standard error is too small
#' (Bengio & Grandvalet, 2004).
#'
#' @section Nested cross-validation:
#' Picking the best row of a grid and reporting its score is optimistic,
#' because the same folds chose the winner and graded it. With
#' `nested = TRUE`, each held-out project takes no part in the choice. The
#' result estimates the procedure "search the grid, then fit the winner", not
#' any one setting. It is slightly pessimistic, because each model trains on
#' one project fewer. Expect the outer folds to choose different settings.
#'
#' @section Models:
#' `"adaboost"` boosts a tree with [adaboost()] and is scored on the margin.
#' `"stump"` and `"tree"` are rpart trees that differ only in depth. `"logit"`
#' is a binomial [stats::glm()], and `"rf"` needs the randomForest package.
#' Every model is scored on a continuous score, never on class labels.
#'
#' @param x A fit from [adaboost()], or a formula of the form
#'   `outcome ~ predictors`.
#' @param data A data frame with the model's variables and the `group`
#'   column, holding every project.
#' @param group The name of the column that defines the folds, e.g. `"pid"`.
#' @param model The learner: `"adaboost"`, `"stump"`, `"tree"`, `"logit"` or
#'   `"rf"`.
#' @param grid An optional data frame of settings, one row per setting.
#'   Columns `T`, `eta` and `depth` set those values; missing ones fall back to
#'   the arguments below. A column named after an [rpart::rpart.control()]
#'   setting, such as `cp` or `minbucket`, applies to its row only. Any other
#'   column gives a warning.
#' @param T,eta,depth Settings used when `grid` is `NULL` or lacks the column.
#'   `T` and `eta` apply to `"adaboost"`, `depth` to the tree learners.
#' @param treehypar An optional list of [rpart::rpart.control()] settings that
#'   all tree learners share. Defaults to `xval = 0`, `maxsurrogate = 0` and
#'   `cp = 0.01`. `maxdepth` is ignored here; use `depth`. A setting in `grid`
#'   wins over the same setting here.
#' @param nested Whether to nest the grid search. The default, `FALSE`, scores
#'   every row of `grid` on every fold. `TRUE` needs a `grid` and at least
#'   three groups.
#' @param criterion The measure the inner search maximizes when
#'   `nested = TRUE`. Defaults to `"auroc"`. Fix it in advance: it decides
#'   which setting is chosen.
#' @param verbose Whether to report progress. Defaults to `FALSE`.
#' @param ... Ignored.
#'
#' @return An object of class `logo_cv`.
#'   * `estimates`: an array of setting x project x metric, e.g.
#'     `res$estimates[, , "auroc"]`. `as.data.frame()` gives the same values
#'     as one long table. An `NA` cell is an undefined measure, not a failed
#'     fit.
#'   * `grid`: the settings, one row per setting.
#'   * `projects`: each fold's `n` and `base_rate`.
#'   * `scores`: the held-out scores, one row per study and setting, which
#'     [bootstrap()] resamples.
#'   * `selected`: when nested, the setting each outer fold chose. `estimates`
#'     is then project x metric.
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
#' res <- h |>
#'   adaboost(n_iter = 10, eta = 1, verbose = FALSE) |>
#'   logo_cv(data = altmejd, group = "pid")
#' res
#' plot(res)
#'
#' # the formula interface compares learners and searches grids
#' logo_cv(
#'   replicate ~ power.o + effect_size.o + n.o + p_value.o,
#'   data = altmejd,
#'   group = "pid",
#'   model = "logit"
#' )
#'
#' @export
logo_cv <- function(x, ...) {
  UseMethod("logo_cv")
}

#' @rdname logo_cv
#' @export
logo_cv.formula <- function(
  x,
  data,
  group,
  model = c("adaboost", "stump", "tree", "logit", "rf"),
  grid = NULL,
  T = 10,
  eta = 1,
  depth = 1,
  treehypar = NULL,
  nested = FALSE,
  criterion = "auroc",
  verbose = FALSE,
  ...
) {
  model <- match.arg(model)
  treehypar <- normalize_treehypar(treehypar)
  folds <- read_folds(data, group)
  if (model == "rf" && !requireNamespace("randomForest", quietly = TRUE)) {
    stop(
      "model = \"rf\" needs the randomForest package. ",
      "Install it with install.packages(\"randomForest\").",
      call. = FALSE
    )
  }

  if (nested) {
    if (is.null(grid)) {
      stop(
        "`nested = TRUE` needs a `grid`: with nothing to choose between, ",
        "there is no selection step to protect against.",
        call. = FALSE
      )
    }
    if (length(folds) < 3L) {
      stop(
        "`nested = TRUE` needs at least three groups: one is held out and the ",
        "inner search needs two or more of the rest.",
        call. = FALSE
      )
    }
    return(cross_validate_nested(
      formula = x,
      data = data,
      group = group,
      model = model,
      grid = normalize_grid(grid, T = T, eta = eta, depth = depth),
      treehypar = treehypar,
      folds = folds,
      criterion = criterion,
      verbose = verbose
    ))
  }

  cross_validate(
    formula = x,
    data = data,
    group = group,
    model = model,
    grid = normalize_grid(grid, T = T, eta = eta, depth = depth),
    treehypar = treehypar,
    folds = folds,
    verbose = verbose
  )
}

#' @rdname logo_cv
#' @export
logo_cv.adaboost <- function(x, data, group, verbose = FALSE, ...) {
  check_ada_fit(x)
  control <- attr(x, "control")
  folds <- read_folds(data, group)
  # the fit's settings, not its data: cross-validation refits the recipe
  cross_validate(
    formula = attr(x, "formula"),
    data = data,
    group = group,
    model = "adaboost",
    grid = data.frame(
      T = attr(x, "n_iter"),
      eta = attr(x, "eta"),
      depth = control$maxdepth
    ),
    treehypar = normalize_treehypar(
      control[setdiff(names(control), "maxdepth")]
    ),
    folds = folds,
    # fits made before the split attribute existed used gini
    split = attr(x, "split") %||% "gini",
    verbose = verbose
  )
}

#' Check `data` and `group`, and return the folds
#' @noRd
read_folds <- function(data, group) {
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
  folds
}

#' Score every setting on every fold, keeping the held-out scores
#' @noRd
cross_validate <- function(
  formula,
  data,
  group,
  model,
  grid,
  treehypar,
  folds,
  split = "gini",
  verbose = FALSE
) {
  outcome <- all.vars(formula)[1L]

  # allocate the setting x project x metric cube up front
  measure_names <- list_measures()
  estimates <- array(
    NA_real_,
    dim = c(nrow(grid), length(folds), length(measure_names)),
    dimnames = list(
      setting = as.character(seq_len(nrow(grid))),
      project = folds,
      metric = measure_names
    )
  )
  # n and base_rate describe a project, so store them once per project
  projects <- data.frame(
    project = folds,
    n = NA_integer_,
    base_rate = NA_real_,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  scores <- list()

  for (i in seq_len(nrow(grid))) {
    for (fold in folds) {
      if (verbose) {
        message("setting ", i, "/", nrow(grid), " | fold ", fold)
      }

      held_out <- as.character(data[[group]]) == fold
      train <- data[!held_out, , drop = FALSE]
      test <- data[held_out, , drop = FALSE]

      score <- score_fold(
        formula = formula,
        train = train,
        test = test,
        model = model,
        setting = grid[i, , drop = FALSE],
        treehypar = treehypar,
        split = split
      )

      actual <- test[[outcome]]
      measures <- assess(actual, score, threshold = choose_threshold(model))

      estimates[i, fold, names(measures)] <- unname(measures)
      # keep the held-out scores, so bootstrap() needs no refit
      scores[[length(scores) + 1L]] <- data.frame(
        setting = i,
        project = fold,
        actual = actual,
        score = unname(score),
        row.names = NULL,
        stringsAsFactors = FALSE
      )

      # the same for every setting, so record it once
      if (i == 1L) {
        j <- match(fold, projects$project)
        projects$n[j] <- nrow(test)
        projects$base_rate[j] <- mean(as_binary(actual))
      }
    }
  }

  structure(
    list(
      estimates = estimates,
      # after the loop, which fits from these columns
      grid = mask_unused(grid, model),
      projects = projects,
      scores = do.call(rbind, scores),
      model = model,
      nested = FALSE,
      criterion = NA_character_,
      selected = NULL
    ),
    class = "logo_cv"
  )
}

#' Run the outer loop of nested LOGO-CV, calling logo_cv() for each inner search
#' @noRd
cross_validate_nested <- function(
  formula,
  data,
  group,
  model,
  grid,
  treehypar,
  folds,
  criterion,
  verbose
) {
  outcome <- all.vars(formula)[1L]
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

    inner_cv <- logo_cv(
      formula,
      data = train,
      group = group,
      model = model,
      grid = grid,
      treehypar = treehypar,
      verbose = FALSE
    )

    if (!criterion %in% dimnames(inner_cv$estimates)$metric) {
      stop(
        "`criterion` \"",
        criterion,
        "\" is not one of the measures returned by assess().",
        call. = FALSE
      )
    }

    # the mean over the inner folds ranks settings; it is not an estimate
    # (see ?logo_cv)
    inner_means <- rowMeans(
      inner_cv$estimates[,, criterion, drop = FALSE],
      na.rm = TRUE
    )
    winner <- unname(which.max(inner_means))

    score <- score_fold(
      formula = formula,
      train = train,
      test = test,
      model = model,
      setting = grid[winner, , drop = FALSE],
      treehypar = treehypar
    )

    actual <- test[[outcome]]
    measures <- assess(actual, score, threshold = choose_threshold(model))

    # keep the whole grid row, so a control tuned in the grid travels along;
    # depth, T and eta keep fixed positions
    choice <- grid[winner, , drop = FALSE]
    choice <- choice[,
      union(c("depth", "T", "eta"), names(choice)),
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
      grid = mask_unused(grid, model),
      projects = projects,
      scores = do.call(rbind, scores),
      model = model,
      nested = TRUE,
      criterion = criterion,
      selected = do.call(rbind, selected)
    ),
    class = "logo_cv"
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

#' Set the hyperparameters a learner ignores to NA
#' @noRd
mask_unused <- function(grid, model) {
  # NA, not a stale default that reads as if it were used
  if (model %in% c("logit", "rf")) {
    grid$depth <- NA_real_
  }
  if (model != "adaboost") {
    grid$T <- NA_real_
    grid$eta <- NA_real_
  }
  grid
}

#' List the tree controls a grid row may set
#' @noRd
list_tree_controls <- function() {
  # maxdepth is excluded: depth is tuned
  setdiff(names(formals(rpart::rpart.control)), c("maxdepth", "..."))
}

#' Fill in the grid's missing columns and warn about unknown ones
#' @noRd
normalize_grid <- function(grid, T, eta, depth) {
  if (is.null(grid)) {
    return(data.frame(T = T, eta = eta, depth = depth))
  }
  grid <- as.data.frame(grid)
  if (nrow(grid) == 0L) {
    stop("`grid` has no rows.", call. = FALSE)
  }
  if (is.null(grid$T)) {
    grid$T <- T
  }
  if (is.null(grid$eta)) {
    grid$eta <- eta
  }
  if (is.null(grid$depth)) {
    grid$depth <- depth
  }
  # read extra columns as rpart controls; warn once about the rest instead of
  # dropping a column meant to tune something
  unknown <- setdiff(names(grid), c("T", "eta", "depth", list_tree_controls()))
  if (length(unknown)) {
    warning(
      "grid column(s) not used for fitting: ",
      paste(unknown, collapse = ", "),
      ". Beyond `T`, `eta` and `depth`, only rpart controls are read (",
      paste(list_tree_controls(), collapse = ", "),
      ").",
      call. = FALSE
    )
  }
  grid
}

#' Build the one tree control that all tree learners share
#' @noRd
normalize_treehypar <- function(treehypar, setting = NULL) {
  # one control for every tree learner, so they differ only in depth
  control <- rpart::rpart.control(xval = 0, maxsurrogate = 0, cp = 0.01)
  if (!is.null(treehypar)) {
    if (!is.list(treehypar)) {
      stop(
        "`treehypar` must be a list of rpart control parameters.",
        call. = FALSE
      )
    }
    if (!is.null(treehypar$maxdepth)) {
      warning(
        "`treehypar$maxdepth` is ignored: depth is the tuned dimension, so it ",
        "comes from `depth` or `grid$depth`.",
        call. = FALSE
      )
      treehypar$maxdepth <- NULL
    }
    control <- utils::modifyList(control, treehypar)
  }
  # a grid-row control wins over `treehypar`: the row is more specific
  if (!is.null(setting)) {
    row_controls <- intersect(names(setting), list_tree_controls())
    if (length(row_controls)) {
      control <- utils::modifyList(
        control,
        as.list(setting[, row_controls, drop = FALSE])
      )
    }
  }
  # depth is supplied per grid row, never here
  control$maxdepth <- NULL
  control
}

#' Fit one learner on one fold and score the held-out rows
#' @noRd
score_fold <- function(
  formula,
  train,
  test,
  model,
  setting,
  treehypar = NULL,
  split = "gini"
) {
  treehypar <- normalize_treehypar(treehypar, setting = setting)
  outcome <- all.vars(formula)[1L]
  predictors <- all.vars(formula)[-1L]
  if (identical(predictors, ".")) {
    predictors <- setdiff(names(train), outcome)
  }
  variables <- c(predictors, outcome)

  if (model == "adaboost") {
    # build the weak learner like the stump branch; model = TRUE carries the
    # data adaboost() needs
    h <- rpart::rpart(
      formula,
      data = train[, variables, drop = FALSE],
      method = "class",
      control = utils::modifyList(treehypar, list(maxdepth = setting$depth)),
      parms = list(split = split),
      model = TRUE
    )
    fit <- adaboost(
      h,
      n_iter = setting$T,
      eta = setting$eta,
      # each fit is thrown away, so keep no data for retrodictions
      keep_data = FALSE,
      verbose = FALSE,
      check_inputs = FALSE
    )
    # the margin, not labels: labels collapse auroc onto balanced accuracy
    return(predict(
      fit,
      test[, predictors, drop = FALSE],
      type = "margin",
      verbose = FALSE,
      check_inputs = FALSE
    ))
  }

  if (model %in% c("stump", "tree")) {
    maxdepth <- if (model == "stump") 1 else setting$depth
    fit <- rpart::rpart(
      formula,
      data = train[, variables, drop = FALSE],
      method = "class",
      control = utils::modifyList(treehypar, list(maxdepth = maxdepth))
    )
    prob <- stats::predict(fit, newdata = test, type = "prob")
    return(prob[, ncol(prob)])
  }

  if (model == "logit") {
    train_coded <- train[, variables, drop = FALSE]
    train_coded[[outcome]] <- as_binary(train_coded[[outcome]])
    fit <- stats::glm(formula, data = train_coded, family = stats::binomial)
    return(stats::predict(fit, newdata = test, type = "response"))
  }

  fit <- randomForest::randomForest(
    formula,
    data = train[, variables, drop = FALSE]
  )
  prob <- stats::predict(fit, newdata = test, type = "prob")
  prob[, ncol(prob)]
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

  columns <- c("depth", "T", "eta", "project", "n", "base_rate", "estimate")
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
  rows[order(rows$depth, rows$T, rows$eta, rows$project), , drop = FALSE]
}

#' @rdname logo_cv_methods
#' @export
as.data.frame.logo_cv <- function(x, ...) {
  cube <- x$estimates
  dim_names <- dimnames(cube)
  core_columns <- c("depth", "T", "eta")

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
#'   would score: 0.5 for `auroc` and `bacc`, 0 for `mcc`, and each project's
#'   base rate for the others. A number sets it; `NA` leaves it out.
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
  alphas <- seq(1, 0.32, length.out = length(levels))
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
        col = grDevices::adjustcolor(colors[i], alpha.f = alphas[k])
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
  # chance level differs by measure: 0.5 for auroc and bacc, 0 for mcc, the
  # project base rate (.30 to .85) for the precision-type rest
  if (metric %in% c("auroc", "bacc")) {
    list(values = rep(0.5, n_projects), label = "dashed line: chance (0.5)")
  } else if (metric == "mcc") {
    list(values = rep(0, n_projects), label = "dashed line: no association (0)")
  } else if (
    metric %in% c("auprc", "ppv", "f1", "acc") || grepl("^patk(_|$)", metric)
  ) {
    list(values = base_rate, label = "dashed line: base rate of each project")
  } else {
    list(values = rep(NA_real_, n_projects), label = "")
  }
}
