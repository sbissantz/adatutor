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
  formula <- x
  model <- match.arg(model)
  treehypar <- normalize_treehypar(treehypar)
  folds <- logo_cv_folds(data, group)
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
    return(logo_cv_nested(
      formula = formula,
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

  logo_cv_run(
    formula = formula,
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
  ctrl <- attr(x, "control")
  split <- attr(x, "split")
  folds <- logo_cv_folds(data, group)
  # the fit's settings, not its data: cross-validation refits the recipe
  logo_cv_run(
    formula = attr(x, "formula"),
    data = data,
    group = group,
    model = "adaboost",
    grid = data.frame(
      T = attr(x, "n_iter"),
      eta = attr(x, "eta"),
      depth = ctrl$maxdepth
    ),
    treehypar = normalize_treehypar(ctrl[setdiff(names(ctrl), "maxdepth")]),
    folds = folds,
    split = if (is.null(split)) "gini" else split,
    verbose = verbose
  )
}

#' Check `data` and `group`, and return the folds
#' @noRd
logo_cv_folds <- function(data, group) {
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
logo_cv_run <- function(
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

  # allocate the setting x project x metric cube up front; assess() names
  # the measures
  measure_names <- names(assess(c(1, 0), c(1, -1)))
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

      in_test <- as.character(data[[group]]) == fold
      train <- data[!in_test, , drop = FALSE]
      test <- data[in_test, , drop = FALSE]

      score <- fold_score(
        formula = formula,
        train = train,
        test = test,
        model = model,
        pars = grid[i, , drop = FALSE],
        treehypar = treehypar,
        split = split
      )

      actual <- test[[outcome]]
      # threshold 0 for the AdaBoost margin, 0.5 for probabilities
      measures <- assess(
        actual,
        score,
        threshold = if (model == "adaboost") 0 else 0.5
      )

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

  # hyperparameters meaningless for this learner are NA, not a stale default;
  # set after the loop, which fits from the same columns
  if (model %in% c("logit", "rf")) {
    grid$depth <- NA_real_
  }
  if (model != "adaboost") {
    grid$T <- NA_real_
    grid$eta <- NA_real_
  }

  structure(
    list(
      estimates = estimates,
      grid = grid,
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

#' Internal helpers for logo_cv()
#' @noRd
NULL

#' Run the outer loop of nested LOGO-CV, calling logo_cv() for each inner search
#' @noRd
logo_cv_nested <- function(
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
  measure_names <- names(assess(c(1, 0), c(1, -1)))

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
  picked <- vector("list", length(folds))
  scores <- vector("list", length(folds))

  for (k in seq_along(folds)) {
    fold <- folds[k]
    if (verbose) {
      message("outer fold ", k, "/", length(folds), ": ", fold)
    }

    # drop the held-out project entirely, which keeps the selection honest
    in_test <- as.character(data[[group]]) == fold
    train <- data[!in_test, , drop = FALSE]
    test <- data[in_test, , drop = FALSE]

    # inner loop: reuse logo_cv() on the remaining projects
    inner <- logo_cv(
      formula,
      data = train,
      group = group,
      model = model,
      grid = grid,
      treehypar = treehypar,
      verbose = FALSE
    )

    if (!criterion %in% dimnames(inner$estimates)$metric) {
      stop(
        "`criterion` \"",
        criterion,
        "\" is not one of the measures returned by assess().",
        call. = FALSE
      )
    }

    # rank settings by their mean over the inner folds: a ranking, not an
    # estimate (see ?logo_cv)
    ranked <- rowMeans(
      inner$estimates[,, criterion, drop = FALSE],
      na.rm = TRUE
    )
    best <- unname(which.max(ranked))

    score <- fold_score(
      formula = formula,
      train = train,
      test = test,
      model = model,
      pars = grid[best, , drop = FALSE],
      treehypar = treehypar
    )

    actual <- test[[outcome]]
    measures <- assess(
      actual,
      score,
      threshold = if (model == "adaboost") 0 else 0.5
    )

    # keep the whole grid row, so a control tuned in the grid travels along;
    # depth, T and eta keep fixed positions
    row <- grid[best, , drop = FALSE]
    row <- row[, union(c("depth", "T", "eta"), names(row)), drop = FALSE]
    picked[[k]] <- data.frame(
      project = fold,
      setting = best,
      `rownames<-`(row, NULL),
      inner_criterion = max(ranked, na.rm = TRUE),
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

  if (model %in% c("logit", "rf")) {
    grid$depth <- NA_real_
  }
  if (model != "adaboost") {
    grid$T <- NA_real_
    grid$eta <- NA_real_
  }

  structure(
    list(
      estimates = estimates,
      grid = grid,
      projects = projects,
      scores = do.call(rbind, scores),
      model = model,
      nested = TRUE,
      criterion = criterion,
      selected = do.call(rbind, picked)
    ),
    class = "logo_cv"
  )
}

#' List the tree controls a grid row may set
#' @noRd
tree_control_names <- function() {
  # controls a grid row may set; maxdepth is excluded because depth is tuned
  setdiff(names(formals(rpart::rpart.control)), c("maxdepth", "..."))
}

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
  unknown <- setdiff(names(grid), c("T", "eta", "depth", tree_control_names()))
  if (length(unknown)) {
    warning(
      "grid column(s) not used for fitting: ",
      paste(unknown, collapse = ", "),
      ". Beyond `T`, `eta` and `depth`, only rpart controls are read (",
      paste(tree_control_names(), collapse = ", "),
      ").",
      call. = FALSE
    )
  }
  grid
}

#' Build the one tree control that all tree learners share
#' @noRd
normalize_treehypar <- function(treehypar, row = NULL) {
  # one control for every tree learner, so they differ only in depth
  def_ctrl <- rpart::rpart.control(xval = 0, maxsurrogate = 0, cp = 0.01)
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
    def_ctrl <- utils::modifyList(def_ctrl, treehypar)
  }
  # a grid-row control wins over `treehypar`: the row is more specific
  if (!is.null(row)) {
    ctrl <- intersect(names(row), tree_control_names())
    if (length(ctrl)) {
      def_ctrl <- utils::modifyList(
        def_ctrl,
        as.list(row[, ctrl, drop = FALSE])
      )
    }
  }
  # depth is supplied per grid row, never here
  def_ctrl$maxdepth <- NULL
  def_ctrl
}

#' Fit one learner on one fold and score the held-out rows
#' @noRd
fold_score <- function(
  formula,
  train,
  test,
  model,
  pars,
  treehypar = NULL,
  split = "gini"
) {
  treehypar <- normalize_treehypar(treehypar, row = pars)
  outcome <- all.vars(formula)[1L]
  prednms <- all.vars(formula)[-1L]
  if (identical(prednms, ".")) {
    prednms <- setdiff(names(train), outcome)
  }
  voi <- c(prednms, outcome)

  if (model == "adaboost") {
    # build the weak learner like the stump branch; model = TRUE carries the
    # data adaboost() needs
    h <- rpart::rpart(
      formula,
      data = train[, voi, drop = FALSE],
      method = "class",
      control = utils::modifyList(treehypar, list(maxdepth = pars$depth)),
      parms = list(split = split),
      model = TRUE
    )
    fit <- adaboost(
      h,
      n_iter = pars$T,
      eta = pars$eta,
      # each fit is thrown away, so keep no data for retrodictions
      keep_data = FALSE,
      verbose = FALSE,
      input_checks = FALSE
    )
    # the margin, not labels: labels collapse auroc onto balanced accuracy
    return(predict(
      fit,
      test[, prednms, drop = FALSE],
      type = "margin",
      verbose = FALSE,
      input_checks = FALSE
    ))
  }

  if (model %in% c("stump", "tree")) {
    maxdepth <- if (model == "stump") 1 else pars$depth
    fit <- rpart::rpart(
      formula,
      data = train[, voi, drop = FALSE],
      method = "class",
      control = utils::modifyList(treehypar, list(maxdepth = maxdepth))
    )
    prob <- stats::predict(fit, newdata = test, type = "prob")
    return(prob[, ncol(prob)])
  }

  if (model == "logit") {
    tr <- train[, voi, drop = FALSE]
    tr[[outcome]] <- as_binary(tr[[outcome]])
    fit <- stats::glm(formula, data = tr, family = stats::binomial)
    return(stats::predict(fit, newdata = test, type = "response"))
  }

  fit <- randomForest::randomForest(formula, data = train[, voi, drop = FALSE])
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

  est <- as.data.frame(x)
  sub <- est[est$metric == metric, , drop = FALSE]
  if (nrow(sub) == 0L) {
    cat("no estimates for `", metric, "`\n", sep = "")
    return(invisible(x))
  }

  keep <- c("depth", "T", "eta", "project", "n", "base_rate", "estimate")
  keep <- keep[vapply(sub[keep], function(z) !all(is.na(z)), logical(1))]
  print(format(sub[, keep, drop = FALSE], digits = 3), row.names = FALSE)

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
  est <- as.data.frame(object)
  sub <- est[est$metric == metric, , drop = FALSE]
  sub[order(sub$depth, sub$T, sub$eta, sub$project), , drop = FALSE]
}

#' @rdname logo_cv_methods
#' @export
as.data.frame.logo_cv <- function(x, ...) {
  arr <- x$estimates
  dn <- dimnames(arr)
  reserved <- c("depth", "T", "eta")

  # aperm() then as.vector() order rows metric fastest, then project, then
  # setting, matching expand.grid() below
  if (isTRUE(x$nested)) {
    idx <- expand.grid(
      metric = dn$metric,
      project = dn$project,
      stringsAsFactors = FALSE
    )
    est <- as.vector(aperm(arr, c(2L, 1L)))
    setting <- x$selected$setting[match(idx$project, x$selected$project)]
  } else {
    idx <- expand.grid(
      metric = dn$metric,
      project = dn$project,
      setting = dn$setting,
      stringsAsFactors = FALSE
    )
    est <- as.vector(aperm(arr, c(3L, 2L, 1L)))
    setting <- as.integer(idx$setting)
  }

  gr <- x$grid[setting, , drop = FALSE]
  pj <- x$projects[match(idx$project, x$projects$project), , drop = FALSE]
  extra <- setdiff(names(gr), reserved)

  out <- data.frame(
    model = x$model,
    setting = setting,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  out <- cbind(out, `rownames<-`(gr[, reserved, drop = FALSE], NULL))
  if (length(extra)) {
    out <- cbind(out, `rownames<-`(gr[, extra, drop = FALSE], NULL))
  }
  cbind(
    out,
    data.frame(
      project = idx$project,
      n = pj$n,
      base_rate = pj$base_rate,
      metric = idx$metric,
      estimate = est,
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
#' @param drop_flag Projects whose bootstrap rejected more than this share of
#'   draws get a star and a footnote. Defaults to 0.05.
#'
#' @export
plot.logo_cv <- function(
  x,
  metric = "auroc",
  levels = c(0.50, 0.80, 0.95),
  baseline = "auto",
  drop_flag = 0.05,
  ...
) {
  old_par <- graphics::par(mar = c(5.2, 5, 4, 2) + 0.1)
  on.exit(graphics::par(old_par), add = TRUE)
  draw_logo_cv(
    x,
    metric = metric,
    levels = levels,
    baseline = baseline,
    drop_flag = drop_flag,
    main = "Leave-one-group-out performance",
    ylab = metric,
    footnote = TRUE
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
  drop_flag = 0.05,
  main = NULL,
  ylab = metric,
  footnote = TRUE
) {
  long <- as.data.frame(x)
  est <- long[long$metric == metric, , drop = FALSE]
  if (nrow(est) == 0L) {
    stop("no estimates for metric `", metric, "`.", call. = FALSE)
  }
  est <- est[order(est$project), , drop = FALSE]
  ci <- x$ci
  np <- nrow(est)
  at <- seq_len(np)
  levels <- sort(levels)

  # bands from the draws, so any level can be asked for later
  bands <- vector("list", np)
  rej <- rep(0, np)
  for (i in at) {
    cij <- ci[[est$project[i]]]
    if (is.null(cij)) {
      next
    }
    d <- attr(cij, "draws")
    if (is.null(d) || !metric %in% colnames(d)) {
      next
    }
    bands[[i]] <- vapply(
      levels,
      function(lv) {
        a <- (1 - lv) / 2
        stats::quantile(d[, metric], c(a, 1 - a), na.rm = TRUE)
      },
      numeric(2)
    )
    dropped <- cij$drop[1]
    rej[i] <- dropped / (dropped + attr(cij, "n_resample"))
  }
  flag <- rej > drop_flag
  # the star points at the footnote, so it goes where the footnote goes
  star <- footnote & flag

  # chance level differs by measure: 0.5 for auroc and bacc, 0 for mcc, the
  # project base rate (.30 to .85) for the rest
  ref_lab <- ""
  ref <- if (identical(baseline, "auto")) {
    if (metric %in% c("auroc", "bacc")) {
      ref_lab <- "dashed line: chance (0.5)"
      rep(0.5, np)
    } else if (metric == "mcc") {
      ref_lab <- "dashed line: no association (0)"
      rep(0, np)
    } else if (
      metric %in% c("auprc", "ppv", "f1", "acc") || grepl("^patk(_|$)", metric)
    ) {
      ref_lab <- "dashed line: base rate of each project"
      est$base_rate
    } else {
      rep(NA_real_, np)
    }
  } else if (length(baseline) == 1L && is.na(baseline)) {
    rep(NA_real_, np)
  } else {
    ref_lab <- paste0("dashed line: ", signif(baseline, 3))
    rep(as.numeric(baseline), np)
  }

  vals <- c(est$estimate, unlist(bands), ref)
  rng <- range(vals, na.rm = TRUE)
  pad <- max(diff(rng), 0.05) * 0.14
  cols <- viridisLite::viridis(np, end = 0.85)

  ylim <- c(rng[1] - pad, rng[2] + pad * 2)
  graphics::plot(
    NULL,
    xlim = c(0.5, np + 0.5),
    ylim = ylim,
    xaxt = "n",
    yaxt = "n",
    xlab = "",
    ylab = ylab,
    main = main
  )
  graphics::axis(
    1,
    at = at,
    labels = paste0(est$project, ifelse(star, "*", ""))
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

  if (any(is.finite(ref))) {
    graphics::segments(
      at - 0.42,
      ref,
      at + 0.42,
      ref,
      col = "gray55",
      lty = 2,
      lwd = 2
    )
  }

  lwds <- seq(9, 2.2, length.out = length(levels))
  alph <- seq(1, 0.32, length.out = length(levels))
  for (i in at) {
    b <- bands[[i]]
    if (is.null(b)) {
      next
    }
    for (k in seq_along(levels)) {
      graphics::segments(
        i,
        b[1, k],
        i,
        b[2, k],
        lwd = lwds[k],
        col = grDevices::adjustcolor(cols[i], alpha.f = alph[k])
      )
    }
  }

  # halo first, so the estimate stays legible against the darkest band
  graphics::points(at, est$estimate, pch = 19, cex = 1.6, col = "white")
  graphics::points(at, est$estimate, pch = 19, cex = 1.15, col = cols)

  # fold size drives the band widths, so it belongs on the plot
  graphics::text(
    at,
    rng[2] + pad * 1.5,
    paste0("n = ", est$n),
    cex = 0.75,
    col = "gray30"
  )

  # keys go in the margin rather than over the data
  if (!is.null(ci) && any(!vapply(bands, is.null, logical(1)))) {
    graphics::mtext(
      paste0("bands: ", paste0(round(100 * levels), collapse = " / "), "%"),
      side = 3,
      line = 0.25,
      adj = 1,
      cex = 0.72,
      col = "gray35"
    )
  }
  if (nzchar(ref_lab)) {
    graphics::mtext(
      ref_lab,
      side = 3,
      line = 0.25,
      adj = 0,
      cex = 0.72,
      col = "gray35"
    )
  }
  if (footnote && any(flag)) {
    graphics::mtext(
      paste0(
        "* resamples rejected as single-class: ",
        paste0(
          est$project[flag],
          " ",
          round(100 * rej[flag]),
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

  widest <- function(i, row) {
    if (is.null(bands[[i]])) NA_real_ else bands[[i]][row, length(levels)]
  }
  data.frame(
    project = est$project,
    n = est$n,
    estimate = est$estimate,
    lower = vapply(at, widest, numeric(1), row = 1L),
    upper = vapply(at, widest, numeric(1), row = 2L),
    rejected = rej,
    flagged = flag,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
