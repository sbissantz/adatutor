#' Bootstrap confidence intervals for performance measures
#'
#' Resamples the test cases and scores each resample with [assess()], so
#' every measure gets an interval from the same draws. The model stays fixed:
#' the interval shows how much an estimate would move if the project had
#' drawn different studies.
#'
#' The default method bootstraps one set of labels and scores. The `logo_cv`
#' method bootstraps each held-out project of an [logo_cv()] result and attaches
#' the intervals, so [plot()][logo_cv_methods] draws them as bands.
#'
#' @section Rejected resamples:
#' A resample with only one class makes `auroc` and `auprc` undefined, so it
#' is drawn again. `n_resample` counts the usable resamples, and `drop` counts
#' the rejected ones. A high `drop` means the interval is too narrow: on a fold
#' of 13 studies with 2 failures, about 11 percent of draws contain no failure.
#'
#' @section Stratified resampling:
#' With `stratified = TRUE`, each class is resampled separately, so no draw is
#' rejected. The base rate then never varies, which suits `auroc` and `bacc`
#' but understates the uncertainty of `auprc`, `ppv`, `npv`, `f1`, `acc` and
#' `mcc`.
#'
#' @param x For the default method, the true labels: a two-level factor (the
#'   second level is the positive class), 0/1, -1/1, or logical. For the
#'   `logo_cv` method, a result of [logo_cv()].
#' @param score Continuous scores, one per observation.
#' @param threshold The cutoff for `score`. Use 0 for margins (the default)
#'   and 0.5 for probabilities. The `logo_cv` method picks it from the model.
#' @param n_resample The number of usable resamples. Defaults to 1000.
#' @param stratified Whether to resample each class separately. Defaults to
#'   `FALSE`.
#' @param conf The confidence level. Defaults to 0.95.
#' @param k Passed to [assess()].
#' @param setting For an `logo_cv` result with several settings, the row of its
#'   `grid` to bootstrap.
#' @param ... Ignored.
#'
#' @return The default method returns a data frame with one row per measure
#'   and the columns `metric`, `estimate` (from the observed data), `lower`,
#'   `upper` and `drop`. Attributes record the settings and keep the draws.
#'
#'   The `logo_cv` method returns `x`, reduced to one setting, with one such data
#'   frame per project in `x$ci`.
#'
#' @family cross-validation
#'
#' @examples
#' data(altmejd)
#' prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
#' train <- altmejd[altmejd$pid != "ssrp", ]
#' test <- altmejd[altmejd$pid == "ssrp", ]
#'
#' h <- rpart::rpart(
#'   replicate ~ .,
#'   data = train[, c(prednms, "replicate")],
#'   maxdepth = 1,
#'   model = TRUE
#' )
#' fit <- adaboost(h, n_iter = 10, eta = 1, verbose = FALSE, check_inputs = FALSE)
#' margin <- predict(
#'   fit,
#'   test[, prednms],
#'   type = "margin",
#'   verbose = FALSE,
#'   check_inputs = FALSE
#' )
#'
#' # one set of labels and scores
#' set.seed(1)
#' ci <- bootstrap(test$replicate, margin, n_resample = 200)
#' ci[ci$metric %in% c("auroc", "auprc", "bacc"), ]
#'
#' # every held-out project of a cross-validation, then plotted as bands
#' res <- logo_cv(fit, data = altmejd, group = "pid")
#' set.seed(1)
#' res |>
#'   bootstrap(n_resample = 200) |>
#'   plot()
#'
#' @export
bootstrap <- function(x, ...) {
  UseMethod("bootstrap")
}

#' @rdname bootstrap
#' @export
bootstrap.default <- function(
  x,
  score,
  n_resample = 1000,
  stratified = FALSE,
  conf = 0.95,
  threshold = 0,
  k = NULL,
  ...
) {
  y <- as_binary(x)
  score <- as.numeric(score)
  if (length(y) != length(score)) {
    stop("`x` and `score` must have the same length.", call. = FALSE)
  }
  if (n_resample < 1L) {
    stop("`n_resample` must be at least 1.", call. = FALSE)
  }
  if (conf <= 0 || conf >= 1) {
    stop("`conf` must lie strictly between 0 and 1.", call. = FALSE)
  }

  notes <- character(0)
  if (stratified) {
    note <- paste0(
      "stratified = TRUE holds the class balance fixed, so intervals for the ",
      "prevalence-dependent measures (auprc, ppv, npv, f1, acc, mcc) are ",
      "understated. auroc and bacc are unaffected."
    )
    notes <- c(notes, note)
    warning(note, call. = FALSE)
  }

  # keep only the names: no class, no print attributes
  observed <- c(assess(y, score, threshold = threshold, k = k))
  n <- length(y)
  positives <- which(y == 1L)
  negatives <- which(y == 0L)

  # cap the attempts: only an all-one-class fold fails, and no count fixes it
  max_attempts <- 100L * as.integer(n_resample)

  draws <- matrix(
    NA_real_,
    nrow = n_resample,
    ncol = length(observed),
    dimnames = list(NULL, names(observed))
  )

  kept <- 0L
  attempts <- 0L
  while (kept < n_resample && attempts < max_attempts) {
    attempts <- attempts + 1L
    rows <- if (stratified) {
      # resample within class, so the class counts never change
      c(
        sample(positives, length(positives), replace = TRUE),
        sample(negatives, length(negatives), replace = TRUE)
      )
    } else {
      sample.int(n, n, replace = TRUE)
    }
    if (length(unique(y[rows])) < 2L) {
      next
    }
    kept <- kept + 1L
    # one assess() call per draw keeps the intervals of all measures coherent;
    # the scale check already ran on the observed data, so mute it per draw
    draws[kept, ] <- withCallingHandlers(
      assess(y[rows], score[rows], threshold = threshold, k = k),
      warning = function(w) {
        if (
          startsWith(conditionMessage(w), "`score` lies entirely in [0, 1]")
        ) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  n_dropped <- attempts - kept

  if (kept < n_resample) {
    warning(
      "gave up after ",
      attempts,
      " attempts with only ",
      kept,
      " usable resamples: this fold cannot reliably produce draws containing ",
      "both classes. Intervals are NA.",
      call. = FALSE
    )
    lower <- upper <- rep(NA_real_, length(observed))
  } else {
    tail_prob <- (1 - conf) / 2
    lower <- apply(draws, 2, stats::quantile, probs = tail_prob, na.rm = TRUE)
    upper <- apply(
      draws,
      2,
      stats::quantile,
      probs = 1 - tail_prob,
      na.rm = TRUE
    )

    if (!stratified && n_dropped / attempts > 0.05) {
      note <- sprintf(
        paste0(
          "%.1f%% of resamples were rejected as single-class. The interval is ",
          "conditional on both classes appearing, so its bounds are optimistic."
        ),
        100 * n_dropped / attempts
      )
      notes <- c(notes, note)
      warning(note, call. = FALSE)
    }
  }

  out <- data.frame(
    metric = names(observed),
    estimate = unname(observed),
    lower = unname(lower),
    upper = unname(upper),
    drop = n_dropped,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  attr(out, "n_resample") <- n_resample
  attr(out, "stratified") <- stratified
  attr(out, "conf") <- conf
  attr(out, "notes") <- notes
  # keep the draws, so other quantiles need no new resampling
  attr(out, "draws") <- if (kept < n_resample) NULL else draws
  out
}

#' @rdname bootstrap
#' @export
bootstrap.logo_cv <- function(
  x,
  n_resample = 1000,
  stratified = FALSE,
  conf = 0.95,
  setting = NULL,
  ...
) {
  if (is.null(x$scores)) {
    stop(
      "`x` holds no held-out scores. Rerun logo_cv() to keep them.",
      call. = FALSE
    )
  }
  scores <- x$scores

  if (!isTRUE(x$nested)) {
    n_settings <- nrow(x$grid)
    if (is.null(setting)) {
      if (n_settings > 1L) {
        stop(
          "`x` holds ",
          n_settings,
          " settings. Choose one with `setting`.",
          call. = FALSE
        )
      }
      setting <- 1L
    }
    if (length(setting) != 1L || !setting %in% seq_len(n_settings)) {
      stop("`setting` must be one row number of `x$grid`.", call. = FALSE)
    }
    # keep only the bootstrapped setting, so print() and plot() show that one
    x$estimates <- x$estimates[setting, , , drop = FALSE]
    dimnames(x$estimates)$setting <- "1"
    x$grid <- `rownames<-`(x$grid[setting, , drop = FALSE], NULL)
    scores <- scores[scores$setting == setting, , drop = FALSE]
    scores$setting <- 1L
    x$scores <- scores
  }

  threshold <- choose_threshold(x$model)
  projects <- x$projects$project
  x$ci <- stats::setNames(
    lapply(projects, function(project) {
      in_project <- scores$project == project
      # name the project, so warnings from different folds stay apart
      withCallingHandlers(
        bootstrap.default(
          scores$actual[in_project],
          scores$score[in_project],
          n_resample = n_resample,
          stratified = stratified,
          conf = conf,
          threshold = threshold
        ),
        warning = function(w) {
          warning("project ", project, ": ", conditionMessage(w), call. = FALSE)
          invokeRestart("muffleWarning")
        }
      )
    }),
    projects
  )
  x
}
