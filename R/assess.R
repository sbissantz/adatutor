#' Assess classification performance
#'
#' Computes performance metrics from true labels and continuous scores. Most
#' metrics come from one confusion matrix at `threshold`; `auroc`, `auprc` and
#' the `patk` metrics use the ranking of the scores.
#'
#' @section Metrics:
#' From the counts at `threshold`: true positives `tp`, true negatives `tn`,
#' false positives `fp` and false negatives `fn`.
#' * `sens` (sensitivity):
#'   \eqn{\frac{\mathrm{tp}}{\mathrm{tp} + \mathrm{fn}}}{tp / (tp + fn)}.
#' * `spec` (specificity):
#'   \eqn{\frac{\mathrm{tn}}{\mathrm{tn} + \mathrm{fp}}}{tn / (tn + fp)}.
#' * `ppv` (positive predictive value):
#'   \eqn{\frac{\mathrm{tp}}{\mathrm{tp} + \mathrm{fp}}}{tp / (tp + fp)}.
#' * `npv` (negative predictive value):
#'   \eqn{\frac{\mathrm{tn}}{\mathrm{tn} + \mathrm{fn}}}{tn / (tn + fn)}.
#' * `acc` (accuracy): \eqn{\frac{\mathrm{tp} + \mathrm{tn}}{\mathrm{tp} +
#'   \mathrm{tn} + \mathrm{fp} + \mathrm{fn}}}{(tp + tn) / (tp + tn + fp +
#'   fn)}.
#' * `bacc` (balanced accuracy):
#'   \eqn{\frac{\mathrm{sens} + \mathrm{spec}}{2}}{(sens + spec) / 2}.
#' * `f1` (F1 score): \eqn{\frac{2 \cdot \mathrm{tp}}{2 \cdot \mathrm{tp} +
#'   \mathrm{fp} + \mathrm{fn}}}{2 * tp / (2 * tp + fp + fn)}, the harmonic
#'   mean of `ppv` and `sens`.
#' * `mcc` (Matthews correlation coefficient; Chicco & Jurman, 2020):
#'   \eqn{\frac{\mathrm{tp} \cdot \mathrm{tn} - \mathrm{fp} \cdot \mathrm{fn}}
#'   {\sqrt{(\mathrm{tp} + \mathrm{fp})(\mathrm{tp} + \mathrm{fn})
#'   (\mathrm{tn} + \mathrm{fp})(\mathrm{tn} + \mathrm{fn})}}}{(tp * tn -
#'   fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))}.
#'
#' From the ranking of `score`:
#' * `auroc` (area under the ROC curve): the chance that a random positive
#'   scores higher than a random negative, with ties counting half.
#' * `auprc` (area under the precision-recall curve): estimated as in Davis
#'   and Goadrich (2006).
#' * `patk_3`, `patk_5`, `patk` (precision at `k`): the share of positives among
#'   the `k` highest scores, for `k` of 3, 5 and the number of positives. See
#'   [patk()].
#'
#' @section Missing values:
#' A metric is `NA` when the data cannot support it, such as `ppv` when
#' nothing is predicted positive, `auroc` and `auprc` when only one class is
#' present, and `patk` when `k` is larger than the number of positives. `mcc`
#' is 0 when its denominator is 0, and `f1` is 0 when nothing is predicted
#' positive but positives exist.
#'
#' @param x For the default method, the true labels: a two-level factor (the
#'   second level is the positive class), 0/1, -1/1, or logical. For the
#'   `confusion` method, a result of [confusion()]. For `print()`, a result
#'   of `assess()`.
#' @param score Continuous scores, one per observation, such as an AdaBoost
#'   margin or a predicted probability. Class labels turn `auroc` into
#'   balanced accuracy; with [predict.adaboost()], use `type = "margin"`.
#' @param threshold The cutoff for `score`. Use 0 for margins (the default)
#'   and 0.5 for probabilities.
#' @param k The budgets for the `patk` metrics. Defaults to 3, 5 and the
#'   number of positives. A budget given names its metric, such as `patk_10`.
#' @param digits The number of decimals `print()` shows.
#' @param ... Ignored.
#'
#' @return A named numeric vector of class `assessment`: the counts `tp`,
#'   `tn`, `fp` and `fn`, then the metrics in the order of the Metrics
#'   section, 17 values by default. The `confusion` method returns the first
#'   12. `print()` rounds only what it shows.
#'
#' @family performance metrics
#'
#' @references
#' Chicco, D., & Jurman, G. (2020). The advantages of the Matthews correlation
#' coefficient (MCC) over F1 score and accuracy in binary classification
#' evaluation. *BMC Genomics*, 21, 6.
#'
#' Davis, J., & Goadrich, M. (2006). The relationship between precision-recall
#' and ROC curves. *Proceedings of the 23rd International Conference on
#' Machine Learning*, 233-240.
#'
#' @examples
#' data(altmext)
#' prednms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
#' # train on the six tuning projects, test on HOLZ, the project kept back
#' train <- altmext[altmext$pid != "holz", ]
#' test <- altmext[altmext$pid == "holz", ]
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
#' assess(test$replicate, margin)
#'
#' # fewer decimals; the stored values stay exact
#' print(assess(test$replicate, margin), digits = 2)
#'
#' # the same metrics from a confusion matrix, without the ranking ones
#' cm <- confusion(test$replicate, margin)
#' assess(cm)
#'
#' @export
assess <- function(x, ...) {
  UseMethod("assess")
}

#' @rdname assess
#' @export
assess.default <- function(x, score, threshold = 0, k = NULL, ...) {
  # `score`, not `predicted`: neutral between prediction and retrodiction
  y <- as_binary(x)
  score <- as.numeric(score)

  if (length(y) != length(score)) {
    stop(
      "`x` and `score` must have the same length.",
      call. = FALSE
    )
  }

  cm <- confusion(y, score, threshold)

  # two small fixed budgets, then R-precision, which keeps the name `patk`
  if (is.null(k)) {
    k <- c(3L, 5L, sum(y == 1))
    k_names <- c("patk_3", "patk_5", "patk")
  } else {
    k_names <- paste0("patk_", k)
  }
  at_k <- stats::setNames(unname(patk(y, score, k)), k_names)

  metrics <- c(
    unclass(assess(cm)),
    auroc = auroc(y, score),
    auprc = auprc(y, score),
    at_k
  )
  structure(metrics, class = "assessment", threshold = threshold, k = k)
}

#' @rdname assess
#' @export
assess.confusion <- function(x, ...) {
  # one function per metric, so each formula is written once
  metrics <- c(
    tp = x[["tp"]],
    tn = x[["tn"]],
    fp = x[["fp"]],
    fn = x[["fn"]],
    sens = measure_sens(x),
    spec = measure_spec(x),
    ppv = measure_ppv(x),
    npv = measure_npv(x),
    acc = measure_acc(x),
    bacc = measure_bacc(x),
    f1 = measure_f1(x),
    mcc = measure_mcc(x)
  )
  # pass NULL on for a hand-built vector: plain header
  structure(metrics, class = "assessment", threshold = attr(x, "threshold"))
}

#' @rdname assess
#' @export
print.assessment <- function(x, digits = 3, ...) {
  values <- unclass(x)
  attributes(values) <- list(names = names(x))
  counts <- c("tp", "tn", "fp", "fn")
  at_cut <- c("sens", "spec", "ppv", "npv", "acc", "bacc", "f1", "mcc")
  areas <- c("auroc", "auprc")

  # fall back when arithmetic has changed the names
  if (!all(c(counts, at_cut) %in% names(values))) {
    print(values)
    return(invisible(x))
  }

  blocks <- list(
    list(label = label_threshold(attr(x, "threshold")), values = values[at_cut])
  )
  k <- attr(x, "k")
  at_k <- values[grepl("^patk(_|$)", names(values))]
  if (length(at_k) && length(at_k) == length(k)) {
    names(at_k) <- paste0("k=", k)
    # drop what is out of reach, and a default that repeats 3 or 5
    at_k <- at_k[!is.na(at_k) & !duplicated(k)]
    if (length(at_k)) {
      blocks <- c(blocks, list(list(label = "Among top k", values = at_k)))
    }
  }
  if (all(areas %in% names(values))) {
    blocks <- c(
      blocks,
      list(list(label = "Over thresholds", values = values[areas]))
    )
  }

  rows <- list()
  for (block in blocks) {
    chunks <- split(block$values, ceiling(seq_along(block$values) / 4))
    for (i in seq_along(chunks)) {
      rows[[length(rows) + 1L]] <- list(
        label = if (i == 1L) block$label else "",
        values = chunks[[i]]
      )
    }
  }

  labels <- vapply(rows, `[[`, character(1), "label")
  width <- max(nchar(c(labels, "predicted 1"))) + 2L
  cat("Performance metrics\n")
  cat(format_confusion(values, width), sep = "\n")
  cat("\n")

  format_value <- function(value) {
    ifelse(is.na(value), "NA", formatC(value, format = "f", digits = digits))
  }
  value_width <- max(nchar(format_value(unlist(lapply(rows, `[[`, "values")))))
  name_width <- vapply(
    1:4,
    function(j) {
      max(
        0L,
        vapply(
          rows,
          \(entry) {
            if (length(entry$values) >= j) nchar(names(entry$values)[j]) else 0L
          },
          integer(1)
        )
      )
    },
    integer(1)
  )
  for (entry in rows) {
    cells <- sprintf(
      "%-*s %*s",
      name_width[seq_along(entry$values)],
      names(entry$values),
      value_width,
      format_value(entry$values)
    )
    cat(
      pad_label(entry$label, width),
      paste(cells, collapse = "  "),
      "\n",
      sep = ""
    )
  }
  invisible(x)
}

label_threshold <- function(threshold) {
  if (is.null(threshold)) "At threshold" else paste("At threshold", threshold)
}

# pad first, then style, so the escape bytes cannot shift the columns
pad_label <- function(label, width) {
  padded <- formatC(label, width = -width)
  if (nzchar(label)) {
    style_ansi(padded, ansi_gray)
  } else {
    padded
  }
}

format_confusion <- function(cm, width) {
  cells <- paste(
    c("tp", "fp", "fn", "tn"),
    formatC(cm[c("tp", "fp", "fn", "tn")], format = "d")
  )
  cell_width <- max(nchar(c("actual 1", cells)))
  header <- formatC(c("actual 1", "actual 0"), width = -cell_width)
  c(
    paste0(
      strrep(" ", width),
      style_ansi(header[1], ansi_gray),
      "  ",
      style_ansi(trimws(header[2]), ansi_gray)
    ),
    paste0(
      pad_label("predicted 1", width),
      formatC(cells[1], width = -cell_width),
      "  ",
      cells[2]
    ),
    paste0(
      pad_label("predicted 0", width),
      formatC(cells[3], width = -cell_width),
      "  ",
      cells[4]
    )
  )
}

#' Count a confusion matrix
#'
#' Cuts `score` at `threshold` and counts true positives `tp`, true negatives
#' `tn`, false positives `fp` and false negatives `fn`. [assess()] computes
#' the metrics from these counts.
#'
#' @param actual The true labels: a two-level factor (the second level is the
#'   positive class), 0/1, -1/1, or logical.
#' @param score Continuous scores, one per observation, such as an AdaBoost
#'   margin or a predicted probability.
#' @param threshold The cutoff for `score`. Use 0 for margins (the default)
#'   and 0.5 for probabilities. With `threshold = 0`, scores that look like
#'   probabilities give a warning.
#' @param x A result of `confusion()`.
#' @param ... Ignored.
#'
#' @return A named integer vector of class `confusion` with `tp`, `tn`, `fp`
#'   and `fn`. The threshold is stored as an attribute.
#'
#' @family performance metrics
#'
#' @examples
#' confusion(c(1, 1, 0, 0), c(2, -1, 1, -2))
#'
#' @export
confusion <- function(actual, score, threshold = 0) {
  y <- as_binary(actual)
  # warn on probabilities cut at 0 (every case positive) rather than guess:
  # margins can lie in [0, 1] too, and exact 0/1 are labels, where 0 is right
  if (
    threshold == 0 &&
      all(score >= 0 & score <= 1, na.rm = TRUE) &&
      any(score > 0 & score < 1, na.rm = TRUE)
  ) {
    warning(
      "`score` lies entirely in [0, 1] but `threshold` is 0, so every case is ",
      "classed positive. If these are probabilities, use `threshold = 0.5`.",
      call. = FALSE
    )
  }
  predicted <- score > threshold
  # the class lets assess(cm) dispatch to the matrix method
  structure(
    c(
      tp = sum(predicted & y == 1),
      tn = sum(!predicted & y == 0),
      fp = sum(predicted & y == 0),
      fn = sum(!predicted & y == 1)
    ),
    class = "confusion",
    threshold = threshold
  )
}

#' @rdname confusion
#' @export
print.confusion <- function(x, ...) {
  values <- unclass(x)
  attributes(values) <- list(names = names(x))
  width <- nchar("predicted 1") + 2L
  # a plain title, as print.assessment() has: the threshold is a label
  cat("Confusion matrix\n")
  cat(
    style_ansi(label_threshold(attr(x, "threshold")), ansi_gray),
    "\n",
    sep = ""
  )
  cat(format_confusion(values, width), sep = "\n")
  invisible(x)
}

#' Divide, returning NA for a zero denominator
#' @noRd
divide_safely <- function(numerator, denominator) {
  # NA, not NaN: the metric is undefined for these data; nothing failed
  if (denominator == 0) NA_real_ else numerator / denominator
}

#' Area under the ROC curve
#'
#' Computes the area under the receiver operating characteristic (ROC) curve
#' with the PRROC package (Grau et al., 2015). Class labels turn it into
#' balanced accuracy; with [predict.adaboost()], use `type = "margin"`.
#'
#' @inheritParams confusion
#'
#' @return A number, or `NA` if `actual` has only one class.
#'
#' @family performance metrics
#'
#' @references Grau, J., Grosse, I., & Keilwagen, J. (2015). PRROC: computing
#'   and visualizing precision-recall and receiver operating characteristic
#'   curves in R. *Bioinformatics*, 31(15), 2595-2597.
#'
#' @examples
#' auroc(c(1, 1, 0, 0), c(2, 1, -1, -2))
#'
#' @export
auroc <- function(actual, score) {
  y <- as_binary(actual)
  if (length(unique(y)) < 2L) {
    return(NA_real_)
  }
  area <- PRROC::roc.curve(scores.class0 = score, weights.class0 = y)$auc
  if (is.nan(area)) NA_real_ else area
}

#' Area under the precision-recall curve
#'
#' Computes the area under the precision-recall curve with the PRROC package
#' (Grau et al., 2015), using the estimator of Davis and Goadrich (2006).
#'
#' @inheritParams confusion
#'
#' @return A number, or `NA` if `actual` has only one class. A random score
#'   gets about the base rate, not 0.5.
#'
#' @family performance metrics
#'
#' @references
#' Davis, J., & Goadrich, M. (2006). The relationship between precision-recall
#' and ROC curves. *Proceedings of the 23rd International Conference on
#' Machine Learning*, 233-240.
#'
#' Grau, J., Grosse, I., & Keilwagen, J. (2015). PRROC: computing and
#' visualizing precision-recall and receiver operating characteristic curves in
#' R. *Bioinformatics*, 31(15), 2595-2597.
#'
#' @examples
#' auprc(c(1, 1, 0, 0), c(2, 1, -1, -2))
#'
#' @export
auprc <- function(actual, score) {
  y <- as_binary(actual)
  if (length(unique(y)) < 2L) {
    return(NA_real_)
  }
  # Davis & Goadrich: interpolate at the local skew, integrate by trapezoid;
  # operating points at distinct scores make ties order-invariant
  area <- PRROC::pr.curve(
    scores.class0 = score,
    weights.class0 = y
  )$auc.davis.goadrich
  if (is.nan(area)) NA_real_ else area
}

#' Precision among the top-ranked cases
#'
#' Computes the share of positives among the `k` highest scores. Scores tied
#' at the cut count by their share of positives.
#'
#' @inheritParams confusion
#' @param k One or more budgets. Defaults to the number of positives in
#'   `actual`.
#'
#' @return A number for a single `k`; for several, a vector named by `k`.
#'   `NA` where `k` is larger than the number of positives.
#'
#' @family performance metrics
#'
#' @examples
#' # three positives, so the default budget is the top three
#' patk(c(1, 1, 1, 0, 0), c(5, 4, 1, 3, 2))
#'
#' # several budgets at once
#' patk(c(1, 1, 1, 0, 0), c(5, 4, 1, 3, 2), k = c(1, 2))
#'
#' @export
patk <- function(actual, score, k = NULL) {
  y <- as_binary(actual)
  n_pos <- sum(y == 1L)
  if (is.null(k)) {
    k <- n_pos
  }
  precision <- vapply(
    as.integer(k),
    function(budget) {
      # out of reach: fewer positives than places
      if (is.na(budget) || budget < 1L || budget > n_pos) {
        return(NA_real_)
      }
      cutoff <- sort(score, decreasing = TRUE)[budget]
      above <- score > cutoff
      # share the places left across the tie, as random tie-breaking would
      (sum(y[above]) + (budget - sum(above)) * mean(y[score == cutoff])) /
        budget
    },
    numeric(1)
  )
  if (length(k) > 1L) {
    names(precision) <- k
  }
  precision
}

#' Compute one metric from a confusion matrix
#' @noRd
NULL
# the counts, not raw data, so every metric in a report shares one cutoff

measure_sens <- function(cm) {
  check_confusion(cm)
  divide_safely(cm[["tp"]], cm[["tp"]] + cm[["fn"]])
}

measure_spec <- function(cm) {
  check_confusion(cm)
  divide_safely(cm[["tn"]], cm[["tn"]] + cm[["fp"]])
}

measure_ppv <- function(cm) {
  check_confusion(cm)
  divide_safely(cm[["tp"]], cm[["tp"]] + cm[["fp"]])
}

measure_npv <- function(cm) {
  check_confusion(cm)
  divide_safely(cm[["tn"]], cm[["tn"]] + cm[["fn"]])
}

measure_acc <- function(cm) {
  check_confusion(cm)
  divide_safely(
    cm[["tp"]] + cm[["tn"]],
    cm[["tp"]] + cm[["tn"]] + cm[["fp"]] + cm[["fn"]]
  )
}

measure_bacc <- function(cm) {
  (measure_sens(cm) + measure_spec(cm)) / 2
}

measure_f1 <- function(cm) {
  check_confusion(cm)
  divide_safely(
    2 * cm[["tp"]],
    2 * cm[["tp"]] + cm[["fp"]] + cm[["fn"]]
  )
}

measure_mcc <- function(cm) {
  check_confusion(cm)
  tp <- cm[["tp"]]
  tn <- cm[["tn"]]
  fp <- cm[["fp"]]
  fn <- cm[["fn"]]
  # as.double(): the integer product overflows from roughly n > 430
  denominator <- sqrt(
    as.double(tp + fp) *
      as.double(tp + fn) *
      as.double(tn + fp) *
      as.double(tn + fn)
  )
  numerator <- as.double(tp) * as.double(tn) - as.double(fp) * as.double(fn)
  # undefined when a row or column of the table is empty; 0 by convention
  if (denominator == 0) 0 else numerator / denominator
}
