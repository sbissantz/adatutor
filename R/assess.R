#' Assess classification performance
#'
#' Computes performance measures from true labels and continuous scores. Most
#' measures come from one confusion matrix, made by cutting the scores at
#' `threshold`. The rest (`auroc`, `auprc` and the `patk` family) use the
#' whole ranking of the scores, so a confusion matrix cannot give them.
#'
#' @section Use scores, not class labels:
#' `score` must be continuous. Class labels have only two values, so the ROC
#' curve shrinks to a single point and `auroc` quietly becomes balanced
#' accuracy. With [predict.adaboost()], use `type = "margin"`.
#'
#' @section Measures:
#' At the threshold, from the counts `tp`, `tn`, `fp` and `fn`:
#' * `sens`: TP / (TP + FN). Also called true positive rate, recall or hit
#'   rate.
#' * `spec`: TN / (TN + FP). Also called true negative rate. 1 - `spec` is the
#'   false positive rate, or false-alarm rate.
#' * `ppv`: TP / (TP + FP). Also called precision.
#' * `npv`: TN / (TN + FN).
#' * `acc`: (TP + TN) / (TP + TN + FP + FN).
#' * `bacc`: (`sens` + `spec`) / 2, the same as (J + 1) / 2 with Youden's J.
#' * `f1`: 2TP / (2TP + FP + FN), the harmonic mean of `ppv` and `sens`. The
#'   same formula as the Dice coefficient.
#' * `mcc`: \eqn{(TP \cdot TN - FP \cdot FN) /
#'   \sqrt{(TP + FP)(TP + FN)(TN + FP)(TN + FN)}}{(TP x TN - FP x FN) /
#'   sqrt((TP + FP)(TP + FN)(TN + FP)(TN + FN))}. This is the phi coefficient,
#'   the correlation between true and predicted labels (Chicco & Jurman, 2020).
#'
#' Over the ranking:
#' * `auroc`: the chance that a random positive scores higher than a random
#'   negative, with ties counting half. Also called the c statistic, or
#'   Vargha and Delaney's (2000) A.
#' * `auprc`: the area under the precision-recall curve, estimated as in Davis
#'   and Goadrich (2006). This is not the same as average precision.
#' * `patk_3`, `patk_5`, `patk`: the share of positives among the top 3, the
#'   top 5 and the top n+ scores, where n+ is the number of positives. See
#'   [patk()].
#'
#' The synonyms follow Fawcett (2006).
#'
#' @section Baselines:
#' A random score gets an `auroc` of 0.5, but an `auprc` near the base rate,
#' not 0.5. `auroc` and `bacc` ignore the base rate; `ppv` and `mcc` do not.
#'
#' @section Missing values:
#' A measure is `NA` when the data cannot support it. That happens to `auroc`
#' and `auprc` when only one class is present, to `ppv` when nothing is
#' predicted positive, to `npv` when nothing is predicted negative, and to
#' `patk` when `k` is larger than the number of positives. `f1` is 0 when
#' nothing is predicted positive but positives exist. `mcc` is 0 when its
#' denominator is 0.
#'
#' @param x For the default method, the true labels: a two-level factor (the
#'   second level is the positive class), 0/1, -1/1, or logical. For the
#'   `confusion` method, the result of [confusion()].
#' @param score Continuous scores, one per observation, such as an AdaBoost
#'   margin or a predicted probability.
#' @param threshold The cutoff for `score`. Use 0 for margins (the default)
#'   and 0.5 for probabilities.
#' @param k The budgets for `patk`. By default 3, 5 and the number of
#'   positives. Budgets you pass are named by their value, e.g. `patk_10`.
#' @param digits The number of decimals `print()` shows.
#' @param ... Ignored.
#'
#' @return A named numeric vector of class `assessment`. By default it has 17
#'   measures, in the order listed above; the `confusion` method returns the
#'   first 12. `print()` shows them grouped by cut and rounds only the
#'   display.
#'
#' @family performance measures
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
#' Fawcett, T. (2006). An introduction to ROC analysis. *Pattern Recognition
#' Letters*, 27(8), 861-874.
#'
#' Grau, J., Grosse, I., & Keilwagen, J. (2015). PRROC: computing and
#' visualizing precision-recall and receiver operating characteristic curves in
#' R. *Bioinformatics*, 31(15), 2595-2597.
#'
#' Vargha, A., & Delaney, H. D. (2000). A critique and improvement of the CL
#' common language effect size statistics of McGraw and Wong. *Journal of
#' Educational and Behavioral Statistics*, 25(2), 101-132.
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
#' assess(test$replicate, margin)
#'
#' # fewer decimals; the stored values stay exact
#' print(assess(test$replicate, margin), digits = 2)
#'
#' # the same measures from a confusion matrix, without the ranking ones
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

  # cut once; the confusion method does every measure a matrix supports
  cm <- confusion(y, score, threshold)

  # two small fixed budgets, then R-precision, which keeps the name `patk`
  if (is.null(k)) {
    k <- c(3L, 5L, sum(y == 1))
    k_names <- c("patk_3", "patk_5", "patk")
  } else {
    k_names <- paste0("patk_", k)
  }
  at_k <- stats::setNames(unname(patk(y, score, k)), k_names)

  measures <- c(
    unclass(assess(cm)),
    auroc = auroc(y, score),
    auprc = auprc(y, score),
    at_k
  )
  structure(measures, class = "assessment", threshold = threshold, k = k)
}

#' @rdname assess
#' @export
assess.confusion <- function(x, ...) {
  # one function per measure, so each formula is written once
  measures <- c(
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
  structure(measures, class = "assessment", threshold = attr(x, "threshold"))
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

  # four cells per line; a continued block leaves its label blank
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
    style_ansi(padded, ansi_note)
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
      style_ansi(header[1], ansi_note),
      "  ",
      style_ansi(trimws(header[2]), ansi_note)
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
#' Cuts `score` at `threshold` and counts the four cells of the 2x2 table:
#' true positives, true negatives, false positives and false negatives.
#' [assess()] computes every threshold measure from these counts.
#'
#' @param actual The true labels: a two-level factor (the second level is the
#'   positive class), 0/1, -1/1, or logical.
#' @param score Continuous scores, one per observation.
#' @param threshold The cutoff for `score`. Use 0 for margins (the default)
#'   and 0.5 for probabilities. Probabilities cut at 0 would all count as
#'   positive, so this case gives a warning.
#' @param x An object of class `confusion`.
#' @param ... Ignored.
#'
#' @return A named integer vector of class `confusion` with `tp`, `tn`, `fp`
#'   and `fn`. The threshold is stored as an attribute.
#'
#' @family performance measures
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
  cat(
    style_ansi(label_threshold(attr(x, "threshold")), ansi_note),
    "\n",
    sep = ""
  )
  cat(format_confusion(values, width), sep = "\n")
  invisible(x)
}

#' Divide, returning NA for a zero denominator
#'
#' `NA`, not `NaN`: the measure has no value for this data, rather than a
#' failed calculation. logo_cv() stores such cells as undefined.
#'
#' @noRd
divide_safely <- function(numerator, denominator) {
  if (denominator == 0) NA_real_ else numerator / denominator
}

#' Area under the ROC curve
#'
#' Computes the area under the receiver operating characteristic curve with
#' the PRROC package. It measures the ranking only, so it ignores the base
#' rate and any monotone transformation of `score`.
#'
#' Use continuous scores. Class labels turn this into balanced accuracy,
#' (`sens` + `spec`) / 2. With [predict.adaboost()], use `type = "margin"`.
#'
#' @inheritParams confusion
#'
#' @return A number, or `NA` if `actual` has only one class.
#'
#' @family performance measures
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
#' Computes the area under the precision-recall curve with the PRROC package,
#' using the Davis and Goadrich (2006) estimator.
#'
#' A useless model scores about the base rate here, not 0.5 as for [auroc()].
#' Compare each value with the base rate of its own data set.
#'
#' @inheritParams confusion
#'
#' @return A number, or `NA` if `actual` has only one class.
#'
#' @family performance measures
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
#' Computes the share of positives among the `k` highest scores: precision
#' under a budget, when you can act on only the top of the ranking.
#'
#' By default `k` is the number of positives (R-precision), which scales with
#' each data set. A fixed `k` means something different in a set of 10 than in
#' a set of 90.
#'
#' @section Rules:
#' * `k` larger than the number of positives gives `NA`. There are fewer
#'   positives than places, so even a perfect ranking could not reach 1.
#' * When the cut falls inside a group of tied scores, the group counts by its
#'   share of positives. The result is the expected precision under random
#'   tie-breaking, so row order cannot change it.
#' * Recall and F1 at `k` are not reported. With TP_k positives among the top
#'   `k`, they are TP_k / n+ and 2 TP_k / (k + n+): rescalings of precision at
#'   `k`. At the default `k` all three are equal.
#'
#' @inheritParams confusion
#' @param k One or more budgets. Defaults to the number of positives in
#'   `actual`.
#'
#' @return A number for a single `k`. For several, a vector named by `k`.
#'
#' @family performance measures
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

#' Measures from a confusion matrix
#'
#' Each takes the counts from confusion() and returns one number; the formulas
#' are listed in ?assess. They take the table, not raw data, so every measure
#' in a report shares one cutoff. Not exported: assess(cm) returns them all.
#'
#' @noRd
NULL

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
