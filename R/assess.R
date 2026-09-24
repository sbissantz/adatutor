#' @title Assess Classification Performance
#'
#' @description Computes a bundle of performance measures from a vector of true
#'   labels and a vector of continuous scores. Most measures are read off a
#'   single confusion matrix, obtained by cutting the score at
#'   \code{threshold}; three summarize the score's whole ranking instead.
#'
#' @details
#' \strong{\code{score} must be continuous, never class labels.} This is the
#' easiest mistake to make here and the most expensive. Hard labels take only
#' two values, which collapses the ROC curve to a single interior point whose
#' area is exactly \eqn{(\text{sens} + \text{spec}) / 2} -- so \code{auroc}
#' comes back as balanced accuracy, silently and without an error. With
#' \code{\link[=predict.adaboost]{predict()}}, that means
#' \code{type = "margin"} rather than \code{type = "class"}.
#'
#' \strong{Why \code{score} and not \code{predicted}.} A margin computed on
#' training data and one computed on test data are both scores, so the argument
#' is deliberately neutral between prediction and retrodiction. That distinction
#' belongs in the variable you pass -- \code{yretro} against \code{ypred} -- not
#' in the parameter.
#'
#' \strong{How the two methods compose.} \code{assess()} cuts the score once with
#' \code{\link[adatutor]{confusion}} and hands the resulting matrix to
#' \code{assess()} again, which is where the threshold measures come from; the
#' ranking measures are then appended. So \code{assess(cm)} returns the
#' first twelve entries of \code{assess(x, score)}, and the two methods
#' differ only in what the input can support.
#'
#' \strong{Why the last ones are not matrix measures.} \code{auroc},
#' \code{auprc} and the \code{patk} family need the whole ranking, so no confusion matrix
#' at any single threshold can produce them -- a matrix has already thrown the
#' ordering away. That is not an inconsistency in the interface: it is the same
#' distinction that produces the collapse described above, made visible by which
#' method is able to compute what.
#'
#' The four cells of the confusion matrix are a sufficient statistic for every
#' threshold-based measure, so they are computed once and reused:
#'
#' \tabular{lll}{
#'   \tab \strong{actual = 1} \tab \strong{actual = 0} \cr
#'   \strong{predicted = 1} \tab TP \tab FP \cr
#'   \strong{predicted = 0} \tab FN \tab TN
#' }
#'
#' \strong{Measures at the threshold.}
#'
#' \deqn{\text{sens} = \frac{TP}{TP + FN} \qquad
#'       \text{spec} = \frac{TN}{TN + FP}}
#' \deqn{\text{ppv} = \frac{TP}{TP + FP} \qquad
#'       \text{npv} = \frac{TN}{TN + FN}}
#' \deqn{\text{acc} = \frac{TP + TN}{TP + TN + FP + FN} \qquad
#'       \text{bacc} = \frac{\text{sens} + \text{spec}}{2}}
#' \deqn{\text{f1} = \frac{2TP}{2TP + FP + FN}}
#' \deqn{\text{mcc} = \frac{TP \cdot TN - FP \cdot FN}
#'       {\sqrt{(TP+FP)(TP+FN)(TN+FP)(TN+FN)}}}
#'
#' \strong{Other names for the same quantities.} The names above are the ones
#' used in psychology and medicine. Machine learning names several of them
#' differently (Fawcett, 2006):
#'
#' \describe{
#'   \item{\code{sens}}{True positive rate (TPR), recall, hit rate.}
#'   \item{\code{spec}}{True negative rate (TNR). Its complement,
#'     \eqn{1 - \text{spec}}, is the false positive rate (FPR), or false-alarm
#'     rate.}
#'   \item{\code{ppv}}{Precision.}
#'   \item{\code{bacc}}{\eqn{(J + 1) / 2}, where \eqn{J = \text{sens} +
#'     \text{spec} - 1} is Youden's J.}
#'   \item{\code{f1}}{The harmonic mean of \code{ppv} and \code{sens}; the
#'     same formula as the Dice coefficient.}
#'   \item{\code{mcc}}{The phi coefficient: the Pearson correlation between
#'     the true and the predicted 0/1 labels (Chicco & Jurman, 2020).}
#'   \item{\code{auroc}}{The probability that a randomly chosen positive
#'     scores higher than a randomly chosen negative, ties counting half. This
#'     is the Mann-Whitney \eqn{U} divided by \eqn{n_1 n_0}, known as the
#'     c statistic and as Vargha and Delaney's (2000) \eqn{A}.}
#'   \item{\code{auprc}}{Not the same estimator as \emph{average precision},
#'     which sums precision in steps rather than interpolating, so the two can
#'     differ.}
#'   \item{\code{patk}}{Precision at \eqn{k}. With \eqn{k} the number of
#'     positives, it is R-precision, and there precision equals recall.
#'     \code{patk_3} and \code{patk_5} are the same measure at \eqn{k} = 3
#'     and 5.}
#' }
#'
#' \code{npv} has no common second name.
#'
#' \strong{Measures over the whole ranking.} \code{auroc} and \code{auprc} are
#' the areas under the ROC and precision-recall curves, both computed with the
#' \code{PRROC} package (Grau, Grosse & Keilwagen, 2015). For the
#' precision-recall curve we report the estimator of Davis & Goadrich (2006):
#' the curve is densified by interpolating non-linearly at the local skew
#' \eqn{(FP_B - FP_A) / (TP_B - TP_A)} between neighboring operating points,
#' and the area is then taken by the composite trapezoidal rule. Interpolating
#' \emph{linearly} in precision-recall space instead is incorrect and gives an
#' optimistic area, because precision does not change linearly with recall.
#' \code{patk} is the precision among the \code{k} highest-scoring
#' observations.
#'
#' \strong{Two baselines worth remembering.} A random score gives
#' \code{auroc} = 0.5, but \code{auprc} approximately equal to the
#' \emph{prevalence}, not 0.5 (slightly above it in small samples). And both
#' \code{auroc} and \code{bacc} are insensitive to the base rate, so neither
#' reveals a shift in prevalence between training and test data; \code{mcc} and
#' \code{ppv} do respond to it.
#'
#' \strong{Degenerate cases.} If \code{x} contains a single class,
#' \code{auroc} and \code{auprc} are \code{NA}. If no observation is predicted
#' positive, \code{ppv} is \code{NA}; if none is predicted negative, \code{npv}
#' is \code{NA}. \code{f1} is \code{0} rather than \code{NA} when nothing is
#' predicted positive but positives exist -- its denominator
#' \eqn{2TP + FP + FN} is still non-zero, and zero is the right score for
#' catching none of them; it is \code{NA} only when that denominator vanishes.
#' When the \code{mcc} denominator is zero the coefficient is undefined and is
#' reported as \code{0}, the usual convention.
#'
#' \strong{Printing.} The result has class \code{"assessment"}, whose
#' \code{print()} method shows the confusion matrix first, then the measures
#' grouped by the cut they rest on: at the threshold, among the top \code{k},
#' and over all thresholds. Each value is labeled with its name, and each
#' \code{patk} with its budget (\code{k=5} is \code{out[["patk_5"]]}).
#' Values that are \code{NA} because \code{k} exceeds the number of
#' positives are left out. Counts print as integers and the rest to
#' \code{digits} decimals. The stored values are never rounded, so
#' \code{out[["auroc"]]} is exact; indexing also drops the class. Labels are
#' bold teal in a live console only, never in a knitr document or captured
#' output.
#'
#' @param x What to score. For the default method, the true labels: either a
#'   factor with two levels (the second level is taken as the positive class), a
#'   numeric vector of 0/1, a numeric vector of -1/1, or a logical vector. For
#'   the \code{confusion} method, an object from
#'   \code{\link[adatutor]{confusion}}.
#'
#' @param score A numeric vector of continuous scores, one per observation --
#'   an AdaBoost margin or a predicted probability, not a class label. See the
#'   details. For \code{\link[=predict.adaboost]{predict()}} use
#'   \code{type = "margin"}.
#'
#' @param ... Ignored.
#'
#' @param threshold The cutoff applied to \code{score} to obtain predicted
#'   classes. Defaults to 0, the boundary for AdaBoost margins. Pass 0.5 when
#'   \code{score} holds probabilities.
#'
#' @param k The budgets for \code{patk}. Defaults to 3, 5 and the number of
#'   positive cases in \code{x}, reported as \code{patk_3}, \code{patk_5}
#'   and \code{patk}. The last is R-precision, which scales itself across test
#'   sets of different sizes; the two small fixed budgets answer \dQuote{how
#'   good are the model's few favorites?}. A \code{k} above the number of
#'   positives gives \code{NA} (see \code{\link[adatutor]{patk}}), so the
#'   names and length never depend on the data. Budgets you pass are reported
#'   as \code{patk_<k>}.
#'
#' @param digits The number of decimals \code{print()} shows for the rates.
#'   Defaults to 3. Counts always print as integers.
#'
#' @return A named numeric vector of class \code{"assessment"}. With the
#'   default \code{k}, the default method returns seventeen entries, always in
#'   this order: \code{tp}, \code{tn}, \code{fp}, \code{fn}, \code{sens},
#'   \code{spec}, \code{ppv}, \code{npv}, \code{acc}, \code{bacc},
#'   \code{f1}, \code{mcc}, \code{auroc}, \code{auprc}, \code{patk_3},
#'   \code{patk_5}, \code{patk}. The \code{confusion} method returns the
#'   first twelve of them. The threshold and \code{k} travel along
#'   as attributes, for \code{print()}.
#'
#' @references
#' Chicco, D., & Jurman, G. (2020). The advantages of the Matthews correlation
#' coefficient (MCC) over F1 score and accuracy in binary classification
#' evaluation. \emph{BMC Genomics}, 21, 6.
#'
#' Davis, J., & Goadrich, M. (2006). The relationship between precision-recall
#' and ROC curves. \emph{Proceedings of the 23rd International Conference on
#' Machine Learning}, 233-240.
#'
#' Grau, J., Grosse, I., & Keilwagen, J. (2015). PRROC: computing and
#' visualizing precision-recall and receiver operating characteristic curves in
#' R. \emph{Bioinformatics}, 31(15), 2595-2597.
#'
#' Fawcett, T. (2006). An introduction to ROC analysis. \emph{Pattern
#' Recognition Letters}, 27(8), 861-874.
#'
#' Vargha, A., & Delaney, H. D. (2000). A critique and improvement of the CL
#' common language effect size statistics of McGraw and Wong. \emph{Journal of
#' Educational and Behavioral Statistics}, 25(2), 101-132.
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
#' fit <- adaboost(h, n_iter = 10, eta = 1, verbose = FALSE, input_checks = FALSE)
#' margin <- predict(
#'   fit,
#'   test[, prednms],
#'   type = "margin",
#'   verbose = FALSE,
#'   input_checks = FALSE
#' )
#'
#' assess(test$replicate, margin)
#'
#' # Fewer decimals; the stored values stay exact
#' print(assess(test$replicate, margin), digits = 2)
#'
#' # The same measures from a confusion matrix, without the ranking three
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
  y <- as_binary(x)
  score <- as.numeric(score)

  if (length(y) != length(score)) {
    stop(
      "`x` and `score` must have the same length.",
      call. = FALSE
    )
  }

  # Cut the score once, then let the confusion method do every measure a matrix
  # can support. The three below it are the ones a matrix cannot.
  cm <- confusion(y, score, threshold)

  # two small fixed budgets, then R-precision, which keeps the name `patk`
  if (is.null(k)) {
    k <- c(3L, 5L, sum(y == 1))
    k_names <- c("patk_3", "patk_5", "patk")
  } else {
    k_names <- paste0("patk_", k)
  }
  at_k <- stats::setNames(unname(patk(y, score, k)), k_names)

  out <- c(
    unclass(assess(cm)),
    auroc = auroc(y, score),
    auprc = auprc(y, score),
    at_k
  )
  structure(out, class = "assessment", threshold = threshold, k = k)
}

#' @rdname assess
#' @export
assess.confusion <- function(x, ...) {
  # Every measure goes through its own function, so each formula is written
  # once and reads the way it does in the details above.
  out <- c(
    tp = x[["tp"]],
    tn = x[["tn"]],
    fp = x[["fp"]],
    fn = x[["fn"]],
    sens = confusion_sens(x),
    spec = confusion_spec(x),
    ppv = confusion_ppv(x),
    npv = confusion_npv(x),
    acc = confusion_acc(x),
    bacc = confusion_bacc(x),
    f1 = confusion_f1(x),
    mcc = confusion_mcc(x)
  )
  # pass NULL on for a hand-built vector: plain header
  structure(out, class = "assessment", threshold = attr(x, "threshold"))
}

#' @rdname assess
#' @export
print.assessment <- function(x, digits = 3, ...) {
  v <- unclass(x)
  attributes(v) <- list(names = names(x))
  counts <- c("tp", "tn", "fp", "fn")
  at_cut <- c("sens", "spec", "ppv", "npv", "acc", "bacc", "f1", "mcc")
  areas <- c("auroc", "auprc")

  # fall back when arithmetic has changed the names
  if (!all(c(counts, at_cut) %in% names(v))) {
    print(v)
    return(invisible(x))
  }

  blocks <- list(
    list(label = threshold_label(attr(x, "threshold")), vals = v[at_cut])
  )
  k <- attr(x, "k")
  at_k <- v[grepl("^patk(_|$)", names(v))]
  if (length(at_k) && length(at_k) == length(k)) {
    names(at_k) <- paste0("k=", k)
    # drop what is out of reach, and a default that repeats 3 or 5
    at_k <- at_k[!is.na(at_k) & !duplicated(k)]
    if (length(at_k)) {
      blocks <- c(blocks, list(list(label = "Among top k", vals = at_k)))
    }
  }
  if (all(areas %in% names(v))) {
    blocks <- c(blocks, list(list(label = "Over thresholds", vals = v[areas])))
  }

  # four cells per line; a continued block leaves its label blank
  rows <- list()
  for (b in blocks) {
    chunks <- split(b$vals, ceiling(seq_along(b$vals) / 4))
    for (i in seq_along(chunks)) {
      rows[[length(rows) + 1L]] <- list(
        label = if (i == 1L) b$label else "",
        vals = chunks[[i]]
      )
    }
  }

  labels <- vapply(rows, `[[`, character(1), "label")
  width <- max(nchar(c(labels, "predicted 1"))) + 2L
  cat(confusion_lines(v, width), sep = "\n")
  cat("\n")

  fmt <- function(z) ifelse(is.na(z), "NA", formatC(z, format = "f", digits = digits))
  val_width <- max(nchar(fmt(unlist(lapply(rows, `[[`, "vals")))))
  name_width <- vapply(
    1:4,
    function(j) {
      max(0L, vapply(
        rows,
        function(r) if (length(r$vals) >= j) nchar(names(r$vals)[j]) else 0L,
        integer(1)
      ))
    },
    integer(1)
  )
  for (r in rows) {
    cells <- sprintf(
      "%-*s %*s",
      name_width[seq_along(r$vals)],
      names(r$vals),
      val_width,
      fmt(r$vals)
    )
    cat(pad_label(r$label, width), paste(cells, collapse = "  "), "\n", sep = "")
  }
  invisible(x)
}

threshold_label <- function(threshold) {
  if (is.null(threshold)) "At threshold" else paste("At threshold", threshold)
}

# pad first, then style, so the escape bytes cannot shift the columns
pad_label <- function(label, width) {
  padded <- formatC(label, width = -width)
  if (nzchar(label)) {
    ansi_style(padded, ansi_note)
  } else {
    padded
  }
}

confusion_lines <- function(cm, width) {
  cell <- paste(c("tp", "fp", "fn", "tn"), formatC(cm[c("tp", "fp", "fn", "tn")], format = "d"))
  cw <- max(nchar(c("actual 1", cell)))
  head <- formatC(c("actual 1", "actual 0"), width = -cw)
  c(
    paste0(
      strrep(" ", width),
      ansi_style(head[1], ansi_note),
      "  ",
      ansi_style(trimws(head[2]), ansi_note)
    ),
    paste0(pad_label("predicted 1", width), formatC(cell[1], width = -cw), "  ", cell[2]),
    paste0(pad_label("predicted 0", width), formatC(cell[3], width = -cw), "  ", cell[4])
  )
}

#' @title Confusion Matrix Counts
#'
#' @description Cuts \code{score} at \code{threshold} and returns the four
#'   counts of the 2x2 table. Those counts are a sufficient statistic for every
#'   threshold-based measure in \code{\link[adatutor]{assess}}, which is why it
#'   builds them once and derives the rest by arithmetic.
#'
#' @details The result carries the class \code{"confusion"}, which is what lets
#'   \code{assess(cm)} dispatch to the method that reads a matrix. Without the
#'   class it would be a bare named vector and the sub-interface would not
#'   exist: scoring is the umbrella, and a confusion matrix is one of the two
#'   things that can be scored.
#'
#'   \strong{The threshold trap.} A vector of probabilities cut at 0 -- the
#'   AdaBoost default -- puts every case in the positive class, since
#'   probabilities are never negative. The result is a confusion matrix with no
#'   predicted negatives at all, and an \code{npv} of \code{NA} downstream. The
#'   default is deliberately not inferred from the data, because a margin vector
#'   can sit inside the unit interval too and guessing would occasionally change
#'   a result without saying so. Passing an evident probability at
#'   \code{threshold = 0} raises a warning instead.
#'
#' @param actual The true labels. Either a factor with two levels (the second
#'   level is the positive class), a numeric vector of 0/1 or -1/1, or a logical
#'   vector. Converted internally, so the caller never has to write
#'   \code{as.integer(y) - 1}.
#'
#' @param score A numeric vector of continuous scores, one per observation.
#'
#' @param threshold The cutoff applied to \code{score}. Defaults to 0, the
#'   boundary for AdaBoost margins; pass 0.5 for probabilities. The default is
#'   deliberately not inferred from the data: a margin vector can sit inside
#'   the unit interval as well, so guessing would occasionally change a result without
#'   saying so. Instead, scoring an evident probability at 0 raises a warning.
#'
#' @return A named integer vector of class \code{"confusion"}: \code{tp},
#'   \code{tn}, \code{fp}, \code{fn}, with the \code{threshold} it was cut at
#'   as an attribute. Ordinary indexing still works, so \code{cm[["tp"]]}
#'   reads the count as before.
#'
#' @examples
#' confusion(c(1, 1, 0, 0), c(2, -1, 1, -2))
#'
#' @export
confusion <- function(actual, score, threshold = 0) {
  y <- as_binary(actual)
  # A probability scored at the margin cutoff puts every case in the positive
  # class, which is a real mistake and an easy one. Warn rather than guess:
  # switching the cutoff silently would be worse than the error it prevents,
  # since a margin vector can legitimately sit inside [0, 1] too. Values of
  # exactly 0/1 are excluded -- those are class labels, for which 0 is right.
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
  pred <- score > threshold
  structure(
    c(
      tp = sum(pred & y == 1),
      tn = sum(!pred & y == 0),
      fp = sum(pred & y == 0),
      fn = sum(!pred & y == 1)
    ),
    class = "confusion",
    threshold = threshold
  )
}

#' @rdname confusion
#' @param x An object of class \code{"confusion"}.
#' @param ... Ignored.
#' @export
print.confusion <- function(x, ...) {
  v <- unclass(x)
  attributes(v) <- list(names = names(x))
  width <- nchar("predicted 1") + 2L
  cat(ansi_style(threshold_label(attr(x, "threshold")), ansi_note), "\n", sep = "")
  cat(confusion_lines(v, width), sep = "\n")
  invisible(x)
}

#' @title Divide Without Turning Undefined Into NaN
#'
#' @description Not exported. Returns \code{NA} rather than \code{NaN} when the
#'   denominator is zero, which is what every measure in
#'   \code{\link[adatutor]{confusion_measures}} uses to guard its ratio.
#'
#' @details The distinction is the point. \code{NaN} says a calculation went
#'   wrong; \code{NA} says the quantity has no value for this data. A \code{ppv}
#'   computed where nothing was predicted positive is undefined, not broken, and
#'   the two should not look alike to whatever reads the result afterwards --
#'   \code{\link[adatutor]{lpocv}} stores those cells and reports them as
#'   undefined rather than as failures.
#'
#' @param num,den The numerator and denominator.
#'
#' @return A single number, or \code{NA_real_} when \code{den} is zero.
#'
#' @name divide_safely
#'
#' @keywords internal
divide_safely <- function(num, den) {
  if (den == 0) NA_real_ else num / den
}

#' @title Area Under the ROC Curve
#'
#' @description The area under the receiver operating characteristic curve,
#'   computed with the \code{PRROC} package. Scores a \emph{ranking}: it is
#'   unchanged by any monotone transform of \code{score}, and it does not depend
#'   on the base rate.
#'
#' @details Feed this hard class labels and it silently becomes balanced
#'   accuracy -- a two-valued score gives a curve with one interior point, whose
#'   area is exactly \eqn{(\mathrm{sens} + \mathrm{spec})/2}. Use
#'   \code{predict(type = "margin")}, not \code{"class"}.
#'
#' @param actual The true labels. Either a factor with two levels (the second
#'   level is the positive class), a numeric vector of 0/1 or -1/1, or a logical
#'   vector. Converted internally, so the caller never has to write
#'   \code{as.integer(y) - 1}.
#'
#' @param score A numeric vector of continuous scores, one per observation.
#'
#' @return A single number, or \code{NA} if \code{actual} has only one class.
#'
#' @references Grau, J., Grosse, I., & Keilwagen, J. (2015). PRROC: Computing
#'   and visualizing precision-recall and receiver operating characteristic
#'   curves in R. \emph{Bioinformatics}.
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
  out <- PRROC::roc.curve(scores.class0 = score, weights.class0 = y)$auc
  if (is.nan(out)) NA_real_ else out
}

#' @title Area Under the Precision-Recall Curve
#'
#' @description The area under the precision-recall curve, computed with the
#'   \code{PRROC} package using the Davis & Goadrich estimator.
#'
#' @details Unlike \code{\link[adatutor]{auroc}}, whose baseline is always 0.5,
#'   a worthless model scores approximately the \emph{prevalence} here. Compare
#'   values against the base rate of the set they were computed on, never against
#'   0.5, and be careful comparing across sets whose base rates differ.
#'
#'   The curve is interpolated non-linearly between operating points rather than
#'   joined by straight lines, which would overstate the area (Davis & Goadrich,
#'   2006).
#'
#' @param actual The true labels. Either a factor with two levels (the second
#'   level is the positive class), a numeric vector of 0/1 or -1/1, or a logical
#'   vector. Converted internally, so the caller never has to write
#'   \code{as.integer(y) - 1}.
#'
#' @param score A numeric vector of continuous scores, one per observation.
#'
#' @return A single number, or \code{NA} if \code{actual} has only one class.
#'
#' @references Davis, J., & Goadrich, M. (2006). The relationship between
#'   precision-recall and ROC curves. \emph{Proceedings of the 23rd
#'   International Conference on Machine Learning}, 233-240.
#'
#' @references Grau, J., Grosse, I., & Keilwagen, J. (2015). PRROC: Computing
#'   and visualizing precision-recall and receiver operating characteristic
#'   curves in R. \emph{Bioinformatics}.
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
  # Davis & Goadrich estimator: the curve is densified by interpolating at the
  # local skew between neighboring operating points, then integrated by
  # trapezoid. Operating points are taken at distinct score values, which is
  # what makes this invariant to the order of tied observations.
  out <- PRROC::pr.curve(
    scores.class0 = score,
    weights.class0 = y
  )$auc.davis.goadrich
  if (is.nan(out)) NA_real_ else out
}

#' @title Precision Among the Top-Ranked Cases
#'
#' @description The share of true positives among the \code{k}
#'   highest-scoring observations -- precision under a budget, for when only the
#'   top of the ranking will be acted on.
#'
#' @details With the default \code{k} this is R-precision, which scales itself
#'   to each test set. That matters when sets differ in size: a fixed \code{k}
#'   is not comparable between a fold of 10 and a fold of 90.
#'
#'   \strong{Defined only for \eqn{k \le n_+}}, the number of positives.
#'   Above it even a perfect ranking cannot reach 1, since there are fewer
#'   positives than places, so the result is \code{NA} rather than a number
#'   with a hidden ceiling.
#'
#'   \strong{Ties.} When the cut at \code{k} falls inside a group of tied
#'   scores, the places left are shared across the group by its share of
#'   positives: the expected precision under random tie-breaking. Row order
#'   therefore cannot change the result. This matters for boosted stumps,
#'   whose margins take few distinct values.
#'
#'   \strong{Why no recall or F1 at \code{k}.} With \eqn{TP_k} the positives
#'   among the top \code{k}, recall at \code{k} is \eqn{TP_k / n_+} and F1 at
#'   \code{k} is \eqn{2 TP_k / (k + n_+)}. Both denominators are fixed before
#'   the model scores anything, so both are rescalings of precision at
#'   \code{k} and carry no further information. At the default \code{k} all
#'   three are equal.
#'
#' @param actual The true labels. Either a factor with two levels (the second
#'   level is the positive class), a numeric vector of 0/1 or -1/1, or a logical
#'   vector. Converted internally, so the caller never has to write
#'   \code{as.integer(y) - 1}.
#'
#' @param score A numeric vector of continuous scores, one per observation.
#'
#' @param k One or more budgets. Defaults to the number of positive cases in
#'   \code{actual}.
#'
#' @return A number for a single \code{k}; for several, a vector named by
#'   \code{k}. \code{NA} where \code{k} is below 1 or above the number of
#'   positives.
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
  out <- vapply(
    as.integer(k),
    function(kk) {
      # out of reach: fewer positives than places
      if (is.na(kk) || kk < 1L || kk > n_pos) {
        return(NA_real_)
      }
      v <- sort(score, decreasing = TRUE)[kk]
      above <- score > v
      # share the places left across the tie, as random tie-breaking would
      (sum(y[above]) + (kk - sum(above)) * mean(y[score == v])) / kk
    },
    numeric(1)
  )
  if (length(k) > 1L) {
    names(out) <- k
  }
  out
}

#' @title Measures Derived From a Confusion Matrix
#'
#' @description Each of these takes the four counts produced by
#'   \code{\link[adatutor]{confusion}} and returns a single number. They are
#'   documented together because they are the same object seen from different
#'   angles: once you have TP, TN, FP and FN, nothing else about the data is
#'   needed.
#'
#' @details
#' \deqn{\text{sensitivity} = \frac{TP}{TP + FN} \qquad
#'       \text{specificity} = \frac{TN}{TN + FP}}
#' \deqn{\text{ppv} = \frac{TP}{TP + FP} \qquad
#'       \text{npv} = \frac{TN}{TN + FN}}
#' \deqn{\text{accuracy} = \frac{TP + TN}{TP + TN + FP + FN} \qquad
#'       \text{balanced\_accuracy} = \frac{\text{sensitivity} +
#'       \text{specificity}}{2}}
#' \deqn{\text{f1} = \frac{2TP}{2TP + FP + FN}}
#' \deqn{\text{mcc} = \frac{TP \cdot TN - FP \cdot FN}
#'       {\sqrt{(TP+FP)(TP+FN)(TN+FP)(TN+FN)}}}
#'
#' \strong{Every one of these is conditional on a threshold.} The cut is chosen
#' once, in the call to \code{\link[adatutor]{confusion}}, and every measure
#' derived from that table inherits it. Move the cut and they all move together:
#'
#' \tabular{lrrrr}{
#'   \strong{threshold} \tab \strong{sens} \tab \strong{spec} \tab
#'     \strong{bacc} \tab \strong{auroc} \cr
#'   -0.5 \tab 0.583 \tab 0.556 \tab 0.569 \tab 0.676 \cr
#'   0.0 \tab 0.500 \tab 0.889 \tab 0.694 \tab 0.676 \cr
#'   0.5 \tab 0.083 \tab 0.889 \tab 0.486 \tab 0.676
#' }
#'
#' Passing the table rather than the raw data is what makes that visible, and it
#' also makes one mistake impossible: with a \code{(actual, score, threshold)}
#' signature nothing would stop you computing sensitivity at one cut and
#' specificity at another and reporting them side by side -- a table describing
#' no single classifier. Here every measure in a report necessarily describes the
#' same decision rule.
#'
#' \strong{Why these take a table and \code{auroc()} does not.} Everything here
#' is a function of four counts, so a confusion matrix is all it can possibly
#' need. \code{\link[adatutor]{auroc}} and \code{\link[adatutor]{auprc}} take
#' \code{actual} and \code{score} instead, because they summarize the whole
#' \emph{ranking} -- and a confusion matrix has already thrown the ranking away.
#' That difference is the reason hard class labels quietly turn an AUC into a
#' balanced accuracy: cut the score into two values and the ranking is gone.
#'
#' \strong{Names, and why these are internal.} Each function is named for what
#' it consumes rather than what it returns: \code{confusion_acc()} takes a
#' confusion matrix, as every function in this family does, and gives the value
#' \code{\link[adatutor]{assess}} labels \code{acc}. They are not exported,
#' because \code{assess(cm)} already returns all of them at once and
#' \code{assess(cm)[["acc"]]} is the one-measure form -- a family of eight
#' one-line exports would compete with the verb rather than support it. They are
#' documented here for the formulas, and reachable as
#' \code{adatutor:::confusion_acc()} for anyone who wants the function itself.
#'
#' \strong{Undefined cases} return \code{NA} rather than \code{NaN}, except
#' \code{mcc}, which is 0 by convention when its denominator vanishes.
#'
#' @param cm A named vector of counts from \code{\link[adatutor]{confusion}},
#'   containing \code{tp}, \code{tn}, \code{fp} and \code{fn}.
#'
#' @return A single number.
#'
#' @examples
#' cm <- confusion(c(1, 1, 1, 0, 0, 0), c(2, 1, -1, 1, -1, -2))
#' cm
#'
#' # All of them at once, which is the exported route
#' assess(cm)
#'
#' # One of them
#' assess(cm)[["bacc"]]
#'
#' # The function itself, for anyone reading the formula off the source
#' adatutor:::confusion_mcc(cm)
#'
#' @name confusion_measures
NULL

#' @rdname confusion_measures
#' @keywords internal
confusion_sens <- function(cm) {
  check_confusion(cm)
  divide_safely(cm[["tp"]], cm[["tp"]] + cm[["fn"]])
}

#' @rdname confusion_measures
#' @keywords internal
confusion_spec <- function(cm) {
  check_confusion(cm)
  divide_safely(cm[["tn"]], cm[["tn"]] + cm[["fp"]])
}

#' @rdname confusion_measures
#' @keywords internal
confusion_ppv <- function(cm) {
  check_confusion(cm)
  divide_safely(cm[["tp"]], cm[["tp"]] + cm[["fp"]])
}

#' @rdname confusion_measures
#' @keywords internal
confusion_npv <- function(cm) {
  check_confusion(cm)
  divide_safely(cm[["tn"]], cm[["tn"]] + cm[["fn"]])
}

#' @rdname confusion_measures
#' @keywords internal
confusion_acc <- function(cm) {
  check_confusion(cm)
  divide_safely(
    cm[["tp"]] + cm[["tn"]],
    cm[["tp"]] + cm[["tn"]] + cm[["fp"]] + cm[["fn"]]
  )
}

#' @rdname confusion_measures
#' @keywords internal
confusion_bacc <- function(cm) {
  (confusion_sens(cm) + confusion_spec(cm)) / 2
}

#' @rdname confusion_measures
#' @keywords internal
confusion_f1 <- function(cm) {
  check_confusion(cm)
  divide_safely(
    2 * cm[["tp"]],
    2 * cm[["tp"]] + cm[["fp"]] + cm[["fn"]]
  )
}

#' @rdname confusion_measures
#' @keywords internal
confusion_mcc <- function(cm) {
  check_confusion(cm)
  tp <- cm[["tp"]]
  tn <- cm[["tn"]]
  fp <- cm[["fp"]]
  fn <- cm[["fn"]]
  # as.double() first: the counts are integers and their four-way product
  # overflows the integer range from roughly n > 430 onwards.
  denom <- sqrt(
    as.double(tp + fp) *
      as.double(tp + fn) *
      as.double(tn + fp) *
      as.double(tn + fn)
  )
  num <- as.double(tp) * as.double(tn) - as.double(fp) * as.double(fn)
  # undefined when a row or column of the table is empty; 0 by convention
  if (denom == 0) 0 else num / denom
}
