test_that("assess() reproduces a hand-built confusion matrix", {
  # 4 TP, 3 TN, 2 FP, 1 FN -- scores chosen so `score > 0` gives exactly that
  y <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 1)
  s <- c(1, 1, 1, 1, -1, -1, -1, 1, 1, -1)

  out <- assess(y, s)

  expect_equal(unname(out["tp"]), 4)
  expect_equal(unname(out["tn"]), 3)
  expect_equal(unname(out["fp"]), 2)
  expect_equal(unname(out["fn"]), 1)

  expect_equal(unname(out["sens"]), 4 / 5)
  expect_equal(unname(out["spec"]), 3 / 5)
  expect_equal(unname(out["ppv"]), 4 / 6)
  expect_equal(unname(out["npv"]), 3 / 4)
  expect_equal(unname(out["acc"]), 7 / 10)
  expect_equal(unname(out["bacc"]), (4 / 5 + 3 / 5) / 2)
  expect_equal(unname(out["f1"]), (2 * 4) / (2 * 4 + 2 + 1))

  mcc <- (4 * 3 - 2 * 1) / sqrt(6 * 5 * 5 * 4)
  expect_equal(unname(out["mcc"]), mcc)
})

test_that("assess() returns a stable named vector", {
  set.seed(1)
  y <- rbinom(40, 1, 0.5)
  s <- rnorm(40)
  out <- assess(y, s)

  nms <- c(
    "tp",
    "tn",
    "fp",
    "fn",
    "sens",
    "spec",
    "ppv",
    "npv",
    "acc",
    "bacc",
    "f1",
    "mcc",
    "auroc",
    "auprc",
    "patk_3",
    "patk_5",
    "patk"
  )
  # lpocv()/bootCI() will vapply() over this, so length and order must not drift
  expect_named(out, nms)
  expect_type(out, "double")
  expect_length(out, 17L)
})

test_that("hard class labels make auroc collapse to balanced accuracy", {
  # Regression test for the metric bug in the submitted manuscript: feeding
  # sign() output to an AUC call yields a two-valued score, whose ROC curve has
  # a single interior point and area exactly (sens + spec) / 2.
  set.seed(112)
  y <- rbinom(60, 1, 0.45)
  margin <- rnorm(60) + y

  hard <- assess(y, sign(margin))
  expect_equal(unname(hard["auroc"]), unname(hard["bacc"]))

  # the margin keeps the ranking the labels threw away
  soft <- assess(y, margin)
  expect_false(isTRUE(all.equal(unname(soft["auroc"]), unname(soft["bacc"]))))
  expect_gt(soft["auroc"], hard["auroc"])
})

test_that("auroc matches an independent Mann-Whitney statistic, ties included", {
  # Deliberately not a second call into PRROC: the AUC of a ranking equals the
  # Mann-Whitney U statistic, so this checks the number from the other side.
  mannwhitney <- function(y, s) {
    r <- rank(s)
    n1 <- sum(y == 1)
    n0 <- sum(y == 0)
    (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  }

  set.seed(2)
  for (i in 1:20) {
    n <- sample(20:120, 1)
    y <- rbinom(n, 1, 0.5)
    if (length(unique(y)) < 2) {
      next
    }
    s <- rnorm(n) + y
    expect_equal(unname(assess(y, s)["auroc"]), mannwhitney(y, s))
  }

  # AdaBoost margins from few stumps are heavily tied; rank() gives ties the
  # half credit the ROC curve does, so the two must still agree
  set.seed(3)
  y <- rbinom(50, 1, 0.5)
  s <- sample(c(-3, -1, 1, 3), 50, replace = TRUE)
  expect_equal(unname(assess(y, s)["auroc"]), mannwhitney(y, s))
})

test_that("auprc uses the Davis & Goadrich estimator", {
  y <- c(rep(1, 5), rep(0, 5))
  expect_equal(unname(assess(y, c(5:1, -1:-5))["auprc"]), 1)

  # The paper's own Figure 6 example: 433 positives, 56,164 negatives and a
  # single operating point. They report 0.031 for this estimator, and 0.50 for
  # the linear interpolation they argue against.
  y_fig6 <- c(rep(1, 433), rep(0, 56164))
  s_fig6 <- c(rep(2, 9), rep(0, 424), rep(0, 56164))
  expect_equal(
    unname(assess(y_fig6, s_fig6)["auprc"]),
    0.031,
    tolerance = 0.05
  )

  # A random score scores about the prevalence, not 0.5 -- that is the baseline
  # to remember for auprc. The estimate carries a small upward bias at finite n,
  # so this is a band around the prevalence rather than a point.
  set.seed(4)
  for (p in c(0.2, 0.5)) {
    ap <- replicate(200, {
      yy <- rbinom(100, 1, p)
      assess(yy, rnorm(100))["auprc"]
    })
    expect_gt(mean(ap), p - 0.05)
    expect_lt(mean(ap), p + 0.06)
  }
})

test_that("auprc does not depend on the order of tied observations", {
  # Regression test. Average precision, the estimator used before, sorted by
  # score and let ties fall in arbitrary order, so shuffling the input rows
  # moved the answer by as much as 0.36. AdaBoost margins are tied by
  # construction, so this has to be exact.
  set.seed(11)
  for (nlev in c(2, 4, 8)) {
    y <- rbinom(40, 1, 0.4)
    s <- sample(seq_len(nlev), 40, replace = TRUE)
    reference <- unname(assess(y, s)["auprc"])

    shuffled <- replicate(50, {
      i <- sample(length(y))
      unname(assess(y[i], s[i])["auprc"])
    })
    expect_equal(unique(shuffled), reference)
  }
})

test_that("assess() reports patk at 3, 5 and R-precision, and honors an explicit k", {
  y <- c(rep(1, 4), rep(0, 6))
  s <- c(10:7, 6:1) # perfectly separating

  # the default: two small budgets, then k = number of positives (4 here)
  out <- assess(y, s)
  expect_equal(out[["patk_3"]], 1)
  expect_equal(out[["patk"]], 1)
  expect_equal(attr(out, "k"), c(3, 5, 4))
  # five places but only four positives: out of reach, not a capped 4 / 5
  expect_true(is.na(out[["patk_5"]]))

  # an explicit k replaces the default and names each budget
  out <- assess(y, s, k = c(1, 2))
  expect_equal(unname(out[c("patk_1", "patk_2")]), c(1, 1))
  expect_false("patk" %in% names(out))
  expect_true(is.na(assess(y, s, k = 8)[["patk_8"]]))
})

test_that("label encodings are interchangeable", {
  set.seed(5)
  s <- rnorm(30)
  y01 <- rbinom(30, 1, 0.5)

  ref <- assess(y01, s)
  expect_equal(assess(as.logical(y01), s), ref)
  expect_equal(assess(2 * y01 - 1, s), ref) # -1/1
  expect_equal(
    assess(factor(y01, labels = c("failure", "success")), s),
    ref
  )

  expect_error(assess(factor(rep("a", 5)), rnorm(5)))
  expect_error(assess(c(1, 2, 3), rnorm(3)))
  expect_error(assess(y01, s[1:5]), "same length")
})

test_that("degenerate inputs give NA rather than NaN", {
  # single class: the ranking measures are undefined
  one <- assess(rep(1, 6), rnorm(6))
  expect_true(is.na(one[["auroc"]]))
  expect_true(is.na(one[["auprc"]]))

  # nothing predicted positive -> ppv undefined, mcc 0 by convention. f1 is 0,
  # not NA: its denominator 2TP + FP + FN is still 2, and catching none of the
  # positives genuinely scores zero.
  none <- assess(c(1, 1, 0, 0), c(-1, -2, -3, -4))
  expect_true(is.na(none[["ppv"]]))
  expect_equal(none[["f1"]], 0)
  expect_equal(none[["mcc"]], 0)

  # f1 is NA only when its denominator vanishes: no positives, none predicted
  empty <- assess(c(0, 0, 0, 0), c(-1, -2, -3, -4))
  expect_true(is.na(empty[["f1"]]))

  # everything predicted positive -> npv undefined
  all_pos <- assess(c(1, 1, 0, 0), c(1, 2, 3, 4))
  expect_true(is.na(all_pos[["npv"]]))
})

test_that("mcc survives large samples", {
  # Regression test. The confusion counts come back from sum() as integers, and
  # their four-way product in the mcc denominator overflows the integer range
  # from roughly n > 430, which silently turned mcc into NA.
  set.seed(6)
  n <- 5000
  y <- rbinom(n, 1, 0.4)
  s <- rnorm(n) + y

  out <- assess(y, s)
  expect_false(is.na(out[["mcc"]]))
  expect_gt(out[["mcc"]], 0)
  expect_lte(abs(out[["mcc"]]), 1)
})

test_that("the threshold argument moves the decision boundary", {
  y <- c(1, 1, 0, 0)
  p <- c(0.9, 0.6, 0.4, 0.1)

  at_half <- assess(y, p, threshold = 0.5)
  expect_equal(unname(at_half["acc"]), 1)

  # at 0 every case is predicted positive, so specificity collapses -- and
  # confusion() says so, which is the behavior tested below
  expect_warning(at_zero <- assess(y, p, threshold = 0), "threshold = 0.5")
  expect_equal(unname(at_zero["spec"]), 0)

  # thresholds are monotone in the count of predicted positives
  expect_gte(
    at_zero[["tp"]] + at_zero[["fp"]],
    at_half[["tp"]] + at_half[["fp"]]
  )
})

# ---- the individually exported measures ------------------------------------

test_that("the exported measures agree with the assess() bundle", {
  set.seed(1)
  y <- rbinom(60, 1, 0.4)
  s <- rnorm(60) + y
  g <- assess(y, s)

  expect_equal(auroc(y, s), unname(g[["auroc"]]))
  expect_equal(auprc(y, s), unname(g[["auprc"]]))
  expect_equal(patk(y, s), unname(g[["patk"]]))
  expect_equal(
    c(confusion(y, s)),
    c(tp = g[["tp"]], tn = g[["tn"]], fp = g[["fp"]], fn = g[["fn"]])
  )
})

test_that("they accept any label encoding, so callers need no `- 1`", {
  set.seed(2)
  y01 <- rbinom(40, 1, 0.5)
  s <- rnorm(40) + y01
  ref <- auroc(y01, s)

  expect_equal(auroc(as.logical(y01), s), ref)
  expect_equal(auroc(2 * y01 - 1, s), ref)
  expect_equal(auroc(factor(y01, labels = c("failure", "success")), s), ref)

  # and the same for the rest
  expect_equal(auprc(factor(y01, labels = c("a", "b")), s), auprc(y01, s))
  expect_equal(patk(factor(y01, labels = c("a", "b")), s), patk(y01, s))
  expect_equal(
    confusion(factor(y01, labels = c("a", "b")), s),
    confusion(y01, s)
  )
})

test_that("patk defaults to R-precision and refuses a budget out of reach", {
  y <- c(1, 1, 1, 0, 0)
  s <- c(5, 4, 1, 3, 2)
  # three positives -> the default budget is the top three
  expect_equal(patk(y, s), 2 / 3)
  expect_equal(patk(y, s, k = 2), 1)
  # more places than positives: even a perfect ranking could not reach 1
  expect_true(is.na(patk(y, s, k = 4)))
  expect_true(is.na(patk(y, s, k = 99)))
  expect_true(is.na(patk(y, s, k = 0)))
})

test_that("patk takes several budgets and names them", {
  y <- c(1, 1, 1, 0, 0)
  s <- c(5, 4, 1, 3, 2)
  out <- patk(y, s, k = c(1, 2, 3))
  expect_named(out, c("1", "2", "3"))
  expect_equal(unname(out), c(patk(y, s, 1), patk(y, s, 2), patk(y, s, 3)))
  # a single budget stays an unnamed number
  expect_null(names(patk(y, s, k = 2)))
})

test_that("patk shares a tied cut by the tie's positives, whatever the row order", {
  # one place, two tied candidates, one of them positive: expected 1/2
  expect_equal(patk(c(1, 0, 1), c(1, 1, 0), k = 1), 0.5)
  expect_equal(patk(c(0, 1, 1), c(1, 1, 0), k = 1), 0.5)

  # stumps give few distinct margins, so ties are the rule, not the exception
  set.seed(51)
  y <- rbinom(40, 1, 0.5)
  s <- round(rnorm(40), 1)
  perm <- sample(40)
  k <- c(1, 3, 5, sum(y))
  expect_equal(patk(y, s, k), patk(y[perm], s[perm], k))
})

test_that("auroc and auprc are NA on a single-class vector", {
  expect_true(is.na(auroc(rep(1, 6), rnorm(6))))
  expect_true(is.na(auprc(rep(0, 6), rnorm(6))))
})

test_that("auroc collapses onto balanced accuracy for hard labels", {
  # the same trap as assess(), reachable now through the exported function
  set.seed(3)
  y <- rbinom(50, 1, 0.45)
  m <- rnorm(50) + y
  expect_equal(auroc(y, sign(m)), unname(assess(y, sign(m))[["bacc"]]))
})

test_that("confusion() honors the threshold", {
  y <- c(1, 1, 0, 0)
  p <- c(0.9, 0.6, 0.4, 0.1)
  expect_equal(as.vector(confusion(y, p, threshold = 0.5)), c(2, 2, 0, 0))
  # at 0 everything is predicted positive, and the mismatch is flagged
  expect_warning(cm0 <- confusion(y, p, threshold = 0), "threshold = 0.5")
  expect_equal(as.vector(cm0), c(2, 0, 2, 0))
})

# ---- the confusion-matrix measures -----------------------------------------

test_that("the derived measures agree with the assess() bundle", {
  set.seed(11)
  y <- rbinom(80, 1, 0.4)
  s <- rnorm(80) + y
  g <- assess(y, s)
  cm <- confusion(y, s)

  expect_equal(confusion_sens(cm), unname(g[["sens"]]))
  expect_equal(confusion_spec(cm), unname(g[["spec"]]))
  expect_equal(confusion_ppv(cm), unname(g[["ppv"]]))
  expect_equal(confusion_npv(cm), unname(g[["npv"]]))
  expect_equal(confusion_acc(cm), unname(g[["acc"]]))
  expect_equal(confusion_bacc(cm), unname(g[["bacc"]]))
  expect_equal(confusion_f1(cm), unname(g[["f1"]]))
  expect_equal(confusion_mcc(cm), unname(g[["mcc"]]))
})

test_that("they reproduce a hand-computed table", {
  # 4 TP, 3 TN, 2 FP, 1 FN
  cm <- c(tp = 4, tn = 3, fp = 2, fn = 1)
  expect_equal(confusion_sens(cm), 4 / 5)
  expect_equal(confusion_spec(cm), 3 / 5)
  expect_equal(confusion_ppv(cm), 4 / 6)
  expect_equal(confusion_npv(cm), 3 / 4)
  expect_equal(confusion_acc(cm), 7 / 10)
  expect_equal(confusion_bacc(cm), (4 / 5 + 3 / 5) / 2)
  expect_equal(confusion_f1(cm), 8 / 11)
  expect_equal(confusion_mcc(cm), (4 * 3 - 2 * 1) / sqrt(6 * 5 * 5 * 4))
})

test_that("undefined cases give NA, and mcc gives 0 by convention", {
  # nothing predicted positive
  cm <- c(tp = 0, tn = 2, fp = 0, fn = 2)
  expect_true(is.na(confusion_ppv(cm)))
  expect_equal(confusion_f1(cm), 0)
  expect_equal(confusion_mcc(cm), 0)

  # nothing predicted negative
  cm2 <- c(tp = 2, tn = 0, fp = 2, fn = 0)
  expect_true(is.na(confusion_npv(cm2)))

  # no positives at all -- f1's denominator vanishes
  cm3 <- c(tp = 0, tn = 4, fp = 0, fn = 0)
  expect_true(is.na(confusion_f1(cm3)))
})

test_that("mcc survives counts whose four-way product overflows integers", {
  # the product of the four margins passes .Machine$integer.max well before
  # the counts themselves look large
  cm <- c(tp = 300L, tn = 300L, fp = 300L, fn = 300L)
  expect_equal(confusion_mcc(cm), 0)
  cm2 <- c(tp = 500L, tn = 500L, fp = 100L, fn = 100L)
  expect_false(is.na(confusion_mcc(cm2)))
  expect_gt(confusion_mcc(cm2), 0)
})

test_that("they reject anything that is not a confusion matrix", {
  expect_error(confusion_sens(c(a = 1, b = 2)), "output of confusion")
  expect_error(confusion_spec(1:4), "output of confusion")
  expect_error(confusion_mcc(list(tp = 1)), "output of confusion")
})

test_that("confusion() warns when a probability is scored at the margin cutoff", {
  y <- c(1, 1, 0, 0)
  p <- c(0.9, 0.6, 0.4, 0.1)

  expect_warning(cm <- confusion(y, p), "use `threshold = 0.5`")
  # and the warning is earned: everything lands in the positive class
  expect_equal(as.vector(cm), c(2, 0, 2, 0))

  # no warning once the cutoff matches the scale
  expect_silent(confusion(y, p, threshold = 0.5))

  # 0/1 class labels are not probabilities; 0 is the right cutoff for them
  expect_silent(confusion(y, c(1, 1, 0, 0)))

  # an ordinary margin straddles zero and says nothing
  expect_silent(confusion(y, c(1.2, 0.3, -0.4, -1.1)))
})

test_that("the threshold reaches every derived measure from one place", {
  # the point of passing a table: no way to mix cutoffs between measures
  set.seed(21)
  y <- rbinom(60, 1, 0.5)
  s <- rnorm(60) + y

  low <- confusion(y, s, threshold = -0.5)
  high <- confusion(y, s, threshold = 0.5)

  expect_gt(confusion_sens(low), confusion_sens(high))
  expect_lt(confusion_spec(low), confusion_spec(high))
  # the ranking measures cannot move, having no cutoff to move
  expect_equal(auroc(y, s), auroc(y, s))
  expect_false(isTRUE(all.equal(confusion_bacc(low), confusion_bacc(high))))
})

# ---- the synonyms in the documentation --------------------------------------

test_that("the documented synonyms hold as identities", {
  # each line here is a claim in ?assess, so the docs cannot drift from the code
  set.seed(31)
  y <- rbinom(90, 1, 0.4)
  s <- rnorm(90) + y
  g <- assess(y, s)

  # mcc is the phi coefficient: Pearson's r of true and predicted 0/1
  expect_equal(g[["mcc"]], cor(y, as.integer(s > 0)))
  # f1 is the harmonic mean of precision and recall
  expect_equal(g[["f1"]], 2 * g[["ppv"]] * g[["sens"]] / (g[["ppv"]] + g[["sens"]]))
  # bacc is (J + 1) / 2 with Youden's J
  j <- g[["sens"]] + g[["spec"]] - 1
  expect_equal(g[["bacc"]], (j + 1) / 2)
  # at the default k, precision among the top k equals recall among them
  top <- order(s, decreasing = TRUE)[seq_len(sum(y))]
  expect_equal(g[["patk"]], sum(y[top]) / sum(y))
})

# ---- printing ----------------------------------------------------------------

test_that("assess() keeps full precision and carries its cut for print()", {
  set.seed(41)
  y <- rbinom(30, 1, 0.4)
  s <- rnorm(30) + y
  out <- assess(y, s)

  expect_s3_class(out, "assessment")
  expect_equal(attr(out, "threshold"), 0)
  expect_equal(attr(out, "k"), c(3, 5, sum(y)))
  # indexing drops the class and returns the exact value
  expect_equal(out[["auroc"]], auroc(y, s))
  expect_false(inherits(out["auroc"], "assessment"))
})

test_that("print.assessment() shows the matrix, then one line per cut", {
  y <- c(rep(1, 12), rep(0, 9))
  set.seed(41)
  s <- rnorm(21) + y
  out <- capture.output(print(assess(y, s)))

  expect_match(out[1], "^ +actual 1  actual 0$")
  expect_match(out[2], "^predicted 1 +tp \\d+ +fp \\d+$")
  expect_match(out[3], "^predicted 0 +fn \\d+ +tn \\d+$")
  expect_identical(out[4], "")
  expect_length(out, 8L)
  expect_identical(
    trimws(substr(out[5:8], 1, 16)),
    c("At threshold 0", "", "Among top k", "Over thresholds")
  )
  expect_match(out[7], "k=3 .*k=5 .*k=12 ")
  expect_false(any(grepl("attr(", out, fixed = TRUE)))
  # nothing colored outside a live console
  expect_false(any(grepl("\033[", out, fixed = TRUE)))
  # every rate has three decimals
  rates <- regmatches(out[5:8], gregexpr("\\d+\\.\\d+", out[5:8]))
  expect_true(all(grepl("^\\d\\.\\d{3}$", unlist(rates))))

  two <- capture.output(print(assess(y, s), digits = 2))
  rates <- regmatches(two[5:8], gregexpr("\\d+\\.\\d+", two[5:8]))
  expect_true(all(grepl("^\\d\\.\\d{2}$", unlist(rates))))
})

test_that("print.assessment() drops a budget out of reach and a repeated k", {
  # three positives: patk_5 is NA and the default k repeats k = 3
  y <- c(1, 1, 1, 0, 0, 0, 0)
  s <- c(3, 2, -1, 1, -2, -3, -4)
  out <- capture.output(print(assess(y, s)))
  line <- grep("^Among top k", out, value = TRUE)
  expect_match(line, "^Among top k +k=3 +0\\.667$")
})

test_that("print.assessment() wraps a long list of budgets after four", {
  y <- c(rep(1, 8), rep(0, 4))
  s <- seq(12, 1)
  out <- capture.output(print(assess(y, s, k = 1:6)))
  i <- grep("^Among top k", out)
  expect_match(out[i], "k=4 +1\\.000$")
  expect_match(out[i + 1], "^ +k=5 +1\\.000 +k=6 +1\\.000$")
})

test_that("assess(cm) prints no ranking lines, and print() returns invisibly", {
  cm <- confusion(c(1, 1, 0, 0), c(2, -1, 1, -2))
  out <- capture.output(res <- withVisible(print(assess(cm))))

  expect_length(out, 6L)
  expect_match(out[5], "^At threshold 0 ")
  expect_false(res$visible)

  # a hand-built table carries no threshold, so the label stays plain
  hand <- structure(c(tp = 4, tn = 3, fp = 2, fn = 1), class = "confusion")
  expect_match(capture.output(print(assess(hand)))[5], "^At threshold  ")
})

test_that("print.confusion() names its threshold above the matrix", {
  out <- capture.output(print(confusion(c(1, 1, 0, 0), c(2, -1, 1, -2), 0.5)))
  expect_identical(out[1], "At threshold 0.5")
  expect_match(out[3], "^predicted 1 +tp 1 +fp 1$")
  expect_length(out, 4L)
})
