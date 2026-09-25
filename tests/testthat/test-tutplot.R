# Every call here draws. Without a device of our own R would open its default
# one and leave an `Rplots.pdf` behind in tests/testthat/, so each test opens a
# null device first and closes it on exit.
stump <- function(formula = replicate ~ power.o + n.o, weights = NULL) {
  args <- list(
    formula = formula,
    data = altmejd_splits$train,
    method = "class",
    maxdepth = 1,
    maxsurrogate = 0,
    parms = list(split = "gini")
  )
  # only when asked: rpart() evaluates `weights` in the caller, so a NULL
  # passed through finds stats::weights instead
  if (!is.null(weights)) {
    args$weights <- weights
  }
  do.call(rpart::rpart, args)
}

test_that("tutplot_cstump() returns the numbers Figure 2's caption states", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # the caption: power.o at 0.9305, leaves of 66 and 41, about 62% and 38%
  facts <- tutplot_cstump(stump())

  expect_identical(facts$variable, "power.o")
  expect_equal(round(facts$cutpoint, 4), 0.9305)
  expect_equal(unname(facts$leaves), c(66, 41))
  expect_equal(round(100 * facts$leaves / sum(facts$leaves)), c(62, 38))
})

test_that("the weighted stump is drawn by passing its fit, not its weights", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # fig:cstumpwght is the same function given the weighted fit and extra = 100
  set.seed(112)
  d_raw <- stats::runif(nrow(altmejd_splits$train))
  d <- d_raw / sum(d_raw)
  fit <- stump(
    replicate ~ power.o + effect_size.o + n.o + p_value.o,
    weights = d
  )

  facts <- tutplot_cstump(fit, extra = 100)

  # reweighting moves the split to power.o; the caption's 63.48 / 36.52 are
  # weighted shares, not row counts, which is why they differ from 62 / 38
  expect_identical(facts$variable, "power.o")
  expect_equal(round(facts$cutpoint, 4), 0.9305)
  lo <- altmejd_splits$train$power.o < 0.9305
  expect_equal(round(100 * c(sum(d[lo]), sum(d[!lo])), 2), c(63.48, 36.52))
})

test_that("file = NULL draws on the current device and writes nothing", {
  wd <- file.path(tempdir(), "tutplot-nofile")
  dir.create(wd, showWarnings = FALSE)
  old <- setwd(wd)
  grDevices::pdf(NULL)
  on.exit(
    {
      grDevices::dev.off()
      setwd(old)
      unlink(wd, recursive = TRUE)
    },
    add = TRUE
  )

  tutplot_cstump(stump())

  expect_identical(list.files(wd), character(0))
})

test_that("a path writes one PDF and leaves the device stack intact", {
  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f), add = TRUE)

  before <- length(grDevices::dev.list())
  tutplot_cstump(stump(), file = f)

  expect_true(file.exists(f))
  expect_identical(length(grDevices::dev.list()), before)
})

test_that("`extra` reaches rpart.plot()", {
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext not available")

  f102 <- tempfile(fileext = ".pdf")
  f100 <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(f102, f100)), add = TRUE)

  tutplot_cstump(stump(), file = f102, extra = 102)
  tutplot_cstump(stump(), file = f100, extra = 100)

  txt <- function(p) system2("pdftotext", c(p, "-"), stdout = TRUE)
  # 102 prints the counts behind each node, 100 prints percentages only
  expect_false(identical(txt(f102), txt(f100)))
  expect_true(any(grepl("49 / 66", txt(f102), fixed = TRUE)))
  expect_false(any(grepl("49 / 66", txt(f100), fixed = TRUE)))
})

test_that("tutplot_cstump() refuses something that is not an rpart fit", {
  expect_error(tutplot_cstump(altmejd_splits$train), "rpart object")
})

test_that("tutplot_gini() returns the curve Figure 4's caption describes", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  gini <- tutplot_gini()

  # "highest when the classes are equally represented ... proportion 0.5"
  expect_equal(gini$peak, 0.5)
  expect_equal(max(gini$y), 0.5)
  # "zero if there are only instances of one type"
  expect_equal(gini$y[gini$x == 1], 0)
  # and symmetric about the peak, which is what makes it read as a hill
  expect_equal(gini$y[gini$x == 0.2], gini$y[gini$x == 0.8])
})

test_that("tutplot_gini() takes no data and leaves par() as it found it", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  before <- graphics::par(c("mar", "mgp", "tcl", "cex.axis"))
  tutplot_gini()
  expect_identical(graphics::par(c("mar", "mgp", "tcl", "cex.axis")), before)

  # every argument has a default, so the figure needs nothing from the caller
  expect_true(all(nzchar(sapply(formals(tutplot_gini), deparse))))
})

test_that("tutplot_gini() honors `x_seq`", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  coarse <- tutplot_gini(x_seq = seq(0, 1, by = 0.25))

  expect_identical(coarse$x, seq(0, 1, by = 0.25))
  # the closed form still holds at the ends and the peak
  expect_equal(coarse$y[coarse$x == 0], 0)
  expect_equal(coarse$y[coarse$x == 1], 0)
  expect_equal(coarse$peak, 0.5)
  # and the default is untouched by having asked for something else
  expect_identical(tutplot_gini()$x, seq(0.01, 1, by = 0.01))
})

test_that("tutplot_gini() rejects an x_seq that is not a proportion", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(tutplot_gini(x_seq = c(0.5, 1.5)), "between 0 and 1")
  expect_error(tutplot_gini(x_seq = c(-0.1, 0.5)), "between 0 and 1")
  expect_error(tutplot_gini(x_seq = "half"))
})

test_that("tutplot_gini() honors `lwd`", {
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext not available")

  thin <- tempfile(fileext = ".pdf")
  thick <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(thin, thick)), add = TRUE)

  tutplot_gini(lwd = 1, file = thin)
  tutplot_gini(lwd = 6, file = thick)

  # the curve is the only heavy stroke on the page, so a wider one is a bigger
  # file; the axis text is identical either way
  expect_gt(file.size(thick), file.size(thin))
  txt <- function(p) system2("pdftotext", c(p, "-"), stdout = TRUE)
  expect_identical(txt(thin), txt(thick))
})

test_that("tutplot_updatefactor() evaluates both points from the curve's own alpha", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  uf <- tutplot_updatefactor()

  # the defect this replaced drew the left points at 0.7 / 0.6 / 0.51, giving
  # 2.0138 / 1.8221 / 1.6653 -- markers that missed their own curves
  expect_equal(uf$wrong, exp(uf$alpha))
  expect_equal(round(uf$wrong, 4), c(2.0524, 1.8571, 1.6803))
  expect_equal(uf$correct, exp(-uf$alpha))
})

test_that("tutplot_updatefactor() shows what its caption claims", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  uf <- tutplot_updatefactor()

  # "classified correctly ... the weight is reduced in the next iteration"
  expect_true(all(uf$correct < 1))
  # "in the case of a misclassification ... the weight increases"
  expect_true(all(uf$wrong > 1))
})

test_that("tutplot_updatefactor() generalizes past three curves", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  four <- tutplot_updatefactor(
    alpha = c(0.8, 0.6, 0.4, 0.2),
    ylim = c(0.2, 2.6)
  )

  expect_length(four$alpha, 4)
  expect_length(four$wrong, 4)
  # pch is recycled rather than erroring on the shorter default
  expect_equal(four$wrong, exp(c(0.8, 0.6, 0.4, 0.2)))
})

test_that("tutplot_updatefactor() validates alpha and writes nothing by default", {
  wd <- file.path(tempdir(), "tutplot-uf")
  dir.create(wd, showWarnings = FALSE)
  old <- setwd(wd)
  grDevices::pdf(NULL)
  on.exit(
    {
      grDevices::dev.off()
      setwd(old)
      unlink(wd, recursive = TRUE)
    },
    add = TRUE
  )

  expect_error(tutplot_updatefactor(alpha = "big"))
  tutplot_updatefactor()
  expect_identical(list.files(wd), character(0))
})

test_that("tutplot_importance() shows what its caption claims", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  imp <- tutplot_importance()
  finite <- is.finite(imp$importance[, 1])

  # "the better the performance of a stump, the higher its weight"
  for (i in seq_along(imp$eta)) {
    col <- imp$importance[finite, i]
    expect_false(is.unsorted(col))
  }
  # chance performance earns no importance at all
  expect_equal(imp$importance[imp$performance == 0.5, ], rep(0, 3))
  # "eta allows to further attenuate the weight": at any fixed performance the
  # importance is proportional to it
  at <- which(round(imp$performance, 3) == 0.9)
  expect_equal(
    imp$importance[at, ] / imp$importance[at, 1],
    imp$eta / imp$eta[1]
  )
})

test_that("tutplot_importance() sizes its panel from every curve", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # the default range is unchanged: descending eta means curve 1 is the widest
  imp <- tutplot_importance()
  expect_equal(round(max(imp$importance[is.finite(imp$importance)]), 3), 2.647)

  # ascending eta used to clip, because R sized the panel from curve 1 alone
  up <- tutplot_importance(eta = c(0.1, 1))
  usr <- graphics::par("usr")
  tallest <- max(up$importance[is.finite(up$importance)])
  expect_gte(usr[4], tallest)
})

test_that("tutplot_importance() validates eta and writes nothing by default", {
  wd <- file.path(tempdir(), "tutplot-imp")
  dir.create(wd, showWarnings = FALSE)
  old <- setwd(wd)
  grDevices::pdf(NULL)
  on.exit(
    {
      grDevices::dev.off()
      setwd(old)
      unlink(wd, recursive = TRUE)
    },
    add = TRUE
  )

  expect_error(tutplot_importance(eta = "fast"))
  tutplot_importance()
  expect_identical(list.files(wd), character(0))
})

test_that("tutplot_weightone() derives the misclassified set from the weights", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  w <- tutplot_weightone()

  # the algebra, checked rather than trusted: recompute the round the long way
  # the reviewer metrics only -- on the full frame `eid` identifies every row,
  # so the learner would be perfect and nothing would ever be reweighted
  voinms <- c("power.o", "effect_size.o", "n.o", "p_value.o", "replicate")
  train <- altmejd_splits$train[, voinms]
  y <- train$replicate
  d1 <- rep(1, nrow(train)) / nrow(train)
  h1 <- rpart::rpart(
    replicate ~ .,
    data = train,
    method = "class",
    maxdepth = 1,
    maxsurrogate = 0,
    weights = d1
  )
  yretro <- predict(h1, newdata = train, type = "class")

  expect_identical(w$wrong, which((w$d2 > w$d1)[seq_len(w$n)]))
  expect_identical(w$wrong, which((yretro != y)[seq_len(w$n)]))
  # what the chunk drew
  expect_length(w$wrong, 7)
})

test_that("tutplot_weightone() shows the mechanism its caption describes", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  w <- tutplot_weightone()
  missed <- w$d2 > w$d1

  # a missed point gets more important, a hit gets less
  expect_true(all(w$d2[missed] > w$d1[missed]))
  expect_true(all(w$d2[!missed] < w$d1[!missed]))
  # and both are still distributions
  expect_equal(sum(w$d1), 1)
  expect_equal(sum(w$d2), 1)
})

test_that("tutplot_weightone() takes its own weights and honors n", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  ten <- tutplot_weightone(n = 10)
  expect_identical(ten$n, 10)
  expect_true(all(ten$wrong <= 10))

  # weights supplied directly, no model in sight -- chi comes with them
  d1 <- rep(0.1, 10)
  d2 <- c(rep(0.05, 8), 0.3, 0.3)
  chi <- c(rep(1, 8), -1, -1)
  own <- tutplot_weightone(d1 = d1, d2 = d2, chi = chi, n = 10)
  expect_identical(own$wrong, c(9L, 10L))

  expect_error(
    tutplot_weightone(d1 = rep(0.1, 10), d2 = rep(0.1, 9), chi = rep(1, 10)),
    "same length"
  )
})

test_that("tut_round_one() reproduces the tutorial's D1 and D2", {
  train <- altmejd_splits$train
  r <- tut_round_one(train)

  expect_equal(r$d1, rep(1, nrow(train)) / nrow(train))
  expect_equal(sum(r$d2), 1)
  # alpha is positive, which is what makes a missed point grow
  expect_gt(r$alpha, 0)
})

test_that("tutplot_importance() marks the reader's own learner exactly", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # the tutorial's own numbers, Listings 21-22 recomputed independently
  voinms <- c("power.o", "effect_size.o", "n.o", "p_value.o", "replicate")
  train <- altmejd_splits$train[, voinms]
  y <- train$replicate
  d1 <- rep(1, nrow(train)) / nrow(train)
  h1 <- rpart::rpart(
    replicate ~ .,
    data = train,
    method = "class",
    maxdepth = 1,
    weights = d1
  )
  yretro <- predict(h1, newdata = train, type = "class")
  perf <- sum(d1 * (yretro == y))
  alpha1 <- 1 / 2 * log(perf / (1 - perf)) * 1

  marked <- tutplot_importance(mark_perf = perf)

  expect_equal(marked$alpha, alpha1)
  expect_equal(round(marked$alpha, 4), 0.5431)
})

test_that("the mark sits on the curve it belongs to", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # for a mark_eta that is one of the drawn curves, the marked importance must
  # equal that curve's value there -- it cannot float off its own line
  for (e in c(1, 0.5, 0.1)) {
    m <- tutplot_importance(mark_perf = 0.8, mark_eta = e)
    expect_equal(m$alpha, 1 / 2 * log(0.8 / 0.2) * e)
  }
  # and it moves when the learner does
  a <- tutplot_importance(mark_perf = 0.6)$alpha
  b <- tutplot_importance(mark_perf = 0.9)$alpha
  expect_lt(a, b)
})

test_that("no mark leaves the figure exactly as it was", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  plain <- tutplot_importance()
  marked <- tutplot_importance(mark_perf = 0.75)

  expect_null(plain$alpha)
  expect_identical(plain$importance, marked$importance)
  expect_identical(plain$performance, marked$performance)
})

test_that("tutplot_importance() refuses a mark it could not draw", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # worse than chance: the importance is negative and off the panel
  expect_error(tutplot_importance(mark_perf = 0.3), "must be one value")
  # perfect: the importance is infinite
  expect_error(tutplot_importance(mark_perf = 1), "must be one value")
  expect_error(tutplot_importance(mark_perf = c(0.6, 0.8)), "must be one value")
  expect_error(tutplot_importance(mark_perf = "good"))
})

test_that("the tutorial's eta merge cannot duplicate or drop a curve", {
  # the expression the `figure-6` chunk runs, tested as such. A bare
  # `c(eta, 0.5, 0.1)` draws 0.5 twice for `eta = 0.5` and loses the unshrunk
  # curve for any rate that is not one of the three
  merge <- function(eta) sort(unique(c(eta, 1, 0.5, 0.1)), decreasing = TRUE)

  for (e in c(1, 0.5, 0.1, 0.75, 2)) {
    etas <- merge(e)
    expect_false(anyDuplicated(etas) > 0)
    # the reference curve survives, and so does the reader's own rate
    expect_true(1 %in% etas)
    expect_true(e %in% etas)
    # descending, so the palette runs with the rate
    expect_identical(etas, sort(etas, decreasing = TRUE))
  }

  # a rate already among the three folds in; anything else adds a fourth
  expect_identical(merge(1), c(1, 0.5, 0.1))
  expect_identical(merge(0.5), c(1, 0.5, 0.1))
  expect_identical(merge(0.75), c(1, 0.75, 0.5, 0.1))
})

test_that("a duplicated eta draws the same curve twice, silently", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # pins what the unmerged call used to do: no error, no warning, two identical
  # columns and a doubled legend entry. A guard inside the function would be a
  # deliberate change, and this is what would catch it
  dup <- expect_silent(tutplot_importance(eta = c(0.5, 0.5, 0.1)))
  expect_identical(dup$importance[, 1], dup$importance[, 2])
})

test_that("four curves get four line types", {
  # `eta` merged with the three references can reach four, and `lty` is
  # recycled across them -- three entries would give curves 1 and 4 the same
  # line type, separable only by color on a color print
  default <- eval(formals(tutplot_importance)$lty)
  expect_false(anyDuplicated(rep_len(default, 4)) > 0)
  # and the shipped three-curve figure is untouched by the fourth entry
  expect_identical(rep_len(default, 3), c(1, 2, 4))
})

test_that("the spliced call marks the reader's own alpha", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # `perf` and `eta` as Listings 21 and 22 leave them
  perf <- 0.82
  eta <- 0.75
  etas <- sort(unique(c(eta, 1, 0.5, 0.1)), decreasing = TRUE)

  imp <- tutplot_importance(eta = etas, mark_perf = perf, mark_eta = eta)
  expect_equal(imp$alpha, 1 / 2 * log(perf / (1 - perf)) * eta)
  # the marked rate is one of the drawn curves, so the crosshair sits on a line
  expect_true(eta %in% imp$eta)
})

test_that("tutplot_weightone() bands what `chi` says, not what the weights imply", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # a worse-than-chance learner: alpha goes negative and the update reverses,
  # so the weights that grew are the ones it got RIGHT
  m <- 20
  d1 <- rep(1, m) / m
  chi <- c(rep(-1, 13), rep(1, 7))
  perf <- 0.35
  alpha <- 0.5 * log(perf / (1 - perf))
  d2 <- d1 * exp(-alpha * chi)
  d2 <- d2 / sum(d2)

  expect_lt(alpha, 0)
  # the trap: deriving would pick out the seven correct points
  expect_identical(which(d2 > d1), 14:20)

  w <- tutplot_weightone(d1 = d1, d2 = d2, chi = chi, n = m)
  expect_identical(w$wrong, 1:13)
  expect_false(identical(w$wrong, which(d2 > d1)))
})

test_that("supplied weights must come with chi, because it cannot be inferred", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  m <- 20
  d1 <- rep(1, m) / m
  build <- function(perf, n_wrong) {
    chi <- c(rep(-1, n_wrong), rep(1, m - n_wrong))
    a <- 0.5 * log(perf / (1 - perf))
    d2 <- d1 * exp(-a * chi)
    list(chi = chi, d2 = d2 / sum(d2), grew = sum(d2 / sum(d2) > d1))
  }

  # the reason it cannot be inferred: whether the learner beat chance or not,
  # the grown set is the minority, so its size gives nothing away
  good <- build(0.75, 5)
  bad <- build(0.35, 13)
  expect_lt(good$grew, m / 2)
  expect_lt(bad$grew, m / 2)

  expect_error(
    tutplot_weightone(d1 = d1, d2 = bad$d2, n = m),
    "cannot say which points were missed"
  )
})

test_that("tut_round_one() hands back a real chi, so nothing is derived", {
  r <- tut_round_one(altmejd_splits$train)

  voinms <- c("power.o", "effect_size.o", "n.o", "p_value.o", "replicate")
  train <- altmejd_splits$train[, voinms]
  h1 <- rpart::rpart(
    replicate ~ .,
    data = train,
    method = "class",
    maxdepth = 1,
    maxsurrogate = 0,
    weights = r$d1
  )
  yretro <- predict(h1, newdata = train, type = "class")

  expect_identical(r$chi == -1, unname(yretro != train$replicate))
  expect_true(all(r$chi %in% c(-1, 1)))
})

test_that("tutplot_weightone() validates chi", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  d1 <- rep(0.1, 10)
  d2 <- c(rep(0.05, 8), 0.3, 0.3)
  expect_error(
    tutplot_weightone(d1 = d1, d2 = d2, chi = rep(-1, 9), n = 10),
    "one per weight"
  )
  expect_error(
    tutplot_weightone(d1 = d1, d2 = d2, n = 10),
    "`chi` is required"
  )
  expect_error(
    tutplot_weightone(d1 = d1, d2 = d2, chi = rep(0, 10), n = 10),
    "must be \\+1 or -1"
  )
})

test_that("the mark follows mark_eta, whatever the reader sets", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  voinms <- c("power.o", "effect_size.o", "n.o", "p_value.o", "replicate")
  train <- altmejd_splits$train[, voinms]
  y <- train$replicate
  d1 <- rep(1, nrow(train)) / nrow(train)
  h1 <- rpart::rpart(
    replicate ~ .,
    data = train,
    method = "class",
    maxdepth = 1,
    weights = d1
  )
  yretro <- predict(h1, newdata = train, type = "class")
  perf <- sum(d1 * (yretro == y))
  epsilon <- 1 - perf

  # 0.75 is deliberately not one of the drawn curves: the mark still belongs to
  # it, it simply sits between two of them
  for (e in c(1, 0.5, 0.1, 0.75)) {
    m <- tutplot_importance(mark_perf = perf, mark_eta = e)
    expect_equal(m$alpha, 1 / 2 * log(perf / epsilon) * e)
  }

  # the regression this fixes: omitting mark_eta pins the mark to eta[1], so a
  # reader who changes eta in Listing 22 would see a crosshair that lies
  pinned <- tutplot_importance(mark_perf = perf)$alpha
  expect_equal(pinned, 1 / 2 * log(perf / epsilon) * 1)
  expect_false(isTRUE(all.equal(
    pinned,
    1 / 2 * log(perf / epsilon) * 0.5
  )))
})

test_that("tutplot_opts holds the numbers its documentation states", {
  expect_equal(round(unname(tutplot_opts$col), 2), c(4.30, 3.06))
  expect_equal(round(unname(tutplot_opts$full), 2), c(5.55, 3.95))
  expect_identical(tutplot_opts$pointsize, 9)
  expect_identical(tutplot_opts$viridis_end, 0.85)
  # the margins differ in the third element only: room for a legend on top
  expect_identical(
    tutplot_opts$mar_plain[-3],
    tutplot_opts$mar_legend[-3]
  )
  expect_identical(tutplot_opts$mar_plain[3], 1.9)
  expect_identical(tutplot_opts$mar_legend[3], 3.0)
})

test_that("tutplot_opts is a reference, not a switch", {
  skip_if_not(nzchar(Sys.which("pdfinfo")), "pdfinfo not available")
  f <- tempfile(fileext = ".pdf")
  g <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(f, g)), add = TRUE)

  size <- function(p) {
    out <- system2("pdfinfo", p, stdout = TRUE)
    sub(".*: *", "", grep("^Page size", out, value = TRUE))
  }

  # the surprising half of the design, so assert it rather than only say it:
  # R resolves a default inside the package, not in the caller's environment
  tutplot_gini(file = f)
  tutplot_opts <- list(
    col = c(width = 99, height = 99),
    pointsize = 99,
    mar_plain = c(1, 1, 1, 1),
    mgp = c(1, 1, 0),
    tcl = -0.25,
    cex_axis = 1,
    viridis_end = 0.5
  )
  tutplot_gini(file = g)

  # the shadowed copy changed nothing
  expect_identical(size(g), size(f))
  expect_match(size(f), "^309 x 220")

  # passing the value is what works
  h <- tempfile(fileext = ".pdf")
  on.exit(unlink(h), add = TRUE)
  tutplot_gini(file = h, width = tutplot_opts$col[["width"]], height = 6)
  expect_false(identical(size(h), size(f)))
})

test_that("tut_par() restores what it found, both ways", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  keys <- c("mar", "mgp", "tcl", "cex.axis")
  for (leg in c(TRUE, FALSE)) {
    before <- graphics::par(keys)
    op <- tut_par(legend = leg)
    # it set what the object says
    want <- if (leg) tutplot_opts$mar_legend else tutplot_opts$mar_plain
    expect_identical(graphics::par("mar"), want)
    graphics::par(op)
    expect_identical(graphics::par(keys), before)
  }
})

test_that("each figure asks for the margins it needs", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # gini has no legend above the panel; the other three do
  body_of <- function(f) paste(deparse(body(f)), collapse = " ")
  expect_match(body_of(tutplot_gini), "tut_par(legend = FALSE)", fixed = TRUE)
  for (f in list(tutplot_updatefactor, tutplot_importance, tutplot_weightone)) {
    expect_match(body_of(f), "tut_par(legend = TRUE)", fixed = TRUE)
  }
})

test_that("tutplot_boundary() draws at the manuscript's full text width", {
  skip_if_not(nzchar(Sys.which("pdfinfo")), "pdfinfo not available")
  f <- tempfile(fileext = ".pdf")
  g <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(f, g)), add = TRUE)

  pts <- function(p) {
    out <- system2("pdfinfo", p, stdout = TRUE)
    v <- sub(".*: *", "", grep("^Page size", out, value = TRUE))
    as.numeric(regmatches(v, gregexpr("[0-9.]+", v))[[1]])[1:2]
  }

  tutplot_boundary(stump(), data = altmejd_splits$train, file = f)
  # derived from the object rather than hardcoded, so the two cannot disagree
  expect_equal(
    round(pts(f)),
    unname(round(tutplot_opts$full * 72)),
    tolerance = 1
  )

  # `full`, not `col` -- this is the assertion that catches the two being
  # crossed, since both are plausible page sizes on their own
  tutplot_gini(file = g)
  expect_gt(pts(f)[1], pts(g)[1])
})

test_that("tutplot_boundary() takes either learner, and writes no file", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  train <- altmejd_splits$train
  before <- list.files(".", pattern = "\\.pdf$")

  # a single stump: two leaves, so the score takes exactly two values
  one <- tutplot_boundary(stump(), data = train, resolution = 40)
  expect_identical(one$features, c("power.o", "n.o"))
  expect_identical(dim(one$z), c(40L, 40L))
  expect_length(unique(as.vector(one$z)), 2L)

  # an ensemble: the same call, and the margin takes many values. `T` is small
  # because this is about the wrapper, not about boosting
  ens <- rpart::rpart(
    replicate ~ .,
    data = train[, c("power.o", "n.o", "replicate")],
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 20, eta = 1, verbose = FALSE, input_checks = FALSE)
  many <- tutplot_boundary(ens, data = train, resolution = 40)
  expect_identical(many$features, c("power.o", "n.o"))
  expect_gt(length(unique(as.vector(many$z))), 2L)

  # `file = NULL` draws on the current device and leaves no file behind
  expect_identical(list.files(".", pattern = "\\.pdf$"), before)
})

test_that("tutplot_boundary() shades by the score, not by the class", {
  # both printed boundaries are margin-shaded, which is what makes the stump
  # and the ensemble comparable. The returned grid is the same under either
  # shading, so only the formal can witness this
  expect_identical(eval(formals(tutplot_boundary)$shade), "margin")

  # and the axis labels are not baked in, so a different pair of features
  # cannot end up captioned as power and sample size
  expect_null(eval(formals(tutplot_boundary)$xlab))
  expect_null(eval(formals(tutplot_boundary)$ylab))
})

test_that("tutplot_logoscheme() builds one fold per project, test on the diagonal", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  g <- altmejd$pid
  f <- tutplot_logoscheme(g)

  # one iteration per project, and every row accounted for
  expect_identical(f$k, length(unique(g)))
  expect_identical(sum(f$n), length(g))
  expect_identical(as.integer(f$n[["rpp"]]), sum(g == "rpp"))

  # heights are the project shares, so they carry the imbalance
  expect_equal(sum(f$heights), 1)
  expect_equal(f$heights, as.numeric(f$n) / sum(f$n))
  expect_equal(round(max(f$heights), 3), 0.592)
})

test_that("tutplot_logoscheme() draws the order it is given, not table()'s", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # `table()` sorts, which would silently rearrange the manuscript's figure
  paper <- c("ml1", "rpp", "ml3", "eerp", "ssrp")
  f <- tutplot_logoscheme(altmejd$pid, levels = paper)
  expect_identical(f$levels, paper)
  expect_identical(names(f$n), paper)
  expect_false(identical(names(f$n), sort(paper)))

  # a factor brings its own order
  g <- factor(altmejd$pid, levels = paper)
  expect_identical(tutplot_logoscheme(g)$levels, paper)

  # and a level that is not in the data is refused rather than drawn empty
  expect_error(tutplot_logoscheme(altmejd$pid, levels = c(paper, "nope")), "nope")
})

test_that("tutplot_logoscheme() equal blocks hide what proportional ones show", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  prop <- tutplot_logoscheme(altmejd$pid, proportional = TRUE)
  flat <- tutplot_logoscheme(altmejd$pid, proportional = FALSE)

  expect_equal(flat$heights, rep(1 / flat$k, flat$k))
  expect_false(isTRUE(all.equal(prop$heights, flat$heights)))
  # the counts are the same either way; only the drawing differs
  expect_identical(prop$n, flat$n)

  # the fact the proportional variant exists to show: one fold holds out more
  # than it trains on
  tot <- sum(prop$n)
  expect_true(any(prop$n > tot - prop$n))
})

test_that("tut_roundrect() keeps its corners circular on any panel", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # a radius in user units would go oval here; one in inches must not
  plot(NULL, xlim = c(0, 10), ylim = c(0, 1), axes = FALSE)
  usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  rx <- 0.04 * (usr[2] - usr[1]) / pin[1]
  ry <- 0.04 * (usr[4] - usr[3]) / pin[2]
  # same size on paper, very different in user units
  expect_gt(rx / ry, 3)
  expect_silent(tut_roundrect(1, 0.1, 3, 0.9, col = "grey80"))
})

test_that("tut_ink() picks readable text for both ends of viridis", {
  pal <- viridisLite::viridis(5)
  expect_identical(tut_ink(pal[1]), "white")
  expect_identical(tut_ink(pal[5]), "grey15")
  expect_identical(tut_ink(c("#440154", "#FDE725")), c("white", "grey15"))
})

test_that("axis labels are title case, as APA asks", {
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext not available")
  f <- tempfile(fileext = ".pdf")
  g <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(f, g)), add = TRUE)

  txt <- function(p) {
    paste(system2("pdftotext", c(p, "-"), stdout = TRUE), collapse = " ")
  }

  # the labels are literals inside the drawing code, not defaults, so
  # `formals()` cannot see them -- assert on what is actually drawn
  tutplot_updatefactor(file = f)
  expect_match(txt(f), "Weight Update Factor")
  expect_no_match(txt(f), "Weight update factor")

  tutplot_weightone(file = g)
  expect_match(txt(g), "Data Point")
  expect_no_match(txt(g), "Data point")
})

test_that("tutplot_logoscheme() leaves the figure's title to the caption", {
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext not available")
  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f), add = TRUE)

  tutplot_logoscheme(altmejd$pid, file = f)
  out <- paste(system2("pdftotext", c(f, "-"), stdout = TRUE), collapse = " ")

  # the axis label and the column headers stay
  expect_match(out, "Data")
  expect_match(out, "Iteration 1")
  # the in-panel title does not: it duplicated the numbered caption
  expect_no_match(out, "Leave-Project-Out Cross-Validation")
})

# ---- tutplot_logocv ---------------------------------------------------------

logocv_res <- function() {
  data(altmejd)
  logo_cv(
    replicate ~ power.o + effect_size.o + n.o + p_value.o,
    data = altmejd,
    group = "pid",
    T = 5,
    eta = 1
  )
}

test_that("tutplot_logocv(bootstrap = FALSE) draws estimates without resampling", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  res <- logocv_res()

  set.seed(1)
  before <- .Random.seed
  facts <- tutplot_logocv(res, bootstrap = FALSE)
  expect_identical(.Random.seed, before)

  expect_equal(
    facts$estimate,
    unname(res$estimates[1, facts$project, "auroc"])
  )
  expect_true(all(is.na(facts$lower) & is.na(facts$upper)))
})

test_that("tutplot_logocv(bootstrap = TRUE) brackets every estimate", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  set.seed(1)
  facts <- suppressWarnings(tutplot_logocv(logocv_res(), n_resample = 100))
  expect_true(all(facts$lower <= facts$estimate & facts$estimate <= facts$upper))
  expect_type(facts$flagged, "logical")
})

test_that("tutplot_logocv() reuses attached intervals instead of resampling", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  set.seed(1)
  boot <- suppressWarnings(bootstrap(logocv_res(), n_resample = 100))

  before <- .Random.seed
  facts <- tutplot_logocv(boot)
  expect_identical(.Random.seed, before)

  ssrp <- attr(boot$ci$ssrp, "draws")[, "auroc"]
  expect_equal(
    facts$upper[facts$project == "ssrp"],
    unname(stats::quantile(ssrp, 0.975, na.rm = TRUE))
  )
})

test_that("tutplot_logocv() restores par() and writes a file", {
  res <- logocv_res()
  grDevices::pdf(NULL)
  before <- graphics::par(c("mar", "mgp", "tcl", "cex.axis"))
  tutplot_logocv(res, bootstrap = FALSE)
  expect_identical(graphics::par(c("mar", "mgp", "tcl", "cex.axis")), before)
  grDevices::dev.off()

  f <- tempfile(fileext = ".pdf")
  tutplot_logocv(res, bootstrap = FALSE, file = f)
  expect_gt(file.size(f), 0)
  expect_error(tutplot_logocv(list()), "result of logo_cv")
})
