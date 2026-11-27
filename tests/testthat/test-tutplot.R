# Most calls here draw. A call without `file` draws on the current device; with
# none open, R would open its default one and leave an `Rplots.pdf` behind in
# tests/testthat/. So before such a call a test opens a device of its own,
# usually a null device, and closes it afterward. The test of `file` opens
# none, so that a device a figure leaves open shows.
stump <- function(formula = replicate ~ power.o + n.o, weights = NULL) {
  args <- list(
    formula = formula,
    data = altmext_splits$train,
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

# the text and the fonts a PDF draws; callers match bytes, because both tools
# print UTF-8 and pdf() draws a hyphen as a minus sign
read_pdf_text <- function(path) {
  paste(system2("pdftotext", c(path, "-"), stdout = TRUE), collapse = " ")
}
read_pdf_fonts <- function(path) system2("pdffonts", path, stdout = TRUE)

# record a figure on a device of the manuscript's column size, where the
# named ticks and their fallback are decided
record_column <- function(expr) {
  record_calls(
    expr,
    width = tutplot_opts$col[["width"]],
    height = tutplot_opts$col[["height"]],
    pointsize = tutplot_opts$pointsize
  )
}

# the y-axis calls that carry positions: plot() also records one without
# them, which `yaxt = "n"` suppresses
find_y_axes <- function(calls) {
  Filter(
    \(call) identical(call[[2]], 2) && !is.null(call[[3]]),
    find_calls(calls, "C_axis")
  )
}

test_that("tutplot_cstump() returns the numbers Figure 2's caption states", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # the caption: power.o at 0.927, leaves of 89 and 52, about 63% and 37%
  facts <- tutplot_cstump(stump())

  expect_identical(facts$variable, "power.o")
  expect_equal(round(facts$cutpoint, 4), 0.927)
  expect_equal(unname(facts$leaves), c(89, 52))
  expect_equal(round(100 * facts$leaves / sum(facts$leaves)), c(63, 37))
})

test_that("the weighted stump is drawn by passing its fit, not its weights", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # fig:cstumpwght is the same function given the weighted fit and extra = 100;
  # the weights and the two predictors are those of Listing 16
  set.seed(112)
  d_raw <- stats::runif(nrow(altmext_splits$train))
  d <- d_raw / sum(d_raw)
  fit <- stump(weights = d)

  facts <- tutplot_cstump(fit, extra = 100)

  # reweighting moves the cut from 0.927 to 0.9206; the caption's 59.85 /
  # 40.15 are weighted shares, not row counts, which is why they differ from
  # the leaves' 86 / 55
  expect_identical(facts$variable, "power.o")
  expect_equal(round(facts$cutpoint, 4), 0.9206)
  expect_equal(unname(facts$leaves), c(86, 55))
  lo <- altmext_splits$train$power.o < facts$cutpoint
  expect_equal(round(100 * c(sum(d[lo]), sum(d[!lo])), 2), c(59.85, 40.15))
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

test_that("a path writes one PDF and leaves the open devices as they were", {
  # no device of our own, on purpose: par() restored after the PDF closed
  # would land on it, and the stray device would not show. One open here
  # leaked from an earlier test; close it, so the checks below can still see
  # a stray device
  expect_null(grDevices::dev.list(), label = "the devices open at the start")
  grDevices::graphics.off()
  df_noholz <- drop_holz()
  boosted <- rpart::rpart(
    replicate ~ power.o + n.o + effect_size.o + p_value.o,
    data = df_noholz,
    method = "class",
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE)
  res <- logo_cv(boosted, data = df_noholz, group = "pid", verbose = FALSE)
  # every figure that takes `file`
  figures <- list(
    tutplot_cstump = \(file) tutplot_cstump(stump(), file = file),
    tutplot_gini = \(file) tutplot_gini(file = file),
    tutplot_updatefactor = \(file) tutplot_updatefactor(file = file),
    tutplot_modelweight = \(file) tutplot_modelweight(file = file),
    tutplot_weightupdate = \(file) tutplot_weightupdate(file = file),
    tutplot_boundary = function(file) {
      tutplot_boundary(
        stump(),
        data = altmext_splits$train,
        resolution = 20,
        file = file
      )
    },
    tutplot_importance = \(file) {
      tutplot_importance(gauge(boosted), file = file)
    },
    tutplot_logoscheme = \(file) tutplot_logoscheme(df_noholz$pid, file = file),
    tutplot_logocv = \(file) tutplot_logocv(res, bootstrap = FALSE, file = file)
  )

  for (name in names(figures)) {
    f <- tempfile(fileext = ".pdf")
    before <- grDevices::dev.list()
    figures[[name]](f)
    after <- grDevices::dev.list()
    # close a stray device before asserting, so it cannot hide the next one
    for (device in setdiff(after, before)) {
      grDevices::dev.off(device)
    }

    expect_identical(
      after,
      before,
      label = paste0("devices after ", name, "()")
    )
    expect_gt(file.size(f), 0, label = paste0("the PDF of ", name, "()"))
    unlink(f)
  }
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
  expect_true(any(grepl("62 / 89", txt(f102), fixed = TRUE)))
  expect_false(any(grepl("62 / 89", txt(f100), fixed = TRUE)))
})

test_that("tutplot_cstump() refuses something that is not an rpart fit", {
  expect_error(tutplot_cstump(altmext_splits$train), "rpart object")
})

test_that("tutplot_cstump() draws labels and title-case classes", {
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext not available")
  f <- tempfile(fileext = ".pdf")
  g <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(f, g)), add = TRUE)

  fit <- stump()
  facts <- tutplot_cstump(
    fit,
    labels = c(power.o = "Power", n.o = "Sample Size"),
    file = f
  )
  drawn <- read_pdf_text(f)
  expect_match(drawn, "Power <", fixed = TRUE, useBytes = TRUE)
  expect_no_match(drawn, "power.o", fixed = TRUE, useBytes = TRUE)
  expect_match(drawn, "Failure", fixed = TRUE, useBytes = TRUE)
  expect_match(drawn, "Success", fixed = TRUE, useBytes = TRUE)
  expect_no_match(drawn, "failure", fixed = TRUE, useBytes = TRUE)

  # the facts describe the fit as given, not the drawn copy
  expect_identical(facts$fit, fit)
  expect_identical(facts$variable, "power.o")
  expect_equal(round(facts$cutpoint, 4), 0.927)
  expect_equal(unname(facts$leaves), c(89, 52))

  # without labels the names stay as typed; the classes are title case anyway
  tutplot_cstump(fit, file = g)
  plain <- read_pdf_text(g)
  expect_match(plain, "power.o <", fixed = TRUE, useBytes = TRUE)
  expect_match(plain, "Failure", fixed = TRUE, useBytes = TRUE)
  expect_no_match(plain, "failure", fixed = TRUE, useBytes = TRUE)
})

test_that("a label keeps the rounded cutpoint of a whole-number split", {
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext not available")
  f <- tempfile(fileext = ".pdf")
  g <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(f, g)), add = TRUE)

  # n.o is a count, so rpart.plot() rounds its cutpoint; renaming the variable
  # inside the fit would hide it from the data and print 131.5 instead
  fit <- stump(replicate ~ n.o)
  tutplot_cstump(fit, file = f)
  tutplot_cstump(fit, labels = c(n.o = "Sample Size"), file = g)
  read_cutpoint <- function(path) {
    sub(".*< ([0-9.]+).*", "\\1", read_pdf_text(path), useBytes = TRUE)
  }

  expect_match(read_pdf_text(g), "Sample Size <", fixed = TRUE, useBytes = TRUE)
  expect_identical(read_cutpoint(g), read_cutpoint(f))
  expect_no_match(read_cutpoint(f), ".", fixed = TRUE)
})

test_that("tutplot_cstump() refuses labels it cannot draw", {
  expect_error(
    tutplot_cstump(stump(), labels = list(power.o = quote(italic(P)))),
    "plain text only"
  )
  expect_error(
    tutplot_cstump(stump(), labels = c(p_value.o = "p Value")),
    "not in `fit`: p_value.o"
  )
  expect_error(tutplot_cstump(stump(), labels = "Power"), "must be named")
})

test_that("tutplot_cstump() tags its branches Yes and No, upright", {
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext not available")
  skip_if_not(nzchar(Sys.which("pdffonts")), "pdffonts not available")
  paths <- c(tempfile(fileext = ".pdf"), tempfile(fileext = ".pdf"))
  on.exit(unlink(paths), add = TRUE)

  # the node labels both printed stumps use
  tutplot_cstump(stump(), extra = 102, file = paths[1])
  tutplot_cstump(stump(), extra = 100, file = paths[2])
  for (path in paths) {
    drawn <- read_pdf_text(path)
    expect_match(drawn, "\\bYes\\b", perl = TRUE, useBytes = TRUE)
    expect_match(drawn, "\\bNo\\b", perl = TRUE, useBytes = TRUE)
    expect_no_match(drawn, "\\b(yes|no)\\b", perl = TRUE, useBytes = TRUE)
    # rpart.plot() sets the tags in italic unless told otherwise
    expect_false(any(grepl("Oblique", read_pdf_fonts(path), useBytes = TRUE)))
  }
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

  # the defect this replaced drew the left points at fixed heights, whatever
  # the curve's alpha -- markers that missed their own curves. The pins are
  # the default curves at 0.7, 0.499 and 0.3
  expect_equal(uf$wrong, exp(uf$alpha))
  expect_equal(round(uf$wrong, 4), c(2.0138, 1.6471, 1.3499))
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

test_that("Figure 7 draws one point shape, told apart by color alone", {
  expect_identical(eval(formals(tutplot_updatefactor)$pch), 20)

  out <- record_column(tutplot_updatefactor())
  # the end points: two per curve, at chi = 1 and chi = -1
  ends <- Filter(
    \(call) identical(call[[3]], "p") && identical(call[[2]]$x, c(1, -1)),
    find_calls(out$calls, "C_plotXY")
  )
  expect_length(ends, 3)
  expect_identical(vapply(ends, \(call) call[[4]], numeric(1)), c(20, 20, 20))
  expect_false(anyDuplicated(vapply(ends, \(call) call[[6]], character(1))) > 0)
})

test_that("Figure 7's legend names the tutorial's curves without numbers", {
  out <- record_column(tutplot_updatefactor())
  words <- expression("Larger" ~ alpha[t], alpha[1], "Smaller" ~ alpha[t])

  expect_identical(out$value$legend, words)
  # and the legend draws them
  texts <- lapply(find_calls(out$calls, "C_text"), \(call) call[[3]])
  expect_identical(texts, list(words))

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  # the tutorial's call, its numbers typed out, and its first stump's weight
  # unrounded: compared at the three digits the default keeps
  alpha1 <- run_round_one(altmext_splits$train)$alpha
  expect_identical(
    tutplot_updatefactor(alpha = c(0.7, 0.499, 0.3))$legend,
    words
  )
  expect_identical(
    tutplot_updatefactor(alpha = c(0.7, alpha1, 0.3))$legend,
    words
  )
})

test_that("Figure 7's legend shows the numbers for any other alpha", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  others <- list(
    # not the first stump's weight, at three digits
    c(0.8, 0.5, 0.2),
    c(0.7, 0.498, 0.3),
    # not decreasing
    c(0.3, 0.499, 0.7),
    c(0.7, 0.499, 0.499),
    # not three
    c(0.7, 0.499),
    c(0.8, 0.7, 0.499, 0.3)
  )
  for (alpha in others) {
    legend <- tutplot_updatefactor(alpha = alpha, ylim = c(0.2, 2.6))$legend
    expect_identical(
      vapply(legend, deparse1, character(1)),
      paste0("alpha[t] * \" = \" * \"", vapply(alpha, format, ""), "\"")
    )
  }
})

test_that("Figure 7's legend gives each key the same room on both sides", {
  # the label widths and the panel's scale, read while the legend draws
  real_legend <- draw_legend
  seen <- NULL
  local_mocked_bindings(
    draw_legend = function(labels, cex = 1, ...) {
      seen <<- list(
        widths = graphics::strwidth(labels, units = "user", cex = cex),
        pt = 72 * graphics::par("pin")[1] / diff(graphics::par("usr")[1:2])
      )
      real_legend(labels, cex = cex, ...)
    }
  )
  out <- record_column(tutplot_updatefactor())

  # one segments() call draws the three keys, one text() call the labels,
  # left aligned
  keys <- find_calls(out$calls, "C_segments")
  expect_length(keys, 1)
  key_left <- keys[[1]][[2]]
  key_right <- keys[[1]][[4]]
  label_left <- find_calls(out$calls, "C_text")[[1]][[2]]$x
  label_right <- label_left + seen$widths
  own_label <- (label_left - key_right) * seen$pt
  label_before <- (key_left[-1] - label_right[-3]) * seen$pt

  expect_true(all(own_label > 0))
  expect_equal(label_before, own_label[-1])
})

test_that("Figure 7 names its six factors and 1 on the y-axis, with no grid", {
  out <- record_column(tutplot_updatefactor())
  uf <- out$value

  expect_equal(uf$ticks$at, sort(c(uf$correct, uf$wrong, 1)))
  expect_identical(
    uf$ticks$labels,
    c("0.50", "0.61", "0.74", "1", "1.35", "1.65", "2.01")
  )
  y_axes <- find_y_axes(out$calls)
  expect_length(y_axes, 1)
  expect_identical(y_axes[[1]][[3]], uf$ticks$at)
  expect_identical(y_axes[[1]][[4]], uf$ticks$labels)
  expect_identical(y_axes[[1]]$las, 1)

  # the only line across the panel is the dashed one at 1: no grid. abline()
  # records `a`, `b`, `h`, `v` in that order
  ablines <- find_calls(out$calls, "C_abline")
  expect_length(ablines, 1)
  expect_identical(ablines[[1]][[4]], 1)
  expect_null(ablines[[1]][[5]])
  # no line from a point to its tick: the only segments are the legend's
  # keys, above the panel
  ylim <- eval(formals(tutplot_updatefactor)$ylim)
  segments <- find_calls(out$calls, "C_segments")
  expect_gt(length(segments), 0)
  for (call in segments) {
    expect_true(all(c(call[[3]], call[[5]]) > ylim[2]))
  }
})

test_that("Figure 7's tick labels are smaller, but not below Figure 3's", {
  # the y-axis call that sets its own label size; the boundary figure's has
  # no positions, so find_y_axes() would miss it
  axis_cex <- function(out) {
    sized <- Filter(
      \(call) identical(call[[2]], 2) && !is.null(call$cex.axis),
      find_calls(out$calls, "C_axis")
    )
    sized[[1]]$cex.axis
  }
  factor_cex <- axis_cex(record_column(tutplot_updatefactor()))
  boundary <- record_calls(
    tutplot_boundary(stump(), data = altmext_splits$train, resolution = 20),
    width = tutplot_opts$full[["width"]],
    height = tutplot_opts$full[["height"]],
    pointsize = tutplot_opts$pointsize
  )
  boundary_cex <- axis_cex(boundary)

  # both figures print at the same pointsize, so cex compares their points
  expect_identical(
    eval(formals(tutplot_updatefactor)$pointsize),
    eval(formals(tutplot_boundary)$pointsize)
  )
  expect_lt(factor_cex, tutplot_opts$cex_axis)
  expect_gte(factor_cex, boundary_cex)
})

test_that("Figure 7 falls back to ordinary ticks and the grid", {
  # grid() draws its vertical and horizontal lines in one abline() call; the
  # dashed line at 1 is the other
  expect_fallback <- function(out) {
    expect_null(out$value$ticks)
    expect_length(find_y_axes(out$calls), 0)
    ablines <- find_calls(out$calls, "C_abline")
    expect_length(ablines, 2)
    expect_false(is.null(ablines[[1]][[5]]))
    expect_identical(ablines[[2]][[4]], 1)
  }

  # more than three curves
  expect_fallback(record_column(tutplot_updatefactor(
    alpha = c(0.8, 0.6, 0.4, 0.2),
    ylim = c(0.2, 2.6)
  )))
  # labels that would collide: 0.50 and 0.50
  expect_fallback(record_column(tutplot_updatefactor(
    alpha = c(0.7, 0.69, 0.3)
  )))

  # the two rules are separate: numbers in the legend, names on the axis.
  # The top factor, 2.23, lies past `ylim` but on the panel, so it is named
  other <- record_column(tutplot_updatefactor(alpha = c(0.8, 0.5, 0.2)))
  expect_identical(
    other$value$ticks$labels,
    c("0.45", "0.61", "0.82", "1", "1.22", "1.65", "2.23")
  )
})

test_that("tutplot_modelweight() shows what its caption claims", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  imp <- tutplot_modelweight()
  finite <- is.finite(imp$weight[, 1])

  # "the better a stump performs, the larger its weight"
  for (i in seq_along(imp$eta)) {
    col <- imp$weight[finite, i]
    expect_false(is.unsorted(col))
  }
  # chance performance earns no weight at all
  expect_equal(imp$weight[imp$performance == 0.5, ], rep(0, 3))
  # "the learning rate scales the weight": at any fixed performance the
  # weight is proportional to it
  at <- which(round(imp$performance, 3) == 0.9)
  expect_equal(
    imp$weight[at, ] / imp$weight[at, 1],
    imp$eta / imp$eta[1]
  )
})

test_that("tutplot_modelweight() sizes its panel from every curve", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # the default range is unchanged: descending eta means curve 1 is the widest
  imp <- tutplot_modelweight()
  expect_equal(round(max(imp$weight[is.finite(imp$weight)]), 3), 2.647)

  # ascending eta used to clip, because R sized the panel from curve 1 alone
  up <- tutplot_modelweight(eta = c(0.1, 1))
  usr <- graphics::par("usr")
  tallest <- max(up$weight[is.finite(up$weight)])
  expect_gte(usr[4], tallest)
})

test_that("tutplot_modelweight() validates eta and writes nothing by default", {
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

  expect_error(tutplot_modelweight(eta = "fast"))
  tutplot_modelweight()
  expect_identical(list.files(wd), character(0))
})

test_that("tutplot_weightupdate() derives the misclassified set from the weights", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  w <- tutplot_weightupdate()

  # the algebra, checked rather than trusted: recompute the round the long way
  # the reviewer metrics only -- on the full frame `eid` identifies every row,
  # so the learner would be perfect and nothing would ever be reweighted
  voinms <- c("power.o", "effect_size.o", "n.o", "p_value.o", "replicate")
  train <- altmext_splits$train[, voinms]
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
  y_retro <- predict(h1, newdata = train, type = "class")

  # the figure marks no misclassified point; they are the dots that moved up
  grew <- which((w$d2 > w$d1)[seq_len(w$n)])
  expect_identical(grew, which((y_retro != y)[seq_len(w$n)]))
  # what the caption states: 9 of the first 25 were misclassified
  expect_length(grew, 9)
})

test_that("tutplot_weightupdate() shows the mechanism its caption describes", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  w <- tutplot_weightupdate()
  missed <- w$d2 > w$d1

  # a missed point gets more important, a hit gets less
  expect_true(all(w$d2[missed] > w$d1[missed]))
  expect_true(all(w$d2[!missed] < w$d1[!missed]))
  # and both are still distributions
  expect_equal(sum(w$d1), 1)
  expect_equal(sum(w$d2), 1)
})

test_that("tutplot_weightupdate() takes its own weights and honors n", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  ten <- tutplot_weightupdate(n = 10)
  expect_identical(ten$n, 10)
  # one class and one color per point drawn, not per row
  expect_length(ten$class, 10)
  expect_length(ten$fill, 10)

  # weights supplied directly, no model in sight; one per row of `data`
  rows <- altmext_splits$train[1:10, ]
  d1 <- rep(0.1, 10)
  d2 <- c(rep(0.05, 8), 0.3, 0.3)
  own <- tutplot_weightupdate(data = rows, d1 = d1, d2 = d2, n = 10)
  expect_identical(own$d1, d1)
  expect_identical(own$d2, d2)
  expect_identical(which(own$d2 > own$d1), c(9L, 10L))

  expect_error(
    tutplot_weightupdate(data = rows, d1 = rep(0.1, 10), d2 = rep(0.1, 9)),
    "same length"
  )
})

test_that("run_round_one() reproduces the tutorial's D1 and D2", {
  train <- altmext_splits$train
  r <- run_round_one(train)

  expect_equal(r$d1, rep(1, nrow(train)) / nrow(train))
  expect_equal(sum(r$d2), 1)
  # alpha is positive, which is what makes a missed point grow
  expect_gt(r$alpha, 0)
})

test_that("tutplot_modelweight() marks the reader's own learner exactly", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # the tutorial's own numbers, Listings 21-22 recomputed independently
  voinms <- c("power.o", "effect_size.o", "n.o", "p_value.o", "replicate")
  train <- altmext_splits$train[, voinms]
  y <- train$replicate
  d1 <- rep(1, nrow(train)) / nrow(train)
  h1 <- rpart::rpart(
    replicate ~ .,
    data = train,
    method = "class",
    maxdepth = 1,
    weights = d1
  )
  y_retro <- predict(h1, newdata = train, type = "class")
  perf <- sum(d1 * (y_retro == y))
  alpha1 <- 1 / 2 * log(perf / (1 - perf)) * 1

  marked <- tutplot_modelweight(marked_perf = perf)

  expect_equal(marked$alpha, alpha1)
  # the caption's model weight, and four decimals as a regression check
  expect_equal(round(marked$alpha, 3), 0.499)
  expect_equal(round(marked$alpha, 4), 0.4986)
})

test_that("the mark sits on the curve it belongs to", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # for a marked_eta that is one of the drawn curves, the marked weight must
  # equal that curve's value there -- it cannot float off its own line
  for (e in c(1, 0.5, 0.1)) {
    m <- tutplot_modelweight(marked_perf = 0.8, marked_eta = e)
    expect_equal(m$alpha, 1 / 2 * log(0.8 / 0.2) * e)
  }
  # and it moves when the learner does
  a <- tutplot_modelweight(marked_perf = 0.6)$alpha
  b <- tutplot_modelweight(marked_perf = 0.9)$alpha
  expect_lt(a, b)
})

test_that("no mark leaves the figure exactly as it was", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # NA asks for no mark; NULL, the default, marks the tutorial's stump
  plain <- tutplot_modelweight(marked_perf = NA)
  marked <- tutplot_modelweight(marked_perf = 0.75)

  expect_null(plain$alpha)
  expect_identical(plain$weight, marked$weight)
  expect_identical(plain$performance, marked$performance)
})

test_that("tutplot_modelweight() refuses a mark it could not draw", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # worse than chance: the weight is negative and off the panel
  expect_error(tutplot_modelweight(marked_perf = 0.3), "must be one value")
  # perfect: the weight is infinite
  expect_error(tutplot_modelweight(marked_perf = 1), "must be one value")
  expect_error(
    tutplot_modelweight(marked_perf = c(0.6, 0.8)),
    "must be one value"
  )
  expect_error(tutplot_modelweight(marked_perf = "good"))
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
  dup <- expect_silent(tutplot_modelweight(eta = c(0.5, 0.5, 0.1)))
  expect_identical(dup$weight[, 1], dup$weight[, 2])
})

test_that("Figure 6 draws solid curves, told apart by color alone", {
  # the curves lines() draws, type "l"; the legend keys are segments
  draw_curves <- function(...) {
    out <- record_column(tutplot_modelweight(...))
    Filter(\(call) identical(call[[3]], "l"), find_calls(out$calls, "C_plotXY"))
  }
  read_lty <- function(curves) vapply(curves, \(call) call[[5]], numeric(1))
  read_col <- function(curves) vapply(curves, \(call) call[[6]], character(1))

  # one solid line type, recycled over however many curves `eta` gives
  expect_identical(eval(formals(tutplot_modelweight)$lty), 1)
  curves <- draw_curves()
  expect_length(curves, 3)
  expect_identical(read_lty(curves), c(1, 1, 1))
  expect_false(anyDuplicated(read_col(curves)) > 0)
  expect_identical(
    read_lty(draw_curves(eta = c(1, 0.75, 0.5, 0.1))),
    c(1, 1, 1, 1)
  )

  # the argument stays, so a caller can still vary the line type
  expect_identical(read_lty(draw_curves(lty = c(1, 2, 4))), c(1, 2, 4))
})

test_that("the spliced call marks the reader's own alpha", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # `perf` and `eta` as Listings 21 and 22 leave them
  perf <- 0.82
  eta <- 0.75
  etas <- sort(unique(c(eta, 1, 0.5, 0.1)), decreasing = TRUE)

  imp <- tutplot_modelweight(eta = etas, marked_perf = perf, marked_eta = eta)
  expect_equal(imp$alpha, 1 / 2 * log(perf / (1 - perf)) * eta)
  # the marked rate is one of the drawn curves, so the crosshair sits on a line
  expect_true(eta %in% imp$eta)
})

test_that("tutplot_weightupdate() reads the class from `data`, not a chi", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # the class colors the dots, so no argument says which points were missed
  expect_false("chi" %in% names(formals(tutplot_weightupdate)))
  # no message to match: R translates its own "unused argument"
  expect_error(tutplot_weightupdate(chi = rep(1, 141)))

  # the class comes from the `replicate` column, as a two-class outcome
  train <- altmext_splits$train
  expect_error(
    tutplot_weightupdate(data = train[, names(train) != "replicate"]),
    "`replicate` column"
  )
  three <- train
  three$replicate <- factor(rep(c("a", "b", "c"), length.out = nrow(train)))
  expect_error(tutplot_weightupdate(data = three), "two levels")
})

test_that("d1 and d2 need one value per row of `data`, in row order", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  rows <- altmext_splits$train[1:10, ]

  # ten weights for the 141 rows of the default data: whose class is whose?
  expect_error(
    tutplot_weightupdate(d1 = rep(0.1, 10), d2 = rep(0.1, 10)),
    "one value per row of `data`"
  )
  expect_error(
    tutplot_weightupdate(data = rows, d1 = rep(0.1, 11), d2 = rep(0.1, 11)),
    "one value per row of `data`"
  )
  # a missing one is computed from `data`: 141 weights against the 10 given
  expect_error(
    tutplot_weightupdate(d1 = rep(0.1, 10)),
    "same length"
  )

  # row order: the class of row i colors the dots of weight i
  reordered <- rows[10:1, ]
  flipped <- tutplot_weightupdate(
    data = reordered,
    d1 = rep(0.1, 10),
    d2 = rep(0.1, 10),
    n = 10
  )
  expect_identical(flipped$class, as_binary(reordered$replicate))
})

test_that("both dots of a point take its observed class, as in Figure 3", {
  out <- record_column(tutplot_weightupdate())
  w <- out$value
  palette <- viridisLite::viridis(2)
  observed <- as_binary(altmext_splits$train$replicate)[1:25]

  expect_identical(w$class, observed)
  # failure purple, success yellow, as tutplot_boundary() fills them; full
  # color, so a misclassified point is not drawn paler. One dark ring for all
  expect_identical(w$fill, palette[observed + 1])
  expect_identical(w$ring, rep("grey15", 25))
  expect_identical(
    w$fill,
    style_points(observed, rep(FALSE, 25), palette)$fill
  )

  # the two points() calls, d1 and d2, draw these fills and rings
  dots <- Filter(
    \(call) identical(call[[4]], 21) && length(call[[2]]$x) == 25,
    find_calls(out$calls, "C_plotXY")
  )
  expect_length(dots, 2)
  for (call in dots) {
    expect_identical(call[[7]], w$fill)
    expect_identical(call[[6]], w$ring)
  }
  expect_identical(dots[[1]][[2]]$y, w$d1[1:25])
  expect_identical(dots[[2]][[2]]$y, w$d2[1:25])
  # and no horizontal reference line
  expect_length(find_calls(out$calls, "C_abline"), 0)
})

test_that("Figure 8's legend names the classes as the boundary legend does", {
  out <- record_column(tutplot_weightupdate())
  w <- out$value

  # the boundary figure's rule: the levels in title case, the second first
  expect_identical(w$legend, c("Success", "Failure"))
  expect_identical(
    w$legend,
    rev(tools::toTitleCase(levels(altmext_splits$train$replicate)))
  )
  # the only text drawn: no key for D1 and D2
  texts <- lapply(find_calls(out$calls, "C_text"), \(call) call[[3]])
  expect_identical(texts, list(c("Success", "Failure")))
  # each key in its class's fill, so the labels cannot swap colors, and in the
  # dots' dark ring
  palette <- viridisLite::viridis(2)
  keys <- Filter(
    \(call) length(call[[2]]$x) == 2 && all(call[[4]] == 21),
    find_calls(out$calls, "C_plotXY")
  )
  expect_length(keys, 1)
  expect_identical(keys[[1]][[7]], rev(palette))
  expect_identical(keys[[1]][[6]], rep("grey15", 2))
})

test_that("Figure 8 rings both classes in the same dark ink", {
  out <- record_column(tutplot_weightupdate())
  w <- out$value
  palette <- viridisLite::viridis(2)
  # both classes are among the points drawn, so one ring color is a finding
  expect_setequal(w$class, c(0L, 1L))

  # the ring Figure 3 gives yellow dots, on purple dots too: a white ring
  # vanishes on the white panel, and the dot looks smaller than its weight
  dark <- style_points(1L, FALSE, palette)$ring
  expect_identical(dark, "grey15")
  dots <- Filter(
    \(call) identical(call[[4]], 21) && length(call[[2]]$x) == 25,
    find_calls(out$calls, "C_plotXY")
  )
  expect_length(dots, 2)
  for (call in dots) {
    expect_identical(call[[6]], rep(dark, 25))
  }
  # and the legend keys, so a key matches the dots it names
  keys <- Filter(
    \(call) length(call[[2]]$x) == 2 && all(call[[4]] == 21),
    find_calls(out$calls, "C_plotXY")
  )
  expect_length(keys, 1)
  expect_identical(keys[[1]][[6]], rep(dark, 2))
})

test_that("the boundary figure keeps its white ring on purple dots", {
  train <- altmext_splits$train
  palette <- viridisLite::viridis(2)
  fit <- stump()
  out <- record_calls(tutplot_boundary(fit, data = train, resolution = 20))
  drawn <- Filter(
    \(call) length(call[[2]]$x) == nrow(train),
    find_calls(out$calls, "C_plotXY")
  )
  expect_length(drawn, 1)
  drawn <- drawn[[1]]

  # in drawing order, misclassified first; their rings are paler, so only the
  # correctly classified points carry the plain ring
  wrong <- find_misclassified(fit, train, "replicate")
  in_order <- order(!wrong)
  class <- as_binary(train$replicate)[in_order]
  purple <- !wrong[in_order] & class == 0L
  yellow <- !wrong[in_order] & class == 1L
  # on the colored regions a white ring shows, so Figure 3 keeps it
  expect_identical(unique(drawn[[7]][purple]), palette[1])
  expect_identical(unique(drawn[[6]][purple]), "white")
  expect_identical(unique(drawn[[6]][yellow]), "grey15")
})

test_that("Figure 8 names zero and each weight drawn on its y-axis", {
  out <- record_column(tutplot_weightupdate())
  w <- out$value

  # after round one: the initial 1/141 and the two weights it becomes
  expect_identical(
    w$ticks$at,
    c(0, sort(unique(c(w$d1[1:25], w$d2[1:25]))))
  )
  expect_identical(w$ticks$labels, c("0", "0.0049", "0.0071", "0.0132"))
  y_axes <- find_y_axes(out$calls)
  expect_length(y_axes, 1)
  expect_identical(y_axes[[1]][[3]], w$ticks$at)
  expect_identical(y_axes[[1]][[4]], w$ticks$labels)
  # horizontal, so strheight() measures what can collide
  expect_identical(y_axes[[1]]$las, 1)

  # the top stays at the largest weight plus 15 percent
  window <- find_calls(out$calls, "C_plot_window")
  expect_equal(window[[1]][[3]], c(0, max(c(w$d1, w$d2)) * 1.15))
})

test_that("Figure 8 keeps a caller's ylim, naming only the ticks inside it", {
  out <- record_column(tutplot_weightupdate(ylim = c(0, 0.01)))
  w <- out$value

  window <- find_calls(out$calls, "C_plot_window")
  expect_identical(window[[1]][[3]], c(0, 0.01))
  expect_identical(w$ticks$labels, c("0", "0.0049", "0.0071"))
})

test_that("Figure 8 falls back past four weights or on a collision", {
  rows <- altmext_splits$train[1:10, ]
  d1 <- rep(0.1, 10)
  draw <- function(d2) record_column(tutplot_weightupdate(rows, d1, d2, n = 10))

  # four distinct weights are named
  four <- draw(c(0.02, 0.04, rep(0.1, 6), 0.22, 0.22))
  expect_identical(
    four$value$ticks$labels,
    c("0", "0.02", "0.04", "0.10", "0.22")
  )

  # five are not: plot() draws its own axis, and no y-axis has positions
  five <- draw(c(0.02, 0.04, 0.06, rep(0.1, 5), 0.22, 0.22))
  expect_null(five$value$ticks)
  expect_length(find_y_axes(five$calls), 0)

  # two weights too close for their labels: ordinary ticks as well
  close <- draw(c(0.1005, rep(0.1, 9)))
  expect_null(close$value$ticks)
  expect_length(find_y_axes(close$calls), 0)
})

test_that("has_collision() flags ticks closer than one label height", {
  # one label height apart, the labels touch but do not overlap
  expect_false(has_collision(c(0, 1, 2), 1))
  expect_true(has_collision(c(0, 0.99, 2), 1))
  # in any order
  expect_true(has_collision(c(2, 0, 0.5), 1))
  # a single tick has nothing to collide with
  expect_false(has_collision(0.5, 1))
  expect_false(has_collision(numeric(0), 1))
})

test_that("plan_named_ticks() keeps the ticks inside ylim and makes room", {
  grDevices::pdf(
    NULL,
    width = tutplot_opts$col[["width"]],
    height = tutplot_opts$col[["height"]],
    pointsize = tutplot_opts$pointsize
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  old_par <- set_figure_par(reserve_legend = TRUE)
  on.exit(graphics::par(old_par), add = TRUE, after = FALSE)

  plan <- plan_named_ticks(
    at = c(3, 0, 1, 2),
    labels = c("3.00", "0", "1.00", "2.00"),
    ylim = c(0, 2.5),
    cex = 0.85
  )
  # sorted, and the tick above the panel dropped
  expect_identical(plan$at, c(0, 1, 2))
  expect_identical(plan$labels, c("0", "1.00", "2.00"))
  # horizontal labels are wider than tall, so the margin grows
  expect_gt(plan$shift, 0)

  # a tick past `ylim` but on the panel, which plot() widens by 4 percent
  edge <- plan_named_ticks(c(0, 2.55), c("0", "2.55"), c(0, 2.5), 0.85)
  expect_identical(edge$at, c(0, 2.55))

  # nothing on the panel, or two labels on top of each other: no names
  expect_null(plan_named_ticks(c(5, 6), c("5", "6"), c(0, 2.5), 0.85))
  expect_null(plan_named_ticks(
    c(1, 1.001),
    c("1.000", "1.001"),
    c(0, 2.5),
    0.85
  ))
})
test_that("run_round_one() hands back a real chi, so nothing is derived", {
  r <- run_round_one(altmext_splits$train)

  voinms <- c("power.o", "effect_size.o", "n.o", "p_value.o", "replicate")
  train <- altmext_splits$train[, voinms]
  h1 <- rpart::rpart(
    replicate ~ .,
    data = train,
    method = "class",
    maxdepth = 1,
    maxsurrogate = 0,
    weights = r$d1
  )
  y_retro <- predict(h1, newdata = train, type = "class")

  expect_identical(r$chi == -1, unname(y_retro != train$replicate))
  expect_true(all(r$chi %in% c(-1, 1)))
})

test_that("tutplot_weightupdate() ticks every point and numbers every fifth", {
  out <- record_calls(tutplot_weightupdate())
  # side 1 with tick positions: plot() also records its own x axis, without
  # positions, which `xaxt = "n"` suppresses
  x_axes <- Filter(
    \(call) identical(call[[2]], 1) && !is.null(call[[3]]),
    find_calls(out$calls, "C_axis")
  )
  at <- lapply(x_axes, \(call) call[[3]])
  numbered <- vapply(x_axes, \(call) !isFALSE(call[[4]]), logical(1))

  # the numbers stay at 1, 5, 10, ...
  expect_identical(at[numbered], list(c(1, seq(5, 25, 5))))
  # and a short tick without a number sits under each of the 25 points
  expect_identical(at[!numbered], list(seq_len(25)))
})

test_that("the mark follows marked_eta, whatever the reader sets", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  voinms <- c("power.o", "effect_size.o", "n.o", "p_value.o", "replicate")
  train <- altmext_splits$train[, voinms]
  y <- train$replicate
  d1 <- rep(1, nrow(train)) / nrow(train)
  h1 <- rpart::rpart(
    replicate ~ .,
    data = train,
    method = "class",
    maxdepth = 1,
    weights = d1
  )
  y_retro <- predict(h1, newdata = train, type = "class")
  perf <- sum(d1 * (y_retro == y))
  epsilon <- 1 - perf

  # 0.75 is deliberately not one of the drawn curves: the mark still belongs to
  # it, it simply sits between two of them
  for (e in c(1, 0.5, 0.1, 0.75)) {
    m <- tutplot_modelweight(marked_perf = perf, marked_eta = e)
    expect_equal(m$alpha, 1 / 2 * log(perf / epsilon) * e)
  }

  # the regression this fixes: omitting marked_eta pins the mark to eta[1], so a
  # reader who changes eta in Listing 22 would see a crosshair that lies
  pinned <- tutplot_modelweight(marked_perf = perf)$alpha
  expect_equal(pinned, 1 / 2 * log(perf / epsilon) * 1)
  expect_false(isTRUE(all.equal(
    pinned,
    1 / 2 * log(perf / epsilon) * 0.5
  )))
})

# ---- the bare calls draw the tutorial's Figures 6 to 8 ----------------------

# Listings 17 to 23 and Figure 7's `alpha_int`, as the tutorial script runs
# them on the shipped training set
reproduce_round_one <- function() {
  train <- altmext_splits$train
  pred_nms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
  out_nme <- "replicate"
  voi_nms <- c(pred_nms, out_nme)
  y_train <- train[, out_nme]
  m <- nrow(train)
  D1 <- rep(1, m) / m
  h1 <- rpart::rpart(
    replicate ~ .,
    data = train[, voi_nms],
    method = "class",
    maxdepth = 1,
    parms = list(split = "gini"),
    weights = D1
  )
  y_retro <- predict(h1, newdata = train[, pred_nms], type = "class")
  perf <- sum(D1 * (y_retro == y_train))
  epsilon <- 1 - perf
  eta <- 1
  alpha1 <- 0.5 * log(perf / epsilon) * eta
  chi1 <- (y_retro == y_train) * 1 + (y_retro != y_train) * -1
  D2_raw <- D1 * exp(-alpha1 * chi1)
  Z1 <- sum(D2_raw)
  D2 <- D2_raw / Z1
  alpha_int <- c(0.7, round(alpha1, 3), 0.3)
  list(
    D1 = D1,
    D2 = D2,
    chi1 = chi1,
    perf = perf,
    alpha1 = alpha1,
    alpha_int = alpha_int
  )
}

test_that("the figure defaults resolve to the tutorial's round one", {
  script <- reproduce_round_one()
  defaults <- resolve_figure_defaults()

  expect_identical(defaults$alpha, script$alpha_int)
  expect_identical(defaults$marked_perf, script$perf)
  # what the captions state: performance 0.730, model weight 0.499, and 9 of
  # the first 25 misclassified
  expect_equal(round(script$perf, 3), 0.730)
  expect_equal(round(script$alpha1, 3), 0.499)
  expect_identical(sum(script$chi1[1:25] == -1), 9L)
})

test_that("run_round_one() is the tutorial's round one, to the last bit", {
  script <- reproduce_round_one()
  r <- run_round_one(altmext_splits$train)

  expect_identical(r$d1, script$D1)
  expect_identical(r$d2, script$D2)
  expect_identical(r$chi, script$chi1)
  expect_identical(r$alpha, script$alpha1)
  expect_identical(r$perf, script$perf)
})

test_that("each bare call draws what the tutorial's call draws", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  script <- reproduce_round_one()

  # Figure 6: the dot is the first stump
  bare <- tutplot_modelweight()
  expect_identical(
    bare,
    tutplot_modelweight(eta = c(1, 0.5, 0.1), marked_perf = script$perf)
  )
  expect_identical(bare$alpha, script$alpha1)
  # Figure 7
  expect_identical(
    tutplot_updatefactor(),
    tutplot_updatefactor(alpha = script$alpha_int)
  )
  # Figure 8: the tutorial's call passes the weights only
  expect_identical(
    tutplot_weightupdate(),
    tutplot_weightupdate(d1 = script$D1, d2 = script$D2)
  )
})

test_that("the figure defaults need the package loaded, not attached", {
  # evaluated where the search path is out of reach, as in a session that
  # loaded the package without attaching it
  base_only <- new.env(parent = baseenv())
  expect_identical(
    eval(formals(tutplot_weightupdate)$data, base_only),
    altmext_splits$train
  )
  resolve <- resolve_figure_defaults
  environment(resolve) <- list2env(
    list(run_round_one = run_round_one),
    parent = baseenv()
  )
  expect_identical(resolve(), resolve_figure_defaults())
})

test_that("the default mark gives way to `from`, an asked-for one does not", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  perf <- resolve_figure_defaults()$marked_perf

  # a panel that starts above the tutorial's stump drops the default mark
  above <- perf + 0.05
  expect_null(expect_silent(tutplot_modelweight(from = above))$alpha)
  # but the same mark, asked for, is refused rather than hidden
  expect_error(
    tutplot_modelweight(from = above, marked_perf = perf),
    "must be one value"
  )
  # a numeric NA asks for no mark as well
  expect_null(tutplot_modelweight(marked_perf = NA_real_)$alpha)
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
    # pdfinfo prints dates in the system encoding; match bytes, not characters
    out <- system2("pdfinfo", p, stdout = TRUE)
    sub(".*: *", "", grep("^Page size", out, value = TRUE, useBytes = TRUE))
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

test_that("set_figure_par() restores what it found, both ways", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  keys <- c("mar", "mgp", "tcl", "cex.axis")
  for (leg in c(TRUE, FALSE)) {
    before <- graphics::par(keys)
    op <- set_figure_par(reserve_legend = leg)
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

  # gini, importance and logocv have no legend above the panel; the other
  # three do. The importance chart sets its margins in the helper that
  # tutplot_importance() and plot.gauge() share
  body_of <- function(f) paste(deparse(body(f)), collapse = " ")
  for (f in list(tutplot_gini, draw_importance, tutplot_logocv)) {
    expect_match(
      body_of(f),
      "set_figure_par(reserve_legend = FALSE)",
      fixed = TRUE
    )
  }
  for (f in list(
    tutplot_updatefactor,
    tutplot_modelweight,
    tutplot_weightupdate
  )) {
    expect_match(
      body_of(f),
      "set_figure_par(reserve_legend = TRUE)",
      fixed = TRUE
    )
  }
})

test_that("tutplot_boundary() draws at the manuscript's full text width", {
  skip_if_not(nzchar(Sys.which("pdfinfo")), "pdfinfo not available")
  f <- tempfile(fileext = ".pdf")
  g <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(f, g)), add = TRUE)

  pts <- function(p) {
    # pdfinfo prints dates in the system encoding; match bytes, not characters
    out <- system2("pdfinfo", p, stdout = TRUE)
    v <- sub(
      ".*: *",
      "",
      grep("^Page size", out, value = TRUE, useBytes = TRUE)
    )
    as.numeric(regmatches(v, gregexpr("[0-9.]+", v))[[1]])[1:2]
  }

  tutplot_boundary(stump(), data = altmext_splits$train, file = f)
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

  train <- altmext_splits$train
  before <- list.files(".", pattern = "\\.pdf$")

  # a single stump: two leaves, so the score takes exactly two values
  one <- tutplot_boundary(stump(), data = train, resolution = 40)
  expect_identical(one$features, c("power.o", "n.o"))
  expect_identical(dim(one$z), c(40L, 40L))
  expect_length(unique(as.vector(one$z)), 2L)

  # an ensemble: the same call, and the margin takes many values. `n_iter` is small
  # because this is about the wrapper, not about boosting
  ens <- rpart::rpart(
    replicate ~ .,
    data = train[, c("power.o", "n.o", "replicate")],
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 20, eta = 1, verbose = FALSE, check_inputs = FALSE)
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

test_that("tutplot_boundary() names the classes in title case", {
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext not available")
  paths <- replicate(4, tempfile(fileext = ".pdf"))
  on.exit(unlink(paths), add = TRUE)
  train <- altmext_splits$train

  # the color bar's ends, then the class legend
  tutplot_boundary(stump(), data = train, resolution = 20, file = paths[1])
  tutplot_boundary(
    stump(),
    data = train,
    resolution = 20,
    shade = "class",
    file = paths[2]
  )
  for (path in paths[1:2]) {
    drawn <- read_pdf_text(path)
    expect_match(drawn, "Failure", fixed = TRUE, useBytes = TRUE)
    expect_match(drawn, "Success", fixed = TRUE, useBytes = TRUE)
    expect_no_match(drawn, "failure", fixed = TRUE, useBytes = TRUE)
  }

  # without the outcome in `data`, the bar falls back to its own names
  tutplot_boundary(
    stump(),
    data = train[, c("power.o", "n.o")],
    resolution = 20,
    file = paths[3]
  )
  expect_match(
    read_pdf_text(paths[3]),
    "Negative",
    fixed = TRUE,
    useBytes = TRUE
  )
  expect_match(
    read_pdf_text(paths[3]),
    "Positive",
    fixed = TRUE,
    useBytes = TRUE
  )

  # axis labels the caller passes are drawn as given
  tutplot_boundary(
    stump(),
    data = train,
    resolution = 20,
    xlab = "Power",
    ylab = "Sample Size",
    file = paths[4]
  )
  expect_match(
    read_pdf_text(paths[4]),
    "Sample Size",
    fixed = TRUE,
    useBytes = TRUE
  )
  expect_no_match(read_pdf_text(paths[4]), "n.o", fixed = TRUE, useBytes = TRUE)
})

test_that("tutplot_boundary() draws no key for misclassified points", {
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext not available")
  paths <- replicate(2, tempfile(fileext = ".pdf"))
  on.exit(unlink(paths), add = TRUE)
  train <- altmext_splits$train

  flawed <- tutplot_boundary(
    stump(),
    data = train,
    resolution = 20,
    file = paths[1]
  )
  tutplot_boundary(
    stump(),
    data = train,
    resolution = 20,
    shade = "class",
    file = paths[2]
  )
  # there are misclassified points to key, and still no key
  expect_gt(flawed$n_misclassified, 0L)
  for (path in paths) {
    drawn <- read_pdf_text(path)
    expect_no_match(drawn, "Correct", fixed = TRUE, useBytes = TRUE)
    expect_no_match(drawn, "Misclassified", fixed = TRUE, useBytes = TRUE)
  }
  # the count is the caption's, not the figure's
  expect_no_match(
    read_pdf_text(paths[1]),
    paste(flawed$n_misclassified, "of"),
    fixed = TRUE,
    useBytes = TRUE
  )
})

test_that("only the color bar marks the boundary", {
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext not available")
  paths <- replicate(3, tempfile(fileext = ".pdf"))
  on.exit(unlink(paths), add = TRUE)
  train <- altmext_splits$train

  tutplot_boundary(stump(), data = train, resolution = 20, file = paths[1])
  tutplot_boundary(
    stump(),
    data = train,
    resolution = 20,
    shade = "class",
    file = paths[2]
  )
  tutplot_boundary(
    stump(),
    data = train,
    resolution = 20,
    add_colorbar = FALSE,
    file = paths[3]
  )
  expect_match(
    read_pdf_text(paths[1]),
    "Boundary",
    fixed = TRUE,
    useBytes = TRUE
  )
  for (path in paths[2:3]) {
    expect_no_match(
      read_pdf_text(path),
      "Boundary",
      fixed = TRUE,
      useBytes = TRUE
    )
  }
})

test_that("tutplot_logoscheme() builds one fold per project, test on the diagonal", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  g <- drop_holz()$pid
  f <- tutplot_logoscheme(g)

  # one iteration per project, and every row accounted for
  expect_identical(f$k, length(unique(g)))
  expect_identical(sum(f$n), length(g))
  expect_identical(as.integer(f$n[["rpp"]]), sum(g == "rpp"))

  # heights are the project shares, so they carry the imbalance; ML3 falls
  # under the default minimum height, so the exact shares need it off
  exact <- tutplot_logoscheme(g, min_height = 0)
  expect_equal(sum(exact$heights), 1)
  expect_equal(exact$heights, as.numeric(exact$n) / sum(exact$n))
  expect_equal(max(exact$heights), sum(g == "rpp") / length(g))
  # with the minimum, the heights still fill the figure and the largest
  # project still draws the tallest row
  expect_equal(sum(f$heights), 1)
  expect_identical(f$levels[which.max(f$heights)], "rpp")
})

test_that("tutplot_logoscheme() draws the order it is given, not table()'s", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # `table()` sorts, which would silently rearrange the manuscript's figure;
  # every project, in an order that is not the sorted one
  pid <- drop_holz()$pid
  given <- rev(sort(unique(pid)))
  f <- tutplot_logoscheme(pid, levels = given)
  expect_identical(f$levels, given)
  expect_identical(names(f$n), given)
  expect_false(identical(names(f$n), sort(given)))

  # a factor brings its own order
  g <- factor(pid, levels = given)
  expect_identical(tutplot_logoscheme(g)$levels, given)

  # and a level that is not in the data is refused rather than drawn empty
  expect_error(
    tutplot_logoscheme(pid, levels = c(given, "nope")),
    "nope"
  )
})

test_that("tutplot_logoscheme() equal blocks hide what proportional ones show", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  prop <- tutplot_logoscheme(drop_holz()$pid, proportional = TRUE)
  flat <- tutplot_logoscheme(drop_holz()$pid, proportional = FALSE)

  expect_equal(flat$heights, rep(1 / flat$k, flat$k))
  expect_false(isTRUE(all.equal(prop$heights, flat$heights)))
  # the counts are the same either way; only the drawing differs
  expect_identical(prop$n, flat$n)

  # the fact the proportional variant exists to show: one fold holds out more
  # than it trains on
  tot <- sum(prop$n)
  expect_true(any(prop$n > tot - prop$n))
})

test_that("tutplot_logoscheme() keeps thin projects tall enough to label", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # of altmext's seven projects, only ML3 falls below: 5 percent of the rows
  f <- tutplot_logoscheme(altmext$pid)
  expect_equal(sum(f$heights), 1)
  expect_equal(f$heights[f$levels == "ml3"], 0.06)
  expect_true(all(f$heights[f$levels != "ml3"] > 0.06))
  # no smaller project is drawn taller than a larger one
  expect_true(all(diff(f$heights[order(f$n)]) >= -1e-12))

  # 0 switches the minimum off
  exact <- tutplot_logoscheme(altmext$pid, min_height = 0)
  expect_equal(exact$heights, as.numeric(exact$n) / sum(exact$n))
})

test_that("tutplot_logoscheme() draws only the projects in `levels`", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  shown <- c("rpp", "holz", "ml3")
  f <- tutplot_logoscheme(altmext$pid, levels = shown)
  expect_identical(f$k, 3L)
  expect_identical(names(f$n), shown)
  # the shares are of the shown projects only: ML3 holds 8 percent of these
  # rows, so nothing needs raising
  expect_equal(f$heights, as.numeric(f$n) / sum(f$n))
})

test_that("tutplot_logoscheme() refuses a minimum the projects cannot fit", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(
    tutplot_logoscheme(altmext$pid, min_height = 0.2),
    "at most 1/7"
  )
  # equal blocks have no minimum to fit
  expect_no_error(
    tutplot_logoscheme(altmext$pid, proportional = FALSE, min_height = 0.2)
  )
})

test_that("Figure 1's first column draws each project as its test box does", {
  paper <- c("ml1", "rpp", "ml3", "eerp", "ssrp", "ml2")
  drawn <- record_calls(
    tutplot_logoscheme(drop_holz()$pid, levels = paper),
    width = tutplot_opts$full[["width"]],
    height = tutplot_opts$full[["height"]],
    pointsize = tutplot_opts$pointsize
  )
  colors <- unname(color_projects(paper))
  fill_of <- \(calls) vapply(calls, \(call) call[[4]], character(1))
  boxes <- find_calls(drawn$calls, "C_polygon")

  # the first column ends left of x = 0.5, where Iteration 1 begins
  first <- Filter(\(call) max(call[[2]]) < 0.5, boxes)
  expect_length(first, length(paper))
  expect_identical(fill_of(first), colors)
  # project j's test box: column j, row j, above the performance boxes
  test_boxes <- lapply(seq_along(paper), \(j) {
    column <- Filter(
      \(call) {
        min(call[[2]]) > j - 0.5 &&
          max(call[[2]]) < j + 0.5 &&
          min(call[[3]]) >= 0
      },
      boxes
    )
    column[[j]]
  })
  expect_identical(fill_of(first), fill_of(test_boxes))

  # each name in its test box's ink, so a dark fill gets white letters
  ink_of <- \(calls) vapply(calls, \(call) call[[9]], character(1))
  texts <- find_calls(drawn$calls, "C_text")
  names_drawn <- Filter(\(call) identical(call[[2]]$x, -0.07), texts)
  expect_identical(
    vapply(names_drawn, \(call) call[[3]], character(1)),
    toupper(paper)
  )
  test_labels <- Filter(\(call) identical(call[[3]], "Test"), texts)
  expect_identical(ink_of(names_drawn), ink_of(test_labels))
  expect_identical(ink_of(names_drawn), choose_ink(colors))
  # the paper's six colors need both inks
  expect_setequal(ink_of(names_drawn), c("white", "grey15"))
})

test_that("draw_roundrect() keeps its corners circular on any panel", {
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
  expect_silent(draw_roundrect(1, 0.1, 3, 0.9, col = "grey80"))
})

test_that("choose_ink() picks readable text for both ends of viridis", {
  pal <- viridisLite::viridis(5)
  expect_identical(choose_ink(pal[1]), "white")
  expect_identical(choose_ink(pal[5]), "grey15")
  expect_identical(choose_ink(c("#440154", "#FDE725")), c("white", "grey15"))
})

test_that("color_projects() colors by name, alphabetically without case", {
  paper <- c("ml1", "rpp", "ml3", "eerp", "ssrp", "ml2")
  colors <- color_projects(paper)
  ramp <- viridisLite::viridis(6, end = tutplot_opts$viridis_end)

  # one color per project, named by it, in the order given
  expect_identical(names(colors), paper)
  # the ramp goes out in alphabetical order, whatever order the input has
  expect_identical(unname(colors[sort(paper)]), ramp)
  expect_identical(color_projects(rev(paper)), colors[rev(paper)])
  # a repeated project gets one color
  expect_identical(color_projects(c(paper, "ml1")), colors)

  # capitals change no color, though C collation sorts "ML1" before "eerp"
  expect_identical(unname(color_projects(toupper(paper))), unname(colors))
  expect_identical(
    unname(color_projects(c("ML1", "eerp"))),
    viridisLite::viridis(2, end = tutplot_opts$viridis_end)[2:1]
  )
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

  tutplot_weightupdate(file = g)
  expect_match(txt(g), "Data Point")
  expect_no_match(txt(g), "Data point")
})

test_that("tutplot_logoscheme() leaves the figure's title to the caption", {
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext not available")
  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f), add = TRUE)

  tutplot_logoscheme(drop_holz()$pid, file = f)
  out <- paste(system2("pdftotext", c(f, "-"), stdout = TRUE), collapse = " ")

  # the axis label and the column headers stay
  expect_match(out, "Data")
  expect_match(out, "Iteration 1")
  # the in-panel title does not: it duplicated the numbered caption
  expect_no_match(out, "Leave-Project-Out Cross-Validation")
})

# ---- tutplot_logocv ---------------------------------------------------------

logocv_res <- function() {
  df_noholz <- drop_holz()
  fit <- rpart::rpart(
    replicate ~ power.o + effect_size.o + n.o + p_value.o,
    data = df_noholz,
    method = "class",
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE)
  logo_cv(fit, data = df_noholz, group = "pid", verbose = FALSE)
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
  expect_true(all(
    facts$lower <= facts$estimate & facts$estimate <= facts$upper
  ))
  # the share the caption states; no flag, since nothing is starred any more
  expect_type(facts$rejected, "double")
  expect_false("flagged" %in% names(facts))
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

test_that("tutplot_logocv() checks its arguments before it opens a device", {
  set.seed(1)
  # intervals, so the bands are used
  boot <- suppressWarnings(bootstrap(logocv_res(), n_resample = 20))
  # no device of our own: it would hide one the call opens. One open here
  # leaked from an earlier test
  expect_null(grDevices::dev.list(), label = "the devices open at the start")
  grDevices::graphics.off()
  # R's default device would write Rplots.pdf to the working directory
  wd <- file.path(tempdir(), "tutplot-logocv")
  dir.create(wd, showWarnings = FALSE)
  old <- setwd(wd)
  on.exit(
    {
      # close a device the call opened, so a failure leaves none behind
      grDevices::graphics.off()
      setwd(old)
      unlink(wd, recursive = TRUE)
    },
    add = TRUE
  )

  # each of these once stopped only after pdf() or par() had opened a device
  bad <- list(
    "an unknown metric" = list(metric = "not_a_metric"),
    "a missing metric" = list(metric = NA),
    "two metrics" = list(metric = c("auroc", "acc")),
    "bands in a list" = list(bands = list(0.5)),
    "a band above 1" = list(bands = 1.5),
    "no bands" = list(bands = numeric(0)),
    # do.call() puts the name into the call, so the lookup fails when the
    # label is first used, as a typo in a script would
    "a label that does not exist" = list(ylab = quote(label_not_defined))
  )
  for (case in names(bad)) {
    for (file in list(NULL, "figure.pdf")) {
      label <- paste(case, if (is.null(file)) "and no file" else "and a file")
      # two metrics warn on their way to the error
      expect_error(
        suppressWarnings(do.call(
          tutplot_logocv,
          c(list(boot), bad[[case]], list(file = file))
        )),
        label = paste("tutplot_logocv() with", label)
      )
      expect_null(
        grDevices::dev.list(),
        label = paste("the devices after", label)
      )
      grDevices::graphics.off()
      expect_identical(
        list.files(wd),
        character(0),
        label = paste("the files after", label)
      )
      unlink(list.files(wd, full.names = TRUE))
    }
  }
  expect_error(
    tutplot_logocv(boot, metric = "not_a_metric", file = "figure.pdf"),
    "no estimates"
  )
})

test_that("tutplot_logocv() rejects an unknown metric before it resamples", {
  res <- logocv_res()
  expect_null(res$ci)
  # a stand-in that records the call and passes the result on unchanged, so
  # the call still reaches the metric check when it resamples first
  resampled <- FALSE
  local_mocked_bindings(
    bootstrap.logo_cv = function(x, ...) {
      resampled <<- TRUE
      x
    }
  )

  expect_error(
    tutplot_logocv(res, metric = "aurco"),
    "no estimates for metric `aurco`"
  )
  expect_false(resampled)
})

test_that("tutplot_logocv() and plot() draw on the caller's device", {
  res <- logocv_res()
  grDevices::pdf(NULL)
  mine <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(mine), add = TRUE)
  devices <- grDevices::dev.list()
  grDevices::dev.control("enable")

  tutplot_logocv(res, bootstrap = FALSE)
  expect_identical(grDevices::dev.cur(), mine)
  expect_gt(length(grDevices::recordPlot()[[1]]), 0L)
  plot(res)
  expect_identical(grDevices::dev.cur(), mine)
  expect_identical(grDevices::dev.list(), devices)
})

test_that("tutplot_logocv() draws no keys, capitals and an italic n", {
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext not available")
  res <- logocv_res()
  set.seed(1)
  boot <- suppressWarnings(bootstrap(res, n_resample = 50))
  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f), add = TRUE)

  facts <- tutplot_logocv(boot, file = f)
  drawn <- read_pdf_text(f)

  # the caption carries both keys
  expect_no_match(drawn, "Dashed line", fixed = TRUE, useBytes = TRUE)
  expect_no_match(drawn, "Bootstrap intervals", fixed = TRUE, useBytes = TRUE)
  # project names in capitals, the metric by its readable name
  for (project in toupper(res$projects$project)) {
    expect_match(drawn, project, fixed = TRUE, useBytes = TRUE)
  }
  expect_no_match(drawn, "eerp", fixed = TRUE, useBytes = TRUE)
  expect_match(drawn, "AUROC", fixed = TRUE, useBytes = TRUE)
  # the facts keep the names in the data
  expect_setequal(facts$project, res$projects$project)

  # the n above each project is the figure's only italic text, with the plain
  # " = " and its spaces
  expect_match(
    drawn,
    paste("n =", res$projects$n[1]),
    fixed = TRUE,
    useBytes = TRUE
  )
  skip_if_not(nzchar(Sys.which("pdffonts")), "pdffonts not available")
  expect_true(any(grepl("Oblique", read_pdf_fonts(f), useBytes = TRUE)))
  expect_false(any(grepl("Symbol", read_pdf_fonts(f), useBytes = TRUE)))
  plain <- tempfile(fileext = ".pdf")
  on.exit(unlink(plain), add = TRUE)
  grDevices::pdf(plain)
  plot(boot)
  grDevices::dev.off()
  expect_false(any(grepl("Oblique", read_pdf_fonts(plain), useBytes = TRUE)))
})

test_that("Figure 1 and the estimates figure give a project one color", {
  # the script's six projects
  data(altmext)
  six <- altmext[altmext$pid != "holz", ]
  fit <- rpart::rpart(
    replicate ~ power.o + effect_size.o + n.o + p_value.o,
    data = six,
    method = "class",
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE)
  res <- logo_cv(fit, data = six, group = "pid", verbose = FALSE)
  paper <- c("ml1", "rpp", "ml3", "eerp", "ssrp", "ml2")

  # the colored dots, one per project; the white halos come first
  read_dots <- function(calls) {
    dots <- Filter(
      \(call) length(call[[2]]$x) == 6L && !all(call[[6]] == "white"),
      find_calls(calls, "C_plotXY")
    )
    expect_length(dots, 1L)
    dots[[1]][[6]]
  }
  drawn <- record_calls(tutplot_logocv(res, bootstrap = FALSE))
  estimate_colors <- read_dots(drawn$calls)
  names(estimate_colors) <- drawn$value$project
  # plot() draws through the same code, so it agrees
  expect_identical(
    read_dots(record_calls(plot(res))$calls),
    unname(estimate_colors)
  )

  read_scheme <- function(group, levels) {
    drawn <- record_calls(tutplot_logoscheme(group, levels = levels))
    fills <- vapply(
      find_calls(drawn$calls, "C_polygon"),
      \(call) call[[4]],
      character(1)
    )
    # past the first column's boxes, full strength: each column's test box,
    # then its performance box
    fills <- fills[-seq_along(levels)]
    full <- fills[endsWith(fills, "FF")]
    expect_identical(full[c(TRUE, FALSE)], full[c(FALSE, TRUE)])
    stats::setNames(full[c(TRUE, FALSE)], tolower(levels))
  }
  # the paper's row order, so colors dealt out by row would fail
  expect_identical(read_scheme(altmext$pid, paper), estimate_colors[paper])
  # capitals in the scheme change nothing
  expect_identical(
    read_scheme(toupper(altmext$pid), toupper(paper)),
    estimate_colors[paper]
  )
})

test_that("label_metric() names every metric assess() returns", {
  # each gets a readable name, not its code
  for (metric in list_metrics()) {
    expect_false(identical(label_metric(metric), metric), info = metric)
  }
  # and so does any budget assess() is given
  at_k <- names(assess(c(1, 0, 1, 0), c(2, -1, 1, -2), k = c(1, 10)))
  for (metric in grep("^patk", at_k, value = TRUE)) {
    expect_false(identical(label_metric(metric), metric), info = metric)
  }
  expect_identical(label_metric("auroc"), "AUROC")
  expect_identical(label_metric("bacc"), "Balanced Accuracy")
  expect_identical(label_metric("patk_10"), "Precision at 10")
  # a name it does not know is shown as typed
  expect_identical(label_metric("my_metric"), "my_metric")
  # it is the axis label's default
  expect_identical(
    eval(formals(tutplot_logocv)$ylab, list(metric = "mcc")),
    "MCC"
  )
})

# tutplot_importance(): the lollipop chart, shared with plot.gauge()
boosted_stumps <- function(n_iter = 20) {
  rpart::rpart(
    replicate ~ power.o + n.o + effect_size.o + p_value.o,
    data = altmext_splits$train,
    method = "class",
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = n_iter, eta = 1, verbose = FALSE)
}

test_that("tutplot_importance() draws the gauge rows, most important first", {
  imp <- gauge(boosted_stumps())

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_invisible(tutplot_importance(imp))
  drawn <- tutplot_importance(imp)
  expect_identical(drawn$variable, imp$variable)
  expect_identical(drawn$importance, imp$importance)

  top <- tutplot_importance(imp, top_n = 2)
  expect_identical(top$variable, imp$variable[1:2])
})

test_that("tutplot_importance() restores par() and writes no file by default", {
  wd <- file.path(tempdir(), "tutplot-importance")
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
  before <- graphics::par(c("mar", "mgp", "tcl", "cex.axis"))

  tutplot_importance(gauge(boosted_stumps()))

  expect_identical(graphics::par(c("mar", "mgp", "tcl", "cex.axis")), before)
  expect_identical(list.files(wd), character(0))
})

test_that("tutplot_importance() writes one PDF and leaves the device stack intact", {
  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f), add = TRUE)

  before <- length(grDevices::dev.list())
  tutplot_importance(gauge(boosted_stumps()), file = f)

  expect_true(file.exists(f))
  expect_identical(length(grDevices::dev.list()), before)
})

test_that("tutplot_importance() refuses what gauge() did not make, or an empty gauge", {
  expect_error(
    tutplot_importance(data.frame(variable = "x", importance = 100)),
    "gauge()",
    fixed = TRUE
  )

  bare <- rpart::rpart(replicate ~ power.o, data = altmext_splits$train, cp = 1)
  empty <- gauge(list(t1 = list(h = bare, alpha = 0.5)))
  expect_error(tutplot_importance(empty), "no variable was split on")
})

test_that("plot.gauge() and tutplot_importance() share one drawing", {
  # one helper draws both, so the listing's plot() and the printed figure are
  # one picture; only the axis title's case differs
  body_of <- function(f) paste(deparse(body(f)), collapse = " ")
  expect_match(body_of(plot.gauge), "draw_importance(", fixed = TRUE)
  expect_match(body_of(tutplot_importance), "draw_importance(", fixed = TRUE)
  # and no argument was added to the figure function for it
  expect_false("xlab" %in% names(formals(tutplot_importance)))
})

test_that("tutplot_importance() draws labels in place of variable names", {
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext not available")
  imp <- gauge(boosted_stumps())
  f <- tempfile(fileext = ".pdf")
  g <- tempfile(fileext = ".pdf")
  # a device of our own for the last call, which has no file
  grDevices::pdf(NULL)
  on.exit(
    {
      grDevices::dev.off()
      unlink(c(f, g))
    },
    add = TRUE
  )

  # plain text; a variable without an entry keeps its name
  drawn <- tutplot_importance(
    imp,
    labels = c(n.o = "Sample Size", power.o = "Power"),
    file = f
  )
  text <- read_pdf_text(f)
  expect_match(text, "Sample Size", fixed = TRUE, useBytes = TRUE)
  expect_match(text, "Power", fixed = TRUE, useBytes = TRUE)
  expect_no_match(text, "n.o", fixed = TRUE, useBytes = TRUE)
  expect_match(text, "effect_size.o", fixed = TRUE, useBytes = TRUE)
  # the rows it returns keep the names in the data
  expect_identical(drawn$variable, imp$variable)

  # plotmath, for an italic p
  tutplot_importance(
    imp,
    labels = list(p_value.o = quote(italic(p) ~ Value), n.o = "Sample Size"),
    file = g
  )
  text <- read_pdf_text(g)
  expect_match(text, "Value", fixed = TRUE, useBytes = TRUE)
  expect_match(text, "Sample Size", fixed = TRUE, useBytes = TRUE)
  expect_no_match(text, "p_value.o", fixed = TRUE, useBytes = TRUE)
  # an expression vector works as well as a list
  expect_no_error(
    tutplot_importance(imp, labels = expression(p_value.o = italic(p) ~ Value))
  )

  skip_if_not(nzchar(Sys.which("pdffonts")), "pdffonts not available")
  expect_true(any(grepl("Oblique", read_pdf_fonts(g), useBytes = TRUE)))
  expect_false(any(grepl("Oblique", read_pdf_fonts(f), useBytes = TRUE)))
})

test_that("tutplot_importance() refuses labels it cannot place", {
  imp <- gauge(boosted_stumps())
  expect_error(
    tutplot_importance(imp, labels = c(nope = "Nope", n.o = "Sample Size")),
    "not in `importance`: nope"
  )
  expect_error(tutplot_importance(imp, labels = "Power"), "must be named")
  expect_error(
    tutplot_importance(imp, labels = list(n.o = 1)),
    "Each entry of `labels` must be one string or a plotmath expression."
  )
})

test_that("tutplot_importance() sizes the left margin to the labels drawn", {
  imp <- gauge(boosted_stumps())
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  long <- quote(italic(p) ~ "Value with a very long tail of words")

  # the margin is measured on what axis() draws, not on the variable names
  measured <- NULL
  real_margin <- compute_left_margin
  local_mocked_bindings(
    compute_left_margin = function(labels) {
      measured <<- labels
      real_margin(labels)
    }
  )
  tutplot_importance(imp, labels = list(p_value.o = long))
  expect_true(is.expression(measured))
  expect_true(any(vapply(measured, identical, logical(1), long)))
  expect_false("p_value.o" %in% as.character(measured))

  # a label wider than its name widens the margin
  set_figure_par(reserve_legend = FALSE)
  expect_gt(
    compute_left_margin(as.expression(list(long))),
    compute_left_margin("p_value.o")
  )
})

test_that("plot.gauge() draws the variable names as typed", {
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext not available")
  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f), add = TRUE)

  grDevices::pdf(f)
  plot(gauge(boosted_stumps()))
  grDevices::dev.off()

  text <- read_pdf_text(f)
  expect_match(text, "p_value.o", fixed = TRUE, useBytes = TRUE)
  expect_match(text, "n.o", fixed = TRUE, useBytes = TRUE)
})

test_that("the figure titles its axis in title case, plot() in sentence case", {
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext not available")
  imp <- gauge(boosted_stumps())
  f <- tempfile(fileext = ".pdf")
  g <- tempfile(fileext = ".pdf")
  # a device of our own, so nothing is left open if a call fails
  grDevices::pdf(NULL)
  on.exit(
    {
      grDevices::dev.off()
      unlink(c(f, g))
    },
    add = TRUE
  )

  tutplot_importance(imp, file = f)
  grDevices::pdf(g)
  plot(imp)
  grDevices::dev.off()

  figure <- read_pdf_text(f)
  expect_match(figure, "Relative Importance (%)", fixed = TRUE, useBytes = TRUE)
  expect_no_match(figure, "Relative importance", fixed = TRUE, useBytes = TRUE)
  method <- read_pdf_text(g)
  expect_match(method, "Relative importance (%)", fixed = TRUE, useBytes = TRUE)
  expect_no_match(method, "Relative Importance", fixed = TRUE, useBytes = TRUE)
})
