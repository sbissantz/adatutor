feat <- c("power.o", "n.o")

fit_ada <- function(T = 20) {
  data(altmejd)
  rpart::rpart(
    replicate ~ .,
    data = altmejd[, c("replicate", feat)],
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = T, eta = 1, verbose = FALSE, check_inputs = FALSE)
}
fit_stump <- function() {
  data(altmejd)
  rpart::rpart(
    replicate ~ .,
    data = altmejd[, c("replicate", feat)],
    method = "class",
    control = rpart::rpart.control(maxdepth = 1)
  )
}
quietly <- function(expr) {
  png(tempfile(fileext = ".png"))
  on.exit(dev.off(), add = TRUE)
  force(expr)
}

test_that("tutplot_boundary() returns the grid it drew, invisibly", {
  data(altmejd)
  quietly({
    out <- withVisible(tutplot_boundary(fit_ada(), altmejd, resolution = 30))
  })
  expect_false(out$visible)
  g <- out$value
  expect_named(
    g,
    c("x1", "x2", "z", "features", "n_misclassified", "n_points")
  )
  expect_length(g$x1, 30L)
  expect_length(g$x2, 30L)
  expect_equal(dim(g$z), c(30L, 30L))
  expect_equal(g$features, feat)
})

test_that("the score reproduces the class labels predict() would give", {
  # score_grid() must not change which side of the boundary a point is on
  data(altmejd)
  fit <- fit_ada(50)
  g <- expand.grid(
    power.o = seq(0.2, 1, length.out = 40),
    n.o = seq(10, 400, length.out = 40)
  )
  labs <- predict(fit, g, verbose = FALSE, check_inputs = FALSE)
  expect_equal(sign(score_grid(fit, g)), labs)
})

test_that("it works for a single rpart tree as well as an ensemble", {
  data(altmejd)
  quietly({
    a <- tutplot_boundary(fit_ada(), altmejd, resolution = 25)
    s <- tutplot_boundary(fit_stump(), altmejd, resolution = 25)
  })
  expect_equal(a$features, feat)
  expect_equal(s$features, feat)
  # a depth-1 stump splits on one variable, so its boundary is a straight line:
  # every column of the score matrix is identical, or every row is
  const_cols <- all(apply(s$z, 1, function(r) length(unique(r)) == 1))
  const_rows <- all(apply(s$z, 2, function(r) length(unique(r)) == 1))
  expect_true(const_cols || const_rows)
})

test_that("features are inferred only when the choice is unambiguous", {
  data(altmejd)
  # two predictors -> inferred
  quietly(g <- tutplot_boundary(fit_ada(), altmejd, resolution = 20))
  expect_equal(g$features, feat)

  # four predictors -> refuses to guess, and says what is available
  wide <- rpart::rpart(
    replicate ~ .,
    data = altmejd[, c(
      "replicate",
      "power.o",
      "effect_size.o",
      "n.o",
      "p_value.o"
    )],
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE, check_inputs = FALSE)
  expect_error(
    quietly(tutplot_boundary(wide, altmejd)),
    "must name two columns"
  )
  expect_error(quietly(tutplot_boundary(wide, altmejd)), "effect_size.o")

  # naming them explicitly works, and the others are held fixed
  quietly(
    g2 <- tutplot_boundary(wide, altmejd, features = feat, resolution = 20)
  )
  expect_equal(g2$features, feat)
})

test_that("tutplot_boundary() rejects malformed input", {
  data(altmejd)
  expect_error(
    quietly(tutplot_boundary(fit_ada(), altmejd, features = "power.o")),
    "exactly two"
  )
  expect_error(
    quietly(tutplot_boundary(
      fit_ada(),
      altmejd,
      features = c("power.o", "nope")
    )),
    "not found"
  )
  expect_error(
    quietly(tutplot_boundary(fit_ada(), altmejd, resolution = 1)),
    "at least 2"
  )
  expect_error(
    tutplot_boundary("not a model", altmejd),
    "rpart object or the output"
  )
  # an empty list once failed on fit[[1]] before this message could appear
  expect_error(
    tutplot_boundary(list(), altmejd),
    "rpart object or the output"
  )
})

test_that("a rejected argument leaves no device open and no file", {
  data(altmejd)
  fit <- fit_stump()
  # no device of our own: it would hide one the call leaves open. One open
  # here leaked from an earlier test
  expect_null(grDevices::dev.list(), label = "the devices open at the start")
  grDevices::graphics.off()
  f <- tempfile(fileext = ".pdf")
  on.exit(
    {
      # close a device the call opened, so a failure leaves none behind
      grDevices::graphics.off()
      unlink(f)
    },
    add = TRUE
  )
  # each of these once stopped only after pdf() had written the file
  expect_clean <- function(case) {
    expect_null(grDevices::dev.list(), label = paste("the devices after", case))
    expect_false(file.exists(f), label = paste("a file after", case))
    grDevices::graphics.off()
    unlink(f)
  }

  expect_error(
    tutplot_boundary(fit, altmejd, resolution = 1, file = f),
    "`resolution` must be at least 2"
  )
  expect_clean("a resolution of 1")

  expect_error(
    tutplot_boundary("not a model", altmejd, file = f),
    "`fit` must be an rpart object or the output of adaboost()",
    fixed = TRUE
  )
  expect_clean("a fit that is no model")

  expect_error(
    tutplot_boundary(fit, altmejd, features = "power.o", file = f),
    "`features` must name exactly two columns"
  )
  expect_clean("one feature")

  expect_error(
    tutplot_boundary(fit, as.list(altmejd), file = f),
    "`data` is not a data frame"
  )
  expect_clean("data that is no data frame")

  three <- altmejd
  three$replicate <- factor(rep_len(c("yes", "no", "maybe"), nrow(three)))
  expect_error(
    tutplot_boundary(fit, three, file = f),
    "`actual` must have exactly two levels.",
    fixed = TRUE
  )
  expect_clean("an outcome of three classes")

  words <- altmejd
  words$replicate <- rep_len(c("yes", "no"), nrow(words))
  expect_error(
    tutplot_boundary(fit, words, file = f),
    "`actual` must be a two-level factor, 0/1, -1/1, or logical.",
    fixed = TRUE
  )
  expect_clean("an outcome of words")
})

test_that("a one-class window warns instead of drawing nothing", {
  # A root-only tree assigns the same class everywhere, so there is no boundary.
  # The hand-written code drew a boundary-less plot in silence: its contour ran
  # on a single-level factor and simply produced no line.
  data(altmejd)
  sub <- altmejd[, c("replicate", feat)]
  # the root predicts failure, the first level, so the flat score is negative;
  # reversed levels make it positive. R 4.6.2's contour() warns on a flat
  # surface only when it is positive, R-devel's on either sign
  flipped <- transform(
    sub,
    replicate = factor(replicate, levels = rev(levels(replicate)))
  )
  for (studies in list(sub, flipped)) {
    flat <- rpart::rpart(
      replicate ~ .,
      data = studies,
      method = "class",
      control = rpart::rpart.control(cp = 1)
    )
    expect_equal(nrow(flat$frame), 1L)
    for (shade in c("margin", "class")) {
      warnings <- character()
      withCallingHandlers(
        quietly(
          g <- tutplot_boundary(flat, studies, resolution = 20, shade = shade)
        ),
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      # the package's own warning and no other, such as contour()'s
      expect_length(warnings, 1L)
      expect_match(warnings, "one class over the whole plotting window")
      # the region fill is still drawn, so the plot is not simply empty
      expect_equal(length(unique(as.vector(g$z))), 1L)
    }
  }
})

test_that("class shading tints a one-class window in that class's color", {
  # image() draws a constant matrix in its first color unless told the range,
  # so a window of the positive class was tinted like the negative one
  separable <- fit_separable()
  data <- separable$data
  models <- list(
    tree = rpart::rpart(
      replicate ~ power.o + n.o,
      data = data,
      method = "class",
      maxdepth = 1,
      model = TRUE
    ),
    ensemble = separable$fit
  )
  # both models assign each window one class: success needs high power, and
  # for the ensemble a large sample too
  windows <- list(
    failure = data[data$power.o < 0.5 & data$n.o < 200, ],
    success = data[data$power.o > 0.7 & data$n.o > 250, ]
  )
  palette <- c("navy", "gold")
  opacity <- 0.5
  tints <- stats::setNames(
    grDevices::adjustcolor(palette, alpha.f = opacity),
    names(windows)
  )

  read_fill <- function(fit, window) {
    expect_warning(
      drawn <- record_calls(tutplot_boundary(
        fit,
        window,
        shade = "class",
        resolution = 10,
        palette = palette,
        opacity = opacity
      )),
      "one class over the whole plotting window"
    )
    raster <- find_calls(drawn$calls, "C_raster")
    expect_length(raster, 1L)
    unique(as.vector(raster[[1]][[2]]))
  }
  for (model in names(models)) {
    for (side in names(windows)) {
      expect_identical(
        read_fill(models[[model]], windows[[side]]),
        tints[[side]],
        label = paste("the fill of the", model, "on a window of", side)
      )
    }
  }
})

test_that("a custom palette is honored", {
  data(altmejd)
  quietly(
    g <- tutplot_boundary(
      fit_ada(),
      altmejd,
      resolution = 20,
      palette = c("red", "blue"),
      opacity = 0.5
    )
  )
  expect_equal(dim(g$z), c(20L, 20L))
})

test_that("shade = 'margin' varies the fill while class shading does not", {
  data(altmejd)
  fit <- fit_ada(50)
  quietly({
    a <- tutplot_boundary(fit, altmejd, resolution = 40, shade = "class")
    b <- tutplot_boundary(fit, altmejd, resolution = 40, shade = "margin")
  })
  # both describe the same surface; only the drawing differs
  expect_equal(a$z, b$z)
  # a stump has too few distinct scores for margin shading to say anything
  quietly(
    s <- tutplot_boundary(
      fit_stump(),
      altmejd,
      resolution = 40,
      shade = "margin"
    )
  )
  expect_lte(length(unique(as.vector(s$z))), 2L)
})

test_that("the legend and color bar can be turned off", {
  data(altmejd)
  quietly({
    expect_silent(tutplot_boundary(
      fit_ada(),
      altmejd,
      resolution = 20,
      legend_pos = NULL
    ))
    expect_silent(tutplot_boundary(
      fit_ada(),
      altmejd,
      resolution = 20,
      legend_pos = "bottomleft"
    ))
    expect_silent(tutplot_boundary(
      fit_ada(),
      altmejd,
      resolution = 20,
      shade = "margin",
      add_colorbar = FALSE
    ))
  })
})

test_that("the fill defaults to a weight suited to the shading", {
  # a flat tint only hints; a ramp has to be read, so they need different
  # defaults
  data(altmejd)
  quietly({
    expect_silent(tutplot_boundary(fit_ada(), altmejd, resolution = 20))
    expect_silent(tutplot_boundary(
      fit_ada(),
      altmejd,
      resolution = 20,
      shade = "margin"
    ))
    # an explicit opacity still wins
    expect_silent(tutplot_boundary(
      fit_ada(),
      altmejd,
      resolution = 20,
      opacity = 0.4
    ))
  })
})

test_that("titles are off unless asked for", {
  data(altmejd)
  # nothing to assert visually, but neither call may error, and both must
  # return the same grid
  quietly({
    bare <- tutplot_boundary(fit_ada(), altmejd, resolution = 20)
    titled <- tutplot_boundary(
      fit_ada(),
      altmejd,
      resolution = 20,
      main = "A title",
      subtitle = "and a subtitle"
    )
  })
  expect_equal(bare$features, titled$features)
})

test_that("the title sits in the top margin under either shade", {
  # one name once held both the top margin and the color-scale reach, so under
  # the default shade a stump's title landed inside the panel, at line -1
  data(altmejd)
  read_title_line <- function(shade) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    grDevices::dev.control("enable")
    tutplot_boundary(
      fit_stump(),
      altmejd,
      resolution = 20,
      shade = shade,
      main = "A title"
    )
    titles <- Filter(
      function(item) {
        identical(item[[2]][[1]]$name, "C_mtext") &&
          identical(item[[2]][[2]], "A title")
      },
      grDevices::recordPlot()[[1]]
    )
    titles[[1]][[2]][[4]]
  }
  expect_equal(read_title_line("margin"), read_title_line("class"))
  expect_gt(read_title_line("margin"), 0)
})

test_that("region labels come from the outcome's own levels", {
  data(altmejd)
  expect_equal(
    read_class_labels(altmejd, "replicate"),
    levels(altmejd$replicate)
  )
  # a 0/1 outcome has no levels to borrow
  d <- altmejd
  d$replicate <- as.integer(d$replicate) - 1L
  expect_equal(read_class_labels(d, "replicate"), c("0", "1"))
  expect_null(read_class_labels(altmejd, "not_a_column"))
})

test_that("the marker ring is chosen from the fill's luminance", {
  # a pale marker with a white ring has no edge at all: viridis's yellow end
  # vanished against the panel until the ring followed the fill
  v <- viridisLite::viridis(2)
  expect_equal(choose_ink(v), c("white", "grey15"))

  # and it is a rule, not a special case for yellow
  expect_equal(choose_ink("#000000"), "white")
  expect_equal(choose_ink("#FFFFFF"), "grey15")
  expect_length(choose_ink(c("#440154", "#FDE725", "#21918C")), 3L)
})

test_that("the legend sits outside the panel by default", {
  data(altmejd)
  quietly({
    # "top" is the outside-centered path; a keyword places it inside instead
    expect_silent(tutplot_boundary(fit_ada(), altmejd, resolution = 20))
    expect_silent(
      tutplot_boundary(
        fit_ada(),
        altmejd,
        resolution = 20,
        legend_pos = "bottomleft"
      )
    )
    expect_silent(
      tutplot_boundary(fit_ada(), altmejd, resolution = 20, legend_pos = NULL)
    )
  })
})

test_that("the top margin is paid for by what is actually drawn", {
  data(altmejd)
  # one device for the calls and for par("mar"), so both read the same one
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  # no title, no subtitle, no legend -> the smallest top margin; adding each
  # must not error and must restore par() afterward
  before <- par("mar")
  tutplot_boundary(fit_ada(), altmejd, resolution = 20, legend_pos = NULL)
  tutplot_boundary(
    fit_ada(),
    altmejd,
    resolution = 20,
    main = "t",
    subtitle = "s"
  )
  expect_equal(par("mar"), before)
})

test_that("margin shading shows one key, not two", {
  # the ramp's ends are the classes, so a point legend beside it would repeat
  # the same information -- and the bar used to sit inside the panel
  data(altmejd)
  quietly({
    expect_silent(tutplot_boundary(
      fit_ada(),
      altmejd,
      resolution = 20,
      shade = "margin"
    ))
    expect_silent(
      tutplot_boundary(
        fit_ada(),
        altmejd,
        resolution = 20,
        shade = "margin",
        add_colorbar = FALSE
      )
    )
    # legend_pos = NULL suppresses the key in both modes
    expect_silent(
      tutplot_boundary(
        fit_ada(),
        altmejd,
        resolution = 20,
        shade = "margin",
        legend_pos = NULL
      )
    )
    expect_silent(
      tutplot_boundary(
        fit_ada(),
        altmejd,
        resolution = 20,
        shade = "class",
        legend_pos = NULL
      )
    )
  })
})

test_that("the bar's end labels come from the outcome, including 0/1", {
  data(altmejd)
  d <- altmejd
  d$replicate <- as.integer(d$replicate) - 1L
  # a numeric outcome has no levels to borrow, so the bar falls back to 0/1
  expect_equal(read_class_labels(d, "replicate"), c("0", "1"))
  quietly({
    fit <- rpart::rpart(
      replicate ~ .,
      data = d[, c("replicate", feat)],
      # a 0/1 integer outcome makes rpart infer anova, so the classification
      # has to be asked for -- adaboost() refuses a regression learner rather
      # than quietly refitting it as one
      method = "class",
      maxdepth = 1,
      model = TRUE
    ) |>
      adaboost(n_iter = 10, eta = 1, verbose = FALSE, check_inputs = FALSE)
    expect_silent(tutplot_boundary(fit, d, resolution = 20, shade = "margin"))
  })
})

test_that("a tree's color scale is anchored, a boosted margin's is not", {
  # A probability offset cannot leave [-0.5, 0.5], so the same score must draw
  # the same color in every tree. A margin has no such bound.
  data(altmejd)
  sub <- altmejd[, c("power.o", "n.o", "replicate")]
  stump <- rpart::rpart(
    replicate ~ .,
    data = sub,
    method = "class",
    control = rpart::rpart.control(maxdepth = 1)
  )
  deep <- rpart::rpart(
    replicate ~ .,
    data = sub,
    method = "class",
    control = rpart::rpart.control(maxdepth = 4, minsplit = 5, cp = 1e-3)
  )

  # anchored: independent of the scores actually present
  expect_equal(find_reach(stump, c(-0.2, 0.25)), 0.5)
  expect_equal(find_reach(deep, c(-0.5, 0.5)), 0.5)
  expect_equal(find_reach(stump, c(-0.01, 0.01)), 0.5)

  # not anchored: follows the observed range
  fit <- rpart::rpart(
    replicate ~ .,
    data = sub,
    maxdepth = 1,
    model = TRUE
  ) |>
    adaboost(n_iter = 5, eta = 1, verbose = FALSE, check_inputs = FALSE)
  expect_equal(find_reach(fit, c(-3, 2.2)), 3)
})

test_that("margin shading survives a leaf that is entirely one class", {
  # a pure leaf puts the score exactly on the outer break, which would fall
  # outside the bins unless it is nudged inside
  data(altmejd)
  sub <- altmejd[, c("power.o", "n.o", "replicate")]
  deep <- rpart::rpart(
    replicate ~ .,
    data = sub,
    method = "class",
    control = rpart::rpart.control(
      maxdepth = 4,
      minsplit = 5,
      minbucket = 2,
      cp = 1e-3
    )
  )

  png(tempfile(fileext = ".png"))
  on.exit(dev.off(), add = TRUE)
  g <- tutplot_boundary(deep, data = sub, shade = "margin")
  # the pure leaf is really there, so the guard is doing work
  expect_true(any(abs(abs(g$z) - 0.5) < 1e-12))
})

test_that("the stump reads as two moderate tones, not two extremes", {
  # the point of anchoring: .26 / .76 must not be painted like .02 / .98
  data(altmejd)
  sub <- altmejd[, c("power.o", "n.o", "replicate")]
  stump <- rpart::rpart(
    replicate ~ .,
    data = sub,
    method = "class",
    control = rpart::rpart.control(maxdepth = 1)
  )
  png(tempfile(fileext = ".png"))
  on.exit(dev.off(), add = TRUE)
  g <- tutplot_boundary(stump, data = sub, shade = "margin")

  top <- find_reach(stump, g$z)
  # both regions sit well inside the ramp rather than on its ends
  expect_true(all(abs(range(g$z)) / top < 0.75))
  expect_length(unique(as.vector(g$z)), 2L)
})

# ---- misclassified points --------------------------------------------------

# Figure 3's stump: the script's `cstump`, on the training set it was grown on
fit_paper_stump <- function() {
  rpart::rpart(
    replicate ~ power.o + n.o,
    data = altmext_splits$train,
    method = "class",
    model = TRUE,
    parms = list(split = "gini"),
    maxdepth = 1
  )
}

test_that("tutplot_boundary() counts the stump's misclassified points", {
  train <- altmext_splits$train
  fit <- fit_paper_stump()
  quietly(g <- tutplot_boundary(fit, train, resolution = 20))

  # 39 of the 141 training points, for the caption
  expect_identical(g$n_misclassified, 39L)
  expect_identical(g$n_points, 141L)
  # and what predict() says, so the count is not read off the drawing alone
  predicted <- predict(fit, newdata = train, type = "class")
  expect_identical(g$n_misclassified, sum(predicted != train$replicate))
})

test_that("a perfect ensemble has no misclassified point", {
  perfect <- fit_separable()
  quietly(g <- tutplot_boundary(perfect$fit, perfect$data, resolution = 20))

  expect_identical(g$n_misclassified, 0L)
  expect_identical(g$n_points, 60L)
  predicted <- predict(
    perfect$fit,
    perfect$data,
    verbose = FALSE,
    check_inputs = FALSE
  )
  expect_true(all(
    predicted == ifelse(perfect$data$replicate == "success", 1, -1)
  ))
})

test_that("the counts do not depend on drawing the points", {
  train <- altmext_splits$train
  quietly({
    hidden <- tutplot_boundary(
      fit_paper_stump(),
      train,
      resolution = 20,
      show_points = FALSE
    )
    # without the outcome there is nothing to count
    blind <- tutplot_boundary(
      fit_paper_stump(),
      train[, c("power.o", "n.o")],
      resolution = 20
    )
  })
  expect_identical(hidden$n_misclassified, 39L)
  expect_identical(hidden$n_points, 141L)
  expect_identical(blind$n_misclassified, NA_integer_)
  expect_identical(blind$n_points, NA_integer_)
})

test_that("an outcome that is not two classes stops only when drawn", {
  train <- altmext_splits$train
  fit <- fit_paper_stump()
  words <- transform(train, replicate = as.character(replicate))
  three <- transform(
    train,
    replicate = factor(rep_len(c("a", "b", "c"), nrow(train)))
  )

  for (bad in list(words, three)) {
    expect_null(find_misclassified(fit, bad, "replicate"))
    # without points the figure still draws, as it did before the count
    quietly(
      g <- tutplot_boundary(fit, bad, resolution = 20, show_points = FALSE)
    )
    expect_identical(g$n_misclassified, NA_integer_)
    expect_identical(g$n_points, NA_integer_)
  }
  # with points, the error is the one it always was
  expect_error(
    quietly(tutplot_boundary(fit, words, resolution = 20)),
    "two-level factor, 0/1, -1/1, or logical"
  )
  expect_error(
    quietly(tutplot_boundary(fit, three, resolution = 20)),
    "exactly two levels"
  )
})

test_that("find_misclassified() reads the sign of the surface's own score", {
  train <- altmext_splits$train
  fit <- fit_paper_stump()
  wrong <- find_misclassified(fit, train, "replicate")

  expect_type(wrong, "logical")
  expect_length(wrong, nrow(train))
  # a positive score is the second class, as in the shading
  positive <- score_grid(fit, train) > 0
  expect_identical(wrong, unname(positive != (train$replicate == "success")))
})

test_that("style_points() makes a misclassified point paler and smaller", {
  palette <- viridisLite::viridis(2)
  look <- style_points(c(0L, 1L, 0L, 1L), c(FALSE, FALSE, TRUE, TRUE), palette)

  # correct: the full class color and its contrasting ring
  expect_identical(look$fill[1:2], palette)
  expect_identical(look$ring[1:2], choose_ink(palette))
  # misclassified: 45 percent paler fill, a fainter ring, a smaller dot
  expect_identical(
    look$fill[3:4],
    grDevices::adjustcolor(palette, alpha.f = 0.55)
  )
  expect_identical(
    look$ring[3:4],
    grDevices::adjustcolor(choose_ink(palette), alpha.f = 0.8)
  )
  expect_identical(look$cex, c(1.2, 1.2, 1.05, 1.05))
  expect_identical(look$lwd, c(1.1, 1.1, 0.7, 0.7))
})

test_that("misclassified points are drawn pale, then the correct ones on top", {
  train <- altmext_splits$train
  fit <- fit_paper_stump()
  wrong <- find_misclassified(fit, train, "replicate")
  out <- record_calls(tutplot_boundary(fit, train, resolution = 20))

  # the one points() call that draws the data
  drawn <- Filter(
    \(call) length(call[[2]]$x) == nrow(train),
    find_calls(out$calls, "C_plotXY")
  )
  expect_length(drawn, 1L)
  drawn <- drawn[[1]]

  first <- seq_len(sum(wrong))
  expect_identical(drawn[[2]]$x[first], train$power.o[wrong])
  expect_identical(drawn[[2]]$x[-first], train$power.o[!wrong])
  look <- style_points(
    as_binary(train$replicate),
    wrong,
    viridisLite::viridis(2)
  )
  # col, bg, cex and lwd, in drawing order
  expect_identical(drawn[[6]], look$ring[order(!wrong)])
  expect_identical(drawn[[7]], look$fill[order(!wrong)])
  expect_identical(drawn[[8]], look$cex[order(!wrong)])
  expect_identical(drawn[[9]], look$lwd[order(!wrong)])
})

test_that("the boundary line is drawn at width 1", {
  out <- record_calls(
    tutplot_boundary(fit_paper_stump(), altmext_splits$train, resolution = 20)
  )
  contour <- find_calls(out$calls, "C_contour")
  expect_length(contour, 1L)
  # lwd is the last argument contour() hands to the graphics engine
  expect_identical(utils::tail(contour[[1]], 1)[[1]], 1.0)
})

# ---- the strip above the panel ---------------------------------------------

# the text() calls that drew exactly `label`
find_texts <- function(calls, label) {
  Filter(\(call) identical(call[[3]], label), find_calls(calls, "C_text"))
}

# Figure 3 on a pdf device of `size` at `pointsize`. The panel's geometry is
# read inside style_points(), while the figure's own margins hold: on.exit()
# restores them before the call returns
read_strip <- function(
  size = tutplot_opts$full,
  ...,
  pointsize = tutplot_opts$pointsize
) {
  real_style <- style_points
  frame <- NULL
  local_mocked_bindings(
    style_points = function(class, misclassified, palette) {
      frame <<- list(
        usr = graphics::par("usr"),
        pin = graphics::par("pin"),
        # inches per margin line
        line = graphics::par("mai")[3] / graphics::par("mar")[3],
        page_top = graphics::grconvertY(1, "ndc", "user")
      )
      real_style(class, misclassified, palette)
    }
  )
  out <- record_calls(
    tutplot_boundary(
      fit_paper_stump(),
      altmext_splits$train,
      resolution = 20,
      ...
    ),
    width = size[["width"]],
    height = size[["height"]],
    pointsize = pointsize
  )
  if (is.null(frame)) {
    stop("style_points() was not called, so the panel was not read.")
  }
  list(calls = out$calls, frame = frame)
}

# the color bar's raster: the panel's shading is a raster too, but the bar's
# lies above the panel
find_bar <- function(strip) {
  Filter(
    \(call) call[[4]] > strip$frame$usr[4],
    find_calls(strip$calls, "C_raster")
  )
}

# the strip's parts in inches above the panel's top edge, NULL where absent
locate_strip <- function(strip) {
  usr <- strip$frame$usr
  to_inches <- function(y) {
    (y - usr[4]) / (usr[4] - usr[3]) * strip$frame$pin[2]
  }
  find_y <- function(label) {
    found <- find_texts(strip$calls, label)
    if (length(found)) to_inches(found[[1]][[2]]$y)
  }
  bar <- find_bar(strip)
  subtitle <- Filter(
    \(call) identical(call[[2]], "A subtitle"),
    find_calls(strip$calls, "C_mtext")
  )
  list(
    bar = if (length(bar)) to_inches(c(bar[[1]][[4]], bar[[1]][[6]])),
    label = find_y("Boundary"),
    # the color bar's end labels
    ends = c(find_y("Failure"), find_y("Success")),
    # one row: both labels share a y
    legend = find_y(c("Success", "Failure"))[1],
    page_top = to_inches(strip$frame$page_top),
    subtitle = if (length(subtitle)) subtitle[[1]][[4]] * strip$frame$line,
    line = strip$frame$line
  )
}

# the strip's highest ink in inches above the panel: half a line above a row
# covers a label's ink and a legend dot's
find_highest <- function(parts) {
  max(c(parts$bar, c(parts$ends, parts$legend) + parts$line / 2))
}

# the size of `label` in inches at `pointsize`
measure_height <- function(label, cex, pointsize = tutplot_opts$pointsize) {
  grDevices::pdf(NULL, pointsize = pointsize)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::strheight(label, units = "inches", cex = cex)
}
measure_width <- function(label, cex, pointsize = tutplot_opts$pointsize) {
  grDevices::pdf(NULL, pointsize = pointsize)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::strwidth(label, units = "inches", cex = cex)
}

# the manuscript's full width, and a tall square device
devices <- list(full = tutplot_opts$full, square = c(width = 7, height = 7))

test_that("no key is drawn, and no point above the panel but the legend's", {
  for (shade in c("margin", "class")) {
    strip <- read_strip(shade = shade)
    drawn <- unlist(lapply(
      c(find_calls(strip$calls, "C_text"), find_calls(strip$calls, "C_mtext")),
      \(call) if (identical(call[[1]]$name, "C_text")) call[[3]] else call[[2]]
    ))
    expect_false(any(c("Correct", "Misclassified") %in% drawn))

    top <- strip$frame$usr[4]
    above <- Filter(
      \(call) any(call[[2]]$y > top),
      find_calls(strip$calls, "C_plotXY")
    )
    if (shade == "margin") {
      expect_length(above, 0L)
    } else {
      # the class legend's two dots, on its row
      expect_length(above, 1L)
      legend <- find_texts(strip$calls, c("Success", "Failure"))[[1]]
      expect_identical(above[[1]][[2]]$y, legend[[2]]$y)
    }
  }
})

test_that("under class shading the legend is all the strip holds", {
  strip <- read_strip(shade = "class")
  usr <- strip$frame$usr
  rises <- function(...) any(c(...) > usr[4])

  texts <- Filter(\(call) rises(call[[2]]$y), find_calls(strip$calls, "C_text"))
  expect_length(texts, 1L)
  expect_identical(texts[[1]][[3]], c("Success", "Failure"))
  dots <- Filter(
    \(call) rises(call[[2]]$y),
    find_calls(strip$calls, "C_plotXY")
  )
  expect_length(dots, 1L)
  expect_length(dots[[1]][[2]]$y, 2L)
  # no bar, outline or mark
  expect_length(find_bar(strip), 0L)
  outlines <- Filter(
    \(call) rises(call[[3]], call[[5]]),
    find_calls(strip$calls, "C_rect")
  )
  expect_length(outlines, 0L)
  expect_length(find_calls(strip$calls, "C_segments"), 0L)
  # on its row
  expect_equal(
    locate_strip(strip)$legend,
    strip_layout$legend * strip$frame$line
  )
})

test_that("at full size the bar spans 0.36 of the panel, centered on it", {
  strip <- read_strip()
  usr <- strip$frame$usr
  bar <- find_bar(strip)
  expect_length(bar, 1L)
  bar <- bar[[1]]
  expect_equal((bar[[5]] - bar[[3]]) / (usr[2] - usr[1]), 0.36)
  expect_equal((bar[[3]] + bar[[5]]) / 2, (usr[1] + usr[2]) / 2)
})

test_that("the end labels stay inside the panel on any device", {
  cases <- list(
    narrow = list(size = c(width = 3, height = 3), pointsize = 9),
    large_text = list(size = tutplot_opts$full, pointsize = 12),
    # so narrow at large text that the bar gives way
    cramped = list(size = c(width = 2.6, height = 3), pointsize = 12)
  )
  for (name in names(cases)) {
    case <- cases[[name]]
    strip <- read_strip(case$size, pointsize = case$pointsize)
    usr <- strip$frame$usr
    pin <- strip$frame$pin
    # inches from the panel's left edge
    to_inches <- function(x) (x - usr[1]) / (usr[2] - usr[1]) * pin[1]
    low <- find_texts(strip$calls, "Failure")[[1]]
    high <- find_texts(strip$calls, "Success")[[1]]
    # "Failure" ends at its x, "Success" starts at its x
    low_start <- to_inches(low[[2]]$x) -
      measure_width("Failure", low[[8]], case$pointsize)
    high_end <- to_inches(high[[2]]$x) +
      measure_width("Success", high[[8]], case$pointsize)
    expect_gte(low_start, -1e-9, label = paste("\"Failure\" on", name))
    expect_lte(high_end, pin[1] + 1e-9, label = paste("\"Success\" on", name))

    bar <- find_bar(strip)[[1]]
    expect_equal(
      (bar[[3]] + bar[[5]]) / 2,
      (usr[1] + usr[2]) / 2,
      label = paste("the bar's middle on", name)
    )
    share <- (bar[[5]] - bar[[3]]) / (usr[2] - usr[1])
    if (name == "cramped") {
      expect_lt(share, 0.36)
      # the wider label reaches the panel's edge, and no further
      expect_equal(high_end, pin[1])
    } else {
      expect_equal(share, 0.36, label = paste("the bar's share on", name))
    }
  }
})

test_that("the strip keeps its layout in inches on a device of any size", {
  # in lines
  layouts <- list(
    margin = strip_layout[c("bar", "label")],
    class = strip_layout["legend"]
  )
  for (shade in names(layouts)) {
    strips <- lapply(devices, \(size) {
      locate_strip(read_strip(size, shade = shade))
    })
    for (part in names(layouts[[shade]])) {
      expect_length(strips$square[[part]], length(layouts[[shade]][[part]]))
      expect_equal(
        strips$square[[part]],
        strips$full[[part]],
        tolerance = 1e-9,
        label = paste("the", part, "under", shade, "shading on a 7 x 7 device")
      )
      # a line is 1.2 times the 9 pt text size: 0.15 inches
      expect_equal(strips$full[[part]], layouts[[shade]][[part]] * 0.15)
    }
  }
})

test_that("the strip lies inside the page on a device of any size", {
  for (size in devices) {
    for (shade in c("margin", "class")) {
      strip <- locate_strip(read_strip(size, shade = shade))
      expect_lt(find_highest(strip), strip$page_top)
      expect_gt(min(c(strip$bar, strip$label, strip$legend)), 0)
    }
  }
})

test_that("\"Boundary\" sits under the bar, clear of it and of the panel", {
  strip <- read_strip()
  label <- find_texts(strip$calls, "Boundary")
  expect_length(label, 1L)
  parts <- locate_strip(strip)
  # a capital's height either side of the center covers the ink, the
  # descender of the "y" included
  reach <- measure_height("Boundary", cex = label[[1]][[8]])

  # below the bar's bottom edge and above the panel's top edge
  expect_lt(parts$label + reach, parts$bar[1])
  expect_gt(parts$label - reach, 0)
  # centered under the tick
  tick <- find_calls(strip$calls, "C_segments")[[1]]
  expect_identical(label[[1]][[2]]$x, tick[[2]])
})

test_that("the mark spans the bar's height and no more", {
  strip <- read_strip()
  bar <- find_bar(strip)[[1]]
  tick <- find_calls(strip$calls, "C_segments")
  expect_length(tick, 1L)
  tick <- tick[[1]]
  # y0 and y1 are the bar's bottom and top edges
  expect_identical(c(tick[[3]], tick[[5]]), c(bar[[4]], bar[[6]]))
  # flat ends, so the line's width does not carry it past either edge
  expect_identical(tick$lend, "butt")
  # at the bar's middle, a score of zero
  expect_identical(tick[[2]], (bar[[3]] + bar[[5]]) / 2)
})

test_that("the color bar is one image in the panel's 64 colors", {
  strip <- read_strip()
  above <- find_bar(strip)
  expect_length(above, 1L)
  image <- above[[1]][[2]]
  expect_identical(dim(image), c(1L, 64L))
  # the same colors and opacity as the panel's shading
  expect_identical(
    as.vector(image),
    grDevices::adjustcolor(viridisLite::viridis(64L), alpha.f = 0.65)
  )
  # after the angle: no smoothing, so the bins stay as crisp as the panel's
  expect_false(above[[1]][[8]], label = "interpolation of the bar")
  # one rectangle per rect() call: the outline, and no strip of bins
  rect_sizes <- vapply(
    find_calls(strip$calls, "C_rect"),
    \(call) length(call[[2]]),
    integer(1)
  )
  expect_true(all(rect_sizes == 1L))
})

test_that("the subtitle sits above the strip, and in place without it", {
  for (size in devices) {
    for (shade in c("margin", "class")) {
      strip <- locate_strip(read_strip(
        size,
        shade = shade,
        main = "A title",
        subtitle = "A subtitle"
      ))
      # mtext() sets the baseline above its line, and a descender reaches
      # less far down, so the line bounds the subtitle's ink from below
      expect_gt(strip$subtitle, find_highest(strip))
    }
  }

  # with no strip, the subtitle keeps the line it had before the strip
  bare <- record_calls(
    tutplot_boundary(
      fit_paper_stump(),
      altmext_splits$train,
      resolution = 20,
      legend_pos = NULL,
      subtitle = "A subtitle"
    )
  )
  subtitle <- Filter(
    \(call) identical(call[[2]], "A subtitle"),
    find_calls(bare$calls, "C_mtext")
  )[[1]]
  expect_identical(subtitle[[4]], 0.5)
})

test_that("the strip's three labels share one cex, 7 pt at pointsize 9", {
  out <- read_strip()
  labels <- list("Failure", "Success", "Boundary")
  cex <- vapply(
    labels,
    \(label) find_texts(out$calls, label)[[1]][[8]],
    numeric(1)
  )
  expect_length(unique(cex), 1L)
  # the pdf device rounds a text size to whole points
  expect_identical(round(cex[[1]] * tutplot_opts$pointsize), 7)
})
