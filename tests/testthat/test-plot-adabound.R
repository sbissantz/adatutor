feat <- c("power.o", "n.o")

fit_ada <- function(T = 20) {
  data(altmejd)
  adaboost(
    replicate ~ .,
    data = altmejd[, c("replicate", feat)],
    T = T,
    eta = 1,
    verbose = FALSE,
    input_checks = FALSE
  )
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

test_that("plot_adabound() returns the grid it drew, invisibly", {
  data(altmejd)
  quietly({
    out <- withVisible(plot_adabound(fit_ada(), altmejd, resolution = 30))
  })
  expect_false(out$visible)
  g <- out$value
  expect_named(g, c("x1", "x2", "z", "features"))
  expect_length(g$x1, 30L)
  expect_length(g$x2, 30L)
  expect_equal(dim(g$z), c(30L, 30L))
  expect_equal(g$features, feat)
})

test_that("the score reproduces the class labels predict() would give", {
  # boundary_score() must not change which side of the boundary a point is on
  data(altmejd)
  fit <- fit_ada(50)
  g <- expand.grid(
    power.o = seq(0.2, 1, length.out = 40),
    n.o = seq(10, 400, length.out = 40)
  )
  labs <- predict(fit, g, verbose = FALSE, input_checks = FALSE)
  expect_equal(sign(boundary_score(fit, g)), labs)
})

test_that("it works for a single rpart tree as well as an ensemble", {
  data(altmejd)
  quietly({
    a <- plot_adabound(fit_ada(), altmejd, resolution = 25)
    s <- plot_adabound(fit_stump(), altmejd, resolution = 25)
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
  quietly(g <- plot_adabound(fit_ada(), altmejd, resolution = 20))
  expect_equal(g$features, feat)

  # four predictors -> refuses to guess, and says what is available
  wide <- adaboost(
    replicate ~ .,
    data = altmejd[, c(
      "replicate",
      "power.o",
      "effect_size.o",
      "n.o",
      "p_value.o"
    )],
    T = 5,
    eta = 1,
    verbose = FALSE,
    input_checks = FALSE
  )
  expect_error(quietly(plot_adabound(wide, altmejd)), "must name two columns")
  expect_error(quietly(plot_adabound(wide, altmejd)), "effect_size.o")

  # naming them explicitly works, and the others are held fixed
  quietly(g2 <- plot_adabound(wide, altmejd, features = feat, resolution = 20))
  expect_equal(g2$features, feat)
})

test_that("plot_adabound() rejects malformed input", {
  data(altmejd)
  expect_error(
    quietly(plot_adabound(fit_ada(), altmejd, features = "power.o")),
    "exactly two"
  )
  expect_error(
    quietly(plot_adabound(fit_ada(), altmejd, features = c("power.o", "nope"))),
    "not found"
  )
  expect_error(
    quietly(plot_adabound(fit_ada(), altmejd, resolution = 1)),
    "at least 2"
  )
  expect_error(
    plot_adabound("not a model", altmejd),
    "rpart object or the output"
  )
})

test_that("a one-class window warns instead of drawing nothing", {
  # A root-only tree assigns the same class everywhere, so there is no boundary.
  # The hand-written code drew a boundary-less plot in silence: its contour ran
  # on a single-level factor and simply produced no line.
  data(altmejd)
  flat <- rpart::rpart(
    replicate ~ .,
    data = altmejd[, c("replicate", feat)],
    method = "class",
    control = rpart::rpart.control(cp = 1)
  )
  expect_equal(nrow(flat$frame), 1L)
  expect_warning(
    quietly(g <- plot_adabound(flat, altmejd, resolution = 20)),
    "one class over the whole plotting window"
  )
  # the region fill is still drawn, so the plot is not simply empty
  expect_equal(length(unique(as.vector(g$z))), 1L)
})

test_that("a custom palette is honoured", {
  data(altmejd)
  quietly(
    g <- plot_adabound(
      fit_ada(),
      altmejd,
      resolution = 20,
      palette = c("red", "blue"),
      alpha = 0.5
    )
  )
  expect_equal(dim(g$z), c(20L, 20L))
})

test_that("shade = 'margin' varies the fill while class shading does not", {
  data(altmejd)
  fit <- fit_ada(50)
  quietly({
    a <- plot_adabound(fit, altmejd, resolution = 40, shade = "class")
    b <- plot_adabound(fit, altmejd, resolution = 40, shade = "margin")
  })
  # both describe the same surface; only the drawing differs
  expect_equal(a$z, b$z)
  # a stump has too few distinct scores for margin shading to say anything
  quietly(
    s <- plot_adabound(fit_stump(), altmejd, resolution = 40, shade = "margin")
  )
  expect_lte(length(unique(as.vector(s$z))), 2L)
})

test_that("the legend and colour bar can be turned off", {
  data(altmejd)
  quietly({
    expect_silent(plot_adabound(
      fit_ada(),
      altmejd,
      resolution = 20,
      legend_pos = NULL
    ))
    expect_silent(plot_adabound(
      fit_ada(),
      altmejd,
      resolution = 20,
      legend_pos = "bottomleft"
    ))
    expect_silent(plot_adabound(
      fit_ada(),
      altmejd,
      resolution = 20,
      shade = "margin",
      colorbar = FALSE
    ))
  })
})

test_that("the fill defaults to a weight suited to the shading", {
  # a flat tint only hints; a ramp has to be read, so they need different
  # defaults
  data(altmejd)
  quietly({
    expect_silent(plot_adabound(fit_ada(), altmejd, resolution = 20))
    expect_silent(plot_adabound(
      fit_ada(),
      altmejd,
      resolution = 20,
      shade = "margin"
    ))
    # an explicit alpha still wins
    expect_silent(plot_adabound(
      fit_ada(),
      altmejd,
      resolution = 20,
      alpha = 0.4
    ))
  })
})

test_that("titles are off unless asked for", {
  data(altmejd)
  # nothing to assert visually, but neither call may error, and both must
  # return the same grid
  quietly({
    bare <- plot_adabound(fit_ada(), altmejd, resolution = 20)
    titled <- plot_adabound(
      fit_ada(),
      altmejd,
      resolution = 20,
      main = "A title",
      subtitle = "and a subtitle"
    )
  })
  expect_equal(bare$features, titled$features)
})

test_that("region labels come from the outcome's own levels", {
  data(altmejd)
  expect_equal(boundary_labels(altmejd, "replicate"), levels(altmejd$replicate))
  # a 0/1 outcome has no levels to borrow
  d <- altmejd
  d$replicate <- as.integer(d$replicate) - 1L
  expect_equal(boundary_labels(d, "replicate"), c("0", "1"))
  expect_null(boundary_labels(altmejd, "not_a_column"))
})

test_that("the marker ring is chosen from the fill's luminance", {
  # a pale marker with a white ring has no edge at all: viridis's yellow end
  # vanished against the panel until the ring followed the fill
  v <- viridisLite::viridis(2)
  expect_equal(contrast_stroke(v), c("white", "grey15"))

  # and it is a rule, not a special case for yellow
  expect_equal(contrast_stroke("#000000"), "white")
  expect_equal(contrast_stroke("#FFFFFF"), "grey15")
  expect_length(contrast_stroke(c("#440154", "#FDE725", "#21918C")), 3L)
})

test_that("the legend sits outside the panel by default", {
  data(altmejd)
  quietly({
    # "top" is the outside-centred path; a keyword places it inside instead
    expect_silent(plot_adabound(fit_ada(), altmejd, resolution = 20))
    expect_silent(
      plot_adabound(
        fit_ada(),
        altmejd,
        resolution = 20,
        legend_pos = "bottomleft"
      )
    )
    expect_silent(
      plot_adabound(fit_ada(), altmejd, resolution = 20, legend_pos = NULL)
    )
  })
})

test_that("the top margin is paid for by what is actually drawn", {
  data(altmejd)
  # no title, no subtitle, no legend -> the smallest top margin; adding each
  # must not error and must restore par() afterwards
  before <- par("mar")
  quietly({
    plot_adabound(fit_ada(), altmejd, resolution = 20, legend_pos = NULL)
    plot_adabound(
      fit_ada(),
      altmejd,
      resolution = 20,
      main = "t",
      subtitle = "s"
    )
  })
  expect_equal(par("mar"), before)
})

test_that("margin shading shows one key, not two", {
  # the ramp's ends are the classes, so a point legend beside it would repeat
  # the same information -- and the bar used to sit inside the panel
  data(altmejd)
  quietly({
    expect_silent(plot_adabound(
      fit_ada(),
      altmejd,
      resolution = 20,
      shade = "margin"
    ))
    expect_silent(
      plot_adabound(
        fit_ada(),
        altmejd,
        resolution = 20,
        shade = "margin",
        colorbar = FALSE
      )
    )
    # legend_pos = NULL suppresses the key in both modes
    expect_silent(
      plot_adabound(
        fit_ada(),
        altmejd,
        resolution = 20,
        shade = "margin",
        legend_pos = NULL
      )
    )
    expect_silent(
      plot_adabound(
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
  expect_equal(boundary_labels(d, "replicate"), c("0", "1"))
  quietly({
    fit <- adaboost(
      replicate ~ .,
      data = d[, c("replicate", feat)],
      T = 10,
      eta = 1,
      verbose = FALSE,
      input_checks = FALSE
    )
    expect_silent(plot_adabound(fit, d, resolution = 20, shade = "margin"))
  })
})

test_that("a tree's colour scale is anchored, a boosted margin's is not", {
  # A probability offset cannot leave [-0.5, 0.5], so the same score must draw
  # the same colour in every tree. A margin has no such bound.
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
  expect_equal(boundary_top(stump, c(-0.2, 0.25)), 0.5)
  expect_equal(boundary_top(deep, c(-0.5, 0.5)), 0.5)
  expect_equal(boundary_top(stump, c(-0.01, 0.01)), 0.5)

  # not anchored: follows the observed range
  fit <- adaboost(
    replicate ~ .,
    data = sub,
    T = 5,
    eta = 1,
    verbose = FALSE,
    input_checks = FALSE
  )
  expect_equal(boundary_top(fit, c(-3, 2.2)), 3)
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
  g <- plot_adabound(deep, data = sub, shade = "margin")
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
  g <- plot_adabound(stump, data = sub, shade = "margin")

  top <- boundary_top(stump, g$z)
  # both regions sit well inside the ramp rather than on its ends
  expect_true(all(abs(range(g$z)) / top < 0.75))
  expect_length(unique(as.vector(g$z)), 2L)
})
