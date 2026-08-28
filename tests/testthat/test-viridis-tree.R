stump_fit <- function(...) {
  data(altmejd, envir = environment())
  rpart::rpart(
    replicate ~ power.o + n.o,
    data = altmejd,
    method = "class",
    control = rpart::rpart.control(...)
  )
}

test_that("viridis_tree() returns one color per node", {
  fit <- stump_fit(maxdepth = 2, minsplit = 5, minbucket = 2, cp = 1e-2)
  sty <- viridis_tree(fit)

  expect_named(sty, c("box", "text"))
  expect_length(sty$box, nrow(fit$frame))
  expect_length(sty$text, nrow(fit$frame))
  expect_type(sty$box, "character")
})

test_that("the ends of the scale go to the purest nodes", {
  fit <- stump_fit(maxdepth = 3, minsplit = 5, minbucket = 2, cp = 1e-3)
  yv <- fit$frame$yval2
  prob <- yv[, ncol(yv) - 1]
  sty <- viridis_tree(fit)

  ramp <- viridisLite::viridis(100)
  # a node that is all one class sits at the corresponding end of the ramp
  if (any(prob == 1)) {
    expect_true(all(sty$box[prob == 1] == ramp[100]))
  }
  expect_equal(sty$box[which.min(prob)], ramp[max(1, ceiling(min(prob) * 100))])
})

test_that("the scale is absolute, not quantiles of the fitted values", {
  # box.palette partitions by quantile, so the same probability draws a
  # different colour in different trees. That is the behaviour being replaced,
  # so it is the property worth pinning.
  data(altmejd)
  a <- stump_fit(maxdepth = 2, minsplit = 5, minbucket = 2, cp = 1e-2)
  b <- rpart::rpart(
    replicate ~ p_value.o + effect_size.o,
    data = altmejd,
    method = "class",
    control = rpart::rpart.control(maxdepth = 3, minsplit = 5, cp = 1e-3)
  )

  pa <- a$frame$yval2[, ncol(a$frame$yval2) - 1]
  pb <- b$frame$yval2[, ncol(b$frame$yval2) - 1]
  shared <- intersect(round(pa, 10), round(pb, 10))
  skip_if(length(shared) == 0, "no probability shared between the two trees")

  ca <- viridis_tree(a)$box[match(shared[1], round(pa, 10))]
  cb <- viridis_tree(b)$box[match(shared[1], round(pb, 10))]
  expect_identical(ca, cb)
})

test_that("text color is contrast-aware, never a third value", {
  fit <- stump_fit(maxdepth = 3, minsplit = 5, minbucket = 2, cp = 1e-3)
  sty <- viridis_tree(fit)

  expect_true(all(sty$text %in% c("white", "grey15")))
  # the darkest box must not carry dark text, nor the lightest light text
  lum <- function(x) {
    rgb <- grDevices::col2rgb(x) / 255
    unname(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
  }
  expect_equal(sty$text[which.min(lum(sty$box))], "white")
  expect_equal(sty$text[which.max(lum(sty$box))], "grey15")
})

test_that("a palette other than viridis is honored", {
  fit <- stump_fit(maxdepth = 2, minsplit = 5, minbucket = 2, cp = 1e-2)
  sty <- viridis_tree(fit, palette = viridisLite::mako)
  expect_true(all(sty$box %in% viridisLite::mako(100)))
})

test_that("viridis_tree() rejects what it cannot color", {
  data(altmejd)
  expect_error(viridis_tree(1), "must be an rpart object")
  reg <- rpart::rpart(n.o ~ power.o, data = altmejd, method = "anova")
  expect_error(viridis_tree(reg), "classification tree")
})

test_that("the colors survive a real rpart.plot call", {
  fit <- stump_fit(maxdepth = 2, minsplit = 5, minbucket = 2, cp = 1e-2)
  sty <- viridis_tree(fit)
  png(tempfile(fileext = ".png"))
  on.exit(dev.off(), add = TRUE)
  expect_silent(
    rpart.plot::rpart.plot(fit, box.col = sty$box, col = sty$text)
  )
})
