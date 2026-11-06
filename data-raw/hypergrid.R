##########################################
# AdaBoost hyperparameter grid (LOGO-CV) #
##########################################

# Run `pkgload::load_all()` first. One tune_logo_cv() call, about 40 minutes on 4
# cores. `cores = 4` forks on macOS and Linux; on Windows it runs on one core,
# about four times as long. data-raw/cache/ holds slices of an earlier build
# that this script no longer reads.

data("altmext")

# HOLZ is the final test
tune <- altmext[altmext$pid != "holz", ]
predictors <- c("power.o", "effect_size.o", "n.o", "p_value.o")
projects <- sort(unique(tune$pid))
measures <- list_measures()

depths <- 1:8
etas <- c(
  0.0025,
  0.005,
  0.0075,
  0.01,
  0.02,
  0.03,
  0.05,
  0.075,
  0.1,
  0.15,
  0.2,
  0.3,
  0.5,
  0.75,
  1
)
rounds <- c(
  10,
  15,
  20,
  25,
  30,
  40,
  50,
  75,
  100,
  150,
  200,
  300,
  400,
  500,
  600,
  800,
  1000,
  1250,
  1500,
  2000,
  2500,
  3000
)

# The recipe: a stump on the four predictors with rpart's defaults. The grid
# replaces its depth, rounds and rate and keeps the rest (cp = 0.01).
recipe <- rpart::rpart(
  replicate ~ power.o + effect_size.o + n.o + p_value.o,
  data = tune,
  maxdepth = 1,
  model = TRUE
) |>
  adaboost(n_iter = 10, eta = 1, verbose = FALSE)

# n_iter varies fastest, then eta, then depth: the row order of the tables.
# Rows that differ only in n_iter share one fit per fold.
grid <- expand.grid(n_iter = rounds, eta = etas, depth = depths)
started <- Sys.time()
res <- tune_logo_cv(recipe, grid, data = tune, group = "pid", cores = 4)
message(sprintf(
  "tune_logo_cv(): %.1f min",
  as.numeric(difftime(Sys.time(), started, units = "mins"))
))

# the result orders projects by first appearance in the data, so index by name
hypergrid <- lapply(setNames(measures, measures), function(measure) {
  data.frame(
    grid[c("depth", "n_iter", "eta")],
    res$estimates[, projects, measure],
    row.names = NULL
  )
})
info <- res$projects[match(projects, res$projects$project), ]
# a fresh data.frame(): its attribute order is the shipped file's, so an
# unchanged grid saves to the same bytes
attr(hypergrid, "projects") <- data.frame(
  project = info$project,
  n = info$n,
  base_rate = info$base_rate,
  row.names = NULL
)

# Listing 33's settings, fitted directly, must match the tables
listing <- expand.grid(depth = 1:3, n_iter = c(10, 100, 1000), eta = c(1, 0.1))
for (i in seq_len(nrow(listing))) {
  setting <- listing[i, ]
  row <- which(
    hypergrid$auroc$depth == setting$depth &
      hypergrid$auroc$n_iter == setting$n_iter &
      hypergrid$auroc$eta == setting$eta
  )
  stopifnot(length(row) == 1)
  for (project in projects) {
    train <- tune[tune$pid != project, c(predictors, "replicate")]
    test <- tune[tune$pid == project, ]
    h <- rpart::rpart(
      replicate ~ .,
      data = train,
      maxdepth = setting$depth,
      model = TRUE
    )
    fit <- adaboost(
      h,
      n_iter = setting$n_iter,
      eta = setting$eta,
      verbose = FALSE
    )
    margin <- predict(fit, test[, predictors], type = "margin", verbose = FALSE)
    direct <- assess(test$replicate, score = margin)[measures]
    staged <- vapply(measures, \(m) hypergrid[[m]][row, project], numeric(1))
    stopifnot(isTRUE(all.equal(unname(direct), unname(staged))))
  }
}

stopifnot(
  length(hypergrid) == 17,
  all(
    vapply(hypergrid, nrow, integer(1)) ==
      length(depths) * length(etas) * length(rounds)
  ),
  !anyNA(hypergrid$auroc)
)

# The tables must equal those of the script this one replaced, which boosted
# and scored by hand. Drop this check when the grid itself changes.
shipped <- new.env()
load(file.path("data", "hypergrid.rda"), envir = shipped)
differ <- measures[
  !vapply(
    measures,
    \(m) isTRUE(all.equal(hypergrid[[m]], shipped$hypergrid[[m]])),
    logical(1)
  )
]
if (
  !isTRUE(all.equal(
    attr(hypergrid, "projects"),
    attr(shipped$hypergrid, "projects")
  ))
) {
  differ <- c(differ, "the projects attribute")
}
if (length(differ)) {
  stop(
    "The rebuilt grid differs from data/hypergrid.rda in: ",
    paste(differ, collapse = ", "),
    call. = FALSE
  )
}

usethis::use_data(hypergrid, overwrite = TRUE)
