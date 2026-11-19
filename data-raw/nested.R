#######################################################
# Nested LOGO-CV of the AdaBoost hyperparameter grid #
#######################################################

# Run `pkgload::load_all()` first. One tune_logo_cv() call, about 140 minutes
# on 4 cores: one search of the 1,320 settings of hypergrid per outer fold,
# seven in all. `cores = 4` forks on macOS and Linux; on Windows it runs on one
# core, about four times as long.

data("altmext")
data("hypergrid")
data("benchmark")

# The vectors of data-raw/hypergrid.R
depths <- 1:4
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

# All seven projects are outer folds, HOLZ among them. That is fine only after
# the final test, and only because HOLZ's fold repeats it: its inner search is
# hypergrid's run on the six tuning projects, so it must pick hypergrid's
# choice, the setting the benchmark scored on HOLZ. Check that choice first.
ada_best <- summary(hypergrid)[1, ]
stopifnot(
  rownames(ada_best) == "438",
  ada_best$depth == 2,
  ada_best$n_iter == 2000,
  ada_best$eta == 0.02
)

# The recipe of data-raw/hypergrid.R: a stump on the four predictors with
# rpart's defaults. Its own data are not used, so it is fitted on the six
# tuning projects as there.
ada_recipe <- rpart::rpart(
  replicate ~ power.o + effect_size.o + n.o + p_value.o,
  data = altmext[altmext$pid != "holz", ],
  maxdepth = 1,
  model = TRUE
) |>
  adaboost(n_iter = 10, eta = 1, verbose = FALSE)

# hypergrid's grid in its row order: n_iter fastest, then eta, then depth
ada_hgrid <- expand.grid(n_iter = rounds, eta = etas, depth = depths)
stopifnot(
  nrow(ada_hgrid) == nrow(hypergrid$auroc),
  all(vapply(
    c("depth", "n_iter", "eta"),
    \(column) identical(ada_hgrid[[column]], hypergrid$auroc[[column]]),
    logical(1)
  ))
)

started <- Sys.time()
nested <- tune_logo_cv(
  ada_recipe,
  ada_hgrid,
  data = altmext,
  group = "pid",
  nested = TRUE,
  metric = "auroc",
  sort_by = "lower",
  cores = 4
)
message(sprintf(
  "tune_logo_cv(): %.1f min",
  as.numeric(difftime(Sys.time(), started, units = "mins"))
))

# HOLZ's fold must repeat the final test: hypergrid's pick, with the `lower`
# of its row in hypergrid, and the AUROC of benchmark's AdaBoost cell on HOLZ
ada_holz <- nested$selected[nested$selected$project == "holz", ]
stopifnot(
  nrow(ada_holz) == 1,
  ada_holz$setting == 438,
  ada_holz$depth == 2,
  ada_holz$n_iter == 2000,
  ada_holz$eta == 0.02,
  isTRUE(all.equal(
    nested$estimates["holz", "auroc"],
    benchmark$auroc$holz[benchmark$auroc$model == "adaboost"]
  )),
  isTRUE(all.equal(ada_holz$inner_value, ada_best$lower))
)

stopifnot(
  setequal(rownames(nested$estimates), unique(altmext$pid)),
  nrow(nested$grid) == nrow(ada_hgrid),
  !anyNA(nested$estimates[, "auroc"])
)

print(nested)
print(summary(nested))
print(nested$selected)

usethis::use_data(nested, overwrite = TRUE)
