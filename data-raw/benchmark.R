############################################
# Benchmark models by LOGO-CV and on HOLZ #
############################################

# Run `pkgload::load_all()` first. About a minute on one core; needs the
# randomForest and xgboost packages. The fits follow Listing 35, the tree box
# of Module 8 and the XGBoost box under "Where to go next" in
# vignettes/tutorialscript.qmd, with the same object names.

for (pkg in c("randomForest", "xgboost")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("data-raw/benchmark.R needs the ", pkg, " package.", call. = FALSE)
  }
}

data("altmext")
data("hypergrid")
df <- altmext
pred_nms <- c("power.o", "effect_size.o", "n.o", "p_value.o")
out_nme <- "replicate"
voi_nms <- c(pred_nms, out_nme)

# HOLZ is the final test; the other six are the tuning projects
df_holz <- df[df$pid == "holz", ]
df_noholz <- df[df$pid != "holz", ]
df_split <- split(df_noholz, df_noholz$pid)
proj_nms <- names(df_split)
measures <- list_measures()

# The choice of Module 7 from hypergrid: the highest `lower`, the mean across
# the projects minus their SD
auc_all <- hypergrid$auroc
auc_all$lower <- auc_all$mean - auc_all$sd
ada_best <- auc_all[order(-auc_all$lower), ][1, ]
stopifnot(ada_best$depth == 2, ada_best$n_iter == 2000, ada_best$eta == 0.02)

# Listing 35: refit it on all six tuning projects
h_final <- rpart::rpart(
  replicate ~ .,
  data = df_noholz[, voi_nms],
  method = "class",
  maxdepth = ada_best$depth,
  parms = list(split = "gini"),
  model = TRUE
)
ada_final <- h_final |>
  adaboost(n_iter = ada_best$n_iter, eta = ada_best$eta, verbose = FALSE)

# The tree's depth by the rule of Module 7: LOGO-CV on the six tuning
# projects, the highest `lower`
tree_hgrid <- data.frame(depth = 1:4)
tree_tune <- rpart::rpart(
  replicate ~ .,
  data = df_noholz[, voi_nms],
  method = "class"
) |>
  tune_logo_cv(tree_hgrid, data = df_noholz, group = "pid")
# summary() keeps tied rows in grid order, so a tie goes to the smallest depth
tree_best <- summary(tree_tune, "auroc", sort_by = "lower")[1, ]

# The three baselines, fitted on the six tuning projects
logit_fit <- glm(replicate ~ ., data = df_noholz[, voi_nms], family = binomial)
tree_fit <- rpart::rpart(
  replicate ~ .,
  data = df_noholz[, voi_nms],
  method = "class",
  maxdepth = tree_best$depth
)
set.seed(112)
rf_fit <- randomForest::randomForest(replicate ~ ., data = df_noholz[, voi_nms])

# Each model scored as AdaBoost was: LOGO-CV on the six tuning projects, then
# once on HOLZ
fits <- list(
  logit = logit_fit,
  tree = tree_fit,
  rf = rf_fit,
  adaboost = ada_final
)
logo_tune <- list()
logo_holz <- list()
for (nm in names(fits)) {
  # The forest draws at random: the same seed before each call that fits it
  set.seed(112)
  logo_tune[[nm]] <- logo_cv(fits[[nm]], data = df_noholz, group = "pid")
  set.seed(112)
  logo_holz[[nm]] <- logo_cv(
    fits[[nm]],
    data = df,
    group = "pid",
    holdout = "holz"
  )
}

# XGBoost by the same workflow: four hyperparameters crossed with five
# numbers of rounds
xgb_hgrid <- expand.grid(
  depth = c(1, 2, 4),
  eta = c(0.1, 0.05, 0.01),
  min_child_weight = c(1, 5),
  lambda = c(1, 10),
  n_iter = c(50, 100, 200, 500, 1000)
)
xgb_fit <- xgboost::xgboost(
  x = df_noholz[, pred_nms],
  y = df_noholz[, out_nme],
  nthreads = 1
)
started <- Sys.time()
xgb_tune <- tune_logo_cv(
  xgb_fit,
  xgb_hgrid,
  data = df_noholz,
  group = "pid",
  outcome = out_nme
)
message(sprintf(
  "XGBoost grid: %.1f min",
  as.numeric(difftime(Sys.time(), started, units = "mins"))
))
xgb_best <- summary(xgb_tune, "auroc", sort_by = "lower")[1, ]

# Refit the pick on all six tuning projects and score HOLZ once
xgb_holz <- tune_logo_cv(
  xgb_fit,
  xgb_best[names(xgb_hgrid)],
  data = df,
  group = "pid",
  holdout = "holz",
  outcome = out_nme
)

# Every measure for the XGBoost pick: its row of the grid, and HOLZ. The row
# names of summary() are the settings' row numbers in the grid.
xgb_est <- cbind(
  t(xgb_tune$estimates[rownames(xgb_best), proj_nms, measures]),
  holz = xgb_holz$estimates[1, "holz", measures]
)

# One measure x project matrix per model, HOLZ last
est <- lapply(names(fits), \(nm) {
  cbind(
    t(logo_tune[[nm]]$estimates[1, proj_nms, measures]),
    holz = logo_holz[[nm]]$estimates[1, "holz", measures]
  )
})
names(est) <- names(fits)
est$xgboost <- xgb_est

benchmark <- lapply(setNames(measures, measures), \(measure) {
  data.frame(
    model = names(est),
    t(vapply(est, \(e) e[measure, ], numeric(length(proj_nms) + 1))),
    row.names = NULL
  )
})

proj_cols <- c(proj_nms, "holz")
attr(benchmark, "projects") <- data.frame(
  project = proj_cols,
  n = as.vector(table(df$pid)[proj_cols]),
  base_rate = as.vector(
    tapply(df[, out_nme] == "success", df$pid, mean)[proj_cols]
  ),
  row.names = NULL
)
# One row per model and hyperparameter; the logit has none. Names follow the
# tutorial where it has one, the fitting package otherwise.
settings <- list(
  tree = list(depth = tree_best$depth),
  rf = list(ntree = rf_fit$ntree, mtry = rf_fit$mtry),
  adaboost = list(
    depth = ada_best$depth,
    n_iter = ada_best$n_iter,
    eta = ada_best$eta
  ),
  xgboost = list(
    depth = xgb_best$depth,
    n_iter = xgb_best$n_iter,
    eta = xgb_best$eta,
    min_child_weight = xgb_best$min_child_weight,
    lambda = xgb_best$lambda
  )
)
# Text, one value at a time: as numbers, 0.02 prints as 2e-02
attr(benchmark, "settings") <- data.frame(
  model = rep(names(settings), lengths(settings)),
  hyperparameter = unlist(lapply(settings, names), use.names = FALSE),
  value = vapply(
    unlist(settings, use.names = FALSE),
    format,
    character(1),
    scientific = FALSE
  ),
  row.names = NULL
)

# Checks: AdaBoost's six projects are its hypergrid row; the HOLZ cells of
# the tree, the forest, AdaBoost and XGBoost equal direct fits scored on HOLZ
stopifnot(
  length(benchmark) == 17,
  all(vapply(benchmark, nrow, integer(1)) == 5),
  !anyNA(benchmark$auroc),
  isTRUE(all.equal(
    unname(unlist(benchmark$auroc[
      benchmark$auroc$model == "adaboost",
      proj_nms
    ])),
    unname(unlist(ada_best[proj_nms]))
  ))
)
ada_y <- predict(
  ada_final,
  newdata = df_holz[, pred_nms],
  type = "margin",
  verbose = FALSE
)
tree_y <- predict(tree_fit, df_holz[, pred_nms], type = "prob")[, "success"]
rf_y <- predict(rf_fit, df_holz[, pred_nms], type = "prob")[, "success"]
xgb_y <- xgboost::xgboost(
  x = df_noholz[, pred_nms],
  y = df_noholz[, out_nme],
  max_depth = xgb_best$depth,
  learning_rate = xgb_best$eta,
  min_child_weight = xgb_best$min_child_weight,
  reg_lambda = xgb_best$lambda,
  nrounds = xgb_best$n_iter,
  nthreads = 1
) |>
  predict(df_holz[, pred_nms])
direct <- c(
  tree = auroc(df_holz[, out_nme], tree_y),
  rf = auroc(df_holz[, out_nme], rf_y),
  adaboost = auroc(df_holz[, out_nme], ada_y),
  xgboost = auroc(df_holz[, out_nme], xgb_y)
)
stored <- setNames(benchmark$auroc$holz, benchmark$auroc$model)[names(direct)]
stopifnot(isTRUE(all.equal(direct, stored)))

for (measure in c("auroc", "auprc", "bacc")) {
  cat("\n", measure, "\n")
  print(benchmark[[measure]], digits = 3)
}
print(attr(benchmark, "settings"))
print(attr(benchmark, "projects"))

usethis::use_data(benchmark, overwrite = TRUE)
