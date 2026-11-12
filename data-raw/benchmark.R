############################################
# Benchmark models by LOGO-CV and on HOLZ #
############################################

# Run `pkgload::load_all()` first. About a minute on one core; needs the
# randomForest and xgboost packages. The fits are the code of Listing 35 and
# the boxes of Module 8 in vignettes/tutorialscript.qmd, with the same object
# names.

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

# The choice of Module 7 from hypergrid: the best worst project, ties by
# stability
auc_all <- hypergrid$auroc
auc_all$stable <- auc_all$mean - auc_all$sd
best <- auc_all[order(-auc_all$min, -auc_all$stable), ][1, ]
stopifnot(best$depth == 2, best$n_iter == 2000, best$eta == 0.02)

# Listing 35: refit it on all six tuning projects
h_final <- rpart::rpart(
  replicate ~ .,
  data = df_noholz[, voi_nms],
  method = "class",
  maxdepth = best$depth,
  parms = list(split = "gini"),
  model = TRUE
)
ada_final <- h_final |>
  adaboost(n_iter = best$n_iter, eta = best$eta, verbose = FALSE)

# Box 1. The tree's depth by the rule of Module 7: LOGO-CV on the six tuning
# projects, the best worst project, ties by stability
hgrid_tree <- data.frame(depth = 1:8)
logo_tree <- rpart::rpart(
  replicate ~ .,
  data = df_noholz[, voi_nms],
  method = "class"
) |>
  tune_logo_cv(hgrid_tree, data = df_noholz, group = "pid")
# summary() keeps tied rows in grid order, so a tie goes to the smallest depth
best_tree <- summary(logo_tree, "auroc", sort_by = c("min", "stable"))[1, ]

# The three baselines, fitted on the six tuning projects
fit_logit <- glm(replicate ~ ., data = df_noholz[, voi_nms], family = binomial)
fit_tree <- rpart::rpart(
  replicate ~ .,
  data = df_noholz[, voi_nms],
  method = "class",
  maxdepth = best_tree$depth
)
set.seed(112)
fit_rf <- randomForest::randomForest(replicate ~ ., data = df_noholz[, voi_nms])

# Each model scored as AdaBoost was: LOGO-CV on the six tuning projects, then
# once on HOLZ
fits <- list(
  logit = fit_logit,
  tree = fit_tree,
  rf = fit_rf,
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

# Box 2. XGBoost by the same workflow: four hyperparameters crossed with five
# numbers of rounds
hgrid_xgb <- expand.grid(
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
logo_xgb <- tune_logo_cv(
  xgb_fit,
  hgrid_xgb,
  data = df_noholz,
  group = "pid",
  outcome = out_nme
)
message(sprintf(
  "XGBoost grid: %.1f min",
  as.numeric(difftime(Sys.time(), started, units = "mins"))
))
best_xgb <- summary(logo_xgb, "auroc", sort_by = c("min", "stable"))[1, ]

# Refit the pick on all six tuning projects and score HOLZ once
holz_xgb <- tune_logo_cv(
  xgb_fit,
  best_xgb[names(hgrid_xgb)],
  data = df,
  group = "pid",
  holdout = "holz",
  outcome = out_nme
)

# Every measure for the XGBoost pick: its row of the grid, and HOLZ. The row
# names of summary() are the settings' row numbers in the grid.
est_xgb <- cbind(
  t(logo_xgb$estimates[rownames(best_xgb), proj_nms, measures]),
  holz = holz_xgb$estimates[1, "holz", measures]
)

# One measure x project matrix per model, HOLZ last
est <- lapply(names(fits), \(nm) {
  cbind(
    t(logo_tune[[nm]]$estimates[1, proj_nms, measures]),
    holz = logo_holz[[nm]]$estimates[1, "holz", measures]
  )
})
names(est) <- names(fits)
est$xgboost <- est_xgb

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
  tree = list(depth = best_tree$depth),
  rf = list(ntree = fit_rf$ntree, mtry = fit_rf$mtry),
  adaboost = list(depth = best$depth, n_iter = best$n_iter, eta = best$eta),
  xgboost = list(
    depth = best_xgb$depth,
    n_iter = best_xgb$n_iter,
    eta = best_xgb$eta,
    min_child_weight = best_xgb$min_child_weight,
    lambda = best_xgb$lambda
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
    unname(unlist(best[proj_nms]))
  ))
)
y_ada <- predict(
  ada_final,
  newdata = df_holz[, pred_nms],
  type = "margin",
  verbose = FALSE
)
y_tree <- predict(fit_tree, df_holz[, pred_nms], type = "prob")[, "success"]
y_rf <- predict(fit_rf, df_holz[, pred_nms], type = "prob")[, "success"]
y_xgb <- xgboost::xgboost(
  x = df_noholz[, pred_nms],
  y = df_noholz[, out_nme],
  max_depth = best_xgb$depth,
  learning_rate = best_xgb$eta,
  min_child_weight = best_xgb$min_child_weight,
  reg_lambda = best_xgb$lambda,
  nrounds = best_xgb$n_iter,
  nthreads = 1
) |>
  predict(df_holz[, pred_nms])
direct <- c(
  tree = auroc(df_holz[, out_nme], y_tree),
  rf = auroc(df_holz[, out_nme], y_rf),
  adaboost = auroc(df_holz[, out_nme], y_ada),
  xgboost = auroc(df_holz[, out_nme], y_xgb)
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
