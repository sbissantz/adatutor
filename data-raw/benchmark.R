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
proj_hg <- as.matrix(auc_all[proj_nms])
auc_all$auc_stable <- rowMeans(proj_hg) - apply(proj_hg, 1, sd)
auc_all$auc_min <- apply(proj_hg, 1, min)
best <- auc_all[order(-auc_all$auc_min, -auc_all$auc_stable), ][1, ]
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
fit_final <- h_final |>
  adaboost(n_iter = best$n_iter, eta = best$eta, verbose = FALSE)

# Box 1. The tree's depth by the rule of Module 7: LOGO-CV on the six tuning
# projects, the best worst project, ties by stability
tree_grid <- data.frame(depth = 1:8, auc_stable = NA_real_, auc_min = NA_real_)
for (i in seq_len(nrow(tree_grid))) {
  logo_i <- rpart::rpart(
    replicate ~ .,
    data = df_noholz[, voi_nms],
    method = "class",
    maxdepth = tree_grid$depth[i]
  ) |>
    logo_cv(data = df_noholz, group = "pid")
  auc_i <- logo_i$estimates[1, proj_nms, "auroc"]
  tree_grid$auc_stable[i] <- mean(auc_i) - sd(auc_i)
  tree_grid$auc_min[i] <- min(auc_i)
}

# order() keeps tied rows in place, so a tie goes to the smallest depth
tree_order <- order(-tree_grid$auc_min, -tree_grid$auc_stable)
depth_tree <- tree_grid$depth[tree_order[1]]

# The three baselines, fitted on the six tuning projects
fit_logit <- glm(replicate ~ ., data = df_noholz[, voi_nms], family = binomial)
fit_tree <- rpart::rpart(
  replicate ~ .,
  data = df_noholz[, voi_nms],
  method = "class",
  maxdepth = depth_tree
)
set.seed(112)
fit_rf <- randomForest::randomForest(replicate ~ ., data = df_noholz[, voi_nms])

# Each model scored as AdaBoost was: LOGO-CV on the six tuning projects, then
# once on HOLZ
fits <- list(
  logit = fit_logit,
  tree = fit_tree,
  rf = fit_rf,
  adaboost = fit_final
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

# Box 2. XGBoost needs a numeric matrix and a 0/1 outcome
as_dmatrix <- function(d) {
  xgboost::xgb.DMatrix(
    as.matrix(d[, pred_nms]),
    label = as.integer(d[, out_nme] == "success")
  )
}

# One XGBoost fit: the logistic loss, four hyperparameters, n_iter rounds
fit_xgb <- function(d, setting, n_iter) {
  xgboost::xgb.train(
    params = list(
      objective = "binary:logistic",
      max_depth = setting$depth,
      eta = setting$eta,
      min_child_weight = setting$min_child_weight,
      lambda = setting$lambda,
      nthread = 1
    ),
    data = as_dmatrix(d),
    nrounds = n_iter,
    verbose = 0
  )
}

# The grid: four hyperparameters crossed, and the rounds read off one fit
xgb_combos <- expand.grid(
  depth = c(1, 2, 4),
  eta = c(0.1, 0.05, 0.01),
  min_child_weight = c(1, 5),
  lambda = c(1, 10)
)
xgb_rounds <- c(50, 100, 200, 500, 1000)

# LOGO-CV by hand, as in Listing 32: one fit per combination and fold, at the
# most rounds; its first n rounds are the n-round fit
started <- Sys.time()
xgb_grid <- list()
for (i in seq_len(nrow(xgb_combos))) {
  auc_i <- matrix(NA_real_, length(xgb_rounds), length(proj_nms))
  colnames(auc_i) <- proj_nms
  for (rp in proj_nms) {
    test_cv <- df_split[[rp]]
    train_cv <- df_noholz[df_noholz$pid != rp, ]
    bst <- fit_xgb(train_cv, xgb_combos[i, ], max(xgb_rounds))
    for (j in seq_along(xgb_rounds)) {
      p <- predict(
        bst,
        as_dmatrix(test_cv),
        iterationrange = c(1, xgb_rounds[j])
      )
      auc_i[j, rp] <- auroc(test_cv[, out_nme], p)
    }
  }
  xgb_grid[[i]] <- data.frame(
    xgb_combos[i, ],
    n_iter = xgb_rounds,
    auc_i,
    row.names = NULL
  )
}
xgb_grid <- do.call(rbind, xgb_grid)
message(sprintf(
  "XGBoost grid: %.1f min",
  as.numeric(difftime(Sys.time(), started, units = "mins"))
))

# The rule of Module 7: the best worst project, ties by stability
proj_xgb <- as.matrix(xgb_grid[proj_nms])
xgb_grid$auc_stable <- rowMeans(proj_xgb) - apply(proj_xgb, 1, sd)
xgb_grid$auc_min <- apply(proj_xgb, 1, min)
best_xgb <- xgb_grid[order(-xgb_grid$auc_min, -xgb_grid$auc_stable), ][1, ]

# Refit the pick on all six tuning projects and score HOLZ once
y_xgb <- fit_xgb(df_noholz, best_xgb, best_xgb$n_iter) |>
  predict(as_dmatrix(df_holz))

# The rest is for the data set only. Reading rounds off a longer fit must
# equal a fit of that many rounds.
stopifnot(isTRUE(all.equal(
  predict(
    fit_xgb(df_noholz, best_xgb, max(xgb_rounds)),
    as_dmatrix(df_holz),
    iterationrange = c(1, best_xgb$n_iter)
  ),
  y_xgb
)))

# Every measure for the XGBoost pick: refit it on each fold, and HOLZ
assess_xgb <- function(train, test) {
  p <- predict(fit_xgb(train, best_xgb, best_xgb$n_iter), as_dmatrix(test))
  assess(test[, out_nme], p, threshold = 0.5)[measures]
}
est_xgb <- cbind(
  sapply(proj_nms, \(rp) {
    assess_xgb(df_noholz[df_noholz$pid != rp, ], df_split[[rp]])
  }),
  holz = assess(df_holz[, out_nme], y_xgb, threshold = 0.5)[measures]
)
stopifnot(isTRUE(all.equal(
  unname(est_xgb["auroc", proj_nms]),
  unname(unlist(best_xgb[proj_nms]))
)))

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
  tree = list(depth = depth_tree),
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
# the tree, the forest and AdaBoost equal direct fits scored on HOLZ
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
  fit_final,
  newdata = df_holz[, pred_nms],
  type = "margin",
  verbose = FALSE
)
y_tree <- predict(fit_tree, df_holz[, pred_nms], type = "prob")[, "success"]
y_rf <- predict(fit_rf, df_holz[, pred_nms], type = "prob")[, "success"]
direct <- c(
  tree = auroc(df_holz[, out_nme], y_tree),
  rf = auroc(df_holz[, out_nme], y_rf),
  adaboost = auroc(df_holz[, out_nme], y_ada)
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
