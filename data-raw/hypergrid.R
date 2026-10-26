##########################################
# AdaBoost hyperparameter grid (LOGO-CV) #
##########################################

# Run `pkgload::load_all()` first. About an hour on 4 cores; finished depths
# are cached and skipped on a rerun.

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

score_fold <- function(project, depth, eta) {
  train <- tune[tune$pid != project, c(predictors, "replicate")]
  test <- tune[tune$pid == project, ]
  h <- rpart::rpart(replicate ~ ., data = train, maxdepth = depth, model = TRUE)
  fit <- adaboost(
    h,
    n_iter = max(rounds),
    eta = eta,
    verbose = FALSE,
    check_inputs = FALSE
  )
  weights <- vapply(fit, `[[`, numeric(1), "a")
  votes <- vapply(
    fit,
    \(learner) {
      ifelse(predict(learner$h, test, type = "class") == "success", 1, -1)
    },
    numeric(nrow(test))
  )
  # One fit gives every T: column T is the margin after T rounds
  margins <- t(apply(sweep(votes, 2, weights, `*`), 1, cumsum))
  t(vapply(
    rounds,
    \(T) assess(test$replicate, score = margins[, T])[measures],
    numeric(length(measures))
  ))
}

cache_dir <- file.path("data-raw", "cache", "hypergrid-t3000")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

run_depth <- function(depth) {
  file <- file.path(cache_dir, sprintf("depth%d.rds", depth))
  if (file.exists(file)) {
    return(readRDS(file))
  }
  started <- Sys.time()
  # T varies fastest, in the row order of score_fold()
  settings <- expand.grid(T = rounds, eta = etas)
  scores <- array(
    NA_real_,
    dim = c(nrow(settings), length(projects), length(measures)),
    dimnames = list(NULL, projects, measures)
  )
  for (eta in etas) {
    rows <- which(settings$eta == eta)
    for (project in projects) {
      scores[rows, project, ] <- score_fold(project, depth, eta)
    }
  }
  slice <- list(settings = cbind(depth = depth, settings), scores = scores)
  saveRDS(slice, file)
  message(sprintf(
    "depth %d: %.1f min",
    depth,
    as.numeric(difftime(Sys.time(), started, units = "mins"))
  ))
  slice
}

cores <- 4
slices <- parallel::mclapply(
  depths,
  run_depth,
  mc.cores = cores,
  mc.preschedule = FALSE
)
failed <- vapply(slices, inherits, logical(1), "try-error")
if (any(failed)) {
  stop(
    "depth ",
    paste(depths[failed], collapse = ", "),
    " failed: ",
    slices[[which(failed)[1]]]
  )
}

settings <- do.call(rbind, lapply(slices, `[[`, "settings"))
hypergrid <- lapply(setNames(measures, measures), function(measure) {
  values <- do.call(rbind, lapply(slices, \(slice) slice$scores[,, measure]))
  data.frame(settings[c("depth", "T", "eta")], values, row.names = NULL)
})
attr(hypergrid, "projects") <- data.frame(
  project = projects,
  n = as.integer(table(tune$pid)[projects]),
  base_rate = as.numeric(tapply(tune$replicate == "success", tune$pid, mean)[
    projects
  ]),
  row.names = NULL
)

# Listing 33's settings, fitted directly, must match the staged scores
listing <- expand.grid(depth = 1:3, T = c(10, 100, 1000), eta = c(1, 0.1))
for (i in seq_len(nrow(listing))) {
  setting <- listing[i, ]
  row <- which(
    hypergrid$auroc$depth == setting$depth &
      hypergrid$auroc$T == setting$T &
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
    fit <- adaboost(h, n_iter = setting$T, eta = setting$eta, verbose = FALSE)
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

usethis::use_data(hypergrid, overwrite = TRUE)
