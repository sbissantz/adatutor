##############################################
# Stratified 70/15/15 split of 'altmejd'     #
##############################################

# Load data set
data("altmejd")

# Shorten 'altmejd'
df <- altmejd

# ---- Listing 7: the 70/30 stratified split ---------------------------------

# For reproducibility
set.seed(112)

# Split proportion
prop <- 0.7

# Partition data based on outcome
set_split <- split(df, df$replicate)

# Number of training samples, per outcome class
n_train <- lapply(set_split, function(set) round(nrow(set) * prop))

# Training index
strat_idx <- mapply(
  function(set, n) sample(set[, "eid"], n),
  set_split,
  n_train
)
train_idx <- df$eid %in% unlist(strat_idx)

# Training set
altmejd_train <- df[train_idx, ]

# Test set (incl. validation)
testvalid <- df[!train_idx, ]

# ---- Listing 8: halving the rest into validation and test ------------------

# For reproducibility
set.seed(112)

# Split proportion
prop <- 0.5

# Partition data based on outcome
set_split <- split(testvalid, testvalid$replicate)

# Numbers of validation samples, per outcome class
n_valid <- lapply(set_split, function(set) round(nrow(set) * prop))

# Validation index
strat_idx <- mapply(
  function(set, n) sample(set[, "eid"], n),
  set_split,
  n_valid
)
valid_idx <- testvalid$eid %in% unlist(strat_idx)

# Validation set
altmejd_valid <- testvalid[valid_idx, ]

# Test set
altmejd_test <- testvalid[!valid_idx, ]

# ---- one object, not three -------------------------------------------------

# Ship as a bundle so they cannot be loaded apart
altmejd_splits <- list(
  train = altmejd_train,
  valid = altmejd_valid,
  test = altmejd_test
)

# Save data set as R data
usethis::use_data(altmejd_splits, overwrite = TRUE)
