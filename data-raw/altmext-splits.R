##############################################
# Stratified 70/15/15 split of 'altmext'     #
##############################################

# Needs the package itself, so run `pkgload::load_all()` first. Same call as
# altmejd-splits.R: Listing 9 with `df <- altmext`.

# Load data set
data("altmext")

# ---- Listing 9: the 70/15/15 stratified split ------------------------------

# For reproducibility
set.seed(112)

# Proportions of the whole data, one per set
splits <- partition(altmext, prop = c(0.70, 0.15, 0.15), strata = "replicate")

# ---- one object, not three -------------------------------------------------

# Ship as a bundle so they cannot be loaded apart
altmext_splits <- list(
  train = splits$set1,
  valid = splits$set2,
  test = splits$set3
)

# ---- assert the split, so a change fails loudly ----------------------------

sizes <- vapply(altmext_splits, nrow, integer(1))
stopifnot(identical(sizes, c(train = 205L, valid = 44L, test = 45L)))

# every effect lands in exactly one set
eids <- unlist(lapply(altmext_splits, `[[`, "eid"), use.names = FALSE)
stopifnot(
  setequal(eids, altmext$eid),
  !anyDuplicated(eids)
)

# each set keeps the .483 base rate of the whole: .483, .477 and .489
base_rate <- mean(altmext$replicate == "success")
rates <- vapply(
  altmext_splits,
  \(set) mean(set$replicate == "success"),
  numeric(1)
)
stopifnot(max(abs(rates - base_rate)) < 0.02)

# Save data set as R data
usethis::use_data(altmext_splits, overwrite = TRUE)
