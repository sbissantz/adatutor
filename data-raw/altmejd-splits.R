##############################################
# Stratified 70/15/15 split of 'altmejd'     #
##############################################

# Needs the package itself, so run `pkgload::load_all()` first. The split used
# to be copied out of Listings 7 and 8 by hand; it is now the one call those
# listings build up to, which is why the assertions below matter.

# Load data set
data("altmejd")

# ---- Listings 7 to 9: the 70/15/15 stratified split ------------------------

# For reproducibility
set.seed(112)

# Proportions of the whole data, one per set
splits <- partition(altmejd, prop = c(0.70, 0.15, 0.15), strata = "replicate")

# ---- one object, not three -------------------------------------------------

# Ship as a bundle so they cannot be loaded apart
altmejd_splits <- list(
  train = splits$set1,
  valid = splits$set2,
  test = splits$set3
)

# ---- assert the split, so a change fails loudly ----------------------------

# `partition()` builds this now, so a regression there would reship a different
# split in silence. R 3.6.0 changed `sample()` and moved every seeded split in
# the wild with it; these checks are what catch the next such change.

sizes <- vapply(altmejd_splits, nrow, integer(1))
stopifnot(identical(sizes, c(train = 107L, valid = 23L, test = 22L)))

# every effect lands in exactly one set
eids <- unlist(lapply(altmejd_splits, `[[`, "eid"), use.names = FALSE)
stopifnot(
  setequal(eids, altmejd$eid),
  !anyDuplicated(eids)
)

# stratifying is the point, so each set keeps the .447 base rate of the whole:
# .449, .435 and .455, the widest gap being .013 on the 23-row validation set
rate <- function(set) mean(set$replicate == "success")
stopifnot(
  max(abs(vapply(altmejd_splits, rate, numeric(1)) - rate(altmejd))) < 0.02
)

# the stump Module 2 is written against: `power.o` at 0.9305, leaves of 66 and 41
stump <- rpart::rpart(
  replicate ~ power.o + n.o,
  data = altmejd_splits$train,
  control = rpart::rpart.control(maxdepth = 1)
)
stopifnot(
  identical(as.character(stump$frame$var[1]), "power.o"),
  isTRUE(all.equal(unname(stump$splits[1, "index"]), 0.9305, tolerance = 1e-4)),
  identical(stump$frame$n[stump$frame$var == "<leaf>"], c(66L, 41L))
)

# Save data set as R data
usethis::use_data(altmejd_splits, overwrite = TRUE)
