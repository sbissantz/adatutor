##########################################
# Altmejd Extended (altmext) Data Set   #
##########################################
#
# Altmejd et al.'s four projects (altmejd723, unchanged) with SSRP rebuilt
# from its own results file (ssrp1026) and two more projects: Holzmeister et
# al. 2024 (holz1026) and Many Labs 2 (mltwo1026). Same columns and the same
# rule as altmejd: one row per original effect, Many Labs' multi-lab claims as
# the pooled result.

# Load the per-project data sets
data("altmejd723")
data("ssrp1026")
data("holz1026")
data("mltwo1026")

# Variables of interest, in altmejd's order
voi <- c(
  "eid",
  "pid",
  "effect_size.o",
  "n.o",
  "p_value.o",
  "power.o",
  "replicate"
)

# Bind the data sets with the pre-selected variables
projects <- list(altmejd723, ssrp1026, holz1026, mltwo1026)
stopifnot(all(vapply(projects, function(x) all(voi %in% names(x)), TRUE)))
altmext <- do.call(rbind.data.frame, lapply(projects, `[`, voi))

# Sample sizes are counts, an integer as in altmejd
stopifnot(all(altmext$n.o == round(altmext$n.o)))
altmext$n.o <- as.integer(altmext$n.o)

# Make the outcome a factor on the 1/2 scale (rpart consistency), as altmejd
altmext$replicate <- factor(
  altmext$replicate + 1,
  labels = c("failure", "success")
)

# Sort by project prefix and the number after the dot
prefix <- sub("\\..*", "", altmext$eid)
suffix <- as.numeric(sub("^[^.]*\\.", "", altmext$eid))
altmext <- altmext[order(prefix, suffix, method = "radix"), ]
rownames(altmext) <- paste0(seq_len(nrow(altmext)))

stopifnot(
  nrow(altmext) == 131 + 21 + 26 + 24,
  !anyNA(altmext),
  !anyDuplicated(altmext$eid),
  sum(altmext$replicate == "success") == 56 + 13 + 14 + 13
)

# Save the data set as R data
usethis::use_data(altmext, overwrite = TRUE)
