###########################
# SSRP Data Set (Oct 2026) #
###########################
#
# The data come from SSRP's own results file (Camerer et al. 2018,
# https://osf.io/956xg), snapshotted by data-raw/extdata1026.R with Altmejd et
# al.'s ssrp.X ids. ssrp1026 takes the place of ssrp723 in altmext and
# differs from it as follows:
# - replicate: the pooled result of both stages (rep_sr_rp), the paper's
#   headline 13 of 21. ssrp723 holds the first stage's result (12 of 21).
# - effect_size.o: ssrp723 departs from the paper for ssrp.7 and ssrp.21.
# - n.o: the n of the focal test (e.g. 51 groups, not 366 participants), the
#   n behind SSRP's r and p, and behind Altmejd's own power.o.
# - p_value.o: SSRP's two-sided p (ssrp723 halves ssrp.6's, and ssrp.7's and
#   ssrp.21's follow their r).
# - power.o: from r and n as for every other project (data-raw/power-r.R).

source("data-raw/power-r.R")

# Load the CSV from ./inst/extdata/
csv_file <- system.file("extdata", "ssrp1026.csv", package = "adatutor")
extdata <- read.csv(csv_file)
stopifnot(nrow(extdata) == 21)

ssrp1026 <- data.frame(
  eid = extdata$id,
  effect_size.o = extdata$r_os,
  n.o = extdata$n_os,
  p_value.o = extdata$p_os,
  power.o = calculate_power_r(extdata$n_os, extdata$r_os),
  replicate = extdata$rep_sr_rp,
  pid = "ssrp"
)
stopifnot(!anyNA(ssrp1026), sum(ssrp1026$replicate) == 13)

# Every difference from ssrp723, listed
data("ssrp723")
old <- ssrp723[match(ssrp1026$eid, ssrp723$eid), ]
changed <- function(new, old, tolerance) {
  ssrp1026$eid[abs(new - old) > tolerance * pmax(abs(old), 1e-12)]
}
diffs <- list(
  replicate = changed(ssrp1026$replicate, old$replicate, 0),
  effect_size.o = changed(ssrp1026$effect_size.o, old$effect_size.o, 0.01),
  n.o = changed(ssrp1026$n.o, old$n.o, 0),
  p_value.o = changed(ssrp1026$p_value.o, old$p_value.o, 0.01)
)
stopifnot(
  setequal(diffs$replicate, c("ssrp.5", "ssrp.8", "ssrp.18")),
  setequal(diffs$effect_size.o, c("ssrp.7", "ssrp.21")),
  setequal(
    diffs$n.o,
    c("ssrp.1", "ssrp.2", "ssrp.3", "ssrp.7", "ssrp.10", "ssrp.11", "ssrp.21")
  ),
  setequal(diffs$p_value.o, c("ssrp.6", "ssrp.7", "ssrp.21"))
)

# Save the data set as R data
usethis::use_data(ssrp1026, overwrite = TRUE)
