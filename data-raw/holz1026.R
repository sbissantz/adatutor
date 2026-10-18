##################################
# Holzmeister Data Set (Oct 2026) #
##################################
#
# The data come from Holzmeister et al. (2024), https://osf.io/sk82q, file
# data/proc/data_to_use.csv, snapshotted by data-raw/extdata1026.R. 41 MTurk
# experiments published in PNAS 2015-2018 went to a decision market of 162
# social scientists; 26 were replicated: the 12 highest-priced, the 12
# lowest-priced and 2 at random. So the sample is deliberately bimodal, not a
# sample of a literature.
#
# - effect_size.o: r from the original's Cohen's d (d_os) for two equal
#   groups, r = d / sqrt(d^2 + 4), as Altmejd et al. convert d.
# - n.o: the original's number of observations (n_os).
# - p_value.o: the published file rounds p to four decimals, so eight
#   originals read 0. The exact two-sided p comes from the original's test
#   statistic (stat_os, of type ttype_os: t, F(1, df), Chi2(1) or z); each
#   rounds to the published p.
# - replicate: the replication is significant (p <= .05) in the original's
#   direction (d_rs > 0, as Holzmeister et al. sign it). This is their own
#   indicator, ri_ssc.

source("data-raw/power-r.R")

# Load the CSV from ./inst/extdata/
csv_file <- system.file("extdata", "holz1026.csv", package = "adatutor")
extdata <- read.csv(csv_file)
stopifnot(nrow(extdata) == 41)

# Two-sided p from the test statistic: t(df), F(1, df), Chi2(1) or z
calculate_p <- function(type, stat) {
  df <- suppressWarnings(as.numeric(sub("^.*[,(]([0-9.]+)\\)$", "\\1", type)))
  p <- rep(NA_real_, length(type))
  t <- grepl("^t\\(", type)
  f <- grepl("^F\\(1,", type)
  chi2 <- type == "Chi2(1)"
  z <- type == "z"
  p[t] <- 2 * stats::pt(-abs(stat[t]), df[t])
  p[f] <- stats::pf(stat[f], 1, df[f], lower.tail = FALSE)
  p[chi2] <- stats::pchisq(stat[chi2], 1, lower.tail = FALSE)
  p[z] <- 2 * stats::pnorm(-abs(stat[z]))
  p
}
p_exact <- calculate_p(extdata$ttype_os, extdata$stat_os)
stopifnot(
  !anyNA(p_exact),
  all(abs(round(p_exact, 4) - extdata$p_os) < 1e-9)
)

# Only the 26 replicated studies
replicated <- !is.na(extdata$n_rs)
x <- extdata[replicated, ]
stopifnot(nrow(x) == 26)

holz1026 <- data.frame(
  eid = paste0("holz.", as.integer(gsub("[()]", "", x$sref))),
  effect_size.o = x$d_os / sqrt(x$d_os^2 + 4),
  n.o = x$n_os,
  p_value.o = p_exact[replicated],
  power.o = NA_real_,
  replicate = as.integer(x$p_rs <= 0.05 & x$d_rs > 0),
  pid = "holz"
)
holz1026$power.o <- calculate_power_r(holz1026$n.o, holz1026$effect_size.o)
stopifnot(
  !anyNA(holz1026),
  identical(holz1026$replicate, as.integer(x$ri_ssc == "success")),
  sum(holz1026$replicate) == 14
)

# Save the data set as R data
usethis::use_data(holz1026, overwrite = TRUE)
