############################
# SCORE Data Set (Oct 2026) #
############################
#
# The data come from SCORE (Tyner et al. 2026,
# https://doi.org/10.1038/s41586-025-10078-y), the authors' analysis-ready
# tables, snapshotted by data-raw/extdata1026.R: one row per claim, SCORE's
# version of record of its replication. Where SCORE pooled several labs
# (6 of the claims kept), the record is the pooled result, as for Many Labs.
#
# Kept: replications with new data ("new data"), not SCORE's
# generalizability tests, whose original n counts people, with a complete
# original r, n and p. Left out are replications with existing data
# ("secondary data"), hybrids that pool the original's data with new data,
# originals whose n counts years, groups, observations and the like, and 7
# pooled records that include a generalizability lab (they are not pure
# direct replications).
#
# - effect_size.o: |orig_conv_r|, SCORE's conversion to r.
# - n.o: the original's sample size; NA (and left out) if not a whole number.
# - p_value.o: as SCORE gives it. Where SCORE gives only a bound ("p < .001"),
#   the exact two-sided p comes from the original's test statistic (z, t with
#   df, F with df), unless that statistic was itself back-computed from the
#   bound: one within 0.01 of the one- or two-tailed critical value of a round
#   p (.1 to .0001) counts as such, and the bound's value is kept.
# - replicate: the replication is significant (p <= .05) in the original's
#   direction (SCORE's repli_effect_direction).

source("data-raw/power-r.R")

# Load the CSV from ./inst/extdata/
csv_file <- system.file("extdata", "score1026.csv", package = "adatutor")
extdata <- read.csv(csv_file)
stopifnot(nrow(extdata) == 309, !anyDuplicated(extdata$claim_id))
as_flag <- function(x) toupper(as.character(x)) == "TRUE"

# ---- which claims ------------------------------------------------------------
# n counts people: the units name people and no set of them
counts_people <- function(units) {
  person <- grepl(
    "particip|student|respondent|individual|people|person|child|kindergart|adult|adolescen|worker|employee|patient|player|voter|female|male|women|\\bmen\\b|citizen|teacher|manager|grader|subject|consumer|volunteer|infant|parent|mother|father|bishop|athlete|nurse|physician|soldier|inmate|applicant|investor|resident|immigrant|member|youth|teen|undergrad",
    units,
    ignore.case = TRUE,
    perl = TRUE
  )
  set_of <- grepl(
    "year|observation|pair|group|household|dyad|couple|response|judg|team",
    units,
    ignore.case = TRUE
  )
  is.na(units) | units == "" | (person & !set_of)
}
whole <- function(n) ifelse(!is.na(n) & abs(n - round(n)) < 1e-9, n, NA)

# ---- the original's p: exact where the statistic allows ---------------------
stat <- extdata$orig_stat_value
df1 <- extdata$orig_stat_dof_1
df2 <- extdata$orig_stat_dof_2
p_given <- extdata$orig_p_value
p_type <- extdata$orig_p_value_type
is_z <- extdata$orig_stat_type %in% "z" & !is.na(stat)
is_t <- extdata$orig_stat_type %in% "t" & !is.na(stat) & !is.na(df1)
is_f <- extdata$orig_stat_type %in%
  "F" &
  !is.na(stat) &
  !is.na(df1) &
  !is.na(df2)
p_stat <- rep(NA_real_, nrow(extdata))
p_stat[is_z] <- 2 * stats::pnorm(-abs(stat[is_z]))
p_stat[is_t] <- 2 * stats::pt(-abs(stat[is_t]), df1[is_t])
p_stat[is_f] <- stats::pf(stat[is_f], df1[is_f], df2[is_f], lower.tail = FALSE)

# A statistic at a round p's critical value was back-computed from the bound
at_critical <- function(p) {
  two <- one <- rep(NA_real_, nrow(extdata))
  two[is_z] <- stats::qnorm(1 - p / 2)
  two[is_t] <- stats::qt(1 - p / 2, df1[is_t])
  two[is_f] <- stats::qf(1 - p, df1[is_f], df2[is_f])
  one[is_z] <- stats::qnorm(1 - p)
  one[is_t] <- stats::qt(1 - p, df1[is_t])
  (abs(abs(stat) - two) < 0.01) %in%
    TRUE |
    (abs(abs(stat) - one) < 0.01) %in% TRUE
}
back_computed <- Reduce(
  `|`,
  lapply(c(0.1, 0.05, 0.01, 0.005, 0.001, 1e-4), at_critical)
)
recomputed <- !p_type %in% "exact" & !is.na(p_stat) & !back_computed

# A recomputed p must lie inside SCORE's bound (a one-tailed bound halves it)
one_tailed <- extdata$orig_p_value_tails %in% "one-tailed"
p_check <- ifelse(one_tailed, p_stat / 2, p_stat)
inside <- ifelse(
  p_type %in% "less-than",
  p_check < p_given,
  ifelse(p_type %in% "threshold", p_check <= p_given, TRUE)
)
stopifnot(all(inside[recomputed] | is.na(p_given[recomputed])))
p_value_o <- ifelse(recomputed, p_stat, p_given)

# ---- one row per claim kept ---------------------------------------------------
same_direction <- as_flag(extdata$repli_effect_direction)
keep <- extdata$repli_type == "new data" &
  !as_flag(extdata$repli_is_generalizability) &
  extdata$generalizability_labs == 0 &
  counts_people(extdata$orig_sample_size_units) &
  !is.na(extdata$orig_conv_r) &
  !is.na(whole(extdata$orig_sample_size_value)) &
  !is.na(p_value_o) &
  !is.na(extdata$repli_p_value) &
  !is.na(same_direction)
x <- extdata[keep, ]
number <- match(x$claim_id, sort(x$claim_id, method = "radix"))

score1026 <- data.frame(
  eid = paste0("score.", number),
  effect_size.o = abs(x$orig_conv_r),
  n.o = x$orig_sample_size_value,
  p_value.o = p_value_o[keep],
  power.o = calculate_power_r(x$orig_sample_size_value, abs(x$orig_conv_r)),
  replicate = as.integer(x$repli_p_value <= 0.05 & same_direction[keep]),
  pid = "score"
)
score1026 <- score1026[order(number), ]
rownames(score1026) <- NULL
message(sprintf(
  "score1026: %d claims (%d pooled over labs), %d replicated; p exact from the statistic for %d, a bound kept for %d",
  nrow(score1026),
  sum(x$manylabs_type %in% "aggregation"),
  sum(score1026$replicate),
  sum(recomputed[keep]),
  sum(!p_type[keep] %in% "exact" & !recomputed[keep])
))
stopifnot(
  !anyNA(score1026),
  all(score1026$effect_size.o > 0),
  nrow(score1026) == 92,
  sum(score1026$replicate) == 46
)

# Save the data set as R data
usethis::use_data(score1026, overwrite = TRUE)
