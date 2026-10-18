##################################
# Many Labs 2 Data Set (Oct 2026) #
##################################
#
# The data come from Many Labs 2 (Klein et al. 2018,
# https://osf.io/8cd4r): Table 5's 28 headline analyses, pooled over all
# sites, with each original's effect and test, snapshotted by
# data-raw/extdata1026.R. As for Many Labs 1 and 3 in altmejd723, the
# replication is the pooled (aggregated) result.
#
# - effect_size.o: the original's r (ESCI.r, ML2's conversion).
# - n.o: the original's n (ori_N).
# - p_value.o: two-sided, from the original's statistic and df in ML2's key
#   table. Hauser.1's p is below double precision and reads 0.
# - replicate: the pooled replication is significant (p <= .05) in the
#   original's direction. On the 24 analyses kept, ML2's own criterion
#   (p < .0001) gives the same result.
#
# Corrections from the original papers, where ML2's key table and Table 5
# depart from them (the n is the sample the test analysed):
# - vanLange.1: the contrast ML2 replicated is F(1, 535) = 9.14 (Van Lange et
#   al. 1997, p. 740; quoted by Klein et al. 2018, p. 458), not the omnibus
#   F(2, 535) = 4.82 the key table codes as one df; n = 541. Its r follows
#   from the F.
# - Anderson.1: t(115) = 3.05 (Anderson et al. 2012, p. 768); n = 117.
# - Critcher.1: 207 took part, 6 were excluded (Critcher & Gilovich 2008,
#   p. 245); n = 201.
# - Tversky.1: the key table has no statistic for the odds ratio; the paper
#   prints the original's z = 5.14 (Klein et al. 2018, p. 460).
#
# Left out (4 of 28): Norenzayan.1 (no original n or p; coded OR = 1.00),
# Inbar.1a and Schwarz.1a (Fisher-z tests on a difference of correlations, no
# original r), Savani.3a (a null claim: the coded original, an American
# subgroup, found no difference, p = .70).

source("data-raw/power-r.R")

# Load the CSV from ./inst/extdata/
csv_file <- system.file("extdata", "mltwo1026.csv", package = "adatutor")
extdata <- read.csv(csv_file)
stopifnot(nrow(extdata) == 28)

# ---- corrections from the papers --------------------------------------------
paper <- data.frame(
  analysis = c("vanLange.1", "Anderson.1", "Critcher.1"),
  stat = c(9.14, 3.05, 2.12),
  df1 = c(1, 115, 197.5),
  df2 = c(535, NA, NA),
  n = c(541, 117, 201)
)
fix <- match(extdata$ori_ML2analysis, paper$analysis)
hit <- !is.na(fix)
stopifnot(sum(hit) == 3)
extdata$orig.stat.ncp[hit] <- paper$stat[fix[hit]]
extdata$orig.stat.df1[hit] <- paper$df1[fix[hit]]
extdata$orig.stat.df2[hit] <- paper$df2[fix[hit]]
extdata$ori_N[hit] <- paper$n[fix[hit]]
van <- extdata$ori_ML2analysis == "vanLange.1"
extdata$ESCI.r[van] <- sign(extdata$ESCI.r[van]) *
  sqrt(9.14 / (9.14 + 535))

# ---- the original's two-sided p ---------------------------------------------
stat <- extdata$orig.stat.ncp
df1 <- extdata$orig.stat.df1
type <- extdata$orig.stat.type
p_o <- rep(NA_real_, nrow(extdata))
t <- type %in% c("t", "t.r", "t.p")
f <- type == "f"
chi2 <- type == "X2"
z <- type == "Z"
p_o[t] <- 2 * stats::pt(-abs(stat[t]), df1[t])
p_o[f] <- stats::pf(
  stat[f],
  df1[f],
  extdata$orig.stat.df2[f],
  lower.tail = FALSE
)
p_o[chi2] <- stats::pchisq(stat[chi2], df1[chi2], lower.tail = FALSE)
p_o[z] <- 2 * stats::pnorm(-abs(stat[z]))
tversky <- extdata$ori_ML2analysis == "Tversky.1"
p_o[tversky] <- 2 * stats::pnorm(-5.14)

# ---- direction and outcome --------------------------------------------------
# The original's direction: the sign of its r, else of its statistic
reference <- ifelse(
  is.na(extdata$ESCI.r),
  extdata$testInfo.statistic,
  extdata$ESCI.r
)
same <- sign(extdata$ML2_r) == sign(reference)
# The paper's counts on all 28 (Klein et al. 2018, abstract): 15 at p < .05
# and 14 at p < .0001, with Norenzayan's null-coded original not contradicting
# its replication
same_all <- ifelse(reference == 0, TRUE, same)
stopifnot(
  sum(extdata$ML2_pval < 0.05 & same_all) == 15,
  sum(extdata$ML2_pval < 1e-4 & same_all) == 14
)

# ---- the 24 analyses kept ---------------------------------------------------
left_out <- c("Norenzayan.1", "Inbar.1a", "Schwarz.1a", "Savani.3a")
keep <- !extdata$analysis %in% left_out
stopifnot(sum(keep) == 24)
x <- extdata[keep, ]
number <- match(x$analysis, sort(x$analysis, method = "radix"))

mltwo1026 <- data.frame(
  eid = paste0("ml2.", number),
  effect_size.o = x$ESCI.r,
  n.o = x$ori_N,
  p_value.o = p_o[keep],
  power.o = calculate_power_r(x$ori_N, x$ESCI.r),
  replicate = as.integer(x$ML2_pval <= 0.05 & same[keep]),
  pid = "ml2"
)
stopifnot(
  !anyNA(mltwo1026),
  all(mltwo1026$effect_size.o > 0),
  identical(
    mltwo1026$replicate,
    as.integer(x$ML2_pval < 1e-4 & same[keep])
  ),
  sum(mltwo1026$replicate) == 13
)
mltwo1026 <- mltwo1026[order(number), ]
rownames(mltwo1026) <- NULL

# Save the data set as R data
usethis::use_data(mltwo1026, overwrite = TRUE)
