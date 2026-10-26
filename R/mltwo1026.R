#' Many Labs 2
#'
#' 24 of the 28 headline effects of Many Labs 2, used to build [altmext]. As
#' for Many Labs 1 and 3 in [altmejd723], the replication is the result pooled
#' over all sites.
#'
#' * `effect_size.o`: the original's r, as Many Labs 2 converted it.
#' * `p_value.o`: two-sided, from the original's statistic and df in Many
#'   Labs 2's key table. ml2.9's p is below double precision and reads 0.
#' * `replicate`: significant (p <= .05) in the original's direction. On
#'   these 24, Many Labs 2's own criterion (p < .0001) gives the same result.
#'
#' Three originals are corrected from their papers, where Many Labs 2's
#' tables depart from them: van Lange et al. (1997; the contrast replicated,
#' F(1, 535) = 9.14, n = 541), Anderson et al. (2012; t(115) = 3.05, n = 117)
#' and Critcher & Gilovich (2008; n = 201). For Tversky & Kahneman (1981),
#' the p comes from the z = 5.14 the paper prints.
#'
#' Left out: Norenzayan et al. (no original n or p), Inbar et al. and Schwarz
#' et al. (tests of a difference between two correlations, no original r),
#' and Savani et al. (the coded original found no difference, p = .70).
#'
#' @format A data frame with 24 rows (study effects) and the 7 columns of
#'   [altmejd], `replicate` coded 0/1. `ml2.k` is the k-th of Many Labs 2's
#'   analysis labels in alphabetical order (ml2.1 = Alter.1, ml2.24 =
#'   vanLange.1); the labels are in `inst/extdata/mltwo1026.csv`.
#'
#' @source Klein RA, et al. (2018) Many Labs 2:
#'   Investigating variation in replicability across samples and settings.
#'   Advances in Methods and Practices in Psychological Science 1(4):
#'   443-490. <https://doi.org/10.1177/2515245918810225>
#'
#'   Data: <https://osf.io/8cd4r/>, files `table5data_RM.csv`,
#'   `ML2_OriginalEffects.csv` and `ML2_KeyTable.csv`, downloaded in October
#'   2026.
#'
#' @section License:
#' Many Labs 2's data are licensed CC0 1.0
#' (<https://creativecommons.org/publicdomain/zero/1.0/>).
#'
#' @family datasets
#'
"mltwo1026"
