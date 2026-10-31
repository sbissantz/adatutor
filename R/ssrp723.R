#' Replication database, SSRP only
#'
#' A subset of the replication database of Altmejd et al. (2019), used only
#' to build [altmejd]. It holds one project: the Social Science Replication
#' Project (SSRP).
#'
#' It departs from the SSRP's own results file in a few places: `replicate`
#' is the first data collection's result (12 of 21) rather than the pooled
#' headline (13 of 21); two effect sizes (ssrp.7, ssrp.21) and the p-value of
#' ssrp.6 differ; and for seven study effects `n.o` counts participants while
#' `power.o` uses the n of the focal test. [ssrp1026] is rebuilt from the
#' SSRP's file and used in [altmext].
#'
#' @format A data frame with 21 rows (study effects) and the 7 columns of
#'   [altmejd].
#'
#' @source Altmejd A, Dreber A, Forsell E, Huber J, Imai T, et al. (2019)
#'   Predicting the replicability of social science lab experiments. PLOS ONE
#'   14(12): e0225826. <https://doi.org/10.1371/journal.pone.0225826>
#'
#'   The data and more information are in their OSF repository:
#'   <https://osf.io/4fn73/>.
#'
#' @family datasets
#'
"ssrp723"
