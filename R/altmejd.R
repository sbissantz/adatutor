#' Short version of Altmejd et al.'s replication database
#'
#' An adapted version of the replication database of Altmejd et al. (2019):
#' 152 study effects from five large-scale replication projects, and
#' whether each one replicated. The projects are Many Labs 1 and 3, the
#' Reproducibility Project: Psychology, the Experimental Economics Replication
#' Project and the Social Sciences Replication Project.
#'
#' @format A data frame with 152 rows (study effects) and 7 columns:
#' * `eid`: Effect identifier.
#' * `pid`: Project identifier: Many Labs 1 (ML1), Many Labs 3 (ML3),
#'   Reproducibility Project: Psychology (RPP), Experimental Economics
#'   Replication Project (EERP) or Social Sciences Replication Project (SSRP).
#' * `effect_size.o`: Standardized effect size of the original study.
#' * `n.o`: Sample size of the original study.
#' * `p_value.o`: P-value of the original study.
#' * `power.o`: Power of the original study.
#' * `replicate`: Whether the replication succeeded.
#'
#' @source Altmejd A, Dreber A, Forsell E, Huber J, Imai T, et al. (2019)
#'   Predicting the replicability of social science lab experiments. PLOS ONE
#'   14(12): e0225826. \doi{10.1371/journal.pone.0225826}
#'
#'   The data and more information are in their OSF repository:
#'   <https://osf.io/4fn73/>.
#'
#' @family datasets
#'
"altmejd"
