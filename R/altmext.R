#' Altmejd et al.'s replication database, extended
#'
#' Altmejd et al.'s (2019) replication database, extended to seven
#' large-scale replication projects: 202 study effects and whether each one
#' replicated. Each row is one original effect, and the predictors describe
#' the original study.
#'
#' @format A data frame with 202 rows (study effects) and 7 columns:
#' * `eid`: Effect identifier, `<pid>.<number>`.
#' * `pid`: Project identifier: Many Labs 1 (`ml1`), 2 (`ml2`) and 3 (`ml3`),
#'   Reproducibility Project: Psychology (`rpp`), Experimental Economics
#'   Replication Project (`eerp`), Social Sciences Replication Project
#'   (`ssrp`) or Holzmeister et al. (`holz`).
#' * `effect_size.o`: Effect size of the original study, as a correlation r.
#' * `n.o`: Sample size of the original study.
#' * `p_value.o`: P-value of the original study.
#' * `power.o`: Power of the original study: a two-sided test of r at
#'   \eqn{\alpha = .05}{alpha = .05} with sample size `n.o`.
#' * `replicate`: Whether the replication succeeded: significant
#'   (\eqn{p \le .05}{p <= .05}) in the original's direction. A factor with
#'   the levels `failure` and `success`. A multi-site replication counts with
#'   its pooled result.
#'
#' @source Altmejd A, Dreber A, Forsell E, Huber J, Imai T, et al. (2019)
#'   Predicting the replicability of social science lab experiments. PLOS ONE
#'   14(12): e0225826. \doi{10.1371/journal.pone.0225826}
#'
#'   For the other sources, see [ssrp1026], [holz1026] and [mltwo1026].
#'
#' @section License:
#' The rows from Altmejd et al.'s database (Many Labs 1 and 3, the
#' Reproducibility Project: Psychology and the Experimental Economics
#' Replication Project) may be shared under PLOS ONE's data policy, as
#' A. Altmejd confirmed (personal communication). The database is available
#' from <https://osf.io/4fn73/> (\doi{10.17605/OSF.IO/4FN73}). Of the projects
#' behind it, the Reproducibility Project: Psychology and Many Labs 1 are
#' licensed CC0 1.0 and Many Labs 3 is licensed CC BY 4.0. The other three
#' projects carry their own licenses: [ssrp1026] (CC0 1.0), [holz1026]
#' (CC BY 4.0) and [mltwo1026] (CC0 1.0).
#'
#' @family datasets
#'
"altmext"
