#' Holzmeister et al.'s decision-market replications
#'
#' The 26 online experiments that Holzmeister et al. (2024) replicated.
#'
#' @format A data frame with 26 rows (study effects) and the 7 columns of
#'   [altmext], `replicate` coded 0/1. `eid` carries the authors' study
#'   number (`holz.1` to `holz.41`).
#'
#' @source Holzmeister F, Johannesson M, Camerer CF, et al. (2024) Examining
#'   the replicability of online experiments selected by a decision market.
#'   Nature Human Behaviour 9: 316-330.
#'   \doi{10.1038/s41562-024-02062-9}
#'
#'   Data: <https://osf.io/sk82q/>.
#'
#' @section License:
#' Holzmeister et al.'s data are licensed CC BY 4.0
#' (<https://creativecommons.org/licenses/by/4.0/>). This data set changes
#' them: it keeps the 26 replicated studies, converts the original's Cohen's d
#' to r, recomputes p from the original's test statistic and adds the
#' original study's power.
#'
#' @family datasets
#'
"holz1026"
