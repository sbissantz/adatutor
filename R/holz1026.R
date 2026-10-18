#' Holzmeister et al.'s decision-market replications
#'
#' The 26 replications of Holzmeister et al. (2024), used to build [altmext].
#' 41 online (MTurk) experiments published in PNAS between 2015 and 2018 went
#' to a decision market of social scientists; the 12 highest-priced, the 12
#' lowest-priced and 2 at random were replicated. The selection is therefore
#' deliberately bimodal, not a sample of a literature.
#'
#' * `effect_size.o`: r from the original's Cohen's d for two equal groups,
#'   r = d / sqrt(d^2 + 4).
#' * `p_value.o`: two-sided, from the original's test statistic. The
#'   published file rounds p to four decimals (eight originals read 0); each
#'   value here rounds to the published one.
#' * `replicate`: significant (p <= .05) in the original's direction, the
#'   authors' own indicator.
#'
#' @format A data frame with 26 rows (study effects) and the 7 columns of
#'   [altmejd], `replicate` coded 0/1. `eid` carries the authors' study
#'   number (`holz.1` to `holz.41`).
#'
#' @source Holzmeister F, Johannesson M, Camerer CF, et al. (2024) Examining
#'   the replicability of online experiments selected by a decision market.
#'   Nature Human Behaviour 9: 316-330.
#'   <https://doi.org/10.1038/s41562-024-02062-9>
#'
#'   Data: <https://osf.io/sk82q/>, file `data/proc/data_to_use.csv`,
#'   downloaded in October 2026.
#'
#' @family datasets
#'
"holz1026"
