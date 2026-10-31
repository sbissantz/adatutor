#' Social Science Replication Project, from its own results
#'
#' The 21 studies of the Social Science Replication Project (SSRP), rebuilt
#' from the project's own results file, used to build [altmext]. It replaces
#' [ssrp723], Altmejd et al.'s copy, and keeps its `ssrp.X` ids.
#'
#' Differences from [ssrp723]:
#' * `replicate`: the pooled result of both data collections, the SSRP's
#'   headline (13 of 21 replicated). [ssrp723] holds the first collection's
#'   result (12 of 21); ssrp.5, ssrp.8 and ssrp.18 differ.
#' * `effect_size.o`: ssrp.7 (.278 rather than .197) and ssrp.21 (.368
#'   rather than .437).
#' * `n.o`: the n of the focal test, which SSRP's r and p rest on (e.g. 51
#'   groups rather than 366 participants for ssrp.1); seven study effects
#'   differ.
#' * `p_value.o`: two-sided; ssrp.6, ssrp.7 and ssrp.21 differ.
#' * `power.o`: computed from `effect_size.o` and `n.o`, as for every other
#'   project.
#'
#' @format A data frame with 21 rows (study effects) and the 7 columns of
#'   [altmejd], `replicate` coded 0/1.
#'
#' @source Camerer CF, Dreber A, Holzmeister F, et al. (2018) Evaluating the
#'   replicability of social science experiments in Nature and Science
#'   between 2010 and 2015. Nature Human Behaviour 2: 637-644.
#'   <https://doi.org/10.1038/s41562-018-0399-z>
#'
#'   Data: <https://osf.io/956xg/>, file `D3 - ReplicationResults.csv`,
#'   downloaded in October 2026; the `ssrp.X` ids from Altmejd et al.'s
#'   codebook (<https://osf.io/7ad56/>).
#'
#' @section License:
#' The SSRP's data are licensed CC0 1.0
#' (<https://creativecommons.org/publicdomain/zero/1.0/>).
#'
#' @family datasets
#'
"ssrp1026"
