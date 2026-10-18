#' SCORE replications
#'
#' 92 claims replicated by SCORE (Systematizing Confidence in Open Research
#' and Evidence), used to build [altmext]: one row per claim, SCORE's version
#' of record of its replication. Where SCORE pooled several labs (6 claims),
#' the record is the pooled result, as for Many Labs. 12 claims are from
#' SCORE's COVID-era set.
#'
#' Kept are replications with new data whose original n counts people and
#' whose original r, n and p are complete. Left out are replications with
#' existing data, SCORE's generalizability tests (a deliberately different
#' population), hybrids that pool the original's data with new data, pooled
#' records that include a generalizability lab, and originals whose n counts
#' years, groups, observations and the like.
#'
#' * `effect_size.o`: the original's |r|, as SCORE converted it.
#' * `p_value.o`: as SCORE gives it. Where SCORE gives only a bound, the exact
#'   two-sided p comes from the original's test statistic, unless that
#'   statistic was itself computed back from the bound; then the bound's value
#'   stays (5 claims, e.g. .001 for p < .001).
#' * `replicate`: significant (p <= .05) in the original's direction.
#'
#' @format A data frame with 92 rows (claims) and the 7 columns of [altmejd],
#'   `replicate` coded 0/1. `score.k` is the k-th of the kept claim ids in
#'   alphabetical order; the ids are in `inst/extdata/score1026.csv`.
#'
#' @source Tyner AH, et al. (2026) Nature.
#'   <https://doi.org/10.1038/s41586-025-10078-y>
#'
#'   Data: the authors' analysis-ready tables `repli_outcomes.csv`,
#'   `orig_outcomes.csv` and `paper_metadata.csv`, downloaded in October 2026.
#'
#' @family datasets
#'
"score1026"
