#' Altmejd et al.'s replication database, extended
#'
#' Altmejd et al.'s (2019) replication database in the format of [altmejd],
#' extended to eight large-scale replication projects: 294 original studies
#' and whether each one replicated. Many Labs 1 and 3, the Replication Project
#' Psychology and the Experimental Economics Replication Project come from
#' [altmejd723] unchanged. The Social Science Replication Project is rebuilt
#' from its own results file ([ssrp1026]). Three projects are new:
#' Holzmeister et al.'s decision-market replications ([holz1026]), Many Labs 2
#' ([mltwo1026]) and SCORE ([score1026]).
#'
#' Every row follows Altmejd et al.'s rule: one row per original effect, the
#' predictors describe the original study, and a multi-site replication counts
#' with its pooled result. A replication succeeded if it is significant
#' (p <= .05) in the original's direction.
#'
#' @format A data frame with 294 rows (study effects) and the 7 columns of
#'   [altmejd]:
#' * `eid`: Effect identifier, `<pid>.<number>`.
#' * `pid`: Project identifier: Many Labs 1 (`ml1`), 2 (`ml2`) and 3 (`ml3`),
#'   Replication Project Psychology (`rpp`), Experimental Economics
#'   Replication Project (`eerp`), Social Science Replication Project
#'   (`ssrp`), Holzmeister et al. (`holz`) or SCORE (`score`).
#' * `effect_size.o`: Effect size of the original study, as a correlation r.
#' * `n.o`: Number of observations in the original study.
#' * `p_value.o`: P-value of the original study.
#' * `power.o`: Power of the original study: a two-sided test of r at
#'   alpha = .05 with `n.o` observations.
#' * `replicate`: Whether the replication succeeded.
#'
#' @details
#' Projects and replication rates: rpp 90 (31 replicated), eerp 18 (11), ml1
#' 13 (11), ml3 10 (3), ssrp 21 (13), holz 26 (14), ml2 24 (13), score 92 (46);
#' 142 of 294 in all.
#'
#' Things to keep in mind:
#' * The SSRP rows differ from [altmejd]'s: the pooled outcome (13 rather than
#'   12 of 21 replicated), two effect sizes, three p-values and seven sample
#'   sizes. See [ssrp1026].
#' * SCORE is the largest project (31% of rows) and spans more disciplines.
#'   Evaluate by project, e.g. with `logo_cv(..., group = "pid")`, or weight
#'   rows by project, so that it does not dominate a pooled result.
#' * Holzmeister et al.'s 26 studies were selected by a decision market (the
#'   12 highest-priced, the 12 lowest-priced and 2 at random), not sampled
#'   from a literature.
#' * Five SCORE originals report only a bound, and `p_value.o` holds the
#'   bound's value (e.g. .001 for p < .001). ml2.9's p is below double
#'   precision and reads 0.
#' * Two pairs of effects come from the same original paper: ml2.21 and ml1.2
#'   (Tversky & Kahneman 1981, two different problems), ml2.16 and rpp.68
#'   (Risen & Gilovich 2008, Studies 2 and 6).
#'
#' @source Altmejd A, Dreber A, Forsell E, Huber J, Imai T, et al. (2019)
#'   Predicting the replicability of social science lab experiments. PLOS ONE
#'   14(12): e0225826. <https://doi.org/10.1371/journal.pone.0225826>
#'
#'   For the other sources, see [ssrp1026], [holz1026], [mltwo1026] and
#'   [score1026]. The data were downloaded in October 2026; each project's
#'   snapshot is in `inst/extdata` and built by `data-raw/`.
#'
#' @family datasets
#'
"altmext"
