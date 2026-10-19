###################################################
# Snapshots of the Published Files (October 2026) #
###################################################
#
# The download step for the four 1026 datasets: each project's own published
# values, copied into one CSV per project in ./inst/extdata/. Nothing is
# computed here; ssrp1026.R, holz1026.R, mltwo1026.R and score1026.R do all of
# the processing.
#
# The files are read from a local copy of the projects' data (the exmachina
# repository). Every input is checked against the md5 it had when the
# snapshot was taken, so a changed file stops this script instead of changing
# the snapshot in silence.
#
# Sources:
# - SSRP: Camerer et al. (2018), https://osf.io/956xg, file
#   "Analysis/Data/Data Processed/D3 - ReplicationResults.csv". The ssrp.X ids
#   come from Altmejd et al.'s codebook (https://osf.io/7ad56, file
#   "SSRP-evaluation/ssrp_data/codebook.csv"), so they match ssrp723.
# - Holzmeister et al. (2024), https://osf.io/sk82q, file
#   "data/proc/data_to_use.csv".
# - Many Labs 2: Klein et al. (2018), https://osf.io/8cd4r, files
#   "!!Figures_Tables/table5data_RM.csv", "!!RawData/ML2_OriginalEffects.csv"
#   and "!!KeyTables/ML2_KeyTable.csv".
# - SCORE: Tyner et al. (2026), https://doi.org/10.1038/s41586-025-10078-y,
#   the analysis-ready tables "repli_outcomes.csv", "orig_outcomes.csv" and
#   "paper_metadata.csv". The location of the tables is in the paper's data
#   availability statement.

# Location of the local copy
src <- "~/projects/exmachina/inst/context"

files <- c(
  ssrp_d3 = "lsrprojects-et-al/camerer-2018-ssrp/Analysis/Data/Data Processed/D3 - ReplicationResults.csv",
  ssrp_codebook = "crossvalid-et-al/altmejd-2019/code-data/SSRP-evaluation/ssrp_data/codebook.csv",
  holz = "lsrprojects-et-al/holzmeister-2024-oerp/osf-sk82q/data/proc/data_to_use.csv",
  ml2_table5 = "lsrprojects-et-al/klein-2018-mlrp2/OSFdata/!!Figures_Tables/table5data_RM.csv",
  ml2_effects = "lsrprojects-et-al/klein-2018-mlrp2/OSFdata/!!RawData/ML2_OriginalEffects.csv",
  ml2_key = "lsrprojects-et-al/klein-2018-mlrp2/OSFdata/!!KeyTables/ML2_KeyTable.csv",
  score_repli = "lsrprojects-et-al/tyner-2026-score/Analysis-ready Data/Replication and Reproduction Papers/repli_outcomes.csv",
  score_orig = "lsrprojects-et-al/tyner-2026-score/Analysis-ready Data/Replication and Reproduction Papers/orig_outcomes.csv",
  score_papers = "lsrprojects-et-al/tyner-2026-score/Analysis-ready Data/Replication and Reproduction Papers/paper_metadata.csv"
)
paths <- file.path(path.expand(src), files)
names(paths) <- names(files)

# md5 of each input when the snapshot was taken
md5 <- c(
  ssrp_d3 = "207de92b8b70283cf00c3c47dd32bdda",
  ssrp_codebook = "cc97008ac0a195b723b226af9d490383",
  holz = "2413af8d6454d361cd8a9c7d3be5a93f",
  ml2_table5 = "4596c37cf74e2ae2db60f6c16fc87c48",
  ml2_effects = "232c5eb7c1fd576f5473638e69c746d7",
  ml2_key = "701aebd7b4e2bb6d76085eb51e4ba2e4",
  score_repli = "5716b665a29473265146809335f961dc",
  score_orig = "d0d2cc68fc24c5f1c8dc5c510c7a3f62",
  score_papers = "45b9559cb0c9411812ccc23e554d859c"
)
stopifnot(identical(unname(tools::md5sum(paths)), unname(md5[names(paths)])))

read <- function(name) {
  utils::read.csv(paths[[name]], check.names = FALSE, stringsAsFactors = FALSE)
}

# Write a snapshot and make sure it holds plain text only
write_snapshot <- function(x, file) {
  out <- file.path("inst", "extdata", file)
  utils::write.csv(x, out, row.names = FALSE, na = "")
  stopifnot(!any(grepl("<U\\+[0-9A-F]{4}>", readLines(out, warn = FALSE))))
  message(sprintf("%s: %d rows, %d columns", out, nrow(x), ncol(x)))
}

# ---- SSRP: D3 with Altmejd's ids --------------------------------------------
d3 <- read("ssrp_d3")
codebook <- read("ssrp_codebook")
names(codebook)[1] <- "ssrp_id" # drop the byte order mark
k <- match(d3$study, codebook$ssrp_id)
stopifnot(nrow(d3) == 21, !anyNA(k))
ssrp_cols <- c(
  "study",
  "sref",
  "type_os",
  "stat_os",
  "n_os",
  "r_os",
  "p_os",
  "rep_sr_rs1",
  "rep_sr_rs2",
  "n_rp",
  "r_rp",
  "p_rp",
  "rep_sr_rp"
)
write_snapshot(
  cbind(id = codebook$id[k], label = codebook$label[k], d3[, ssrp_cols]),
  "ssrp1026.csv"
)

# ---- Holzmeister: the published file as it is -------------------------------
holz <- read("holz")
stopifnot(nrow(holz) == 41)
write_snapshot(holz, "holz1026.csv")

# ---- ML2: Table 5 with the original's effect and test, per analysis ---------
t5 <- read("ml2_table5")
effects <- read("ml2_effects")
key <- read("ml2_key")
stopifnot(nrow(t5) == 28)
ke <- match(t5$ori_ML2analysis, effects$study.analysis)
kk <- match(t5$ori_ML2analysis, key$study.analysis)
stopifnot(!anyNA(ke), !anyNA(kk))
t5_cols <- c(
  "study",
  "analysis",
  "ori_ML2analysis",
  "ori_stattype",
  "ori_N",
  "ML2_N",
  "ML2_r",
  "ML2_pval"
)
key_cols <- c(
  "orig.stat.type",
  "orig.stat.ncp",
  "orig.stat.df1",
  "orig.stat.df2",
  "orig.stat.estimate1",
  "orig.stat.estimate2",
  "orig.stat.n1",
  "orig.stat.n2",
  "orig.stat.p.value"
)
write_snapshot(
  cbind(
    t5[, t5_cols],
    effects[ke, c("ESCI.r", "testInfo.statistic")],
    key[kk, key_cols]
  ),
  "mltwo1026.csv"
)

# ---- SCORE: the version-of-record replications with their originals ---------
repli <- read("score_repli")
orig <- read("score_orig")
papers <- read("score_papers")
record <- repli[toupper(repli$repli_version_of_record) == "TRUE", ]
stopifnot(nrow(record) == 309, !anyDuplicated(record$claim_id))
ko <- match(record$claim_id, orig$claim_id)
kp <- match(record$paper_id, papers$paper_id)
stopifnot(!anyNA(ko), !anyNA(kp))
# Where the record pools several labs, how many of them are generalizability
# tests (a deliberately different population): the one column not copied
# as is, counted from the labs' own rows
lab <- toupper(repli$repli_version_of_record) != "TRUE" &
  repli$manylabs_type %in% c("count", "instance_primary") &
  toupper(repli$repli_is_generalizability) == "TRUE"
record$generalizability_labs <- ifelse(
  record$manylabs_type %in% "aggregation",
  as.integer(table(factor(repli$claim_id[lab], levels = record$claim_id))),
  0L
)
stopifnot(sum(record$generalizability_labs > 0) == 7)
repli_cols <- c(
  "paper_id",
  "claim_id",
  "report_id",
  "repli_type",
  "repli_is_generalizability",
  "manylabs_type",
  "repli_conv_r",
  "repli_effect_direction",
  "repli_p_value",
  "repli_sample_size_value",
  "repli_score_criteria_met",
  "generalizability_labs"
)
orig_cols <- c(
  "is_covid",
  "orig_conv_r",
  "orig_sample_size_value",
  "orig_sample_size_units",
  "orig_stat_type",
  "orig_stat_dof_1",
  "orig_stat_dof_2",
  "orig_stat_value",
  "orig_p_value_type",
  "orig_p_value",
  "orig_p_value_tails"
)
write_snapshot(
  cbind(
    record[, repli_cols],
    orig[ko, orig_cols],
    doi = papers$DOI[kp]
  ),
  "score1026.csv"
)
