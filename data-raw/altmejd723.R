#####################
# Original Data Set #
#####################
#
# The data come from https://osf.io/7ad56.
#

# Load the CSV from ./inst/extdata/
csv_file <- system.file(
  "extdata",
  "altmejd723.csv",
  package = "exmachina"
)

# Get the headers
headers <- scan(csv_file, what = character(), nlines = 1, sep = ",")

# Read the data set with the given headers
input <- read.csv(csv_file, na.strings = "", col.names = headers)

# Use only the first replication of the effect
extdata <- input[!duplicated(input$id),]

# Specify relevant variables
relevant_vars <- c(

  # Effect ID
  "id",

  # Project ID
  "project",

  # Max Author Citations (O): Nr of citation of the author in the original study
  # with the highest citation count
  # "author_citations_max.o",

  # Max Author Citations (R): Nr of citation of the author in the original study
  # with the highest citation count
  # "author_citations_max.r",

  # Ratio of male authors (O): Ratio of male authors in original study
  # "authors_male.o",

  # Ratio of male authors (R): Ratio of male authors in replication
  # "authors_male.r",

  # Citations (O): Number of citations of original paper
  # "citations",

  # Compensation (O): Compensation in original experiment. One of the following:
  # nothing, cash, credit, mixed
  # "compensation.o",

  # Compensation (R): Compensation in replication.  One of the following:
  # nothing, cash, credit, mixed
  # "compensation.r",

  # Discipline: Discipline of original paper.  One of the following: social,
  # cognitive, economics
  # "discipline",

  # Effect Size (O); Standardized effect size of original paper
  "effect_size.o",

  # Effect Size (R): Standardized effect size of replication
  # "effect_size.r",

  # Effect Type: Type of effect tested. One of the following:
  # main effect, correlation, interaction
  # "effect_type",

  # ES at 80% power: Standardized effect size required in replication to achieve
  # 80% power
  # "es_80power",

  # Journal (O): Journal where the original paper was published
  # "journal",

  # Paper Length (O): Number of pages of original paper
  # "length",

  # Number of Authors (O): Number of authors in original study
  # "n_authors.o",

  # Number of Authors (R): Number of authors in replication
  # "n_authors.r",

  # Sample Size (O): Sample size of original paper
  "n.o",

  # Planned Sample Size (R): Planned sample size of replication
  # "n_planned.r",

  # Replication Project: The replication project that the study was in. One of
  # either: EE, RPP, ML1, ML3
  # "project",

  # Publication year (O): The publication year of the original study
  # "pub_year",
  #  Author note: "Last, while the publication year of the original article is
  #  an ex ante relevant correlate of reproducibility, we have chosen to remove
  #  it from the training data since it also captures a feature of the paper
  #  selection process used in the ML projects. The authors wanted a benchmark
  #  for comparison, and included a number of papers that had been successfully
  #  replicated many times before. These studies are all older, making the
  #  publication  year  variable a proxy for papers with (already known) high
  #  replication rates."

  # P-Value (O): P-value of original paper
  "p_value.o",

  # P-Value (R): P-value of replication
  # "p_value.r",

  # Post-Hoc Power (O): Post-hoc power based on original effect size
  "power.o",

  # Planned Power (R): Planned power of replication based on planned N and
  # original ES
  # "power_planned.r",
  # Author note: "[F]or cases when the planned sample size has not been
  # using actual replication sample size as a proxy."

  # Replicated: Binary outcome variable, study is replicated if p < =0.05 and
  # effect goes in the same direction as the original
  "replicated" #outcome

  # Relative Effect Size; The continuous outcome variable; the standardized
  # replication effect size relative to the original effect
  # "relative_es",

  # Same Country: Original study and replication are in the same country
  # "same_country",

  # Same Language: Original study and replication are in the same language
  # "same_language",

  # Same Online: Original study and replication are both conducted online
  # "same_online",

  # Same Subjects: Original study and replication use same type of subjects
  # "same_subjects",

  # Highest Author Seniority (O): Most senior author in original paper
  # (Professor, Associate Prof., Assistant Prof., Researcher)
  # "seniority.o",

  # Highest Author Seniority (R): Most senior author in original paper
  # (Professor, Associate Prof., Assistant Prof., Researcher)
  # "seniority.r",

  # US Lab (O): Original experiment lab in the US
  # "us_lab.o",

  # US Lab (R): Replication experiment lab in the US
  # "us_lab.r")
)

# Select only valid cases
valid_cases <- extdata$drop == FALSE & !is.na(extdata$replicated)

# Keep only valid cases and relevant variables
altmejd723 <- extdata[valid_cases, relevant_vars]

# Add log p-value as variable
altmejd723$log_p.o <- log(altmejd723$p_value.o)

# Switch from "id" to "eid" - "effect id"
eid_pat <- grep("id", colnames(altmejd723) )
names(altmejd723)[eid_pat] <- "eid"

# Add pid - "project id"
pid_pat <- grep("project", colnames(altmejd723) )
names(altmejd723)[pid_pat] <- "pid"

# Save the data set as R data
usethis::use_data(altmejd723, overwrite = TRUE)

