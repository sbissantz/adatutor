# Short version of Altmejd et al.'s replication database

An adapted version of the replication database by Altmejd et al. (2018).
It includes replication projects of social science lab experiments for
five large-scale replication projects: Many Labs 1 and 3, the
Replication Project Psychology, the Experimental Economics Replication
Project, and the Social Science Replication Project.

## Usage

``` r
altmejd
```

## Format

A data frame with 152 study effects (i.e., rows) rows; 2 identifiers,
and 5 predictors (i.e., 7 columns):

- eid:

  Effect identifier

- pid:

  Project identifier: Many Labs 1 (ML1), Many Labs 3 (ML3), Replication
  Project Psychology (RPP), Experimental Economics Replication Project
  (EERP), Social Science Replication Project (SSRP)

- effect_size.o:

  Standardized effect size of the original study

- n.o:

  Number of observations in the original study

- p_value.o:

  P value of the original study

- power.o:

  Power of the original study

- replicate:

  Replication success indicator

## Source

Altmejd A, Dreber A, Forsell E, Huber J, Imai T, et al. (2019)
Predicting the replicability of social science lab experiments. PLOS ONE
14(12): e0225826. <https://doi.org/10.1371/journal.pone.0225826>

The data and additional information on the data set can be downloaded
from their OSF repository. <https://osf.io/4fn73/>
