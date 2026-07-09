# An even shorter version of Altmejd et al.'s replication database

A subset of Altmejd et al.'s (2018) replication database. Note that this
dataset is only used internally, to build the full database (see
[`altmejd`](https://sbissantz.github.io/adatutor/reference/altmejd.md)).
It includes replication projects of social science lab experiments for
just one of the five large-scale replication projects: the Social
Science Replication Project (SSRP).

## Usage

``` r
ssrp723
```

## Format

A data frame with 21 study effects (i.e., rows); 2 identifiers, and 5
predictors (i.e., 7 columns):

- eid:

  Effect identifier

- pid:

  Project identifier: Social Science Replication Project (SSRP)

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
