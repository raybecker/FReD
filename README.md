# FReD

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/forrtproject/FReD/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/forrtproject/FReD/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of the FReD package is to provide various interfaces to the
FORRT Replication Database. The package includes two shiny apps - the
FReD Explorer and the FReD Annotator - as well as functions to directly
access and analyse the dataset.

## FReD Explorer

The FReD Explorer allows users to explore the dataset, and to assess replicability
and effect sizes of the included studies based on various filters. It primarily
serves as a gateway to meta-science. You can access the FReD Explorer [online](https://forrt-replications.shinyapps.io/fred_explorer/) or
by calling `run_explorer()` in your R console after you have loaded the package.

## FReD Annotator

The FReD Annotator allows users to annotate lists of articles (e.g. reading lists
for classes, or reference lists for draft articles) with any replication findings.
This is intended to make it easier to use replications in teaching and research,
and to thus ensure that claims are based on the best available evidence.
You can access the FReD Annotator [online](https://forrt-replications.shinyapps.io/fred_annotator/) or by calling `run_annotator()` in your
R console after you have loaded the package.

## Installation

You can install the development version of FReD like so:

``` r
devtools::install_github("forrtproject/FReD")
```

## Launching the apps

``` r
library(FReD)
run_explorer()
run_annotator()
```

## Accessing the data

To load the dataset and prepare it for analyses, you can use the following code:

``` r
library(FReD)
load_fred_data()
```

### Changelog

-   Merge red, forrt, as once - and remove "excluded" (so far forrt
    excluded were kept)
-   Replaced the significant\_... columns with p\_ columns (so that
    analyses are more flexible downstream)
-   Renamed power to power_r (in case we want to expand in the future)

### Discuss

-   Decide which other es-aliases to include, and which to clean up.
    Currently the following fail:
    -   b beta cohen's d cohen's g cramer’s v / cramer's v dz etaq ges
        hazards ratio hedge's g / hedges'g odds ratio (study 3) / odds
        ratio percentage phi / φ q regression coefficient smd squared
        semi partial correlation (sr2) w γ
    -   [ ] Report incomparable effect sizes
-   Discuss whether to use r-to-z transformation throughout (current
    take: more valid, but extra hassle?)
-   Should duplicate live in exclusion column?
-   Why should power be calculated based on one-tailed test ("greater")?
    It seems that usually (including here) two-tailed tests would be
    used to judge replication success (even if wrongly?) Changed to
    two-tailed for now.
-   Should z-curve just be based on p-values derived from r? If not, is
    the z-transformation correct? I get closer p-values by working from
    the r-to-z transformation (now default implementation in
    `augment_for_zcurve()`
-   (What is the difference between correlates and moderators?)

### Review

-   clean_variables - should any of that be done in the dataset / is all
    appropriate to merged dataset?

-   Are very small n_replication correct? \~30 entries \< 10. For now,
    set power = 0 where N \<4 - should it be NA?

-   consistent =
    `is.na(red$significant_original) | is.na(red$significant_replication)     ~ "OS n.s."` -
    really??

-   Change `result` so that success is defined as both significant and
    in right direction - that feel like thoughtless rather than serious
    outcome definition?

**ToDo**

-   Rename all variables to x_o & x_r

### Further automation

-   Update OSF citation? (Maybe update citation with dataset update, and
    then read from OSF rather than recreate?)

Todos

-   [ ] filter out exclusion

-   [ ] Consider that non-coded FORRT entries might be duplicated (at
    least based on DOIs)

-   [ ] as - Study listed != 1 // Type == Individual studies

-   [ ] Remove csv download
