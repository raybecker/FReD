---
editor_options: 
  markdown: 
    wrap: 72
---

# FReD

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of the FReD package is to provide various interfaces to the
FORRT Replication Database.

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

### Changelog

-   Merge red, forrt, as once - and remove "excluded" (so far forrt
    excluded were kept)
-   Replaced the significant\_... columns with p\_ columns (so that
    analyses are more flexible downstream)
-   Renamed power to power_replication (in case we want to expand in the
    future)

### Discuss

-   Decide which other es-aliases to include, and which to clean up.
    Currently the following fail:
    -   b beta cohen's d cohen's g cramer’s v / cramer's v dz etaq ges
        hazards ratio hedge's g / hedges'g odds ratio (study 3) / odds
        ratio percentage phi / φ q regression coefficient smd squared
        semi partial correlation (sr2) w γ
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
    `augment_for_zcurve`
-   (What is the difference between correlates and moderators?)

### Review

-   clean_variables - should any of that be done in the dataset / is all
    appropriate to merged dataset?

-   Are very small n_replication correct? \~30 entries \< 10. For now,
    set power = 0 where N \<4 - should it be NA?

-   consistent =
    is.na(red$significant_original) | is.na(red$significant_replication)
    \~ "OS n.s." - really??

-   Change `result` so that success is defined as both significant and
    in right direction - that feel like thoughtless rather than serious
    outcome definition?

### Further automation

-   Update OSF citation? (Maybe update citation with dataset update, and
    then read from OSF rather than recreate?)
