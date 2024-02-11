# Welcome to FReD

FReD is the FORRT Replication Database, aggregating replication results across the social and behavioural sciences. This repository contains the code for the *FReD Replication Explorer*, a Shiny App that allows you to browse, search, visualise and summarise the results.

For more information on FReD, you can visit the [FORRT website](https://forrt.org/replication-hub/) or the [FReD OSF](https://osf.io/9r62x/) project.

To launch the app on your computer, you can run the following code in R:

```r
if (!require(shiny)) install.packages("shiny")
shiny::runGitHub('fred_explorer', 'forrtproject')
```

Alternatively, you can access the current online version of the app [here](https://metaanalyses.shinyapps.io/replicationdatabase/). Either version will automatically pull the latest dataset from the OSF, so that you can always build on our most extensive evidence base.
