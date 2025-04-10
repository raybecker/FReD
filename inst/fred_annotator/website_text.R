### ANNOTATOR ###

about_page <- glue::glue("

## FORRT Replication Database Annotator {packageVersion('FReD')}

**Last Code Update:** {get_last_modified('fred_annotator')}

**Data citation:** {create_citation()}

**Data and Materials:** <a href='https://osf.io/9r62x/' target='_blank'>https://osf.io/9r62x/</a>

**Contribute:** Please send an e-mail to lukas.roeseler(at)uni-muenster.de

**License:** _Data:_ CC-By Attribution 4.0 International, _Code:_ MIT License

**Acknowledgements:** We thank all researchers who have invested resources in conducting replication research, researchers who have submitted their replication studies, and researchers who used the Replication Recipe Post-Completion template to register their results. FORRT Replication Database is supported through the University of Bamberg's Interne Forschungsförderung, by the University of Münster, by the Nederlandse Organisatie voor Wetenschappelijk's (NWO) Open Science Fund, and by the Leuphana University Lüneburg.

**Important note:** This is work in progress. Please beware that there might be bugs or errors in the dataset. If you spot any, please let us know <a href='mailto:lukas.roeseler@uni-muenster.de' target='_blank'>by email</a>, or on <a href='https://github.com/forrtproject/FReD/issues' target='_blank'>GitHub</a>.

_Thanks to our funders:_
<img src='ub.png' height='100' /> <img src='um.png' height='50' /> <img src='nwo.png' height='100' />

")


welcome_title <- HTML("Welcome to the FORRT Replication Database!")
welcome_text <- HTML("<br /><p style=\'color:black;\'>The FReD is a collection of crowdsourced findings from replication studies (i.e., studies that investigated one or more previously tested hypotheses using new data). The aims of this project are:
</br>(1) to document replication attempts across many areas of science and make replications findable and
</br>(2) to provide meta-scientists with a database for research on replicability.
</br></br>This is the FReD <b>Annotator</b>. The annotator reads references (DOIs) from a list and returns replication studies for these references based on the FReD entries. You can go to the online version of the FReD <b>Explorer</b> <a target='_blank' href=https://forrt-replications.shinyapps.io/fred_explorer/>here</a>.
</br></br>We advise researchers to carefully investigate replication findings before making judgments about the robustness of research findings. Note that many entries are not yet validated - if you care more about accuracy than coverage, please select the option to only use validated entries.
                     </br></br>You can find more information on the <a target='_blank' href=https://forrt.org/replication-hub>FORRT Replication Hub</a>. If you would like to contribute replication findings, please check out our <a target='_blank' href=https://osf.io/f3w26>call for results</a> or send us an e-mail.</p><br/>")


dataset_info <- HTML(paste("<h4><b>Replication Rate</b>"
                           , "<h5><br/>There are currently "
                           , nrow(df)
                           , " replication findings entered into the database. Of these, "
                           , length(unique(df$ref_replication))
                           , " replication findings are independent (i.e., use different samples/stem from different studies). Note that the following analyses treat all studies as independent. Apart from the table and bar chart, only studies for which sample sizes and effect sizes are available (for original study and replication) are considered here. The others can be viewed in the Dataset."
                           , " In total, "
                           , length(unique(df$ref_original))
                           , " different original studies have been replicated."
                           # , "According to the original researchers' assessments, there have been "
                           # , sum(red$pc05 == "informative failure to replicate", na.rm = TRUE)
                           # , " informative failures to replicate and "
                           # , sum(red$pc05 == "success", na.rm = TRUE)
                           # , " successes. "
                           # , sum(red$pc05 == "inconclusive", na.rm = TRUE)
                           # , " replications have yielded inconclusive results and "
                           # , sum(red$pc05 == "practical failure to replicate", na.rm = TRUE)
                           # , " were practical failures to replicate."
                           , "<br/><br/><h6>"
                           , sep = ""))

packages_info <- HTML(paste("<br/><br/><br/><h4><b>R-packages used for this App</b><h5>"
                            , sep = ""))

packages_list <- HTML(paste("<br/><br/>- ", names(sessionInfo()[["otherPkgs"]]),  sep = ""))

rc_info  <- HTML(paste("
                                 <h4><br/><br/><b>References-Checker</b><br/> "
                       , "<h4>Paste your entire lists of references or DOIs here. In order to identify replication studies, there need to be DOIs. Please note that not all studies entered in ReD feature a DOI or that some papers may even have no or more than one DOI. Finally, ReD does not contain <i>all</i> replications. That means, if there are no replications listed in ReD, this does not mean that nobody has ever attempted to replicate the entered studies.<h6>"
                       , sep = ""))



packages_headline <- HTML(paste("<br/><br/><br/><h4><b>R-packages used for the FReD Apps</b></h4> Note that many will only be used for the FReD Explorer that presents meta-analytic summaries."
                                , sep = ""))

packages_list <- HTML(paste("<br/><br/>- ", names(sessionInfo()[["otherPkgs"]]),  sep = ""))

breaks <- HTML(paste("<br/><br/>",  sep = ""))


scatterplot_explanation <- HTML(paste("
                                 <h5><i>Note. </i>This plot is based on the code used for the main plot of Open Science Collaboration (2015). Here you can see for each replication study the original effect and the replication effect. Significant replication effects (p < .05) are highlighted in blue. If all studies were perfectly replicable, the dots would be on the solid grey line. If no study was replicable, the dots would be at the dashed line (= null effects). Hover over the plot to see the exact effect sizes and the study. If there are registered replication reports (RRRs) among the selected study, you will see 'columns' of effect sizes because all studies from a RRR have the same 'original effect size' but replication effect sizes vary."
                                      , "<br/><br/>"
                                      , sep = ""))
