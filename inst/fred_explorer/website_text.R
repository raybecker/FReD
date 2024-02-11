
# Format contributor names [old ReD version, used until 05.02.2024 dmy]
# contributors <- openxlsx::read.xlsx(red_link, sheet = "Contributors")
# contributors$first <- substr(contributors$contributor_firstname, 1, 1)
# contributors$apa <- paste(contributors$contributor_lastname, ", ", contributors$first, ".", sep = "")
# contributors$name <- paste(contributors$contributor_lastname, ", ", contributors$first, ".", sep = "")
# c_names <- paste(contributors$apa, collapse = ", ")

# Format contributor names [FReD version, used as of 06.02.2024 dmy]
contributors <- openxlsx::read.xlsx(data_file, sheet = "Contributors FReD")
contributors <- contributors[contributors$Added.to.FReD.website.as.contributor, ]
contributors$first <- substr(contributors$First.name, 1, 1)
contributors$middle <- ifelse(!is.na(contributors$Middle.name), paste(" ", substr(contributors$Middle.name, 1, 1), ".", sep = ""), "")
contributors$apa <- paste(contributors$Surname, ", "
                          , contributors$first, "."
                          , contributors$middle
                          , sep = "")
contributors$name <- paste(contributors$contributor_lastname, ", ", contributors$first, ".", sep = "")
c_names <- paste(contributors$apa, collapse = ", ")


# Contributions
about <- HTML(paste("<h4><b>FORRT Replication Database ", version, "</b></h3>"
                    , "<br/><b>Last Update:</b> ", date

                    ### Core team
                    , "<br/><b>Citation: </b>"
                    , c_names
                    , " (2024). <i>FReD: FORRT Replication Database, ", version, "</i>. <a href=https://dx.doi.org/10.17605/OSF.IO/9r62x>https://dx.doi.org/10.17605/OSF.IO/9r62x</a> *shared first authorship"
                    , "<br/><b>Data and Materials:</b> <a href=https://osf.io/9r62x/>https://osf.io/9r62x/</a>"
                    , "<br/><b>Contribute:</b> Please send an e-mail to lukas.roeseler(at)uni-muenster.de"
                    , "<br/><b>License:</b> CC-By Attribution 4.0 International"
                    , "<br/><b>Acknowledgements:</b> We thank all researchers who have invested resources in conducting replication research, researchers who have submitted their replication studies, and researchers who used the Replication Recipe Post-Completion template to register their results. FORRT Replication Database is supported through the University of Bamberg's Interne Forschungsförderung, by the University of Münster, by the Nederlandse Organisatie voor Wetenschappelijk's (NWO) Open Science Fund, and by the Leuphana University Lüneburg."
                    , "<br/><b>Important note:</b> This is work in progress. Please beware that there might be bugs or errors in the dataset."
                    , sep = ""))

info <- HTML(paste("<h3>Welcome to the FORRT Replication Database!"
                   , "<h4><br/><br/><b>What is ReD?</b><br/> <i>Science appears to be the human enterprise that is most systematic in its attempt to eliminate error in the search for knowledge </i>(Hoyningen-Huene, 2013, p. 89).
                   </br></br>Still, if - or how well - most of our findings replicate, is unknown. The FORRT Replication Database is a crowdsourced effort to include unpublished and published replication results to estimate and track the replicability along various fields and provide researchers with a way to assess replicability of crucial studies in a quick and transparent way. Check out the <a href=https://osf.io/f3w26>Call for Results</a> if you would like to contribute."
                   , "<br/><br/><b>What are your benefits of joining us?</b><br/> You are very welcome to contribute data from your replication studies! In return, (apart from being rewarded by the good feeling of helping research on replicability to improve) we will list you as a co-author of the ReD (CRediT: Resources). Please use the <a href=https://www.soscisurvey.de/replicate>submission portal</a> to submit replication results.
                        <br/><br/>Unpublished datasets as well as data from classroom experiments are also highly appreciated. Get in touch if you have any questions about the submission portal."
                   , "<br/><br/><b>How to use this website</b><br/> Here in our ShinyApp, you can explore replicability for all or filtered entries. Click on the other tabs and filter your results on the left side.
                        <br/><br/>In the <a href=https://osf.io/9r62x/>OSF project</a>, you can find further information and files on our project. There, you also can send us a contribution request to the project.
                        <br/><br/>For questions or comments, please check out the FAQs on this website, or send an e-mail to lukas.roeseler(at)uni-muenster.de"
                   , "<br/><br/>"
                   , sep = ""))

dataset_explanation <- shiny::HTML(paste("<h4><b>ReD Dataset</b>"
                                         , "<h5><br/>This is the entire FORRT Replication Database Datset. It currently contains "
                                         , nrow(red)
                                         , " findings "
                                         , "<br/><br/>"
                                         , sep = ""))

dataset_headline <- HTML(paste("<h4><b>Dataset</b><h6>"
                               , sep = ""))

variables_headline <- HTML(paste("<h4><b>Variables</b><h5>"
                                 , sep = ""))

dataset_info <- HTML(paste("<h4><b>Replication Rate</b>"
                           , "<h5><br/>There are currently "
                           , nrow(red)
                           , " replication findings entered into the database. Of these, "
                           , length(unique(red$ref_replication))
                           , " replication findings are independent (i.e., use different samples/stem from different studies). Note that the following analyses treat all studies as independent. Apart from the table and bar chart, only studies for which sample sizes and effect sizes are available (for original study and replication) are considered here. The other can be viewed in the Dataset."
                           , " In total, "
                           , length(unique(red$ref_original))
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

forest_info <- HTML(paste("<h4><b>Study Overview</b>"
                          , "<h5><br/>The currently included "
                          , nrow(red)
                          , " replication findings entered into the database can be allocated to "
                          , length(unique(red$ref_original))
                          , " independent original studies. This is an overview of these studies."
                          , "<br/><br/><h6>"
                          , sep = ""))

packages_info <- HTML(paste("<br/><br/><br/><h4><b>R-packages used for this App</b><h5>"
                            , sep = ""))

packages_list <- HTML(paste("<br/><br/>- ", names(sessionInfo()[["otherPkgs"]]),  sep = ""))

scatterplot_title <- HTML(paste("
                                 <h4><br/><br/><b>Scatterplot of Original and Replication Effect Sizes</b><br/>"
                                , "<br/><br/>"
                                , sep = ""))

scatterplot_explanation <- HTML(paste("
                                 <h5><i>Note. </i>This plot is based on the code used for the main plot of Open Science Collaboration (2015). Here you can see for each replication study the original effect and the replication effect. Significant replication effects (p < .05) are highlighted in blue. If all studies were perfectly replicable, the dots would be on the solid grey line. If no study was replicable, the dots would be at the dashed line (= null effects). Hover over the plot to see the exact effect sizes and the study. Clicking on rows in the table above this plot will highlight the eslected studies. If there are registered replication reports (RRRs) among the selected study, you will see 'columns' of effect sizes because all studies from a RRR have the same 'original effect size' but replication effect sizes vary."
                                      , "<br/><br/>"
                                      , sep = ""))

barplot2_title <- HTML(paste("
                                 <h4><br/><br/><b>A More Nuanced Interpretation of Replication Effects</b><br/>"
                             , "<br/><br/>"
                             , sep = ""))

barplot2_explanation <- HTML(paste("
                                 <h5><i>Note. </i>Lebel et al. (2018) have suggested a more nuanced interpretation of replication results for cases where the original study found an effect. Whether or not the replication effect is significant, too, is indicated by signal/no-signal. Whether the replication effect is smaller, larger, or the same size is also indicated. We also included cases where the original study was not significant or no information about the original study's significance was available in grey."
                                   , "<br/><br/>"
                                   , sep = ""))

zcurve_title <- HTML(paste("
                                 <h4><br/><br/><b>Z-Curve Analysis (via ReD)</b><br/> "
                           , "<br/><br/>"
                           , sep = ""))

zcurve_explanation <- HTML(paste("
                                 <h5><i>Note. </i>Z-curve (Bartos & Schimmack, 2020) can be used to estimate replicability of a set of studies. Observed discovery rate refers to the proportion of significant (p < .05) studies. Expected discovery rate is the proportion of studies that you would expect to be significant if you ran perfect and high powered replications of <i>all</i> studies. Expected replicability rate is the proportion of studies that you would expect to be significant if you ran perfect and high powered replications of <i>all significant</i> studies. You can compare the discovery and replicability rates with the actual replicability presented at the top of this page. We recommend running bootstraps to get confidence intervals for z-curve's estimates but refrain from doing so as it takes much time."
                                 , "<br/><br/>"
                                 , sep = ""))



correlates_info <- HTML(paste("
                                 <h4><br/><br/><b>Correlates of Replicability</b><br/> "
                              , "<h4>Has replicability increased over time? How do I know if a published finding is replicable? Are there differences betweeen research fields? Using the dataset, we can test what moderators are correlated with certain replication outcomes.</br></br><b><font color=\"#ff0000\">Please keep in mind that these analyses are preliminary for at least two reasons: They include non-validated data and some variables may not have been coded yet for a large proportion of the dataset</font></b>"
                              , sep = ""))

moderators_info <- HTML(paste("
                                 <h4><br/><br/><b>Moderators of Replication Effect Sizes</b><br/> "
                              , "<h4>Here you can explore the influence of several moderators on the replication effect size. </br></br><b><font color=\"#ff0000\">Please keep in mind that these analyses are preliminary for at least two reasons: They include non-validated data and some variables may not have been coded yet for a large proportion of the dataset.<br/><br/></b></font><h5>"
                              , sep = ""))


correlates_decade <- HTML(paste("
                                 <h4><br/><br/><b>Replicability over time</b><br/> "
                                , "For the following analysis, replication findings have been aggregated for each <i>decade</i> of the original finding's publication year."
                                , sep = ""))

correlates_journal <- HTML(paste("
                                 <h4><br/><br/><b>Replicability by Journal</b><br/> "
                                 , "For the following analysis, replication findings have been aggregated for each <i>journal where the original finding was published</i>. This can serve as a shortcut to comparing replicability by research area. Note that mixed findings can be a mix of many succesful and one inconclusive or failed replication or vice versa."
                                 , sep = ""))



rc_info  <- HTML(paste("
                                 <h4><br/><br/><b>References-Checker</b><br/> "
                       , "<h4>Paste your entire lists of references or DOIs here. In order to identify replication studies, there need to be DOIs. Please note that not all studies entered in ReD feature a DOI or that some papers may even have no or more than one DOI. Finally, ReD does not contain <i>all</i> replications. That means, if there are no replications listed in ReD, this does not mean that nobody has ever attempted to replicate the entered studies.<h6>"
                       , sep = ""))


checker_info <- HTML(paste("<h4><br/><br/><b>Replicability Checker</b><br/> "
                           , "<br/><br/><h4>Filter the database via the table's search function and it will return a summary of the state of research regarding the searched entries at the bottom of the page."
                           , "<h6>"
                           , sep = ""))

references_headline <- HTML(paste("<h4><br/><br/><b>References</b><br/> "
                                  , "<br/><br/><h4>Currently, a large proportion of the replication studies stems from the CurateScience database. We added data from CORE, RPP, the OSF Registries, and individual submissions. A synthesis of ReD with FORRT's replications and reversals is coming soon. If you are aware of replications not listed here, please write us an e-mail or add them here: https://docs.google.com/spreadsheets/d/1x68oW2H_Xrdv44fIeycl4fegsmQgCa60GxeZZ_hAR90/edit?pli=1#gid=1463805480"
                                  , sep = ""))



references_list <- HTML(paste(
  "<br/><br/>- Curate Science Database: https://web.archive.org/web/20220128104303mp_/https://curatescience.org/app/replications"
  , "FORRT Replications and Reversals: https://docs.google.com/spreadsheets/d/1IY56xdDmoU9VasdE0sry2xvsGFkWPrqbKXAs1Rw-9LA/edit#gid=0"
  , "Bartoš, F., & Schimmack, U. (2020). Z-Curve.2.0: Estimating Replication Rates and Discovery Rates. Advance online publication. https://doi.org/10.31234/osf.io/urgtn"
  , "Hoyningen-Huene, P. (2013). Systematicity: The nature of science. Oxford studies in philosophy of science. Oxford Univ. Press."
  , "LeBel, E. P., McCarthy, R. J., Earp, B. D., Elson, M., & Vanpaemel, W. (2018). A unified framework to quantify the credibility of scientific findings. Advances in Methods and Practices in Psychological Science, 1(3), 389-402."
  , "Open Science Collaboration (2015). Psychology: Estimating the reproducibility of psychological science. Science (New York, N.Y.), 349(6251), aac4716. https://doi.org/10.1126/science.aac4716"
  ,  sep = "<br/><br/>- "))


references_redpublications <- HTML(paste("<br/><br/><br/><h4><b>Publications Using ReD</b><h5>"
                                         , sep = ""))

references_list_redpublications <- HTML(paste(
  "Röseler, L. (2023). Predicting Replication Rates with Z-Curve: A Brief Exploratory Validation Study Using the FORRT Replication Database. Retrieved from https://osf.io/t7nwk"
  ,  sep = "<br/><br/>- "))


packages_headline <- HTML(paste("<br/><br/><br/><h4><b>R-packages used for this App</b><h5>"
                                , sep = ""))

packages_list <- HTML(paste("<br/><br/>- ", names(sessionInfo()[["otherPkgs"]]),  sep = ""))


faqs <- HTML(paste("<h3>Frequently Asked Questions"
                   , "<h6><i>Hint: Use Ctrl+F to search the FAQs.</i>"
                   , "<h4><br/><br/><b>Q: Can I submit studies that I did not conduct myself?</b><br/>
                   A: Yes, you can! Entering other researchers' replication result will make you eligible for co-authorship on the App."
                   , "<h4><br/><br/><b>Q: Does the replication study that I want to submit need to be peer-reviewed?</b><br/>
                   A: No! Publishing replication studies can be met with quite some resistence, in our experience. Therefore, we want to keep the inclusion threshold as low as possible."
                   , "<h4><br/><br/><b>Q: In what way does the study need to be public or published?</b><br/>
                   A: There needs to be a way to verify that an entered study has indeed been executed."
                   , "<h4><br/><br/><b>Q: I know about replication studies that you have not added yet but I do not have time to add them myself. Is there a way to have you note these studies anyway?</b><br/>
                   A: Yes, please add them to the list in our <a href=https://docs.google.com/spreadsheets/d/1x68oW2H_Xrdv44fIeycl4fegsmQgCa60GxeZZ_hAR90/edit?pli=1#gid=305460056>ReD-spreadsheet</a>! "
                   , "<h4><br/><br/><b>Q: What about large-scale replication projects such as Many Labs 2, those by Camerer et al., etc.?</b><br/>
                   A: These are absolutely on our radar and we will add them as soon as possible. Please note that we are currently working with strongly limited resources. Get in touch if you want to support the project!"
                   , "<h4><br/><br/><b>Q: Somebody entered my replication study. I want to become a contributor, is this still possible?</b><br/>
                   A: The basis of the replication database are other databases that have existed for years, so this is the case for many studies. Still, there are houndreds of replications that are still missing. Contact us if you want to contribute replication study results!"
                   , "<h4><br/><br/><b>Q: How do you make sure that nobody creates mock entries?</b><br/>
                   A: Each entry will be validated. To do this, there needs to be a published paper, a pre-print, an OSF-project, or some kind of findable object that includes the results that were entered."
                   , "<h4><br/><br/><b>Q: What is the definition of a replication study?</b><br/>
                   A: This one is tough: In a nutshell, current definitions allow calling almost all studies replications. With respect to the dataset: For a study to be considered a replication, the hypothesis that is being investigated needs to have been investigated in another study in a way that is as close as possible to your way. If you are unsure about whether there is sufficient overlap between an original study and the replication study, please let us know in the notes or get in touch with us."
                   , "<h4><br/><br/><b>Q: Is the database representative?</b><br/>
                   A: No. We strive to include every replication of a social scientific study in the database. If we succeed, the database will be comprehensive but researchers do not select target studies for replication on the basis of representativeness - they rather choose central and not yet replicated findings."
                   , "<h4><br/><br/><b>Q: How do I know the replication study entered here did not succeed due to methodological shortcomings?</b><br/>
                   A: You don't. We encourage you to check out individual entries and their methodologies."
                   , "<h4><br/><br/><b>Q: What do I need to do to become a contributor on the website?</b><br/>
                   A: You need to enter results from at least one replication finding into the submission form or the spreadsheet, enter your e-mail so that we can contact you, and be available for potential questions during our validation of your entry. Sending us a reference of a replication of yours does not suffice."
                   , "<h4><br/><br/><b>Q: Do failed replication studies mean that original findings are untrustworthy?</b><br/>
                   A: No. Besides scientific misconduct or questionable research practices, replication attempts can fail due to practical reasons, due to unknown background factors, due to changes in (the perception of) concepts, and many more things."
                   , "<h4><br/><br/><b>Q: Can I take the dataset and use it for my research?</b><br/>
                   A: The FORRT Replication Database is open and shared under a CC-By Attribution 4.0 International license. Please cite us (see About-tab) if you use these resources. Reach out to us if you want to code a moderator and maybe we can join forces (some moderators have already been coded for parts of the data). Also, we are happy about feedback or knowing that other people can make use of this project!"
                   , "<h4><br/><br/><b>Q: How is this project related to FORRT's replications and reversals? Should I enter my study in both places?</b><br/>
                   A: This project is independent of FORRT's replications and reversals. Whereas FORRT focuses on research topics, we focus on individual replications. ReD is actively collaborating with FORRT and we plan to merge both databases, thus you do not need to enter your results in both places. However, we do not know yet when a synthesis will be possible, so we encourage you to join both projects."
                   , "<h4><br/><br/><b>Q: I want to enter a replication study that has an average effect size but also item-wise effects. Which should I enter in the FORRT Replication Database?</b><br/>
                   A: Both ways are possible. If it is possible for you, please enter the effect sizes as differentiated as possible. This will facilitate future analyses and better allow researchers, to determine how to successfully replicate prior research. If entering the results on an item-by-item basis is not possible for some reason, we are still very happy about you entering an aggregated effect size. In this case, please write into the note-variable, that more fine-graind results are available."
                   , "<h4><br/><br/><b>Q: What is the data strucure? Can you account for dependent studies? Can there be multiple results for a single study?</b><br/>
                   A: Yes, we do account for multilevel structure / dependent effect sizes. Check out the Figure below for an in-depth explanation.<br/>"
                   , img(src = "datastructure.png", height = 750, width = 750)

                   # ### Q&A Template
                   # , "<h4><br/><br/><b>Q: XXX?</b><br/>
                   # A: XXX"

                   , "<h5><br/><br/><i>Still confused? Send us an e-mail (see the About-tab for contact info)!</i>"
                   , sep = ""))



breaks <- HTML(paste("<br/><br/>",  sep = ""))
