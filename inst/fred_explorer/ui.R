# Input choices
large_scale_project_choices <- list(
  "All studies" = "All studies",
  "Individual submissions" = "Individual submissions",
  "FORRT Replications and Reversals" = "FORRT",
  "CORE (Feldman JDM Replications)" = "CORE",
  "CRSP special issue",
  "Data Replicada" = "datareplicada",
  "Many Labs 1" = "ML1",
  "Many Labs 2" = "ML2",
  "Many Labs 3" = "ML3",
  "OpAQ (Anchoring Effects)" = "OpAQ",
  "OpenMKT.org Replications Catalog" = "openmkt",
  "OSF Registries" = "OSF Registries",
  "Reproducibility Project Psychology (OSC, 2015)" = "OSC 2015",
  "RRR1 (verbal overshadowing)" = "RRR1",
  # , "RRR2 (X)" = "RRR2"
  "RRR3 (grammar on intentionality effect)" = "RRR3",
  "RRR4 (ego depletion)" = "RRR4",
  "RRR8 (professor priming)" = "RRR8",
  "RRR9 (hostility priming)" = "RRR9",
  "Soto (Big5 Correlations)" = "Soto",
  "SSRP (Camerer et al., 2018)" = "SSRP"
)

moderator_choices <- list(
  "Original Effect Size" = "es_original",
  "Journal" = "orig_journal",
  "Year of Original Publication" = "orig_year",
  "Power of Replication Study" = "power"
)

# UI definition

ui <- fluidPage(
  theme = custom_theme,
  navbarPage(
    title = "",
    tabPanel(img(src = "fred.png", height = 67 / 2.5, width = 715 / 2.5), fluidRow(
      column(6, info)
    )),
    tabPanel(
      "Replicability Tracker",
      fluidRow(
        sidebarPanel(
          sliderInput("minpower", "Minimum Power", min = .05, max = .999, value = .05),
          selectInput("source", "Browse Large-Scale Projects:",
            choices = large_scale_project_choices,
            selected = "All studies"
          ),
          checkboxInput("validated", "Show validated entries only", value = TRUE),
          checkboxInput("codedentries", "Show coded entries only", value = TRUE),
          width = 2
        ),
        column(
          8,
          dataset_info,
          # Red-and-green Barplot
          withSpinner(plotly::plotlyOutput("barplot", width = "100%", height = "250px")),
          # Table for filtering
          withSpinner(DT::DTOutput("table")),
          # RPP-Scatterplot
          scatterplot_title,
          withSpinner(plotly::plotlyOutput("overviewplot", width = "100%", height = 800)),
          scatterplot_explanation,
          # Barplot 2
          barplot2_title,
          withSpinner(plotly::plotlyOutput("barplot2", width = "100%", height = "250px")),
          barplot2_explanation,
          # Z-Curve
          breaks,
          zcurve_title, breaks,
          withSpinner(shiny::plotOutput("zcurve_plot")),
          zcurve_explanation, breaks
          # , withSpinner(shiny::tableOutput("overview")), dataset_info1b
        )
      )
    ),
    tabPanel(
      "Study Overview",
      fluidRow(
        column(8, forest_info, withSpinner(plotly::plotlyOutput("forestplot", width = "100%", height = forestplotheight)))
        # , withSpinner(shiny::tableOutput("overview")), dataset_info1b
      )
    ),
    tabPanel(
      "Dataset", dataset_explanation, breaks,
      # downloadButton("reddownload", label = "Download dataset"),
      # breaks,
      dataset_headline,
      withSpinner(DT::DTOutput("dataset")),
      breaks,
      variables_headline,
      withSpinner(DT::DTOutput("variables"))
    ),
    tabPanel(
      "Correlates of Replicability",
      correlates_info,
      correlates_decade,
      withSpinner(plotly::plotlyOutput("correlate_decade")),
      correlates_journal,
      withSpinner(plotly::plotlyOutput("correlate_journal", width = "100%", height = "2000px"))
    ),
    tabPanel(
      "Moderators [alpha]",
      moderators_info,
      shiny::selectInput("moderator",
        label = "Moderators",
        choices = moderator_choices
      ),
      fluidRow(withSpinner(plotly::plotlyOutput("flexibleplot", width = "100%", height = 600))),
      fluidRow(
        column(6, withSpinner(DT::DTOutput("flexiblemodtable"))),
        column(6, withSpinner(shiny::htmlOutput("flexiblemoderatortext")))
      )
    ),

    # , tabPanel("Summarizer [alpha]"
    #            , fluidRow(
    #                    column(12
    #                           , checker_info
    #                           , withSpinner(DT::DTOutput("checkertable"))
    #                           , withSpinner(DT::DTOutput("flexiblecheckertable"))
    #                           , withSpinner(shiny::htmlOutput("flexiblesummarizertext"))
    #                    ), column(6
    #
    #                           , withSpinner(plotly::plotlyOutput("checker_violin", width = "100%", height = "400px"))
    #                       )
    #                       , column(6
    #                           # , withSpinner(plotly::plotlyOutput("checker_maplot")
    #                           , withSpinner(plotly::plotlyOutput("checker_bar")
    #                                 # , checker_matable
    #                                )
    #                       )
    #            ))

    tabPanel(
      "References-Checker [alpha]",
      rc_info,
      textAreaInput("refcheck", "References",
        value = "Judge, T. A., & Bono, J. E. (2000). Five-factor model of personality and transformational leadership. Journal of Applied Psychology, 85, 751-765. 10.1037/0021-9010.85.5.751",
        width = "1000px",
        height = "200px",
        placeholder = NULL
      ),
      withSpinner(plotly::plotlyOutput("references_barplot")),
      withSpinner(tableOutput("references_doi"))
    ),
    tabPanel(
      "References",
      column(6,
      references_headline, references_list,
      references_redpublications, references_list_redpublications,
      packages_headline, packages_list
    )
    ),
    tabPanel(
      "FAQ",
      column(6, includeMarkdown("www/FAQ.md"))
    ),
    tabPanel(
      "About",
      column(6,
      markdown(about_page),
      markdown(paste0("#", get_dataset_changelog())),
      h2("Package changelog"),
      includeMarkdown(system.file("NEWS.md", package = "FReD")),
      img(src = "fred.png", height = 80),
      # , breaks
      # , img(src = "FORRT.svg", height = 100)
      tags$style(HTML("
                                .navbar-default .navbar-brand {color:black;}
        .navbar-default .navbar-brand:hover {color:black;}
        .navbar { background-color:#EAEAEA;}
        .navbar-default .navbar-nav > li > a {color: dark grey;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color:black;background-color:#fc2d2d;}
        .navbar-default .navbar-nav > li > a:hover {color:black;background-color:#A6A6A6;text-decoration}
                               "))
    )
    )
  )
)
