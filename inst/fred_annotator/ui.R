# ANNOTATOR ---------------------------------------------------------------

# UI definition

sidebar_contents <- sidebar(
  id = "mySidebar",
  padding = 10,
  width = 300,
  textAreaInput("references", "Paste references with DOIs", placeholder = "Paste references here...", width = '95%', height = '200px'),
  fileInput("upload", "Or upload reference list (PDF, citation or text file)", accept = c("application/pdf", " text/plain", ".bib", ".ris")),
  uiOutput("button_area"),
  checkboxInput("validated", "Use validated database entries only", value = TRUE),
  selectInput("success_criterion", "Success criterion", choices = c(Significance = "significance", Consistency = "consistency"), selected = "significance"),
  conditionalPanel(
    condition = "output.showToggle",  # This JavaScript condition reacts to Shiny output
    tags$a(id = "toggle_link", "Show/Hide DOIs >", href = "#", class = "btn btn-link"),
    actionButton("hidden_toggle_btn", "Toggle", style = "display: none;")
  ),
  uiOutput("collapsible_dois"),
  tags$script(HTML("
    $(document).on('click', '#toggle_link', function(event) {
      event.preventDefault();
      $('#doi_display_container').toggle();  // Toggle visibility of the DOI display container
      $('#hidden_toggle_btn').click();  // Trigger the hidden button click
    });
    function copyText() {
      const element = document.getElementById('doi_display');
      navigator.clipboard.writeText(element.textContent);
      alert('Copied to clipboard!');
    }
  "))
  )




# Define content for each panel
introduction_content <- nav_panel(
  "Introduction",
  class = "tab-pane-narrow",
  includeMarkdown("www/introduction.md")

)

study_selection_content <- nav_panel(
  "Study Selection",
  h2("Selected studies"),
         DTOutput("selected_references", fill = FALSE),
  h2("Available studies in FReD"),
  HTML(paste("Select/unselect rows to add/remove studies from the report. You can search by DOI, reference or description.
       At present, only studies where the DOI is listed in FReD can be used in the annotator, rows without DOIs will
       be ignored here, but can be retrieved from the FReD dataset."), sep = ""), # <a href=https://www.osf.io/9r62x/>the FReD dataset</a>
         DTOutput("database_search", fill = FALSE)
  )


report_content <- nav_panel(
  "Report",
  div(
    style = "max-width: 1000px; margin: auto;",
    plotly::plotlyOutput("references_barplot", height = 150),
    plotly::plotlyOutput("outcomes_barplot"),
    br(),
    plotly::plotlyOutput("replicability_plot", height = "600px"),

  ),
  div(
    style = "max-width: 200px; margin: auto;",
  downloadButton("downloadPdf", "Download PDF reading list")),
  shinycssloaders::withSpinner(uiOutput("refs_annotated"))
)

about_content <- nav_panel("About",
                           class = "tab-pane-narrow",

                        markdown(about_page),
                        markdown(paste0("#", get_dataset_changelog())),
                        h2("Package changelog"),
                        includeMarkdown(system.file("NEWS.md", package = "FReD")),
                        img(src = "fred.png", height = 80),
                        tags$style(HTML("

                               "))
)

# Construct the page_navbar UI
ui <- tagList(
  shinyjs::useShinyjs(),
  tags$head(tags$style(HTML(custom_css))),
  page_navbar(
  theme = custom_theme,
  id = "navbar",
  title = div(img(src = "fred.png", height = 67 / 2.5, width = 715 / 2.5), style = "padding: 10px; display: flex; align-items: center;"),
  window_title = "FReD Annotator",
  sidebar = sidebar_contents,
  introduction_content,
  study_selection_content,
  report_content,
  about_content
)
)
