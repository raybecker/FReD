
server <- function(input, output, session) {
  doi_vector <- reactiveValues(dois = c(), selected_rows = NULL)

  reactive_df <- reactive({
    df <- df %>% arrange(ref_original)
    if (input$validated == "TRUE") {
      df %>% filter(validated == 1)
    } else {
      df
    }
  })

  reactive_distinct_fred_entries <- reactive({
    reactive_df() %>%
      select(doi_original, ref_original, description) %>%
      distinct() %>%
      arrange(is.na(doi_original), ref_original)
  })

  output$button_area <- renderUI({
    if (length(doi_vector$dois) == 0) {

        actionButton("process_button", "Process References")

    } else {
      tagList(

          actionButton("add_button", "Add references"),

        br(), HTML("&nbsp;"), br(),

          actionButton("replace_button", "Replace references")
      )
    }
  })

  parse_dois <- function() {

    # Check if both text box and file are provided
    if (nchar(input$references) > 0 && is.null(input$upload) == FALSE) {
      showNotification("Both text box and file will be processed.")
    }

    dois <- c()

    # Process text box input
    if (nchar(input$references) > 0) {
      text_dois <- extract_dois_from_text(input$references)
      dois <- c(dois, text_dois)
    }

    # Process uploaded file
    if (is.null(input$upload) == FALSE) {
      file_dois <- extract_dois_from_file(input$upload$datapath)
      dois <- c(dois, file_dois)
    }

    if (input$navbar == "Introduction") {
      nav_select("navbar", "Study Selection")
    }

    unique(dois)
  }


  observe({
    if (input$navbar == "Report") {
      if (!length(doi_vector$dois > 0)) {
        showModal(modalDialog(
          title = "No DOIs selected",
          "Please select DOIs before proceeding to the report.",
          easyClose = TRUE
        ))
        nav_select("navbar", "Study Selection")
      }
    }
  })


  observeEvent(input$process_button, {
    doi_vector$dois <- parse_dois()
  })

  observeEvent(input$add_button, {
    doi_vector$dois <- union(doi_vector$dois, parse_dois())
  })

  observeEvent(input$replace_button, {
    doi_vector$dois <- parse_dois()
  })


  visible <- reactiveVal(FALSE)

  output$doi_display <- renderText({
    paste(doi_vector$dois, collapse = "\n")
  })

  output$collapsible_dois <- renderUI({
    if (length(doi_vector$dois) > 0) {
      tags$div(
        id = "doi_display_container",
        style = if (visible()) "display: block;" else "display: none;",
        verbatimTextOutput("doi_display"),
        actionButton("copy_btn", "Copy DOIs", onclick = "copyText()")
      )
    }
  })

  observeEvent(input$hidden_toggle_btn, {
    visible(!visible())
  })

  output$showToggle <- reactive({
    length(doi_vector$dois) > 0
  })

  # Need to be active in background to allow toggling of DOI display
  outputOptions(output, "showToggle", suspendWhenHidden = FALSE)
  outputOptions(output, "doi_display", suspendWhenHidden = FALSE)




  extract_dois_from_text <- function(text) {

    dois <- regmatches(text, gregexpr("\\b(10\\.\\d{4,}(?:/[a-zA-Z0-9\\.\\-\\(\\)]+)*)\\b", text, perl = TRUE))
    dois <- unlist(dois)

    dois <- sub("\\s*([,;.])*$", "", dois)  # remove trailing punctuation

    dois <- sapply(dois, function(x) {
      if (substr(x, nchar(x), nchar(x)) == ")" && sum(strsplit(x, "")[[1]] == "(") < sum(strsplit(x, "")[[1]] == ")")) {
        substr(x, 1, nchar(x) - 1)
      } else {
        x
      }
    })

    unname(dois)
  }


  extract_dois_from_file <- function(file_path) {
    dois <- c()
    if (file_path != "") {
      if (grepl("\\.pdf$", file_path)) {
        # Extract DOIs from PDF file
        pdf_text <- pdftools::pdf_text(file_path)
        dois <- extract_dois_from_text(pdf_text)
      } else if (grepl("\\.bib$", file_path)) {
        # Extract DOIs from BibTeX file
        bib_text <- readLines(file_path)
        dois <- extract_dois_from_text(bib_text)
      }
    }
    return(dois)
  }


  # Initial rendering of the DataTable
  output$selected_references <- renderDT({
    data <- datatable(
      data.frame(doi_original = character(0), doi_replication = character(0),
                 description = character(0), es_original = numeric(0),
                 es_replication = numeric(0), result = character(0)),
      escape = FALSE, options = list(pageLength = 10), filter = "none"
    )
    formatRound(data, columns = c('es_original', 'es_replication'), digits = 2)

  }, server = TRUE)

  # Observer to update data reactively
  observe({
    proxy <- dataTableProxy('selected_references', deferUntilFlush = FALSE)

    if (length(doi_vector$dois) == 0) {
      replaceData(proxy, data.frame(doi_original = character(0), doi_replication = character(0),
                                    description = character(0), es_original = numeric(0),
                                    es_replication = numeric(0), result = character(0)), resetPaging = TRUE)
      return()
    }

    if (length(doi_vector$dois) > 0) {

    data <- data.frame(doi_original = doi_vector$dois) %>%
      left_join(reactive_df(), by = c("doi_original")) %>%
      mutate(
        in_FReD = doi_original %in% reactive_df()$doi_original,
        doi_original = purrr::map_chr(doi_original, ~sprintf('<a href="https://doi.org/%s" target="_blank">%s</a>', .x, .x)),
        doi_replication = purrr::map_chr(doi_replication, ~if (!is.na(.x)) sprintf('<a href="https://doi.org/%s" target="_blank">%s</a>', .x, .x) else NA_character_)
      ) %>%
      arrange(!in_FReD, is.na(doi_replication)) %>%
      select(doi_original, doi_replication, description, es_original, es_replication, result)

    replaceData(proxy, data, resetPaging = TRUE)
    updateFilters(proxy, data)
    }

  })

  output$database_search <- renderDT({
    datatable(reactive_distinct_fred_entries(),
              selection = 'multiple', options = list(pageLength = 10))
  }, server = TRUE)

  observe({
    selected_rows <- which(reactive_distinct_fred_entries()$doi_original %in% doi_vector$dois)
    proxy <- dataTableProxy('database_search')
    selectRows(proxy, selected_rows)
    doi_vector$selected_rows <- selected_rows
  })

  # Observe changes in selected rows
  observeEvent(input$database_search_rows_selected, {

    current_rows <- input$database_search_rows_selected

    # Determine newly selected and deselected rows
    newly_selected_rows <- setdiff(current_rows, doi_vector$selected_rows)
    deselected_rows <- setdiff(doi_vector$selected_rows, current_rows)

    # Update selected_dois based on row changes
    if (length(newly_selected_rows) > 0) {
      new_dois <- reactive_distinct_fred_entries()$doi_original[newly_selected_rows]
      doi_vector$dois <- union(doi_vector$dois, new_dois)
    }

    if (length(deselected_rows) > 0) {
      remove_dois <- reactive_distinct_fred_entries()$doi_original[deselected_rows]
      doi_vector$dois <- setdiff(doi_vector$dois, remove_dois)
    }

    # Update the reactive values
    doi_vector$selected_rows <- current_rows

  })

  # Need to be active in background to stay synced with DOI list
  outputOptions(output, "selected_references", suspendWhenHidden = FALSE)
  outputOptions(output, "database_search", suspendWhenHidden = FALSE)

  output$references_barplot <- plotly::renderPlotly({

    df <- tibble(doi_original = doi_vector$dois, in_FReD = doi_original %in% reactive_df()$doi_original) %>%
      mutate(in_FReD = ifelse(in_FReD, "yes", "no"))

    # Calculate the counts and proportions
    df_summary <- df %>%
      count(in_FReD) %>%
      mutate(proportion = n / sum(n),
             label = paste(in_FReD, "\n(", scales::percent(proportion, accuracy = 1), ")"))

    color_values = c("no" = "#FF7F7F", "yes" = "#8FBC8F")

    # Create the plot
    p <- df_summary %>%
      ggplot(aes(y = "", x = proportion, fill = in_FReD)) +
      geom_bar(stat = "identity", position = "fill") +
      geom_text(aes(label = label), position = position_fill(vjust = 0.5), size = 3) +  # Adjust size as needed
      theme_minimal() +
      labs(y = "", x = "Share", title = "Are replications for references in FReD?") +
      scale_x_continuous(labels = scales::percent) +
      scale_fill_manual(values = color_values) +
      guides(fill = "none") + theme_void()

    plotly::ggplotly(p, tooltip = NULL, height = 150) %>%
      plotly::config(displayModeBar = FALSE)

  })

  output$outcomes_barplot <- plotly::renderPlotly({

    df <- reactive_df() %>%
      filter(doi_original %in% doi_vector$dois)

    if (input$success_criterion == "consistency") {
      df <- df %>% mutate(result = assess_success(consistency, success_criterion = "consistency"))
      outcome_colors <- c("Inconsistent replication" = "#FF7F7F", "Consistent replication" = "#8FBC8F", "Replication (of n.s. finding)" = "#F0F0F0")

    } else if (input$success_criterion == "significance") {
      df <- df %>% mutate(result = assess_success(result, success_criterion = "significance"))
      outcome_colors <- c("Non-significant replication" = "#FF7F7F", "Significant replication" = "#8FBC8F", "Replication (of n.s. finding)" = "#F0F0F0")
    }

    p <- df %>%
      mutate(result = result %>% forcats::fct_na_value_to_level("not coded")) %>%
      ggplot(aes(y = result, fill = result)) +
      geom_bar() +
      theme_minimal() +
      labs(y = "", x = "Count", title = "Outcomes of replication attempts") +
      scale_fill_manual(values = outcome_colors)

    plotly::ggplotly(p, tooltip = NULL) %>%
      plotly::config(displayModeBar = FALSE)

  })

  # Functions to generate markdown
  assess_success <- function(result, success_criterion = c("consistency", "significance")) {
    if (success_criterion[[1]] == "consistency") {
      case_when(
        result == "OS not significant" ~ "Replication (of n.s. finding)",
        result == "consistent" ~ "Consistent replication",
        result %in% c("inconsistent", "inconsistent, smaller") ~ "Inconsistent replication"
        )

    } else if (success_criterion[[1]] == "significance") {
      case_when(
        result == "successful replication" ~ "Significant replication",
        result == "failed replication" ~ "Non-significant replication",
        result == "OS not significant" ~ "Replication (of n.s. finding)",
        TRUE ~ NA_character_
      )
    } else {
      stop("Invalid type argument. Must be 'consistent'.")
    }
  }


  assess_outcome <- function(replications, ..., success_criterion = c("consistency", "significance"), return_html = TRUE) {

    if (success_criterion[[1]] == "consistency") {
      replications %>%
        mutate(replication_outcome = assess_success(consistency, success_criterion = "consistency"),
        result = consistency) %>%
        summarise(
          replications = paste0("  - **", replication_outcome, ":** ", ref_replication, collapse = "\n"),
          overall_outcome = case_when(
            all(replication_outcome == "Consistent replication") ~ if (return_html) "<span style='color: darkgreen;'>&#x2714;</span>" else "[Re]", # ✔️
            all(replication_outcome == "Inconsistent replication") ~ if (return_html) "<span style='color: darkred;'>&#x2716;</span>" else "[¬Re]", #✖️
            all(replication_outcome == "Replication (of n.s. finding)") ~ if (return_html) "&#x1F6AB;" else "[NA]", # 🚫
            TRUE ~ if (return_html) "&#x2753;" else "[?Re]" #❓
          )
        )
    } else if (success_criterion[[1]] == "significance") {
      replications %>%
        mutate(replication_outcome = assess_success(result, success_criterion = "significance")) %>%
        summarise(
          replications = paste0("  - **", replication_outcome, ":** ", ref_replication, collapse = "\n"),
          overall_outcome = case_when(
            all(replication_outcome == "Significant replication") ~ if (return_html) "<span style='color: darkgreen;'>&#x2714;</span>" else "[Re]", # ✔️
            all(replication_outcome == "Non-significant replication") ~ if (return_html) "<span style='color: darkred;'>&#x2716;</span>" else "[¬Re]", #✖️
            all(replication_outcome == "Replication (of n.s. finding)") ~ if (return_html) "&#x1F6AB;" else "[NA]", # 🚫
            TRUE ~ if (return_html) "&#x2753;" else "[?Re]" #❓
          )
        )

    } else {
      stop("Invalid type argument. Must be 'consistent'.")
    }
  }

  generate_markdown <- function(df_filtered, ...) {
    extra_args <- list(...)
    extra_args["success_criterion"] <- input$success_criterion
    df_filtered %>%
      mutate(
        # Replace newline characters with spaces in ref_original
        ref_original = stringr::str_replace_all(ref_original, "\n", " "),
        # Append DOI link to ref_original if not already present
        ref_original = ifelse(stringr::str_detect(ref_original, stringr::fixed("doi.org")),
                              ref_original,
                              paste0(ref_original, " [https://doi.org/", doi_original, "](https://doi.org/", doi_original, ")")),
        # Construct doi_urls from doi_replication or osf_link
        doi_urls = ifelse(!is.na(doi_replication),
                          paste0("https://doi.org/", doi_replication),
                          ifelse(!is.na(osf_link), osf_link, "")),
        # Replace newline characters with spaces in ref_replication
        ref_replication = stringr::str_replace_all(ref_replication, "\n", " "),
        # Append doi_urls to ref_replication if not already present
        ref_replication = ifelse(stringr::str_detect(ref_replication, stringr::fixed("doi.org")),
                                 ref_replication,
                                 paste0(ref_replication, " [", doi_urls, "](", doi_urls, ")"))
      )  %>%
      group_by(ref_original) %>%
      group_modify(~do.call(assess_outcome, c(list(.x), extra_args))) %>%
      ungroup() %>%
      mutate(
        markdown = paste0(
          "##### ", overall_outcome, " ", ref_original, "\n\n",
          replications, "\n\n"
        )
      ) %>%
      pull(markdown) %>%
      paste(collapse = "\n") %>%
      paste0("## Replication Outcomes\n\n", .)
  }

  output$refs_annotated <- renderUI({

    df <- reactive_df() %>%
      filter(doi_original %in% doi_vector$dois)

    markdown_output <- generate_markdown(df)

    tags$div(
      class = "page-like",
      style = "white-space: pre-wrap; overflow-y: auto; max-height: 800px;",
      HTML(markdown::markdownToHTML(text = markdown_output, fragment.only = TRUE))
    )

    })

  output$downloadPdf <- downloadHandler(
    filename = function() {
      paste("reading_list", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      tempReport <- tempfile(fileext = ".Rmd")

      df <- reactive_df() %>%
        filter(doi_original %in% doi_vector$dois)

      markdown_output <- generate_markdown(df, return_html = FALSE)

      # Create the Rmd content
      rmd_content <- paste0(
        "---\n",
        "title: \"Annotated Reading List\"\n",
        "output:\n  pdf_document:\n    latex_engine: xelatex\n",
        "---\n\n",
        markdown_output
      )

      writeLines(rmd_content, con = tempReport)

      out <- rmarkdown::render(tempReport, quiet = TRUE)
      file.rename(out, file)
    }
  )

  output$replicability_plot <- plotly::renderPlotly({

    df <- reactive_df() %>%
      filter(doi_original %in% doi_vector$dois)

    if (input$success_criterion == "consistency") {
      df <- df %>% mutate(result = assess_success(consistency, success_criterion = "consistency"))
      outcome_colors <- c("Inconsistent replication" = "#FF7F7F", "Consistent replication" = "#8FBC8F", "Replication (of n.s. finding)" = "#F0F0F0")

    } else if (input$success_criterion == "significance") {
      df <- df %>% mutate(result = assess_success(result, success_criterion = "significance"))
      outcome_colors <- c("Non-significant replication" = "#FF7F7F", "Significant replication" = "#8FBC8F", "Replication (of n.s. finding)" = "#F0F0F0")

    }

    df$significant_original <- c("Not significant", "Significant")[(df$p_value_original < .05) + 1] %>% factor()
    df$significant_replication <- c("Not significant", "Significant")[(df$p_value_replication < .05) + 1] %>% factor()

    df$result <- df$result %>% factor()

    df$scatterplotdescription <- paste(stringr::str_wrap(df$description, 50), "\nr(original) = ",
                                            round(df$es_original, 3),
                                            ", r(replication) = ",
                                            round(df$es_replication, 3),
                                            sep = ""
    )

    pointsize <- ifelse(nrow(df) < 10, 5, ifelse(nrow(df) < 100, 4, 3))

    df <- df %>% filter(!is.na(es_replication))

    scatterplot <-
      ggplot(df, aes(x = es_original, y = es_replication, text = scatterplotdescription)) +
      geom_hline(aes(yintercept = 0), linetype = 2) +
      geom_abline(intercept = 0, slope = 1, color = "Grey60") +
      geom_point(aes(fill = result), size = pointsize, color = "Grey30", shape = 21, alpha = .8) +
      # geom_point(data = df_temp[s3, ], fill = "#0077d9", color = "#f2ef1b", shape = 4) +
      geom_rug(data = df[df$significant_original == "Significant", ], color = "darkgreen", linewidth = 1, sides = "b", alpha = .6) +
      geom_rug(data = df[df$significant_original == "Not significant", ], color = "darkred", linewidth = 1, sides = "b", alpha = .6) +
      geom_rug(data = df[df$significant_replication == "Significant", ], color = "darkgreen", linewidth = 1, sides = "l", alpha = .6) +
      geom_rug(data = df[df$significant_replication == "Not significant", ], color = "darkred", linewidth = 1, sides = "l", alpha = .6) +
      scale_x_continuous(name = "Original Effect Size", limits = c(0, 1), breaks = c(0, .25, .5, .75, 1)) +
      scale_y_continuous(name = "Replication Effect Size", limits = c(-.5, 1), breaks = c(-.5, -.25, 0, .25, .5, .75, 1)) +
      # ggtitle("") + #xlab("") + ylab("") +
      # scale_size_continuous(name="Power",range=c(.5,3.5)) +
      #scale_color_discrete(guide = "none") +
      scale_fill_manual(values = outcome_colors) +
      theme_bw() +
      labs(fill = "Replication Outcome", color = "Significance")
      #theme(legend.position = "inside", plot.margin = unit(c(-2,-1.5,2,2), "lines"))
      #theme(legend.position = "none")

    plotly::ggplotly(scatterplot, tooltip = "text") %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  })

}

#   output$replicability_plot <- renderPlotly({
#
#     df <- reactive_df() %>%
#       filter(doi_original %in% doi_vector$dois) %>%
#       mutate(significant = case_when(significant_replication == 0 ~ "Not Significant",
#                                      significant_replication == 1 ~ "Significant",
#                                      TRUE ~ NA_character_)) %>%
#       filter(!is.na(es_replication))
#
#     browser()
#
#     subplot(
#       plot_ly(data = df, x = ~es_original, type = 'histogram',
#               alpha =.5, showlegend = FALSE) ,
#       ggplot(),
#       plot_ly(data = df, x = ~es_original, y = ~es_replication, type = 'scatter', size = ~power_r, fill = ~'',
#               mode = 'markers', color = ~significant, alpha = .5, colors = c("Not Significant" = "red", "Significant" = "darkgreen"),
#               text = ~sprintf("Original (%s): %.2f\nReplication (%s): %.2f\nPower of replication: %.2f",
#                               doi_original, es_original, doi_replication, es_replication, power_r),
#               hoverinfo = "text",  showlegend = TRUE) %>%
#         layout(xaxis = list(title = 'Original Effect Size', zeroline = TRUE, showline = TRUE),
#                yaxis = list(title = 'Replication Effect Size', zeroline = TRUE, showline = TRUE)
#               ),
#       plot_ly(data = df, y = ~es_replication, type = 'histogram',
#               alpha = .5, showlegend = FALSE),
#       nrows = 2, heights = c(.2, .8), widths = c(.8,.2), margin = 0,
#       shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE) %>%
#       layout(title = "Comparison of original and replication findings")
#
#
#   })
#
#
#
#
# }


#
#   # CHECKER / SUMMARIZER -----------------------------------------------------------------
#
#
#   # Checkertable ------------------------------------------------------------
#
#
#   output$checkertable <- DT::renderDT(server = FALSE, {
#     # combine coded and uncoded studies
#     df[is.na(df$result), "result"] <- "not coded yet"
#
#     ## apply filters
#     df_temp <- df
#     df_temp <- df_temp[rev(row.names(df_temp)), ]
#
#     # exclude NAs
#     df_temp <- df_temp[!is.na(df_temp$result), ]
#
#     # exclude non-validated entries
#     df_temp <- df_temp[!is.na(df_temp$validated), ]
#
#
#     # df_temp_filtered <- df_temp[, c("description", "n_original", "n_replication", "power", "result")]
#     # df_temp_filtered <- df_temp[, c("description", "tags", "contributors", "result", "ref_original", "ref_replication")]
#
#     DT::datatable(
#       df_temp[, c("description", "tags", "result", "ref_original", "ref_replication")],
#       extensions = "Buttons",
#       options = list(
#         scrollX = TRUE,
#         dom = "Bfrtip",
#         buttons = c("copy", "csv", "excel"),
#         pageLength = 5
#         # , lengthMenu = c(5, 10, 100) # XXX not working yet
#       ), rownames = FALSE
#     )
#   })
#
#
#
#   # # Checker Violin Plot -----------------------------------------------------
#   #
#   #
#   #
#   #   output$checker_violin <- plotly::renderPlotly({
#   #
#   #     # # combine coded and uncoded studies
#   #     # df <- plyr::rbind.fill(df, as)
#   #     # df <- plyr::rbind.fill(df, forrt)
#   #     # df[is.na(df$result), "result"] <- "not coded yet"
#   #
#   #     # this plot is based on the filtered entries from the checkertable
#   #     df_temp <- df
#   #     df_temp <- df_temp[rev(row.names(df_temp)), ]
#   #
#   #     # exclude non-validated entries
#   #     df_temp <- df_temp[!is.na(df_temp$validated), ]
#   #
#   #     # use only filtered studies
#   #     s1 <- input$checkertable_rows_current  # rows on the current page
#   #     s2 <- input$checkertable_rows_all      # rows on all pages (after being filtered)
#   #     s3 <- input$checkertable_rows_selected # selected rows
#   #
#   #     df_temp <- df_temp[s2, ]
#   #
#   #     # exclude NAs
#   #     df_temp <- df_temp[!is.na(df_temp$result), ]
#   #
#   #     # compute se
#   #     df_temp$se_original <- sqrt((1-abs(as.numeric(df_temp$es_original))^2)/(as.numeric(df_temp$n_original)-2))
#   #     df_temp$se_replication <- sqrt((1-abs(as.numeric(df_temp$es_replication))^2)/(as.numeric(df_temp$n_replication)-2))
#   #
#   #     redlong_original <- df_temp[, c("es_original", "ref_original", "n_original", "se_original")]
#   #     redlong_original$type = "Original"
#   #     names(redlong_original) <- c("es", "ref", "n", "se", "type")
#   #     redlong_original <- redlong_original[!duplicated(redlong_original), ]
#   #
#   #     redlong_replication <- df_temp[ , c("es_replication", "ref_replication", "n_replication", "se_replication")]
#   #     redlong_replication$type = "Replication"
#   #     names(redlong_replication) <- c("es", "ref", "n", "se", "type")
#   #
#   #     redlong <- rbind(redlong_original, redlong_replication)
#   #     checker_gg <- ggplot(redlong, aes(x = type, y = as.numeric(es), text = ref)) + # , text = ref
#   #       # geom_violin(draw_quantiles =  .5) +
#   #       geom_jitter(width = .1, height = 0) +
#   #       xlab("Study Type") + ylab("r") +
#   #       geom_abline(h = 0, slope = 0, lty = 2) +
#   #       theme_bw()
#   #
#   #     checker_plotly <- plotly::ggplotly(checker_gg, tooltip = "text") %>% #
#   #       plotly::config(displayModeBar = FALSE) %>%
#   #       layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
#   #
#   #     checker_plotly
#   #
#   #   })
#
#
#   # # Checker MA Plot ---------------------------------------------------------
#   #
#   #   output$checker_maplot <- plotly::renderPlotly({
#   #
#   #     # this plot is based on the filtered entries from the checkertable
#   #     df_temp <- df
#   #     df_temp <- df_temp[rev(row.names(df_temp)), ]
#   #
#   #     # exclude non-validated entries
#   #     df_temp <- df_temp[!is.na(df_temp$validated), ]
#   #
#   #     # use only filtered studies
#   #     s1 <- input$checkertable_rows_current  # rows on the current page
#   #     s2 <- input$checkertable_rows_all      # rows on all pages (after being filtered)
#   #     s3 <- input$checkertable_rows_selected # selected rows
#   #
#   #     df_temp <- df_temp[s2, ]
#   #
#   #     # exclude NAs
#   #     df_temp <- df_temp[!is.na(df_temp$result), ]
#   #
#   #     # compute se
#   #     df_temp$se_original <- sqrt((1-abs(as.numeric(df_temp$es_original))^2)/(as.numeric(df_temp$n_original)-2))
#   #     df_temp$se_replication <- sqrt((1-abs(as.numeric(df_temp$es_replication))^2)/(as.numeric(df_temp$n_replication)-2))
#   #
#   #     redlong_original <- df_temp[, c("es_original", "ref_original", "n_original", "se_original")]
#   #     redlong_original$type = "Original"
#   #     names(redlong_original) <- c("es", "ref", "n", "se", "type")
#   #     redlong_original <- redlong_original[!duplicated(redlong_original), ]
#   #
#   #     redlong_replication <- df_temp[ , c("es_replication", "ref_replication", "n_replication", "se_replication")]
#   #     redlong_replication$type = "Replication"
#   #     names(redlong_replication) <- c("es", "ref", "n", "se", "type")
#   #
#   #     # remova missing values
#   #     redlong <- rbind(redlong_original, redlong_replication)
#   #
#   #
#   #     redlong <- redlong[!is.na(redlong$ref),]
#   #
#   #     model <- metafor::rma.mv(yi = as.numeric(es)
#   #                              , V = se^2
#   #                              , random = ~1 | ref
#   #                              , tdist = TRUE
#   #                              , data = redlong
#   #                              , mods = ~ as.factor(type) - 1
#   #                              , method = "ML")
#   #
#   #
#   #     summary(model)
#   #
#   #     ma_table <- data.frame("study_type" = as.character(c("Original", "Replication"))
#   #                            , "mean_r" =   round(as.numeric(model$beta), 3)
#   #                            , "se" =       round(as.numeric(model$se), 3)
#   #                            , "lower_ci" = round(as.numeric(model$ci.lb), 3)
#   #                            , "upper_ci" = round(as.numeric(model$ci.ub), 3)
#   #                            , "n" = c(sum(redlong$type == "Original")
#   #                                      , sum(redlong$type == "Replication"))
#   #     )
#   #
#   #     checker_maplot <- ggplot(ma_table, aes(x = study_type, y = mean_r, ymin = lower_ci, ymax = upper_ci)) +
#   #       geom_point() +
#   #       geom_errorbar() +
#   #       theme_bw() +
#   #       geom_abline(slope = 0, lty = 2) +
#   #       ylim(c(-.1, round(max(as.numeric(model$ci.ub)), 1)+.1)) +
#   #       xlab("Study Type") + ylab("Mean Effect Size Estimate (r)")
#   #
#   #     checker_maplotly <- plotly::ggplotly(checker_maplot) %>% # , tooltip = "text"
#   #       plotly::config(displayModeBar = FALSE) %>%
#   #       layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
#   #
#   #     checker_maplotly
#   #   })
#
#
#   # Checker Barplot ---------------------------------------------------------
#
#   output$checker_bar <- plotly::renderPlotly({
#     # this plot is based on the filtered entries from the checkertable
#     df_temp <- df
#     df_temp <- df_temp[rev(row.names(df_temp)), ]
#
#     # exclude non-validated entries
#     df_temp <- df_temp[!is.na(df_temp$validated), ]
#
#     # use only filtered studies
#     s1 <- input$checkertable_rows_current # rows on the current page
#     s2 <- input$checkertable_rows_all # rows on all pages (after being filtered)
#     s3 <- input$checkertable_rows_selected # selected rows
#
#     df_temp <- df_temp[s2, ]
#
#     # exclude NAs
#     df_temp <- df_temp[!is.na(df_temp$result), ]
#
#     ## Exclude NAs
#     df_temp <- df_temp[!is.na(df_temp$result), ]
#
#     bardata <- as.data.frame(base::table(df_temp$result, useNA = "always") / nrow(df_temp))
#     names(bardata) <- c("Result", "Proportion")
#     bardata$Proportion <- round(bardata$Proportion, 4) * 100
#
#     bardata$description <- paste(bardata$Result, ": ", bardata$Proportion, "%", sep = "")
#
#     barchart <- ggplot(bardata, aes(x = "", fill = Result, y = Proportion, text = description)) +
#       geom_bar(position = "fill", stat = "identity") +
#       theme_bw() +
#       ylab("Percentage") +
#       xlab("") +
#       coord_flip() +
#       scale_fill_manual("Result", values = c(
#         "success" = "#30c25a",
#         "informative failure to replicate" = "#f0473e",
#         "practical failure to replicate" = "#f2bbb8",
#         "inconclusive" = "#60bef7"
#       )) + # , NA = "grey"
#       ggtitle(paste(nrow(df_temp), "of", nrow(df), "studies selected."))
#     p <- plotly::ggplotly(barchart, tooltip = "text") %>%
#       plotly::config(displayModeBar = FALSE) %>%
#       plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) #  %>% layout(height = 10000, width = 1200)
#
#     p
#   })
#
#   # Checker MA Table --------------------------------------------------------
#
#
#   output$flexiblecheckertable <- DT::renderDT({
#     # this plot is based on the filtered entries from the checkertable
#     df_temp <- df
#     df_temp <- df_temp[rev(row.names(df_temp)), ]
#
#     # exclude non-validated entries
#     df_temp <- df_temp[!is.na(df_temp$validated), ]
#
#     # use only filtered studies
#     s1 <- input$checkertable_rows_current # rows on the current page
#     s2 <- input$checkertable_rows_all # rows on all pages (after being filtered)
#     s3 <- input$checkertable_rows_selected # selected rows
#
#     df_temp <- df_temp[s2, ]
#
#     # exclude NAs
#     df_temp <- df_temp[!is.na(df_temp$result), ]
#
#     # compute se
#     df_temp$se_original <- sqrt((1 - abs(as.numeric(df_temp$es_original))^2) / (as.numeric(df_temp$n_original) - 2))
#     df_temp$se_replication <- sqrt((1 - abs(as.numeric(df_temp$es_replication))^2) / (as.numeric(df_temp$n_replication) - 2))
#
#     redlong_original <- df_temp[, c("es_original", "ref_original", "n_original", "se_original")]
#     redlong_original$type <- "Original"
#     names(redlong_original) <- c("es", "ref", "n", "se", "type")
#     redlong_original <- redlong_original[!duplicated(redlong_original), ]
#
#     redlong_replication <- df_temp[, c("es_replication", "ref_replication", "n_replication", "se_replication")]
#     redlong_replication$type <- "Replication"
#     names(redlong_replication) <- c("es", "ref", "n", "se", "type")
#
#     # remova missing values
#     redlong <- rbind(redlong_original, redlong_replication)
#
#
#     redlong <- redlong[!is.na(redlong$ref), ]
#
#     model <- metafor::rma.mv(
#       yi = as.numeric(es),
#       V = se^2,
#       random = ~ 1 | ref,
#       tdist = TRUE,
#       data = redlong,
#       mods = ~ as.factor(type) - 1,
#       method = "ML"
#     )
#
#
#     summary(model)
#
#     ma_table <- data.frame(
#       "study_type" = as.character(c("Original", "Replication")),
#       "mean_r" = round(as.numeric(model$beta), 3),
#       "se" = round(as.numeric(model$se), 3),
#       "lower_ci" = round(as.numeric(model$ci.lb), 3),
#       "upper_ci" = round(as.numeric(model$ci.ub), 3),
#       "n" = c(
#         sum(redlong$type == "Original"),
#         sum(redlong$type == "Replication")
#       )
#     )
#
#     # print table
#     DT::datatable(ma_table,
#       options = list(options = list(pageLength = 200, dom = "t")),
#       rownames = FALSE
#     )
#   })
#
#
#   # Checker MA Text ------------------------------------------------------------
#
#   output$flexiblesummarizertext <- shiny::renderText({
#     # this plot is based on the filtered entries from the checkertable
#     df_temp <- df
#     df_temp <- df_temp[rev(row.names(df_temp)), ]
#
#     # exclude non-validated entries
#     df_temp <- df_temp[!is.na(df_temp$validated), ]
#
#     # use only filtered studies
#     s1 <- input$checkertable_rows_current # rows on the current page
#     s2 <- input$checkertable_rows_all # rows on all pages (after being filtered)
#     s3 <- input$checkertable_rows_selected # selected rows
#
#     df_temp <- df_temp[s2, ]
#
#     # exclude NAs
#     df_temp <- df_temp[!is.na(df_temp$result), ]
#
#     # compute se
#     df_temp$se_original <- sqrt((1 - abs(as.numeric(df_temp$es_original))^2) / (as.numeric(df_temp$n_original) - 2))
#     df_temp$se_replication <- sqrt((1 - abs(as.numeric(df_temp$es_replication))^2) / (as.numeric(df_temp$n_replication) - 2))
#
#     redlong_original <- df_temp[, c("es_original", "ref_original", "n_original", "se_original")]
#     redlong_original$type <- "Original"
#     names(redlong_original) <- c("es", "ref", "n", "se", "type")
#     redlong_original <- redlong_original[!duplicated(redlong_original), ]
#
#     redlong_replication <- df_temp[, c("es_replication", "ref_replication", "n_replication", "se_replication")]
#     redlong_replication$type <- "Replication"
#     names(redlong_replication) <- c("es", "ref", "n", "se", "type")
#
#     # remova missing values
#     redlong <- rbind(redlong_original, redlong_replication)
#
#
#     redlong <- redlong[!is.na(redlong$ref), ]
#
#     model <- metafor::rma.mv(
#       yi = as.numeric(es),
#       V = se^2,
#       random = ~ 1 | ref,
#       tdist = TRUE,
#       data = redlong,
#       mods = ~ as.factor(type) - 1,
#       method = "ML"
#     )
#
#     HTML(ma_text <- paste("<br><br><h5>On average and using a random-effects meta-analysis, original effect sizes were ",
#       ifelse(model$pval[1] < .05, "", "not "),
#       "significant. Average replication effect sizes were ",
#       ifelse(model$pval[2] < .05, "", "not "),
#       "significant and ",
#       ifelse(model$ci.ub[2] < model$b[1] & model$ci.lb[1] > model$b[2], "", "not "),
#       "significantly smaller than original effect sizes. ",
#       "<h6>",
#       sep = ""
#     ))
#   })
#
#
#   # Downloadbutton ----------------------------------------------------------
#
#
#   output$reddownload <- downloadHandler(
#     filename = function() {
#       paste("df-", Sys.Date(), ".csv", sep = "")
#     },
#     content = function(con) {
#       write.csv(df, con, fileEncoding = "WINDOWS-1252") # XXX nochmal prüfen
#     }
#   )
# }
#
