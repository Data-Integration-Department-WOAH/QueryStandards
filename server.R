
# --- SERVER ---
server <- function(input, output, session) {
  
  report_path <- reactiveVal(NULL)
  findings_table_data <- reactiveVal(data.frame())
  
  pdf_files <- reactive({
    list.files("Norms&Standards", pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
  })
  
  output$file_list_output <- renderPrint({
    files <- basename(pdf_files())
    if (length(files) == 0) {
      cat("No PDF files found in 'Norms&Standards' folder.")
    } else {
      cat(files, sep = "\n")
    }
  })
  
  observeEvent(input$run_query_button, {
    
    api_key <- Sys.getenv("Google_Gemini_API_key")
    if (nchar(api_key) == 0) {
      showModal(modalDialog(
        title = "Error: API Key Not Found",
        "The 'Google_Gemini_API_key' environment variable is not set.",
        easyClose = TRUE
      ))
      return()
    }
    
    if (length(pdf_files()) == 0) {
      showNotification("Error: No PDF files found.", type = "error")
      return()
    }
    
    if (nchar(trimws(input$keywords_input)) == 0) {
      showNotification("Error: Please provide keywords.", type = "error")
      return()
    }
    
    withProgress(message = 'AI analysis in progress...', value = 0, {
      report_path(NULL)
      findings_table_data(data.frame())
      
      incProgress(0.1, detail = "Loading PDFs...")
      all_text <- tryCatch({
        text_list <- lapply(pdf_files(), pdf_text)
        paste(unlist(text_list), collapse = "\n\n--- END OF PAGE ---\n\n")
      }, error = function(e) {
        showNotification(paste("Error reading PDF files:", e$message), type = "error")
        return(NULL)
      })
      if (is.null(all_text)) return()
      
      incProgress(0.2, detail = "Preparing prompt...")
      # prompt <- gsub("<<KEYWORDS>>", input$keywords_input, input$prompt_template)
      task_part <- gsub("<<KEYWORDS>>", input$keywords_input, input$prompt_task)
      format_part <- input$prompt_format
      
      prompt <- paste(
        task_part,
        "\n\n--- DOCUMENT TEXT ---\n",
        all_text,
        "\n\n--- OUTPUT FORMAT ---\n",
        format_part,
        sep = "\n"
      )
      prompt <- gsub("<<DOCUMENT_TEXT>>", all_text, prompt)
      
      incProgress(0.3, detail = "Querying Gemini...")
      ai_response <- tryCatch({
        ellmer::chat_google_gemini(api_key = api_key)$chat(prompt, echo = FALSE)
      }, error = function(e) {
        showNotification(paste("AI API Error:", e$message), type = "error")
        return(NULL)
      })
      if (is.null(ai_response)) return()
      
      incProgress(0.8, detail = "Saving & rendering...")
      
      # Create timestamped or custom report file names
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      base_name <- ifelse(nchar(trimws(input$report_name)) > 0,
                          gsub("\\s+", "_", trimws(input$report_name)),
                          paste0("ai_report_", timestamp))
      qmd_path <- file.path("Output", paste0(base_name, ".qmd"))
      html_path <- file.path("Output", paste0(base_name, ".html"))
      
      clean_response <- gsub("(^```\\w*\\s*|```$)", "", ai_response)
      writeLines(clean_response, qmd_path)
      
      tryCatch({
        quarto::quarto_render(qmd_path, output_format = "html", quiet = TRUE)
      }, error = function(e) {
        showNotification(paste("Quarto rendering error:", e$message), type = "error")
        return(NULL)
      })
      
      if (file.exists(html_path)) {
        report_path(html_path)
        incProgress(1, detail = "Done!")
        showNotification("Analysis complete. Report generated.", type = "message")
        
        # Basic regex to extract findings for table (could be improved)
        findings <- regmatches(clean_response, gregexpr("(?<=\\#\\# Findings for ')(.*?)(?=')", clean_response, perl=TRUE))[[1]]
        if (length(findings) > 0) {
          findings_table_data(data.frame(Term = findings))
        }
        
      } else {
        showNotification("Error: Report not generated.", type = "error")
      }
    })
  })
  
  # Quarto Report Renderer
  output$quarto_output <- renderUI({
    path <- report_path()
    if (!is.null(path) && file.exists(path)) {
      url_with_cache_buster <- paste0("Output/", basename(path), "?", as.integer(Sys.time()))
      tags$iframe(
        src = url_with_cache_buster,
        width = '100%',
        height = '800px',
        style = "border: 1px solid #ccc;"
      )
    } else {
      HTML("<p style='color: grey; text-align: center; margin-top: 50px;'>The generated report will be displayed here.</p>")
    }
  })
  
  # Findings Table Renderer
  output$findings_table <- renderDT({
    datatable(
      findings_table_data(),
      options = list(pageLength = 10, searchHighlight = TRUE),
      rownames = FALSE
    )
  })
  
  addResourcePath("Output", "Output")
}
