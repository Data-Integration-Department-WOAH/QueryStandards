library(shiny)
library(ellmer)
library(pdftools)
library(quarto)
library(DT)




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
      
      # Load and collapse all PDF text
      incProgress(0.1, detail = "Loading PDFs...")
      all_text <- tryCatch({
        text_list <- lapply(pdf_files(), pdftools::pdf_text)
        paste(unlist(text_list), collapse = "\n\n--- END OF PAGE ---\n\n")
      }, error = function(e) {
        showNotification(paste("Error reading PDF files:", e$message), type = "error")
        return(NULL)
      })
      if (is.null(all_text)) return()
      
      # Prepare keyword list
      #  incProgress(0.2, detail = "Preparing queries...")
      # keywords <- strsplit(input$keywords_input, ",\\s*|\\s+")[[1]]
      keywords <- trimws(strsplit(input$keywords_input, "\\r?\\n")[[1]])
      keywords <- keywords[keywords != ""]
      keyword_outputs <- list()
      
      # Iterate over each keyword
      # for (i in seq_along(keywords)) {
      #   keyword <- keywords[[i]]
      #   print(keywords)
      #   print(keyword)
      #   }
      
      for (i in seq_along(keywords)) {
        keyword <- keywords[[i]]
        
        print(keyword)
        
        task_part <- gsub("<<KEYWORDS>>", keyword, input$prompt_task)
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
        
        incProgress( i / length(keywords), detail = paste("Querying Gemini for:", keyword))
        
        ai_response <- tryCatch({
          ellmer::chat_google_gemini(api_key = api_key)$chat(prompt, echo = FALSE)
        }, error = function(e) {
          showNotification(paste("AI API Error for", keyword, ":", e$message), type = "error")
          return(NULL)
        })
        
        if (!is.null(ai_response)) {
          keyword_outputs[[keyword]] <- gsub("(^```\\w*\\s*|```$)", "", ai_response)
        }
      }
      
      # Create output directory if needed
      if (!dir.exists("Output")) dir.create("Output")
      
      # incProgress(0.9, detail = "Building Quarto report...")
      #  incProgress(i/length(keywords), detail = "Building Quarto report...")
      
      # Create final report content
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      base_name <- ifelse(nchar(trimws(input$report_name)) > 0,
                          gsub("\\s+", "_", trimws(input$report_name)),
                          paste0("ai_report_", timestamp))
      qmd_path <- file.path("Output", paste0(base_name, ".qmd"))
      html_path <- file.path("Output", paste0(base_name, ".html"))
      
      qmd_sections <- unlist(lapply(names(keyword_outputs), function(kw) {
        paste0("# Findings for '", kw, "'\n\n", keyword_outputs[[kw]], "\n")
      }))
      
      
      qmd_content <- c(
        "---",
        "title: \"Keyword query\"",
        paste0("date: last-modified"),
        "format: ",
        "  html:",
        "    toc: true",
        "    toc-depth: 3",
        "    number-sections: true",
        "    self-contained: true",
        "---",
        "",
        qmd_sections
      )
      
      writeLines(qmd_content, qmd_path)
      
      # Render the Quarto file
      tryCatch({
        quarto::quarto_render(qmd_path, output_format = "html", quiet = TRUE)
      }, error = function(e) {
        showNotification(paste("Quarto rendering error:", e$message), type = "error")
        return(NULL)
      })
      
      # Update UI and show report
     
      if (file.exists(html_path)) {
        report_path(html_path)
        incProgress(1, detail = "Done!")
        showNotification("Analysis complete. Report generated.", type = "message")

        # Populate Findings Table
        findings_table_data(data.frame(Term = names(keyword_outputs)))
      } else {
        showNotification("Error: Report not generated.", type = "error")
      }

      
      
      # output$report_viewer <- renderUI({
      #   req(report_path())  # make sure the path is set
      #   tags$iframe(
      #     src = report_path(),
      #     style = "width:100%; height:800px; border:none;")
      # })
      # 
      
    })
  })
 
}
