library(shiny)
library(ellmer)
library(pdftools)
library(quarto)
library(DT)

# --- Directory Setup ---
if (!dir.exists("Norms&Standards")) dir.create("Norms&Standards")
if (!dir.exists("Output")) dir.create("Output")

# --- UI ---
ui <- fluidPage(
  titlePanel("AI Assistant for PDF Document Interrogation"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Configuration"),
      
      # Warning about sensitive data
      div(style = "color: red; font-weight: bold;",
          "⚠️ Warning: PDF content will be sent to Google Gemini for analysis. Do not upload confidential or sensitive documents."
      ),
      br(),
      
      # PDF File List
      h5("Detected PDF Files:"),
      verbatimTextOutput("file_list_output"),
      
      # Keywords Input
      textAreaInput(
        inputId = "keywords_input",
        label = "Enter Keywords/Expressions (one per line):",
        value = "notify\nnotifiable disease\nreport\nreportable disease",
        rows = 5
      ),
      
      # Prompt Editing
      # textAreaInput(
      #   inputId = "prompt_template",
      #   label = "AI Prompt (You can edit before sending):",
      #   value = paste(
      #     "Based on the document text provided below, perform the following tasks for the user's queried terms jointly:",
      #     "Queried Terms: \n<<KEYWORDS>>",
      #     "\n\n--- INSTRUCTIONS ---",
      #     "* Search and extract instances of any of the queried terms (or any close synonym) in the pdf files.",
      #     "* For each instance found:",
      #     "  - provide the chapter, section and page where the instance is found. If this information is not in the text, state 'Context not available'.",
      #     "  - provide the full paragraph where the instance is found (but do not quote the instance as ```instance```).",
      #     "  - highlight in yellow the instance found using Quarto's highlight syntax, like this:  '<span style=\"background-color: yellow\"> This text is highlighted in yellow </span>'",
      #     "* Provide your answer in the form of a neat Quarto .qmd document.",
      #     "   - The Quarto yaml must contain",
      #     "      title: \"Document Analysis\"",
      #     "      toc: true",
      #     "      toc-depth: 3",
      #     "      date: date-modified",
      #     "      number-sections: true",
      #     "      self-contained: true",
      #     "  - Use level-1 headers for each queried term (e.g., `## Findings for 'notifiable disease'`).",
      #     "  - If no instances are found for a term, state that clearly under its header.",
      #     "\n\n--- DOCUMENT TEXT ---\n<<DOCUMENT_TEXT>>",
      #     sep = "\n"
      #   ),
      #   rows = 20
      # ),
      
  #     # Prompt Editing - in collapsible, styled panel
  #     tags$div(
  #       style = "margin-top: 20px;",
  #       shiny::tags$details(
  #         open = FALSE,
  #         shiny::tags$summary(strong("✏️ Edit AI Prompt (Advanced)")),
  #         textAreaInput(
  #           inputId = "prompt_template",
  #           label = NULL,
  #           value = paste(
  #             "Based on the document text provided below, perform the following tasks for the user's queried terms jointly:",
  #             "Queried Terms: \n<<KEYWORDS>>",
  #             "\n\n--- INSTRUCTIONS ---",
  #             "* Search and extract instances of any of the queried terms (or any close synonym) in the pdf files.",
  #             "* For each instance found:",
  #             "  - provide the chapter, section and page where the instance is found. If this information is not in the text, state 'Context not available'.",
  #             "  - provide the full paragraph where the instance is found (but do not quote the instance as ```instance```).",
  #             "  - highlight in yellow the instance found using Quarto's highlight syntax, like this:  '<span style=\"background-color: yellow\"> This text is highlighted in yellow </span>'",
  #             "* Provide your answer in the form of a neat Quarto .qmd document.",
  #             "   - The Quarto yaml must contain",
  #             "      title: \"Document Analysis\"",
  #             "      toc: true",
  #             "      toc-depth: 3",
  #             "      date: date-modified",
  #             "      number-sections: true",
  #             "      self-contained: true",
  #             "  - Use level-1 headers for each queried term (e.g., `## Findings for 'notifiable disease'`).",
  #             "  - If no instances are found for a term, state that clearly under its header.",
  #             "\n\n--- DOCUMENT TEXT ---\n<<DOCUMENT_TEXT>>",
  #             sep = "\n"
  #           ),
  #           rows = 20,
  #           placeholder = "Modify the prompt before sending to Gemini...",
  #           width = "100%"
  #         )
  #       ),
  #       tags$style(HTML("
  #   textarea.form-control {
  #     font-family: 'Courier New', monospace;
  #     background-color: #f8f9fa;
  #     border: 1px solid #ced4da;
  #   }
  # "))
  #     ),
  
  
  # Task and Output Format Prompt Split
  tags$div(
    style = "margin-top: 20px;",
    
    # Task Description Panel
    tags$details(
      open = FALSE,
      tags$summary(strong("🧠 Task Description")),
      textAreaInput(
        inputId = "prompt_task",
        label = NULL,
        value = paste(
          "Based on the document text provided below, perform the following tasks for the user's queried terms jointly:",
          "Queried Terms: \n<<KEYWORDS>>",
          "* Search and extract instances of any of the queried terms (or any close synonym) in the pdf files.",
          "* For each instance found:",
          "  - provide the chapter, section and page where the instance is found. If this information is not in the text, state 'Context not available'.",
          "  - provide the full paragraph where the instance is found (but do not quote the instance as ```instance```).",
          "  - highlight in yellow the instance found using Quarto's highlight syntax, like this: '<span style=\"background-color: yellow\"> This text is highlighted in yellow </span>'",
          sep = "\n"
        ),
        rows = 10,
        width = "100%",
        placeholder = "Define what the AI should do with the input document..."
      )
    ),
    
    # Output Format Panel
    tags$details(
      open = FALSE,
      tags$summary(strong("📄 Output Format Instructions")),
      textAreaInput(
        inputId = "prompt_format",
        label = NULL,
        value = paste(
          "* Provide your answer in the form of a neat Quarto .qmd document.",
          "  - The Quarto yaml must contain:",
          "      title: \"Document Analysis\"",
          "      toc: true",
          "      toc-depth: 3",
          "      date: date-modified",
          "      number-sections: true",
          "      self-contained: true",
          "  - Use level-1 headers for each queried term (e.g., `## Findings for 'notifiable disease'`).",
          "  - If no instances are found for a term, state that clearly under its header.",
          sep = "\n"
        ),
        rows = 10,
        width = "100%",
        placeholder = "Specify how the AI should structure its response..."
      )
    ),
    
    tags$style(HTML("
    textarea.form-control {
      font-family: 'Courier New', monospace;
      background-color: #f8f9fa;
      border: 1px solid #ced4da;
    }
  "))
  ),
  
      
      # Report name input
      textInput("report_name", "Report Name (optional):", placeholder = "Leave blank for automatic timestamp name"),
      
      # Run Button
      actionButton("run_query_button", "Run AI Analysis", icon = icon("robot"), width = "100%"),
      
      hr(),
      helpText(
        strong("Instructions:"),
        br(),
        "1. Place PDF files in the 'Norms&Standards' folder.",
        br(),
        "2. Provide keywords and/or modify the AI prompt.",
        br(),
        "3. Click the Run button to generate the report."
      )
    ),
    
    mainPanel(
      width = 9,
      h4("Analysis Report"),
      fluidRow(
        column(
          width = 6,
          uiOutput("quarto_output")
        ),
        column(
          width = 6,
          h5("Extracted Findings Table"),
          DTOutput("findings_table")
        )
      )
    )
  )
)

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

# --- Run App ---
shinyApp(ui = ui, server = server)
