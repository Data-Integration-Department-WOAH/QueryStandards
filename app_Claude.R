# AI PDF Document Interrogation Shiny App
# This app queries PDF documents using Google Gemini AI to find specific keywords/expressions
# and generates a Quarto document with highlighted results

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ellmer)
library(quarto)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "PDF Document AI Interrogation Tool"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Document Query", tabName = "query", icon = icon("search"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "query",
        fluidRow(
          # Input panel
          box(
            title = "Query Configuration", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            
            # Display PDF files found
            h4("PDF Documents to be Interrogated:"),
            verbatimTextOutput("pdf_files_list"),
            
            br(),
            
            # Keywords input
            h4("Keywords/Expressions to Search:"),
            textAreaInput(
              "keywords",
              label = NULL,
              value = "notify, notifiable disease, report, reportable disease",
              rows = 3,
              width = "100%",
              placeholder = "Enter keywords separated by commas"
            ),
            
            br(),
            
            # Action button
            actionButton(
              "process_docs",
              "Process Documents",
              class = "btn-primary btn-lg",
              style = "width: 100%;"
            )
          )
        ),
        
        fluidRow(
          # Status and results panel
          box(
            title = "Processing Status & Results", 
            status = "success", 
            solidHeader = TRUE,
            width = 12,
            
            # Status indicator
            verbatimTextOutput("status_output"),
            
            br(),
            
            # Rendered Quarto output
            uiOutput("quarto_output")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values to store processing state
  values <- reactiveValues(
    pdf_files = character(0),
    processing = FALSE,
    output_file = NULL
  )
  
  # Check for PDF files in Norms&Standards folder on app start
  observe({
    if (dir.exists("Norms&Standards")) {
      pdf_files <- list.files("Norms&Standards", pattern = "\\.pdf$", full.names = TRUE)
      values$pdf_files <- pdf_files
    } else {
      values$pdf_files <- character(0)
    }
  })
  
  # Display list of PDF files
  output$pdf_files_list <- renderText({
    if (length(values$pdf_files) > 0) {
      paste("Found", length(values$pdf_files), "PDF files:")
      paste(basename(values$pdf_files), collapse = "\n")
    } else {
      "No PDF files found in 'Norms&Standards' folder. Please ensure the folder exists and contains PDF files."
    }
  })
  
  # Process documents when button is clicked
  observeEvent(input$process_docs, {
    
    # Validation checks
    if (length(values$pdf_files) == 0) {
      showNotification("No PDF files found to process!", type = "error")
      return()
    }
    
    if (trimws(input$keywords) == "") {
      showNotification("Please enter keywords to search for!", type = "error")
      return()
    }
    
    # Check if Google Gemini API key is available
    if (Sys.getenv("Google_Gemini_API_key") == "") {
      showNotification("Google Gemini API key not found! Please set 'Google_Gemini_API_key' environment variable.", type = "error")
      return()
    }
    
    # Set processing state
    values$processing <- TRUE
    
    # Create Output folder if it doesn't exist
    if (!dir.exists("Output")) {
      dir.create("Output")
    }
    
    # Process documents asynchronously
    future::future({
      
      tryCatch({
        
        # Initialize Google Gemini client
        client_query <- ellmer::chat_google_gemini
        
        # Configure client with API key
        client_query$configure(api_key = Sys.getenv("Google_Gemini_API_key"))
        
        # Prepare the search terms
        search_terms <- trimws(strsplit(input$keywords, ",")[[1]])
        search_terms <- search_terms[search_terms != ""]
        
        # Create the prompt
        prompt <- paste(
          "* Search and extract instances in the pdf files where a searched term (one word or string of words) is used",
          "* For each occurrence found,",
          "    - provide the chapter, section and page where the instance is found",
          "    - provide the paragraph where the instance is found", 
          "    - highlight the exact sentence where the instance is found in yellow",
          "* Provide your answer in form of a neat Quarto .qmd document",
          paste("* Search terms:", paste(search_terms, collapse = ", ")),
          sep = "\n"
        )
        
        # Read PDF files and send to Gemini
        pdf_contents <- list()
        for (pdf_file in values$pdf_files) {
          # Note: This is a simplified approach. In practice, you might need
          # to use a PDF reading library like pdftools to extract text first
          pdf_contents[[basename(pdf_file)]] <- pdf_file
        }
        
        # Send query to Gemini
        response <- client_query$chat(
          prompt = prompt,
          files = values$pdf_files
        )
        
        # Generate timestamp for unique filename
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        output_filename <- paste0("pdf_analysis_", timestamp, ".qmd")
        output_path <- file.path("Output", output_filename)
        
        # Save the response as a Quarto document
        writeLines(response$content, output_path)
        
        # Return the output file path
        output_path
        
      }, error = function(e) {
        return(paste("Error:", e$message))
      })
      
    }) %...>% (function(result) {
      
      values$processing <- FALSE
      
      if (grepl("^Error:", result)) {
        showNotification(result, type = "error", duration = 10)
        values$output_file <- NULL
      } else {
        values$output_file <- result
        showNotification("Documents processed successfully!", type = "success")
      }
      
    })
    
  })
  
  # Display processing status
  output$status_output <- renderText({
    if (values$processing) {
      "Processing documents with Google Gemini AI... This may take a few minutes."
    } else if (!is.null(values$output_file)) {
      paste("Analysis completed. Output saved to:", values$output_file)
    } else {
      "Ready to process documents. Click 'Process Documents' to start."
    }
  })
  
  # Render Quarto output
  output$quarto_output <- renderUI({
    
    if (!is.null(values$output_file) && file.exists(values$output_file)) {
      
      tryCatch({
        
        # Render Quarto document to HTML
        html_file <- gsub("\\.qmd$", ".html", values$output_file)
        
        # Render the Quarto document
        quarto::quarto_render(
          input = values$output_file,
          output_format = "html"
        )
        
        # Read the HTML content
        if (file.exists(html_file)) {
          html_content <- readLines(html_file, warn = FALSE)
          HTML(paste(html_content, collapse = "\n"))
        } else {
          # If HTML rendering fails, show the raw Quarto content
          qmd_content <- readLines(values$output_file, warn = FALSE)
          tags$pre(paste(qmd_content, collapse = "\n"))
        }
        
      }, error = function(e) {
        
        # If Quarto rendering fails, show the raw content
        if (file.exists(values$output_file)) {
          qmd_content <- readLines(values$output_file, warn = FALSE)
          tags$div(
            tags$h4("Raw Quarto Output (Rendering failed):"),
            tags$pre(paste(qmd_content, collapse = "\n"))
          )
        } else {
          tags$p("Output file not found.")
        }
        
      })
      
    } else {
      tags$p("No results to display yet.")
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)