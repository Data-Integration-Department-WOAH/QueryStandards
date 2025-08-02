# --- R/Shiny App: AI Assistant for PDF Document Interrogation ---

# This app allows users to interrogate a collection of PDF documents using an AI assistant (Google Gemini).
# It extracts text from PDFs, sends it to the AI with user-defined keywords,
# and renders the AI's response as a formatted report.

# --- 1. Package Dependencies ---
# Please ensure you have the following packages installed:
# install.packages(c("shiny", "ellmer", "pdftools", "quarto"))
# You must also have Quarto CLI installed on your system for rendering to work.
# Finally, set your Google Gemini API key as an environment variable:
# Sys.setenv(Google_Gemini_API_key = "YOUR_API_KEY_HERE") 

library(shiny)
library(ellmer)
library(pdftools)
library(quarto)

# --- 2. Setup: Create necessary directories if they don't exist ---
# The user must place their PDF files in the 'Norms&Standards' directory.
if (!dir.exists("Norms&Standards")) {
  dir.create("Norms&Standards")
}
# The app will save AI-generated reports in the 'Output' directory.
if (!dir.exists("Output")) {
  dir.create("Output")
}

# --- 3. UI Definition ---
ui <- fluidPage(
  titlePanel("AI Assistant for PDF Document Interrogation"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Configuration"),
      
      # Display the list of PDF files found in the directory
      h5("Detected PDF Files:"),
      verbatimTextOutput("file_list_output"),
      
      # Text area for user to input keywords, pre-populated with examples
      textAreaInput(
        inputId = "keywords_input",
        label = "Enter Keywords/Expressions (one per line):",
        value = "notify\nnotifiable disease\nreport\nreportable disease",
        rows = 5
      ),
      
      # Action button to start the analysis
      actionButton("run_query_button", "Run AI Analysis", icon = icon("robot"), width = "100%"),
      
      hr(),
      helpText(
        strong("Instructions:"),
        br(),
        "1. Place PDF files in the 'Norms&Standards' folder within the app's directory.",
        br(),
        "2. The app will list the detected PDFs above.",
        br(),
        "3. Enter keywords or phrases to search for.",
        br(),
        "4. Click 'Run AI Analysis' and wait for the report to be generated."
      )
    ),
    
    mainPanel(
      width = 9,
      h4("Analysis Report"),
      # UI Output to render the Quarto HTML document
      uiOutput("quarto_output")
    )
  )
)

# --- 4. Server Logic ---
server <- function(input, output, session) {
  
  # Reactive value to store the path to the rendered HTML report
  report_path <- reactiveVal(NULL)
  
  # --- UI Rendering for Input Panel ---
  
  # Reactive expression to get the list of PDF files
  pdf_files <- reactive({
    list.files(
      path = "Norms&Standards",
      pattern = "\\.pdf$",
      full.names = TRUE,
      ignore.case = TRUE
    )
  })
  
  # Render the list of detected file names
  output$file_list_output <- renderPrint({
    files <- basename(pdf_files())
    if (length(files) == 0) {
      cat("No PDF files found in 'Norms&Standards' folder.")
    } else {
      cat(files, sep = "\n")
    }
  })
  
  # --- Core Logic on Button Click ---
  
  observeEvent(input$run_query_button, {
    
    # --- 4a. Initial Checks ---
    api_key <- Sys.getenv("Google_Gemini_API_key")
    if (nchar(api_key) == 0) {
      showModal(modalDialog(
        title = "Error: API Key Not Found",
        "The 'Google_Gemini_API_key' environment variable is not set.",
        "Please set the environment variable and restart the R session.",
        easyClose = TRUE, footer = NULL
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
    
    # --- 4b. Processing with Progress Indicator ---
    withProgress(message = 'AI analysis in progress...', value = 0, {
      
      # Reset previous report to clear the display
      report_path(NULL)
      
      incProgress(0.1, detail = "Loading PDF documents...")
      
      # Load and concatenate text from all PDF files
     
      all_text <- tryCatch({
        text_list <- lapply(pdf_files(), pdftools::pdf_text)
        paste(unlist(text_list), collapse = "\n\n--- END OF PAGE ---\n\n")
      }, error = function(e) {
        showNotification(paste("Error reading PDF files:", e$message), type = "error")
        return(NULL)
      })
      
      if (is.null(all_text)) return()
      
      incProgress(0.2, detail = "Preparing prompt for AI...")
      
      # Define the prompt for the Gemini model
      # Note: We give a very structured prompt to guide the AI for better results.
      prompt <- paste(
        "Based on the document text provided below, perform the following tasks for the user's queried terms jointly:",
        "Queried Terms: \n", input$keywords_input,
        "\n\n--- INSTRUCTIONS ---",
        "\n* Search and extract instances of any of the queried terms (or any close synonym) in the pdf files.",
        "\n* For each instance found:",
        "  - provide the chapter, section and page where the instance is found. If this information is not in the text, state 'Context not available'.",
        "  - provide the full paragraph where the instance is found.",
        "  - highlight the instance found using Quarto's highlight syntax, like this:  '**This is the highlighted instance**'  ",
        "\n* Provide your answer in the form of a neat Quarto .qmd document.",
        "   - The Quarto doc should contained  toc:true in the yaml ",
        "  - Use a level-1 header for the main title (e.g., `# Document Analysis`).",
        " -  Make sure the Quarto document generated a slef-contained html file",
        "  - Use level-2 headers for each queried term (e.g., `## Findings for 'notifiable disease'`).",
        "  - If no instances are found for a term, state that clearly under its header.",
        "\n\n--- DOCUMENT TEXT ---",
        "\n", all_text
      )
      
      incProgress(0.3, detail = "Querying Google Gemini AI...")
      
      # Call Gemini API
      ai_response <- tryCatch({
        client_query <- ellmer::chat_google_gemini(api_key = api_key)
        client_query$chat(prompt,echo=FALSE)
      }, error = function(e) {
        showNotification(paste("AI API Error:", e$message), type = "error")
        return(NULL)
      })
      
      if (is.null(ai_response)) return()
      
      incProgress(0.8, detail = "Saving and rendering report...")
      
      # Define file paths for the Quarto document and its HTML output
      qmd_path <- file.path("Output", "ai_report.qmd")
      html_path <- file.path("Output", "ai_report.html")
      
      # Clean the AI response to remove potential code fences
      clean_response <- gsub("(^```\\w*\\s*|```$)", "", ai_response)
      writeLines(clean_response, qmd_path)
      
      # Render the QMD file to HTML using the quarto package
      quarto::quarto_render(qmd_path, output_format = "html", quiet = TRUE)
      
      # Set the reactive value to the path of the generated HTML file
      if (file.exists(html_path)) {
        report_path(html_path)
        incProgress(1, detail = "Done!")
        showNotification("Analysis complete! Report generated.", type = "message")
      } else {
        showNotification("Error: Failed to render the Quarto report.", type = "error")
      }
    })
  })
  
  # --- UI Rendering for Output Panel ---
  
  # Render the final HTML report in an iframe
  output$quarto_output <- renderUI({
    path <- report_path()
    if (!is.null(path) && file.exists(path)) {
      # Add a random query parameter to the URL to force browser refresh
      # This is crucial if the user runs the analysis multiple times
      url_with_cache_buster <- paste0("Output/", basename(path), "?", as.integer(Sys.time()))
      tags$iframe(
        src = url_with_cache_buster,
        width = '100%',
        height = '800px',
        style = "border: 1px solid #ddd;"
      )
    } else {
      # Default message before any analysis is run
      HTML("<div style='text-align: center; color: grey; margin-top: 50px;'>
             <p>The generated report will be displayed here.</p>
           </div>")
    }
  })
  
  # To serve the generated HTML file, we must register the 'Output' directory
  addResourcePath("Output", "Output")
  
}

# --- 5. Run the Application ---
shinyApp(ui = ui, server = server)

