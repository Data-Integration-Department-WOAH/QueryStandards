# Load necessary libraries
library(shiny)
library(ellmer)
library(fs)
library(tools)

# Define UI for the app
ui <- fluidPage(
  
  # App title
  titlePanel("AI Assistant for PDF Keyword Query and Synthesis"),
  
  # Layout with two input areas and one output viewer
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      # Show list of PDF files in the Norms&Standards folder
      textAreaInput("pdfList",
                    label = "PDF Files in 'Norms&Standards' folder:",
                    value = paste(dir("Norms&Standards", pattern = "\\.pdf$", full.names = FALSE), collapse = "\n"),
                    rows = 10,
                    readonly = TRUE),
      
      # Input field for keywords with pre-filled values
      textAreaInput("keywords",
                    label = "Enter keywords, expressions or synonyms (comma-separated):",
                    value = "notify, notifiable disease, report, reportable disease",
                    rows = 4),
      
      # Action button to trigger processing
      actionButton("runQuery", "Run Query and Generate Report")
    ),
    
    # Main panel to render the Quarto output
    mainPanel(
      h4("Rendered Quarto Report:"),
      uiOutput("qmdViewer")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$runQuery, {
    
    # Ensure the Output directory exists
    dir_create("Output")
    
    # List all pdf paths in Norms&Standards
    pdf_files <- dir("Norms&Standards", pattern = "\\.pdf$", full.names = TRUE)
    
    # Upload the pdfs to Google Gemini via ellmer
    uploaded_docs <- ellmer::google_upload(files = pdf_files)
    
    # Initialize Gemini client using environment API key
    client_query <- ellmer::chat_google_gemini(
      files = uploaded_docs,
      api_key = Sys.getenv("Google_Gemini_API_key")
    )
    
    # Build prompt as per specs
    prompt_text <- paste(
      "* Search and extract instances in the pdf files where a searched term (one word or string of words) is used",
      "* For each occurrence found,",
      "    - provide the chapter, section and page where the instance is found",
      "    - provide the paragraph  where the instance is found",
      "    - highlight the exact sentence where the instance is found in yellow",
      "* Provide your answer in form of a neat Quarto .qmd document ",
      sep = "\n"
    )
    
    # Run Gemini chat with the prompt
    response <- client_query$chat(prompt = prompt_text)
    
    # Define output Quarto filename
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_qmd <- file.path("Output", paste0("gemini_output_", timestamp, ".qmd"))
    
    # Save the content of the Quarto document
    writeLines(response, con = output_qmd)
    
    # Render Quarto as HTML to display (assuming quarto is installed and available)
    rendered_html <- file_path_sans_ext(output_qmd)
    system2("quarto", args = c("render", shQuote(output_qmd), "--output-dir", "Output"))
    
    # Update the UI to show the rendered HTML
    output$qmdViewer <- renderUI({
      tags$iframe(
        src = paste0("Output/", basename(rendered_html), ".html"),
        width = "100%",
        height = "800px",
        frameborder = 0
      )
    })
    
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
