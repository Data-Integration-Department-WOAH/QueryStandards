library(shiny)
library(ellmer)
library(pdftools)
library(quarto)
library(DT)


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
              "* Provide your answer in the form of a neat Quarto .qmd document following strictly the specifications below:",
              "  - Use level-1 headers for each queried term (e.g., `## Findings for 'notifiable disease'`).",
              "  - If no instances are found for a term, state that clearly under its header.",
              "  - The Quarto yaml must contain exactly the lines below:",
              "      title: \"Document Analysis\"",
              "      toc: true",
              "      toc-depth: 3",
              "      date: last-modified",
              "      number-sections: true",
              "      self-contained: true",
              
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
