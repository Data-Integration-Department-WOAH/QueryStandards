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
              "### Instructions",
              "Based on the document text provided below, please complete the following tasks for the queried keyword:",
              
              "### Queried keyword:",
              "  <<KEYWORDS>> ",

              "### Tasks: ",
              "1. **Search and Extraction:**",
              "  - Locate instances of queried keyword or close synonyms within the PDF files.",
              
              "2. **Detailed Reporting for Each Keyword Instance Found:**",
                "- **Location Information:**",
                "- Identify and provide the chapter, section, and page number where the instance is located. If this information is absent, include the statement: 'Context not available.'",
              "- **Contextual Paragraph:**",
                "- Extract and present the full paragraph containing the instance, ensuring ",
                "- Do not include the instance itself in quotes as ```instance``` **under any circumstances**.",
                "- Highlight the identified keyword instance using Quarto's syntax: `<span style=\"background-color: yellow\"> keyword </span>`",

              "Please ensure clarity and precision in the results for improved readability and accuracy.\n",
              # "**Do not omit any keyword under any circumstance.**",
              sep=" \n "),
            
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
              "  - Use level-2 headers for each instance ",#queried term (e.g., `## Findings for 'notifiable disease'`).",
              "  - If no instances are found for a keyword, state that clearly under its header.",
              "  - Do not include a yaml header in the qmd file",
              "  - Do not add markup such as {.qmd} in the qmd file ",
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
