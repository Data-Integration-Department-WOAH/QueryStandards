

library(shiny)
library(pdftools)
library(purrr)
library(stringr)
library(glue)
library(ellmer)
library(quarto)

# Define UI
ui <- fluidPage(
  
  titlePanel("PDF Query with Google Gemini"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput(
        inputId = "query_term",
        label = "Enter search term(s):",
        placeholder = "Type keyword(s) to search...",
        rows = 3
      ),
      
      fileInput(
        inputId = "pdf_file",
        label = "Upload PDF file:",
        accept = c(".pdf")
      ),
      
      textInput(
        inputId = "output_dir",
        label = "Output directory:",
        placeholder = "Specify a local folder path"
      ),
      
      selectInput(
        inputId = "model",
        label = "Choose model:",
        choices = c("gemini-1.5-flash", "gemini-2.5-pro-preview-06-05"),
        selected = "gemini-1.5-flash"
      ),
      
      selectInput(
        inputId = "output_format",
        label = "Select output format:",
        choices = c("html", "docx", "both"),
        selected = "html"
      ),
      
      actionButton(
        inputId = "run_query",
        label = "Run Query"
      ),
      
      hr(),
      tags$div(
        style = "color: red; font-weight: bold;",
        "⚠️ Warning: PDF content will be sent to Google Gemini for analysis. Do not upload confidential or sensitive documents."
      ),
      
      hr(),
      textOutput("page_count")
    ),
    
    mainPanel(
      h4("Query Results"),
      verbatimTextOutput("query_result")
    )
  )
)



# Define server
server <- function(input, output, session) {
  
  # Display number of pages
  output$page_count <- renderText({
    req(input$pdf_file)
    n_pages <- length(pdftools::pdf_text(input$pdf_file$datapath))
    paste("PDF contains", n_pages, "pages.")
  })
  
  # Reactive expression for running the query
  query_results <- eventReactive(input$run_query, {
    req(input$pdf_file, input$query_term, input$output_dir)
    
    pdf_path <- input$pdf_file$datapath
    query_term <- input$query_term
    model <- input$model
    out_dir <- normalizePath(input$output_dir, mustWork = FALSE)
    if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    # Function to query PDF with progress
    query_pdf <- function(pdf_path, query_term, model) {
      pdf_texts <- pdftools::pdf_text(pdf_path)
      n_pages <- length(pdf_texts)
      
      prompt_template <- function(page_text, page_number) {
        glue::glue("
You are an assistant specialised in analysing **Regulatory Animal Health** documents.  
Your task is to identify **all occurrences** of a given term or phrase and related expressions within the following PDF text.  

### Instructions:
1.  **Search** for the provided term: **{query_term}**.  
2. **Include synonyms and related meanings**, even if the exact word is not used.  
3. For each match:  
   - Provide the **exact text snippet** (at least one full sentence).  
   - Indicate the **page number** if available: {page_number}.
   - Highlight the **term or related expression** found.  
4. Keep answers **structured and concise**
5. Format your answer using Markdown with bullet points for each occurrence.

### Text (Page {page_number}):
{page_text}
")
      }
      
      results <- vector("list", n_pages)
      
      # Use progress bar
      withProgress(message = "Processing PDF pages...", value = 0, {
        for(i in seq_along(pdf_texts)) {
          page_text <- pdf_texts[[i]]
          prompt <- prompt_template(page_text, i)
          
          chat <- ellmer::chat_google_gemini(
            model = model,
            api_key = Sys.getenv("Google_Gemini_API_key")
          )
          results[[i]] <- chat$chat(prompt, echo = FALSE)
          
          # Increment progress
          incProgress(1/n_pages, detail = paste("Processing page", i, "of", n_pages))
        }
      })
      
      paste(unlist(results), collapse = "\n\n")
    }
    
    # Run query
    res_text <- query_pdf(pdf_path, query_term, model)
    
    # Write output
    out_base <- file.path(out_dir, "query_result")
    
    if(input$output_format %in% c("html", "both")) {
      out_md <- paste0(out_base, ".md")
      writeLines(res_text, out_md)
      quarto::quarto_render(out_md, output_format = "html", quiet = TRUE)
    }
    
    if(input$output_format %in% c("docx", "both")) {
      out_md <- paste0(out_base, ".md")
      writeLines(res_text, out_md)
      quarto::quarto_render(out_md, output_format = "docx", quiet = TRUE)
    }
    
    res_text
  })
  
  # Display results
  output$query_result <- renderText({
    query_results()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)