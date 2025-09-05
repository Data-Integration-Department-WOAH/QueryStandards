# app.R

# ---- Packages ----
# install.packages(c("shiny","pdftools","purrr","stringr","glue","ellmer","fs"))
library(shiny)
library(pdftools)
library(purrr)
library(stringr)
library(glue)
library(ellmer)
library(fs)

# ---- Helpers ----
default_output_dir <- "./output"

# Prompt builder
build_prompt <- function(page_text, page_number, query_term) {
  glue("
You are an assistant specialised in analysing **Regulatory Animal Health** documents.  
Your task is to identify **all occurrences** of a given term, phrase, or related / related expressions within the following PDF text.  

### Instructions:
1.  **Search** for the provided term: **{query_term}**.  
2. **Include synonyms and related meanings**, even if the exact word is not used.  
3. For each match:  
   - Provide the **exact text snippet** (at least one full sentence).  
   - Indicate the **page number** if available: {page_number}.
   - Highlight the **term or related expression** found.  
4. Keep answers **structured and concise**
5. Format your answer using Markdown with bullet points for each occurrence.

---

### Text (Page {page_number}):
{page_text}
")
}

# Page-by-page Gemini query
query_pdf_pages <- function(pdf_path, query_term, model, progress_update = NULL) {
  pdf_texts <- pdftools::pdf_text(pdf_path)
  n <- length(pdf_texts)
  chat <- ellmer::chat_google_gemini(
    model = model,
    api_key = Sys.getenv("Google_Gemini_API_key")
  )
  
  results <- vector("list", n)
  for (i in seq_len(n)) {
    prompt <- build_prompt(pdf_texts[[i]], i, query_term)
    results[[i]] <- chat$chat(prompt, echo = FALSE)
    if (!is.null(progress_update)) progress_update(i, n)
  }
  paste(results, collapse = "\n\n")
}

# Ensure directory exists
ensure_dir <- function(path) {
  if (!dir_exists(path)) dir_create(path, recurse = TRUE)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

# Quarto check
has_quarto <- function() {
  if (requireNamespace("quarto", quietly = TRUE)) return(TRUE)
  nzchar(Sys.which("quarto"))
}

# ---- UI ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .warn-banner {background:#FFF3CD;border:1px solid #FFEC9F;padding:10px 12px;border-radius:10px;}
      .card {border:1px solid #eee;border-radius:14px;padding:16px;margin-bottom:16px;box-shadow:0 4px 12px rgba(0,0,0,0.04);}
      .muted {color:#666;}
    "))
  ),
  titlePanel("PDF Query (Gemini) – Regulatory Animal Health"),
  
  div(class = "warn-banner",
      tags$strong("⚠️ Warning: "),
      "PDF content will be sent to Google Gemini for analysis. Do not upload confidential or sensitive documents."
  ),
  
  fluidRow(
    column(
      width = 6,
      div(class = "card",
          h4("Inputs"),
          textAreaInput("keywords", "Search term(s)", rows = 3,
                        placeholder = "e.g. notify; movement restrictions; avian influenza"),
          fileInput("pdf", "Upload PDF file", accept = ".pdf"),
          textAreaInput("outdir", "Output directory",
                        value = default_output_dir, rows = 1),
          textAreaInput("outfile", "Output file name (without extension)",
                        value = "", rows = 1, placeholder = "Auto-generated from PDF + keywords + timestamp"),
          selectInput("model", "Gemini model",
                      choices = c("gemini-1.5-flash", "gemini-2.5-pro-preview-06-05")),
          selectInput("format", "Output format",
                      choices = c("html", "docx", "both"), selected = "html"),
          actionButton("run", "Run search", class = "btn btn-primary")
      )
    ),
    column(
      width = 6,
      div(class = "card",
          h4("PDF Info & Status"),
          p(strong("Pages:"), textOutput("page_count", inline = TRUE)),
          verbatimTextOutput("log", placeholder = TRUE),
          uiOutput("render_summary")
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Reactive: page count
  page_count <- reactive({
    req(input$pdf)
    info <- tryCatch(pdf_info(input$pdf$datapath), error = function(e) NULL)
    if (!is.null(info) && !is.na(info$pages)) info$pages else length(pdf_text(input$pdf$datapath))
  })
  output$page_count <- renderText({ req(input$pdf); page_count() })
  
  # Default output filename: PDF base + keyword + timestamp
  observeEvent(list(input$pdf, input$keywords), {
    req(input$pdf, input$keywords)
    base <- tools::file_path_sans_ext(input$pdf$name)
    kw <- str_replace_all(trimws(input$keywords), "\\s+", "_")
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    updateTextAreaInput(session, "outfile", value = paste(base, kw, ts, sep = "_"))
  }, ignoreInit = TRUE)
  
  # Log
  log_lines <- reactiveVal(character())
  append_log <- function(...) {
    msg <- paste0(format(Sys.time(), "%H:%M:%S"), " - ", paste0(..., collapse = ""))
    log_lines(c(log_lines(), msg))
  }
  output$log <- renderText(paste(log_lines(), collapse = "\n"))
  
  # Run search
  observeEvent(input$run, {
    req(input$pdf, input$keywords, input$outdir, input$outfile)
    
    api_key <- Sys.getenv("Google_Gemini_API_key")
    if (api_key == "") {
      showNotification("Google_Gemini_API_key is not set.", type = "error"); return()
    }
    
    outdir <- ensure_dir(input$outdir)
    outfile_base <- str_trim(input$outfile)
    md_path <- file.path(outdir, paste0(outfile_base, ".md"))
    
    total_pages <- page_count()
    
    withProgress(message = "Analyzing PDF with Gemini…", value = 0, {
      inc <- function(i, n) incProgress(1/n, detail = paste("Page", i, "of", n))
      append_log("Starting analysis on ", total_pages, " pages")
      
      res <- tryCatch(query_pdf_pages(
        pdf_path   = input$pdf$datapath,
        query_term = input$keywords,
        model      = input$model,
        progress_update = inc
      ), error = function(e) { append_log("ERROR: ", e$message); NULL })
      
      if (is.null(res)) return()
      
      header <- glue("# Query Results\n\n- **Source PDF:** {input$pdf$name}\n- **Pages:** {total_pages}\n- **Query:** `{input$keywords}`\n- **Model:** `{input$model}`\n- **Generated:** {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}\n\n---\n\n")
      writeLines(paste0(header, res), md_path)
      append_log("Markdown saved: ", md_path)
      
      if (has_quarto()) {
        if (input$format %in% c("html","both")) {
          quarto::quarto_render(md_path, output_format = "html", quiet = TRUE)
          append_log("HTML saved.")
        }
        if (input$format %in% c("docx","both")) {
          quarto::quarto_render(md_path, output_format = "docx", quiet = TRUE)
          append_log("DOCX saved.")
        }
      } else {
        append_log("Quarto not found – only Markdown written.")
      }
    })
    
    showNotification("Analysis complete.", type = "message")
  })
  
  # Planned output preview
  output$render_summary <- renderUI({
    req(input$outdir, input$outfile)
    outdir <- normalizePath(input$outdir, winslash = "/", mustWork = FALSE)
    tags$div(class = "muted",
             tags$p("Planned outputs:"),
             tags$ul(
               tags$li(file.path(outdir, paste0(input$outfile, ".md"))),
               if (input$format %in% c("html","both")) tags$li(file.path(outdir, paste0(input$outfile, ".html"))),
               if (input$format %in% c("docx","both")) tags$li(file.path(outdir, paste0(input$outfile, ".docx")))
             ))
  })
}

shinyApp(ui, server)
