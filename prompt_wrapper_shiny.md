# Prompt for gpt5 to develop a shiny app

* Your are a professional R programmer and Shiny app developer with a flare for UI/UX design. 

* You have been hired to create a Shiny app that wraps the R code below

```r
# Install packages if needed
# install.packages(c("ellmer", "pdftools", "purrr", "stringr"))

library(ellmer)
library(pdftools)
library(purrr)
library(stringr)
library(glue)



# Function to query a PDF with Gemini
query_pdf <- function(pdf_path, query_term, model = "gemini-1.5-flash") {
  
  # Extract text by page
  pdf_texts <- pdftools::pdf_text(pdf_path)
  
  # Build prompt template
  prompt_template <- function(page_text, page_number) {
    glue::glue("
You are an assistant specialised in analysing **Regulatory Animal Health** documents.  
Your task is to identify **all occurrences** of a given term, phrase, or related  / related expressions within the following PDF text.  

### Instructions:
1.  **Search** for the provided term: **{query_term}**.  
2. **Include synonyms and related meanings**, even if the exact word is not used.  
3. For each match:  
   - Provide the **exact text snippet** (at least one full sentence).  
   - Indicate the **page number** if available: {page_number}.
   - Highlight the **term or related expression** found.  
4. Keep answers **structured and concise**
5. Format your answer using Markdow with bullet points for each occurrence.

### Examples:

**Query:** \"Avian Influenza\"  
**Expected Output:**  
- Page 1: No relevant occurrences found.  
- Page 2: No relevant occurrences found.  
- Page 3: *\"The World Organisation for Animal Health (WOAH) requires immediate notification of **avian influenza** outbreaks.\"*  
- Page 4: *\"Surveillance systems for **HPAI** (highly pathogenic avian influenza) must be implemented by all member states.\"*  

**Query:** \"Movement restrictions\"  
**Expected Output:**  
- Page 1: *\"The competent authority may impose **movement restrictions** on poultry during outbreaks.\"*  
- Page 2: No relevant occurrences found.  
- Page 3: *\"Authorities may enforce **animal transport bans** in affected zones.\"*  

---


### Text (Page {page_number}):
{page_text}
")
  }
  
  
  # Iterate over pages
  results <- map2(pdf_texts, seq_along(pdf_texts), 
                  function(page_text, page_number)                    
                  {
                    prompt <- prompt_template(page_text, page_number)
                    print(c("page number",page_number))
                    chat = ellmer::chat_google_gemini(
                      model = model,
                      api_key=Sys.getenv("Google_Gemini_API_key"))
                    return(chat$chat(prompt,echo=FALSE) )
                  }
  )
  
  # Combine results
  results_clean <- paste(results, collapse = "\n\n")
  return(results_clean)
}

# Calling function
result <- query_pdf(pdf_path="./Norms&Standards/en_csatvol1_2024_pp23-50.pdf", 
                   model = "gemini-2.5-pro-preview-06-05",
                    query_term="notify")

outpath = "./Output/query_no_ui_result.md"
writeLines(result,outpath)
quarto::quarto_render(outpath, output_format = "html", quiet = TRUE)
```

* The app should have the following features:

   + a textAreaInput to enter the searched keywords
   + a widget to browse the local filesystem and upload a PDF file
   + a widget to browse the local filesystem and specify the directory to store the ouput file 
   + textAreaInput to enter the name of the output file (without extension)
   + counter to show the number of pages in the uploaded PDF
   + a progress bar to show the progress of the analysis
   + a selectInput to choose the model (with options "gemini-1.5-flash" and "gemini-2.5-pro-preview-06-05")
   + an actionButton to trigger the search
   + a selectInput to choose the output format (with options "docx", "html", both)
   + a a warning text along the line of "⚠️ Warning: PDF content will be sent to Google Gemini for analysis. Do not upload confidential or sensitive documents."