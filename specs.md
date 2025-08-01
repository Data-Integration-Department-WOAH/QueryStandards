I need an R/shiny app that loads pdf documents in to the R session , 
works as an AI assistant to interrogate (query keywords, expressions and close synonyms) 
and synthesize the pdf documents

Here are the technical specs:

# Package dependence

`ellmer, pdftools`

# UI

-   The pdf documents to be interrogated are to be found in a folder named `Norms&Standards`

-   Read the pdf file names contained in `Norms&Standards`

-   List of pdf files to be interrogated (found in folder `Norms&Standards`) in a text field

-   List of keywords, expressions queried should be passed by the user to the app through a text field

-   The latter text field should be pre-populated with dummy keywords ‘notify’, ‘notifiable disease’, ‘report’ and ‘reportable disease’

# Server

-   Load and concatenate the pdf files found in `Norms&Standards` into the current R session (use `pdftools::pdf_text()` )

-   Call Google Gemini through `client_query` defined as `client_query = ellmer::chat_google_gemini`

-   API key passed to `ellmer::chat_google_gemini` should be `api_key=Sys.getenv("Google_Gemini_API_key")`

-   The pdf documents should be passed to `client_query$chat`

-   The prompt passed to `client_query$chat` should be:

paste( "\* Search and extract instances in the pdf files where a searched term (one word or string of words) is used", "\* For each occurrence found,", " - provide the chapter, section and page where the instance is found" " - provide the paragraph where the instance is found" " - highlight the exact sentence where the instance is found in yellow" "\* Provide your answer in form of a neat Quarto .qmd document ")

-   The quarto document returned by Gemini should be saved in a folder named `Output`

-   Render the quarto document saved in "Output"
