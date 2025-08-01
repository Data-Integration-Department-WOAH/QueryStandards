# Prompt to Gemini
library(pacman)
p_load(tidyverse)
p_load(readxl)
p_load(stringr)
p_load(magrittr)
p_load(ellmer)
p_load(pdftools)
p_load(xml2)

models_google_gemini(
  base_url = "https://generativelanguage.googleapis.com/v1beta/",
  api_key = Sys.getenv("Google_Gemini_API_key"))



{
my_system_prompt = readLines("./my_system_prompt.md")
  




my_client <- chat_google_gemini(system_prompt = my_system_prompt ,
                                api_key=Sys.getenv("Google_Gemini_API_key"),
                                params = params(seed = 123),
                                # model = "gemini-2.5-flash",
                                model="gemini-2.5-pro-preview-06-05")

my_specs = readLines("./specs.md")
my_specs

res = my_client$chat(my_specs,echo=FALSE)

writeLines(res,"./App.R")

}











