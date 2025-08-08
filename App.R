library(shiny)
library(ellmer)
library(pdftools)
library(quarto)
library(DT)

# --- Directory Setup ---
if (!dir.exists("Norms&Standards")) dir.create("Norms&Standards")
if (!dir.exists("Output")) dir.create("Output")

source("ui.r")
source("server.r")

# --- Run App ---
shinyApp(ui = ui, server = server) 
     



     