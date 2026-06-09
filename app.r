library(shiny)
devtools::load_all(".")

source("shiny/app.R")

shinyApp(ui, server)
