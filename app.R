library(shiny)
library(bslib)
library(commons)

source("agent.R")

ui <- page_fillable(
  commons_ui("chat")
)

server <- function(input, output, session) {
  commons_server("chat", tlg_agent)
}

shinyApp(ui, server)
