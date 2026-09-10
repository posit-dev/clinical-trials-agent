library(shiny)
library(bslib)
library(commons)

source("agent.R")

addResourcePath("assets", "assets")

welcome_message <- paste(
  "Ask **Clinical Trials Agent** about a clinical study's safety, disposition,",
  "exposure, demographics, and laboratory results. Answers use trusted measures",
  "or governed SQL over simulated CDISC ADaM data.\n\n",
  "Here are some example questions:\n\n",

  "- <span class='suggestion'>Show an adverse event overview by treatment arm.</span>\n",
  "- <span class='suggestion'>Summarize patient disposition for the safety population.</span>\n",
  "- <span class='suggestion'>What are the demographics of the study population?</span>\n"
)

ui <- shinychat::page_chat(
  title = "Clinical Trials Agent",
  icon = tags$img(
    src = "assets/logo-bird.png",
    height = "26px",
    alt = "",
    style = "display: block;"
  ),
  id = "chat",
  window_title = "Clinical Trials Agent",
  theme = commons::commons_theme(),
  greeting = welcome_message,
  pages_navbar = list(
    shinychat::chat_nav_panel(
      "About",
      includeMarkdown("about.md"),
      value = "about",
      content_width = "780px"
    )
  )
)

server <- function(input, output, session) {
  commons_server(
    "chat",
    new_tlg_agent(),
    history = shinychat::history_options(
      store = "memory",
      title = NULL
    )
  )
}

shinyApp(ui, server)
