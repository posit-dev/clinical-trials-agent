library(shiny)
library(bslib)
library(commons)

source("agent.R")

addResourcePath("assets", "assets")

welcome_message <- paste(
  "Ask the **TLG agent** about a clinical study's safety, disposition,",
  "exposure, and demographics. Answers are computed with validated code over",
  "simulated CDISC ADaM data.\n\nHere are some example questions:\n\n",

  "- <span class='suggestion'>Show an adverse event overview by treatment arm.</span>\n",
  "- <span class='suggestion'>Summarize patient disposition for the safety population.</span>\n",
  "- <span class='suggestion'>What are the demographics of the study population?</span>\n"
)

ui <- page_navbar(
  title = tags$span(
    style = "display: inline-flex; align-items: center; gap: 0.65rem;",
    tags$img(
      src = "assets/logo-bird.png",
      height = "42px",
      alt = "tlg agent",
      style = "display: block;"
    ),
    tags$span("tlg agent", style = "font-weight: 300; font-size: 1.4rem;")
  ),
  window_title = "Clinical trials TLG agent",
  fillable = "Chat",
  navbar_options = navbar_options(collapsible = FALSE),
  nav_panel(
    title = "Chat",
    commons_ui("chat", greeting = welcome_message)
  ),
  nav_panel(
    title = "About",
    div(
      style = "max-width: 780px; margin: 0 auto; padding: 1.5rem 1.5rem 3rem;",
      includeMarkdown("about.md")
    )
  )
)

server <- function(input, output, session) {
  commons_server("chat", tlg_agent)
}

shinyApp(ui, server)
