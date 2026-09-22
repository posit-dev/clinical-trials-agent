library(shiny)
library(bslib)
library(commons)

addResourcePath("assets", "assets")

repository_url <- "https://github.com/posit-dev/tlg-agent"
commons_repository_url <- "https://github.com/posit-dev/commons"

welcome_message <- paste(
  "Ask **Clinical Trials Agent** about a simulated clinical study's safety, efficacy,",
  "survival, disposition, exposure, demographics, and laboratory results.",
  "Here are some example questions:\n\n",

  "- <span class='suggestion'>Show an adverse event overview by treatment arm.</span>\n",
  "- <span class='suggestion'>Show a Kaplan-Meier plot of overall survival by treatment arm.</span>\n",
  "- <span class='suggestion'>Summarize patient disposition for the safety population.</span>\n"
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
  showModal(
    modalDialog(
      tags$p(
        "This demo ",
        tags$a(
          "commons",
          href = commons_repository_url,
          target = "_blank",
          rel = "noopener noreferrer"
        ),
        " agent is no longer connected to a model provider."
      ),
      tags$p(
        "If you want to try it out, clone the ",
        tags$a(
          "repository",
          href = repository_url,
          target = "_blank",
          rel = "noopener noreferrer"
        ),
        " and supply your own API key!"
      ),
      footer = tags$a(
        "View the source code",
        href = repository_url,
        target = "_blank",
        rel = "noopener noreferrer",
        class = "btn btn-primary"
      ),
      size = "l",
      easyClose = FALSE
    )
  )
}

shinyApp(ui, server)
