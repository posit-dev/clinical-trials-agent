library(commons)

source("R/data.R")

adam <- adam_data()

new_tlg_agent <- function() {
  commons(
    client = ellmer::chat_anthropic(model = "claude-sonnet-5"),
    data_sources = list(
      adam = data_source(
        adsl = adam$adsl,
        adae = adam$adae,
        adex = adam$adex,
        adlb = adam$adlb,
        adaette = adam$adaette,
        dictionary = "dictionaries/adam.data-dict.yaml"
      )
    ),
    semantic_layer = semantic_layer("measures"),
    context_layer = context_layer(list.files("context", full.names = TRUE)),
    system_prompt = ellmer::interpolate_file(
      "system-prompt.md",
      date = Sys.Date()
    ),
    log = TRUE
  )
}
