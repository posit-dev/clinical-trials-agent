# Drives the deployed tlg-agent on Connect through headless Chrome so that
# synthetic conversations generate real trajectories in the content's own
# trace store. Each conversation runs in a fresh browser session, which gives
# it a fresh commons conversation id.

library(chromote)

app_url <- "https://connect.posit.it/tlg-agent/"
api_key <- Sys.getenv("CONNECT_API_KEY")
stopifnot(nzchar(api_key))

eval_js <- function(b, js) {
  result <- b$Runtime$evaluate(js, returnByValue = TRUE)
  result$result$value
}

wait_until <- function(b, js, timeout = 60, interval = 0.5, what = js) {
  deadline <- Sys.time() + timeout
  while (Sys.time() < deadline) {
    ok <- tryCatch(eval_js(b, js), error = function(e) NULL)
    if (isTRUE(ok)) {
      return(invisible(TRUE))
    }
    Sys.sleep(interval)
  }
  stop("Timed out waiting for: ", what)
}

transcript_length <- function(b) {
  len <- eval_js(
    b,
    "(document.querySelector('shiny-chat-container') || document.body).innerText.length"
  )
  if (is.null(len)) 0 else len
}

# Wait until the transcript holds still for `quiet` seconds; used to let the
# streamed greeting finish before the first question.
await_settled <- function(b, quiet = 6, timeout = 60) {
  deadline <- Sys.time() + timeout
  last <- transcript_length(b)
  stable_since <- Sys.time()
  while (Sys.time() < deadline) {
    Sys.sleep(2)
    now <- transcript_length(b)
    if (now != last) {
      last <- now
      stable_since <- Sys.time()
    } else if (as.numeric(Sys.time() - stable_since, units = "secs") >= quiet) {
      return(invisible(TRUE))
    }
  }
  invisible(FALSE)
}

# An answer counts as finished once the transcript has grown past its
# pre-question size and then held still for `quiet` seconds. Streamed answers
# and tool cards keep the transcript moving until the turn completes.
await_answer <- function(b, before, quiet = 12, timeout = 300) {
  deadline <- Sys.time() + timeout
  grew <- FALSE
  while (Sys.time() < deadline) {
    if (transcript_length(b) > before) {
      grew <- TRUE
      break
    }
    Sys.sleep(1)
  }
  if (!grew) {
    warning("No answer appeared within ", timeout, "s.")
    return(invisible(FALSE))
  }
  last <- transcript_length(b)
  stable_since <- Sys.time()
  while (Sys.time() < deadline) {
    Sys.sleep(2)
    now <- transcript_length(b)
    if (now != last) {
      last <- now
      stable_since <- Sys.time()
    } else if (as.numeric(Sys.time() - stable_since, units = "secs") >= quiet) {
      return(invisible(TRUE))
    }
  }
  warning("Answer did not settle within ", timeout, "s; moving on.")
  invisible(FALSE)
}

ask <- function(b, question) {
  cat(format(Sys.time(), "%H:%M:%S"), "Q:", question, "\n")
  before <- transcript_length(b)
  eval_js(b, sprintf(
    "(function() {
       const input = document.querySelector('[id$=_user_input]');
       window.Shiny.setInputValue(
         input.id + ':shinychat.userInput',
         %s,
         {priority: 'event'}
       );
     })()",
    jsonlite::toJSON(question, auto_unbox = TRUE)
  ))
  await_answer(b, before)
}

run_conversation <- function(questions) {
  b <- ChromoteSession$new()
  b$default_timeout <- 90
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  b$Network$enable()
  b$Network$setExtraHTTPHeaders(
    headers = list(Authorization = paste("Key", api_key))
  )
  b$Page$enable()
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(app_url, wait_ = FALSE)
  b$wait_for(loaded)
  wait_until(
    b,
    "typeof window.Shiny !== 'undefined' &&
       !!document.querySelector('[id$=_user_input]') &&
       !!(window.Shiny.shinyapp && window.Shiny.shinyapp.isConnected &&
          window.Shiny.shinyapp.isConnected())",
    what = "Shiny session to connect"
  )
  await_settled(b)
  for (question in questions) {
    ask(b, question)
  }
  # Give the batch span exporter time to flush before the session closes.
  Sys.sleep(15)
  cat("-- conversation done --\n")
}

conversations <- list(
  c(
    "Show an adverse event overview by treatment arm.",
    "Now show it for the intent-to-treat population instead."
  ),
  "Summarize patient disposition for the safety population.",
  "What are the demographics of the study population?",
  "How many patients in each treatment arm experienced more than 5 adverse events? Please quote the exact counts.",
  "What is the median age of patients who discontinued the study due to an adverse event?",
  "What kinds of questions can you answer about this study?",
  c(
    "Which system organ class accounts for the most serious adverse events?",
    "Break that down by treatment arm."
  ),
  c(
    "Summarize study drug exposure by treatment arm.",
    "Which arm received the highest total dose on average?",
    "Thanks! Is there anything unusual in this study's safety profile I should look into?"
  )
)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  conversations <- conversations[as.integer(args)]
}

for (i in seq_along(conversations)) {
  cat("== conversation", i, "of", length(conversations), "==\n")
  tryCatch(
    run_conversation(conversations[[i]]),
    error = function(e) cat("conversation failed:", conditionMessage(e), "\n")
  )
}
