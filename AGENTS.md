# tlg-agent

## Project overview

Shiny app (Clinical Trials Agent) deployed to Posit Connect Cloud. Main entry point is `app.R`; supporting code in `R/`, `measures/`, `agent.R`. Dependencies managed with renv (`renv.lock`).

## Deploying to Posit Connect Cloud

Redeploy with:

```r
rsconnect::deployApp(
  appDir = ".",
  appId = "01a04163-015f-f8f9-6f2a-af6d9a33e572",
  account = "posit",
  server = "connect.posit.cloud",
  forceUpdate = TRUE,
  python = "/Users/saraa/.pyenv/versions/3.12.7/bin/python3"
)
```

Note: `python` must be passed explicitly. `renv.lock` includes reticulate (only as a suggested dependency of other packages), which triggers rsconnect's Python environment detection; without an explicit `python` path, deployment fails with "Failed to detect python environment using 'managed'". The app itself does not use Python.

Deployment record: `rsconnect/connect.posit.cloud/posit/tlg-agent.dcf`. Content URL: https://connect.posit.cloud/posit/content/01a04163-015f-f8f9-6f2a-af6d9a33e572
