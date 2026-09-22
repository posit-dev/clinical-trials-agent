# tlg-agent

## Project overview

Shiny app (Clinical Trials Agent) deployed to Posit Connect Cloud. Main entry point is `app.R`; supporting code in `R/`, `measures/`, `agent.R`. Dependencies are declared in `DESCRIPTION`.

## Deploying to Posit Connect Cloud

Redeploy with:

```r
rsconnect::deployApp(
  appDir = ".",
  appId = "01a04163-015f-f8f9-6f2a-af6d9a33e572",
  account = "posit",
  server = "connect.posit.cloud",
  forceUpdate = TRUE
)
```

Deployment record: `rsconnect/connect.posit.cloud/posit/tlg-agent.dcf`. Content URL: https://connect.posit.cloud/posit/content/01a04163-015f-f8f9-6f2a-af6d9a33e572
