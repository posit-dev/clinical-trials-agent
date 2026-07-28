# tlg-agent

An example [commons](https://github.com/posit-dev/commons) self-service data
agent for the pharma clinical-trials space. It answers questions about a
study's safety, disposition, exposure, and demographics by running the *same
validated code a statistical programmer would* — recipes from the public
[insightsengineering/tlg-catalog](https://github.com/insightsengineering/tlg-catalog)
(the NEST `tern`/`rtables` TLG — Tables, Listings, and Graphs — catalog) over
CDISC ADaM sample data from
[`random.cdisc.data`](https://insightsengineering.github.io/random.cdisc.data/).

It is a public, self-contained example of how to build a commons agent in a
regulated, high-trust domain — not a tool for running real trials.

## How it works

The agent gives the model three tiers of trust:

1. **Data source + dictionary** — the three ADaM tables (`adsl`, `adae`,
   `adex`) are loaded into an in-process DuckDB, described by
   `dictionaries/adam.data-dict.yaml` in the
   [data-dict.yaml](https://data-dict.tidyverse.org/) format. The dictionary
   also defines governed **filters** — `safety_population`, `itt_population`,
   `analysis_records` — that keep the fallback path honest.
2. **Semantic layer (the happy path)** — `measures/measures.R` holds validated
   measures, each an extraction of a tlg-catalog recipe (`tern`/`rtables`) that
   returns a tidy data frame. This is what standard questions are answered from.
3. **Context layer** — `context/*.md` explains analysis populations, the AE
   coding hierarchy, and study conventions, so that ad-hoc fallback SQL (for
   questions no measure covers) uses the right population and is clearly marked
   lower-trust.

### Measures

| Measure | tlg-catalog recipe | Tables |
|---|---|---|
| `ae_overview` | AET01 — overview of deaths and adverse events | adsl, adae |
| `disposition` | DST01 — patient disposition | adsl |
| `exposure_summary` | EXT01 — study drug exposure | adsl, adex |
| `demography` | DMT01 — demographics and baseline characteristics | adsl |

Each measure's roxygen block carries an `@provenance` tag pinning the exact
catalog source it was extracted from.

**A note on fidelity.** Several catalog recipes synthesize demo variables
in-line (e.g. AET01 samples `AESDTH`/`AEACN`; EXT01 samples treatment duration
and missed doses). Those rows are deliberately omitted here — every measure
computes only from real ADaM columns, so a governed answer never rests on demo
scaffolding.

## Setup

Requires a recent R (developed on 4.6) and an Anthropic API key.

```r
# commons (dictionary-authored-definitions branch) and the chat UI
pak::pak(c("posit-dev/commons@definitions-6", "posit-dev/shinychat/pkg-r"))

# the TLG stack (tern/rtables from CRAN; random.cdisc.data from R-universe)
install.packages(c("tern", "rtables"))
install.packages(
  "random.cdisc.data",
  repos = c("https://insightsengineering.r-universe.dev", "https://cloud.r-project.org")
)

# optional: local trajectory logging (log = TRUE in agent.R)
install.packages("otelsdk")
```

## Run

```r
Sys.setenv(ANTHROPIC_API_KEY = "...")

# as a Shiny chat app
shiny::runApp()

# or drive the agent directly
source("agent.R")
tlg_agent$chat("How many patients had at least one serious adverse event, by arm?")
```

Call a measure on its own to see its tidy output:

```r
source("measures/measures.R")
ae_overview("SAF")
```

## Layout

```
agent.R                    commons() assembly
app.R                      Shiny chat UI
R/data.R                   loads the ADaM tables and builds the DuckDB source
dictionaries/
  adam.data-dict.yaml      ADaM tables/columns + governed filter definitions
measures/measures.R        the validated measures (tern/rtables -> tidy)
context/*.md               analysis populations, AE conventions, study notes
system-prompt.md           analyst persona and tool-ordering guidance
```
