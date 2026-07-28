---
provenance: tlg-catalog graph conventions (BWG01 box plot) + house-style plotting; https://github.com/insightsengineering/tlg-catalog/tree/b3019fec92280384bac322680face57b2f685bfc/book/graphs (retrieved 2026-07-27)
---

# Plotting (graphs)

When a chart communicates the answer better than a table, render one with
`run_r`; plots are shown to the user. These are **rendering recipes** — the
governed numbers still come from a measure or a filtered query; the plot is a
presentation of them, so treat a chart as no more trusted than the data behind
it.

General flow: get the data first (a measure via `call_measure`, or a
`run_sql` query using the governed filters), which is stored under a handle
(`r1`, `r2`, ...); then call `run_r` on that handle to draw a `ggplot2` figure.
Create at most one figure per `run_r` call.

## Adverse events by system organ class

Patients with at least one analysis AE, by SOC and arm.

```r
# r1 <- run_sql:
#   SELECT ACTARM, AEBODSYS, COUNT(DISTINCT USUBJID) AS n_pt
#   FROM adae WHERE {{analysis_records}} GROUP BY ACTARM, AEBODSYS
library(ggplot2)
ggplot(r1, aes(x = reorder(AEBODSYS, n_pt), y = n_pt, fill = ACTARM)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = "System organ class", y = "Patients with ≥1 AE", fill = "Arm") +
  theme_minimal()
```

## Study drug exposure by arm

Distribution of total dose per patient, by arm (box plot; cf. tlg-catalog
BWG01).

```r
# r1 <- run_sql:
#   SELECT ACTARM, AVAL FROM adex WHERE PARCAT1 = 'OVERALL' AND PARAMCD = 'TDOSE'
library(ggplot2)
ggplot(r1, aes(x = ACTARM, y = AVAL, fill = ACTARM)) +
  geom_boxplot() +
  labs(x = "Treatment arm", y = "Total dose (mg)") +
  theme_minimal() +
  theme(legend.position = "none")
```

## Age distribution by arm

```r
# r1 <- run_sql:  SELECT ACTARM, AGE FROM adsl WHERE {{safety_population}}
library(ggplot2)
ggplot(r1, aes(x = ACTARM, y = AGE, fill = ACTARM)) +
  geom_boxplot() +
  labs(x = "Treatment arm", y = "Age (years)") +
  theme_minimal() +
  theme(legend.position = "none")
```

## Graphs that need data not loaded here

The catalog's marquee graphs rely on `tern`'s validated `g_*` functions over
datasets this agent does not currently expose, so they cannot be produced yet:

- Kaplan-Meier survival curves (`tern::g_km`, catalog KMG01) — need a
  time-to-event dataset (ADTTE).
- Subgroup forest plots (catalog FSTG01) — need efficacy/response data (ADRS).
- PK concentration/parameter plots (catalog PKCG/PKPG) — need PK data
  (ADPC/ADPP).

If one of these is requested, say it needs the corresponding dataset added to
the agent rather than approximating it from the data on hand.
