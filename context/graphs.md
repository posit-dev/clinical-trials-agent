---
provenance: tlg-catalog graph conventions (BWG01 box plot) + house-style plotting; https://github.com/insightsengineering/tlg-catalog/tree/b3019fec92280384bac322680face57b2f685bfc/book/graphs (retrieved 2026-07-27)
---

# Plotting (graphs)

When a chart communicates the answer better than a table, render one with
`run_r`; plots are shown to the user. A chart is presentation — treat it as no
more trusted than the data behind it.

Flow: get the data first (call a relevant measure, or run a query that applies
the governed population/analysis definitions as `{{name}}` tokens), which is
stored under a handle (`r1`, `r2`, ...); then call `run_r` on that handle to
draw a `ggplot2` figure. One figure per `run_r` call.

The recipes below assume the plotting data is in `r1`; each notes the shape it
expects.

## Adverse events by system organ class

Data: one row per arm × SOC, with a patient count `n_pt` (analysis records).

```r
library(ggplot2)
ggplot(r1, aes(x = reorder(AEBODSYS, n_pt), y = n_pt, fill = ACTARM)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = "System organ class", y = "Patients with ≥1 AE", fill = "Arm") +
  theme_minimal()
```

## Study drug exposure by arm

Data: total dose per patient — `AVAL` for `PARAMCD = 'TDOSE'`, `PARCAT1 = 'OVERALL'` — with `ACTARM`.

```r
library(ggplot2)
ggplot(r1, aes(x = ACTARM, y = AVAL, fill = ACTARM)) +
  geom_boxplot() +
  labs(x = "Treatment arm", y = "Total dose (mg)") +
  theme_minimal() +
  theme(legend.position = "none")
```

## Age distribution by arm

Data: `AGE` and `ACTARM`, one row per subject.

```r
library(ggplot2)
ggplot(r1, aes(x = ACTARM, y = AGE, fill = ACTARM)) +
  geom_boxplot() +
  labs(x = "Treatment arm", y = "Age (years)") +
  theme_minimal() +
  theme(legend.position = "none")
```

## Laboratory values: distribution by arm

Data: `AVAL` and `ACTARM` for one lab test (`PARAMCD`, e.g. `'ALT'`) at one
visit (`AVISIT`); label the axis with the parameter and its unit (`AVALU`). (cf. catalog BWG01.)

```r
library(ggplot2)
ggplot(r1, aes(x = ACTARM, y = AVAL, fill = ACTARM)) +
  geom_boxplot() +
  labs(x = "Treatment arm", y = "ALT (U/L)") +
  theme_minimal() +
  theme(legend.position = "none")
```

## Laboratory values over time

Data: mean `AVAL` per `AVISIT` × `ACTARM` for one lab test (order visits by
`AVISITN`). (cf. catalog LTG01 / MNG01; `tern::g_lineplot` is the validated equivalent.)

```r
library(ggplot2)
ggplot(r1, aes(x = reorder(AVISIT, AVISITN), y = mean_aval, colour = ACTARM, group = ACTARM)) +
  geom_line() +
  geom_point() +
  labs(x = "Visit", y = "Mean ALT (U/L)", colour = "Arm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

## Graphs that need data not loaded here

The catalog's marquee graphs rely on `tern`'s `g_*` functions over datasets this
agent does not currently expose, so they cannot be produced yet:

- Kaplan-Meier survival curves (`tern::g_km`, catalog KMG01) — need a
  time-to-event dataset (ADTTE).
- Subgroup forest plots (catalog FSTG01) — need efficacy/response data (ADRS).
- PK concentration/parameter plots (catalog PKCG/PKPG) — need PK data
  (ADPC/ADPP).

If one of these is requested, say it needs the corresponding dataset added to
the agent rather than approximating it from the data on hand.
