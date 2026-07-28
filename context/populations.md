---
provenance: CDISC ADaM Implementation Guide; random.cdisc.data cadsl (retrieved 2026-07-27)
---

# Analysis populations

Analysis populations are defined by flags on `adsl`, one row per subject:

- **Safety (SAF)** — `SAFFL = 'Y'`. Subjects who received any study treatment.
  The standard population for adverse-event and exposure summaries.
- **Intent-to-treat (ITT)** — `ITTFL = 'Y'`. All randomized subjects, analyzed
  by planned treatment. The standard population for efficacy.

In this simulated study every subject is in both populations, so SAF and ITT
give the same denominators here; keep them distinct anyway, because in a real
study they differ.

Denominators come from the population, per treatment arm (`ACTARM`), not from
the number of records in a downstream table. A percentage in an adverse-event
table is patients-with-the-event over patients-in-the-arm, not over AE records.
