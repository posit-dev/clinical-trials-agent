---
provenance: CDISC ADaM ADAE conventions; tlg-catalog adverse-events recipes (retrieved 2026-07-27)
---

# Adverse events (adae)

One row per adverse-event record. Adverse-event summaries are computed on
**analysis records only** (`ANL01FL = 'Y'`), which are the treatment-emergent
set — events with onset on or after the first dose. Always apply that filter.

## Coding hierarchy (MedDRA)

Events are coded on a hierarchy; two levels matter here:

- **System Organ Class (SOC)** — `AEBODSYS`, the broad body-system grouping.
- **Preferred Term (PT)** — `AEDECOD`, the specific term below the SOC.

"By SOC and preferred term" means grouping by `AEBODSYS`, then `AEDECOD` within it.

## Severity, seriousness, relationship

These are distinct concepts — do not conflate them:

- **Severity / intensity** — `AESEV` (MILD / MODERATE / SEVERE): how intense the
  event was.
- **Toxicity grade** — `AETOXGR` (NCI-CTCAE 1–5): 3–5 is the usual "high grade"
  cutoff, 5 is fatal.
- **Serious** — `AESER = 'Y'`: met a regulatory seriousness criterion (a formal
  status, not the same as "severe").
- **Related** — `AEREL = 'Y'`: assessed as related to study treatment.

An event can be serious without being severe, and vice versa.

## Counting

Two different counts answer different questions: number of **patients** with at
least one qualifying event (count distinct `USUBJID`) versus number of
**events** (count records). Patient counts carry a percentage of the arm's
population; event counts do not.
