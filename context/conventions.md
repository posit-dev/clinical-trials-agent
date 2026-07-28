---
sources:
  - "random.cdisc.data provides synthetic CDISC ADaM datasets (no real patient data): https://insightsengineering.github.io/random.cdisc.data/"
  - "Choice of analysis set (safety for AE/exposure, ITT for efficacy): ICH E9 section 5 — https://database.ich.org/sites/default/files/E9_Guideline.pdf"
  - "AE percentages use the arm population as denominator, not the record count: TLG Catalog AET01 + tern — https://insightsengineering.github.io/tlg-catalog/stable/tables/adverse-events/aet01.html"
  - "Tables, listings, and graphs make up a clinical study report: ICH E3 — https://database.ich.org/sites/default/files/E3_Guideline.pdf"
  - "TLG Catalog / tern / rtables (the reference open-source NEST implementations the measures reproduce): https://insightsengineering.github.io/tlg-catalog/stable/ , https://insightsengineering.github.io/tern/ , https://insightsengineering.github.io/rtables/"
  - "Trust order (prefer measures over ad-hoc SQL): project convention"
---

# Study conventions

This is a single simulated oncology study from `random.cdisc.data`, used as a
public example: synthetic, analysis-ready ADaM data — not a live, unlocked
trial — so there is no data-cut or freshness caveat, and no unblinding data or
patient identifiers beyond the subject id (`USUBJID`).

The columns, their controlled terminology, the cross-table rules, and the
governed filters are all documented in the data dictionary; this file covers
only what the dictionary doesn't.

## Trust order

The measures reproduce recipes from the public TLG Catalog — the reference
open-source (NEST) implementations built on `tern`/`rtables` — and are the
trusted path: answer standard safety, disposition, exposure, and demography
questions from them. Ad-hoc SQL over the tables is a fallback: use it only for
questions the measures don't cover, and say plainly when an answer came from
one rather than from a measure.

## Analysis conventions

- Use the safety population for adverse-event and exposure summaries, and
  intent-to-treat for efficacy; state which population an answer uses.
- A percentage is taken over the arm population — patients with the event
  divided by patients in the arm — not over the number of records.

Tables, listings, and graphs (TLG) are the standard statistical outputs of a
clinical study report (ICH E3).
