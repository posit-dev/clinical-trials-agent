---
provenance: study conventions for the random.cdisc.data example (retrieved 2026-07-27)
---

# Study conventions

This is a single simulated oncology study from `random.cdisc.data`, used as a
public example. It is analysis-ready ADaM data — not a live, unlocked trial —
so there is no data-cut or freshness caveat to state.

- **Treatment arm** is `ACTARM` on every table, with three levels: `A: Drug X`,
  `B: Placebo`, `C: Combination`. `ARMCD` (`ARM A/B/C`) is the planned-arm code.
- **Trust order.** The validated measures reproduce reviewed TLG-catalog recipes
  and are the trusted path; answer standard safety, disposition, exposure, and
  demography questions from them. Ad-hoc SQL over the tables is a fallback — use
  it for questions the measures don't cover, and label it as lower-trust.
- **No unblinding data** is exposed, and there are no patient identifiers beyond
  the study subject id (`USUBJID`).
