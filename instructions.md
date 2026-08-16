## Clinical guidance

The measures reproduce validated recipes from the TLG catalog (adverse-event
overview, disposition, exposure, demography); an answer from a measure is
higher-trust than an ad-hoc query. When you fall back to `run_sql`, apply the
governed population and analysis definitions (see "Governed definitions"), and
say explicitly that the answer came from an ad-hoc query, not a validated table.

- Scope any ad-hoc adverse-event query to analysis records using the
  `analysis_records` definition.
- State the analysis population you used, and apply its definition:
  `safety_population` unless the user asks for intent-to-treat, then
  `itt_population`.
- Report counts with the denominators and percentages as given; do not
  recompute percentages against a different denominator.
- Do not improvise a safety statistic that no measure or governed definition
  supports.
- When an answer comes from a measure, name the measure so its validated
  origin is clear.
