# tlg-agent

An example [commons](https://github.com/posit-dev/commons) self-service data
agent for clinical trials data. It answers questions about a
simulated trials's safety, disposition, exposure, and demographics. As a commons agent, it first tries to answer user questions using validated code, called measures. For this agent, these measures are recipes from
[insightsengineering/tlg-catalog](https://github.com/insightsengineering/tlg-catalog). 

**Try it:** a published version runs on Posit Connect at
<https://connect.posit.it/content/04ee623c-3daf-4893-989e-7be4f2c0e7a7/>.

## Coverage

Standard questions are answered by measures; anything a measure doesn't cover
falls back to responsible SQL over the ADaM datasets.

**Adverse events — the full table and listing family** (the agent's safety focus):

| Catalog | Measure |
|---|---|
| AET01 | `ae_overview` |
| AET02 | `ae_by_soc_pt` |
| AET03 | `ae_by_intensity` |
| AET04 | `ae_by_grade` |
| AET04_PI | `ae_frequent_by_grade` |
| AET05 | `ae_incidence_rate` |
| AET06 | `ae_by_sex` |
| AET09 | `ae_related` |
| AET10 | `ae_most_frequent` |
| AEL01 | `ae_term_listing` |
| AEL02 | `ae_detail_listing` |
| AEL03 | `serious_ae_listing` |
| AEL04 | `patient_death_listing` |

**Other domains — representative core outputs:**

| Catalog | Measure |
|---|---|
| DMT01 | `demography` |
| DST01 | `disposition` |
| EXT01 | `exposure_summary` |
| DTHT01 | `deaths` |
| LBT04 | `lab_abnormalities` |

**Graphs** are plotting recipes in `context/graphs.md` (AE by system organ
class, exposure, age, and lab value distribution / over time).

## Attribution

The measures in `measures/` and the graph conventions in `context/graphs.md`
reproduce or adapt recipes from the
[insightsengineering/tlg-catalog](https://github.com/insightsengineering/tlg-catalog),
which is © 2023 F. Hoffmann-La Roche AG and licensed under the
[Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0).
Each measure's `@provenance` tag pins the exact catalog source file and commit
it was extracted from. See [`NOTICE`](NOTICE) for the full attribution.

Sample data comes from
[`random.cdisc.data`](https://insightsengineering.github.io/random.cdisc.data/)
(synthetic CDISC ADaM datasets, no real patient data).
