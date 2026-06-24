# Multimorbidity and health-related quality of life after TB treatment in Zimbabwe

Analysis code for:

> Calderwood CJ, Marambire ET, Musunzuru T, Madziva K, Kavenga F, Muringi E,
> Dixon J, Mutsvangwa J, Gregson CL, Ferrand RA, Fielding K, Kranzer K.
> *Multimorbidity and health-related quality of life after tuberculosis
> treatment in Zimbabwe: a cross-sectional study with comparison to household
> controls.*

A cross-sectional screening study (IMBA Hutano), nested within the ERASE-TB
cohort in Harare, Zimbabwe. Adults with recent pulmonary TB were offered an
integrated health check at/after treatment completion and compared with
household members without prior or current TB.

## Repository structure

```
.
├── run_all.R                   # runs the full pipeline end to end
├── R/
│   ├── 00_functions.R              # shared helper functions
│   ├── 01_data_preparation.R       # write minimal dataset (needs restricted data)
│   ├── 02_analysis.R               # main analysis (Tables 1–3, multimorbidity, HRQoL)
│   ├── 03_tables.R                 # main tables → Word document
│   ├── 02b_analysis_supplement.R   # supplementary analysis (S5, S8, S9, S10, S13, S14)
│   ├── 03b_tables_supplement.R     # supplementary tables → Word document
│   ├── 05_analysis_reviewer.R      # additional analyses requested at peer review
│   └── 05b_tables_reviewer.R       # reviewer-response tables → Word document
├── data/
│   └── imba_analysis_dataset.{Rds,csv}   # minimal, de-identified analysis dataset (377 × 59)
├── templates/                  # ERASE draft-styles reference (document theme)
├── output/                     # imba_tables.docx, imba_supplementary_tables.docx, figures
├── data_raw/                   # restricted source data (not shared) — see below
└── README.md
```

## Data availability

The analysis runs from a **minimal, de-identified dataset** containing only the
variables needed to reproduce the published tables (`data/imba_analysis_dataset.*`,
377 participants × 42 variables). It is available via LSHTM Data Compass:
<https://doi.org/10.17037/DATA.00004267>.

The individual-level **source data** (`data_raw/`: processed study data, the
ERASE-TB baseline file, and the spirometry quality-control workbook) are not
publicly shareable. `01_data_preparation.R` documents how the minimal dataset
is derived from them; it is provided for transparency and will not run without
restricted-access data.

> Direct identifiers have been removed and household identifiers re-coded
> (retained only to allow clustering). Exact age is retained because it is
> required for adjustment; users redistributing the data should confirm
> statistical-disclosure requirements.

## How to run

Tested with **R 4.5.0**. Required packages: `tidyverse`, `sandwich`, `lmtest`,
`gtsummary`, `MASS` (analysis); `readxl`, `haven`, `janitor`, `rspiro` (data
preparation only).

```r
# from the repository root
source("run_all.R")                    # full pipeline → both Word documents
# or step by step:
source("R/02_analysis.R");  source("R/03_tables.R")            # main tables
source("R/02b_analysis_supplement.R"); source("R/03b_tables_supplement.R")  # supplementary
```

Each analysis script (`02`, `02b`) prints a computed-vs-published check for every
estimate and statistical test; the table scripts (`03`, `03b`) render the
results to `output/imba_tables.docx` and `output/imba_supplementary_tables.docx`.

To rebuild the minimal dataset from restricted source data, place the source
files in `data_raw/` (or set the `IMBA_DATA_RAW` environment variable) and run
`source("R/01_data_preparation.R")`.

## Statistical methods

Each condition was compared between people with recent TB and household
comparators using logistic regression, a priori adjusted for **age** and
**sex**, with **household cluster-robust standard errors**. Age is a four-level
categorical variable (18–29, 30–39, 40–49, 50+); a linear age term is used for
diabetes, impaired lung function, underweight and anaemia to avoid
over-parameterisation. Age- and sex-adjusted predicted probabilities are
evaluated at balanced sex and the mean covariate profile (Table 3). HRQoL
(EQ-5D value) and physical activity (IPAQ-SF MET-minutes) were compared with
linear regression, with ordinal-logistic sensitivity analyses.

The published estimates were generated in **R 4.3.1 and Stata 18**; this
repository reproduces them in R, with cluster-robust standard errors
(`sandwich::vcovCL`) standing in for Stata's `vce(cluster …)`. Estimates agree
to rounding.

### Notes on two analysis choices
- **Multimorbidity** is defined as ≥2 of nine conditions. The **primary**
  analysis includes self-reported memory difficulty; a **sensitivity** analysis
  excludes it, as it rests on a single, unvalidated screening item. Both are
  reported by `02_analysis.R`.
- **Impaired lung function** is analysed in the n=39 people with recent TB (and
  n=155 comparators) who had good-quality (A–C grade) post-bronchodilator
  spirometry and complete screening data.

## Licence

Code released under the MIT Licence. The dataset is subject to the terms of the
LSHTM Data Compass record.
