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
> (retained only to allow clustering).
