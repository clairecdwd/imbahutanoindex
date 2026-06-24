# =============================================================================
# IMBA Hutano - Multimorbidity and HRQoL after TB treatment in Zimbabwe
# 01: DATA PREPARATION
# -----------------------------------------------------------------------------
# Reads the curated analysis dataset (IMBA_indexes.dta) and writes a minimal, de-identified analysis dataset
# containing only the variables needed to reproduce the published tables.
#
# IMBA_indexes.dta is RESTRICTED individual-level data and is not publicly
# shareable. Place it in data_raw/ (or point IMBA_DATA_RAW at its location).
# The minimal dataset is used for subsequent analysis and is
# available via LSHTM Data Compass (DOI 10.17037/DATA.00004267).
#
# Author: Claire Calderwood
# Reviewed: June 2026
# =============================================================================

library(tidyverse)
library(haven)
library(rspiro)   # GLI reference-equation z-scores

# Source data location (restricted). Override with: Sys.setenv(IMBA_DATA_RAW=...)
data_raw <- Sys.getenv("IMBA_DATA_RAW", "data_raw/")

# -----------------------------------------------------------------------------
# 1. Read the curated analysis dataset
# -----------------------------------------------------------------------------
analysis <- read_dta(paste0(data_raw, "IMBA_indexes.dta")) %>%
  mutate_if(haven::is.labelled, haven::as_factor)

cat(sprintf("IMBA_indexes.dta: %d participants (TB %d, comparators %d)\n",
            nrow(analysis), sum(analysis$ie_ptype_bin == 1), sum(analysis$ie_ptype_bin == 0)))

# -----------------------------------------------------------------------------
# 2. Derive analysis variables
# -----------------------------------------------------------------------------
df_analysis <- analysis %>%
  mutate(
    # exposure reference = household comparator
    ie_ptype_fct = relevel(factor(as.character(ie_ptype_fct), ordered = FALSE),
                           ref = "Household contact"),
    # age: 4-level categorical (primary adjustment for most outcomes)
    agecat4 = factor(case_when(
      s0_ageyrs_num < 30 ~ "18-29",
      s0_ageyrs_num < 40 ~ "30-39",
      s0_ageyrs_num < 50 ~ "40-49",
      TRUE               ~ "50+"),
      levels = c("18-29", "30-39", "40-49", "50+")),
    # self-reported memory difficulty (binary): "Some" or "A lot"
    r4_cogn_bin = factor(ifelse(r4_cogn_fct %in% c("Some", "A lot"), "Yes", "No"),
                         levels = c("No", "Yes")),
    ImpLungFn_post_Status_AfrAm = factor(ImpLungFn_post_Status_AfrAm, levels = c("No", "Yes")),
    # ever-smoker (current or former)
    s0_tobacsmokever_bin = factor(case_when(
      s0_tobacsmok_bin == "Yes" ~ "Yes",
      s0_tobacsmok_bin == "No" & Smoking_Cat %in% c("Former smoker", "Current smoker") ~ "Yes",
      TRUE ~ "No"), levels = c("No", "Yes")),
    # cough duration in weeks (source recorded in seconds)
    s1_tbsxcough_dur_wk = s1_tbsxcough_dur / 60 / 60 / 24 / 7)

# Impaired lung function under the GLI Global (race-neutral) reference equations,
# as a sensitivity to the primary GLI African-American classification. Same
# definition (post-bronchodilator FVC or FEV1/FVC z-score below the lower limit
# of normal); z-scores computed with rspiro::zscore_GLIgl (gender 1=male, 2=female).
df_analysis <- df_analysis %>%
  mutate(
    FVC_post_Z_GLIgl    = zscore_GLIgl(age = s0_ageyrs_num, height = s2_height_num,
                                       gender = Sex, FVC = FvcPost_Spr),
    FEV1FVC_post_Z_GLIgl = zscore_GLIgl(age = s0_ageyrs_num, height = s2_height_num,
                                       gender = Sex, FEV1FVC = Fev1ByFvcPost_Spr),
    ImpLungFn_post_Status_GLIgl = factor(case_when(
      FEV1FVC_post_Z_GLIgl < -1.645 ~ "Yes",
      FVC_post_Z_GLIgl < -1.645 ~ "Yes",
      FEV1FVC_post_Z_GLIgl >= -1.645 & FVC_post_Z_GLIgl >= -1.645 ~ "No"),
      levels = c("No", "Yes")),
    ImpLungFn_post_Type_GLIgl = case_when(
      FEV1FVC_post_Z_GLIgl < -1.645 ~ "Obstruction",
      FVC_post_Z_GLIgl < -1.645 & FEV1FVC_post_Z_GLIgl >= -1.645 ~ "Restrictive pattern",
      FEV1FVC_post_Z_GLIgl >= -1.645 & FVC_post_Z_GLIgl >= -1.645 ~ "None"))

# HRQoL severity category: worst EQ-5D-5L domain response (none / slight / moderate+)
eq5l_domains <- c("r4_eq5lmob_fct", "r4_eq5lusu_fct", "r4_eq5lpain_fct",
                  "r4_eq5ldep_fct", "r4_eq5lwash_fct")
df_analysis <- df_analysis %>%
  mutate(across(all_of(eq5l_domains), ~case_when(
    .x %in% c("Moderate", "Severe", "Unable") ~ 3L,
    .x %in% c("Slight") ~ 2L,
    .x %in% c("None") ~ 1L), .names = "..{.col}")) %>%
  rowwise() %>%
  mutate(.eq5lmax = suppressWarnings(max(c_across(starts_with("..r4_eq5l")), na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(r4_eq5lcat_fct = factor(case_when(.eq5lmax == 3 ~ "Moderate",
                                           .eq5lmax == 2 ~ "Slight",
                                           .eq5lmax == 1 ~ "None"),
                                 levels = c("None", "Slight", "Moderate"))) %>%
  select(-starts_with("..r4_eq5l"), -.eq5lmax)

# -----------------------------------------------------------------------------
# 3. De-identify and reduce to the MINIMAL analysis dataset
#    Direct identifiers (study PIDs) are replaced with arbitrary sequential
#    codes; the household code is retained (re-coded) for identification of clustering
# -----------------------------------------------------------------------------
df_min <- df_analysis %>%
  mutate(hhid = as.integer(factor(s0_hhid_cha, levels = unique(s0_hhid_cha)))) %>%
  arrange(hhid, ie_ptype_bin) %>%
  mutate(pid = row_number()) %>%
  transmute(
    pid, hhid,
    # exposure and adjustment
    ie_ptype_fct, ie_ptype_bin, ie_sex_fct, s0_ageyrs_num, agecat4,
    # chronic conditions (Table 2 / multimorbidity)
    ie_hivcurr_fct, ie_diabcurr_fct, ie_htncurr_fct, ie_mhcurr_fct,
    ie_viscurr_fct, ie_undwcurr_fct, ie_anaemcurr_fct,
    r4_cogn_fct, r4_cogn_bin,
    ImpLungFn_post_Status_AfrAm, ImpLungFn_post_Type_AfrAm,
    ImpLungFn_post_Status_GLIgl, ImpLungFn_post_Type_GLIgl,   # GLI Global sensitivity
    s4b_spirono_fct,                                          # reason for no spirometry
    # associated clinical measures (Supplementary table 8) + BMI (S8, S11)
    s3_hivartcurr_fct, ie_diaba1cresult_num, s2_bpfins_num, s2_bpfinb_num,
    s5_ssqimpact_fct, s2_bmi_num, s2_bmiadult_fct, ie_anaemresult_num,
    FEV1_post_Z_AfrAm, FVC_post_Z_AfrAm, FEV1FVC_post_Z_AfrAm,
    # mental-health detail (SSQ)
    s5_ssqredflag_fct, s5_ssqscore_num,
    # HRQoL and physical activity (incl. EQ-5D domains for Supplementary table 13)
    r4_eq5lval_num, r4_eq5lcat_fct, r8_overallmets_num, r8_cat_fct,
    r4_eq5lmob_fct, r4_eq5lusu_fct, r4_eq5lpain_fct, r4_eq5ldep_fct, r4_eq5lwash_fct,
    # demographics / behaviours (Table 1 / Supplementary table 5)
    Education, r3_medaid_bin, s0_preg_bin, Employment_CJC,
    s0_recint_num,                       # interval since TB diagnosis, days (index cases)
    HistoryMining, HistorySA, HistoryPrison,
    s0_tobacsmok_bin, s0_tobacsmokever_bin, s0_tobacother_bin,
    s5_audscore_fct, s5_audscorefin_num, s5_subever_fct, s0_covvacc_bin,
    s1_tbsxcough_bin, s1_tbsxcough_dur_wk, s1_tbsxscr_fct, ie_xpertresult_fct,
    # household-level (poverty / crowding; used in Figure 2 and descriptives)
    Hhold_Poverty, Hhold_Crowding_UN)

# -----------------------------------------------------------------------------
# 4. Write the minimal dataset
# -----------------------------------------------------------------------------
dir.create("data", showWarnings = FALSE)
saveRDS(df_min, "data/imba_analysis_dataset.Rds")
write_csv(df_min, "data/imba_analysis_dataset.csv")


cat(sprintf("Minimal analysis dataset written: %d rows x %d variables\n",
            nrow(df_min), ncol(df_min)))
cat(sprintf("  Index cases (recent TB):     %d\n", sum(df_min$ie_ptype_bin == 1)))
cat(sprintf("  Household comparators:       %d\n", sum(df_min$ie_ptype_bin == 0)))
cat(sprintf("  With good-quality spirometry: %d\n",
            sum(!is.na(df_min$ImpLungFn_post_Status_AfrAm))))

# -----------------------------------------------------------------------------
# 5. Data dictionary / codebook for the minimal dataset
#    Laid out to match the ERASE-TB study codebook
#    (ERASE_predictionmodel_data_codebook.xlsx): one header row per variable
#    giving its name, plain-language label and storage type, followed (for
#    categorical variables) by one indented row per answer category. Written to
#    an Excel workbook to accompany the dataset on release.
# -----------------------------------------------------------------------------
library(openxlsx)

var_labels <- c(
  pid                         = "De-identified participant code (sequential)",
  hhid                        = "De-identified household code (clustering only)",
  ie_ptype_fct                = "Participant type",
  ie_ptype_bin                = "Index case (recent TB) indicator (1 = index case, 0 = household comparator)",
  ie_sex_fct                  = "Sex",
  s0_ageyrs_num               = "Age, years",
  agecat4                     = "Age group (4 categories)",
  ie_hivcurr_fct              = "HIV",
  ie_diabcurr_fct             = "Diabetes",
  ie_htncurr_fct              = "Hypertension",
  ie_mhcurr_fct               = "Common mental health disorder",
  ie_viscurr_fct              = "Vision impairment",
  ie_undwcurr_fct             = "Underweight",
  ie_anaemcurr_fct            = "Anaemia",
  r4_cogn_fct                 = "Memory difficulty (last 12 months)",
  r4_cogn_bin                 = "Memory difficulty, binary (Some/A lot vs none)",
  ImpLungFn_post_Status_AfrAm = "Impaired lung function, post-bronchodilator (GLI African-American reference; primary)",
  ImpLungFn_post_Type_AfrAm   = "Type of lung function impairment (GLI African-American reference; primary)",
  ImpLungFn_post_Status_GLIgl = "Impaired lung function, post-bronchodilator (GLI Global reference; sensitivity)",
  ImpLungFn_post_Type_GLIgl   = "Type of lung function impairment (GLI Global reference; sensitivity)",
  s4b_spirono_fct             = "Reason spirometry not done",
  s3_hivartcurr_fct           = "Antiretroviral therapy status",
  ie_diaba1cresult_num        = "HbA1c, %",
  s2_bpfins_num               = "Systolic blood pressure, mmHg",
  s2_bpfinb_num               = "Diastolic blood pressure, mmHg",
  s5_ssqimpact_fct            = "Impact of psychological distress on daily life (SSQ)",
  s2_bmi_num                  = "Body mass index, kg/m2",
  s2_bmiadult_fct             = "BMI category (adult)",
  ie_anaemresult_num          = "Haemoglobin, g/dL",
  FEV1_post_Z_AfrAm           = "FEV1 z-score, post-bronchodilator (GLI African-American)",
  FVC_post_Z_AfrAm            = "FVC z-score, post-bronchodilator (GLI African-American)",
  FEV1FVC_post_Z_AfrAm        = "FEV1/FVC z-score, post-bronchodilator (GLI African-American)",
  s5_ssqredflag_fct           = "Psychological distress red flag (SSQ)",
  s5_ssqscore_num             = "SSQ-14 total score",
  r4_eq5lval_num              = "EQ-5D-5L index value",
  r4_eq5lcat_fct              = "HRQoL severity (worst EQ-5D-5L domain response)",
  r8_overallmets_num          = "Total physical activity, MET-min/week",
  r8_cat_fct                  = "Physical activity category",
  r4_eq5lmob_fct              = "EQ-5D-5L: Mobility",
  r4_eq5lusu_fct              = "EQ-5D-5L: Usual activities",
  r4_eq5lpain_fct             = "EQ-5D-5L: Pain/discomfort",
  r4_eq5ldep_fct              = "EQ-5D-5L: Anxiety/depression",
  r4_eq5lwash_fct             = "EQ-5D-5L: Self-care",
  Education                   = "Highest educational level",
  r3_medaid_bin               = "Has medical aid (insurance)",
  s0_preg_bin                 = "Currently pregnant",
  Employment_CJC              = "Employment status",
  s0_recint_num               = "Interval since TB diagnosis, days (index cases)",
  HistoryMining               = "History of mining",
  HistorySA                   = "History of living in South Africa",
  HistoryPrison               = "History of imprisonment",
  s0_tobacsmok_bin            = "Current smoker",
  s0_tobacsmokever_bin        = "Ever smoker (current or former)",
  s0_tobacother_bin           = "Use of other tobacco products",
  s5_audscore_fct             = "AUDIT category (alcohol use)",
  s5_audscorefin_num          = "AUDIT total score",
  s5_subever_fct              = "Ever used recreational substances",
  s0_covvacc_bin              = "COVID-19 vaccinated",
  s1_tbsxcough_bin            = "Current cough",
  s1_tbsxcough_dur_wk         = "Cough duration, weeks",
  s1_tbsxscr_fct              = "TB symptom screen result",
  ie_xpertresult_fct          = "Xpert MTB/RIF result",
  Hhold_Poverty               = "Household income < 2.15 USD per person per day",
  Hhold_Crowding_UN           = "Household crowding (UN definition)")

# Storage type, ERASE-codebook style: String for categorical, Integer for
# whole-number measures, Numeric otherwise (identifiers reported as Numeric).
var_type <- function(x, nm) {
  if (is.factor(x) || is.character(x)) return("String")
  v <- x[!is.na(x)]
  if (nm %in% c("pid", "hhid"))            return("Numeric")
  if (length(v) && all(v == floor(v)))     return("Integer")
  "Numeric"
}

# Build the long codebook: a header row per variable, then one row per answer
# category for categorical variables (category text in the "Answer code" column).
dict <- map_dfr(names(df_min), function(nm) {
  x   <- df_min[[nm]]
  lvl <- if (is.factor(x)) levels(x)
         else if (is.character(x)) sort(unique(x[!is.na(x)]))
         else NULL
  header <- tibble(`Variable name` = nm,
                   `Variable label` = unname(var_labels[nm]),
                   `Answer label` = NA_character_,
                   `Answer code` = NA_character_,
                   `Variable Type` = var_type(x, nm))
  if (is.null(lvl)) return(header)
  answers <- tibble(`Variable name` = NA_character_, `Variable label` = NA_character_,
                    `Answer label` = NA_character_, `Answer code` = lvl,
                    `Variable Type` = NA_character_)
  bind_rows(header, answers)
})

saveRDS(dict, "data/imba_analysis_dataset_dictionary.Rds")

# Write the Excel workbook, matching the ERASE codebook column widths.
wb <- createWorkbook()
addWorksheet(wb, "Codebook")
writeData(wb, "Codebook", dict)
addStyle(wb, "Codebook", createStyle(textDecoration = "bold"),
         rows = 1, cols = 1:ncol(dict), gridExpand = TRUE)
setColWidths(wb, "Codebook", cols = 1:5, widths = c(22.79, 62.79, 56.79, 28.29, 12.12))
saveWorkbook(wb, "data/imba_analysis_dataset_dictionary.xlsx", overwrite = TRUE)

cat(sprintf("Data dictionary written: %d variables, %d rows -> data/imba_analysis_dataset_dictionary.xlsx\n",
            ncol(df_min), nrow(dict)))
