# =============================================================================
# IMBA Hutano - Multimorbidity and HRQoL after TB treatment in Zimbabwe
# 02: ANALYSIS
# -----------------------------------------------------------------------------
# Reproduces the published tables from the minimal, de-identified analysis
# dataset (data/imba_analysis_dataset.Rds, written by 01_data_preparation.R).
#
# Models are age- and sex-adjusted logistic (or linear) regression with
# household cluster-robust standard errors. Age is a 4-level categorical
# variable, except for diabetes, impaired lung function, underweight and
# anaemia, where a linear age term is used to avoid over-parameterisation.
#
# Author: Claire J Calderwood
# Reviwed: June 2026
# =============================================================================

library(tidyverse)
library(gtsummary)
library(MASS, include.only = "polr")   # ordinal sensitivity analyses

source("R/00_functions.R")

df <- readRDS("data/imba_analysis_dataset.Rds")

# Outcomes adjusted for a LINEAR age term (all others use 4-level agecat4)
age_linear <- c("ie_diabcurr_fct", "ImpLungFn_post_Status_AfrAm",
                "ie_undwcurr_fct", "ie_anaemcurr_fct")
age_term  <- function(out) if (out %in% age_linear) "s0_ageyrs_num" else "agecat4"

# =============================================================================
# Multimorbidity
#   Conditions: HIV, diabetes, hypertension, mental health, memory difficulty,
#   vision, impaired lung function, underweight, anaemia.
#   Primary analysis INCLUDES self-reported memory difficulty; a sensitivity
#   analysis EXCLUDES it (memory rests on a single, unvalidated screening item).
# =============================================================================
conditions_no_memory <- c("ie_hivcurr_fct", "ie_diabcurr_fct", "ie_htncurr_fct",
                          "ie_mhcurr_fct", "ie_viscurr_fct", "ImpLungFn_post_Status_AfrAm",
                          "ie_anaemcurr_fct", "ie_undwcurr_fct")

count_conditions <- function(data, cols) {
  data %>%
    mutate(across(all_of(cols), ~ replace_na(as.integer(.x == "Yes"), 0L))) %>%
    mutate(.n = rowSums(across(all_of(cols)))) %>% pull(.n)
}

df <- df %>%
  mutate(
    n_cond            = count_conditions(., conditions_no_memory) +
                          replace_na(as.integer(r4_cogn_bin == "Yes"), 0L),  # primary (memory in)
    n_cond_nomemory   = count_conditions(., conditions_no_memory),           # sensitivity (memory out)
    ie_mmone_bin   = factor(ifelse(n_cond >= 1, "Yes", "No"), levels = c("No", "Yes")),
    ie_mmcurr_bin  = factor(ifelse(n_cond >= 2, "Yes", "No"), levels = c("No", "Yes")),
    ie_mmone_bin_nomem  = factor(ifelse(n_cond_nomemory >= 1, "Yes", "No"), levels = c("No", "Yes")),
    ie_mmcurr_bin_nomem = factor(ifelse(n_cond_nomemory >= 2, "Yes", "No"), levels = c("No", "Yes")))

# =============================================================================
# TABLE 1. Characteristics of people with recent TB and household comparators,
#          with age- and sex-adjusted Wald p values.
# =============================================================================
# collapse alcohol category (Harmful drinking + Alcohol dependence) and drop
# "Don't know" substance responses, exactly as the published Table 1
df <- df %>% mutate(
  s5_aud3_fct = factor(dplyr::recode(as.character(s5_audscore_fct),
                       "Harmful drinking" = "Harmful/dependent",
                       "Alcohol dependence" = "Harmful/dependent"),
                       levels = c("Non-drinker", "Non-harmful drinking", "Harmful/dependent")),
  s5_sub2_fct = factor(ifelse(as.character(s5_subever_fct) %in% c("No", "Yes"),
                              as.character(s5_subever_fct), NA), levels = c("No", "Yes")))

tab1_vars <- c("ie_sex_fct", "s0_ageyrs_num", "Education", "r3_medaid_bin",
               "s0_preg_bin", "HistoryMining", "HistorySA", "HistoryPrison",
               "s0_tobacsmokever_bin", "s5_aud3_fct", "s5_audscorefin_num",
               "s5_sub2_fct", "s0_covvacc_bin", "s1_tbsxcough_bin",
               "s1_tbsxcough_dur_wk", "s1_tbsxscr_fct")

table1_desc <- df %>%
  select(ie_ptype_fct, all_of(tab1_vars)) %>%
  # AUDIT score is summarised among people who drink (non-drinkers excluded)
  mutate(s5_audscorefin_num = ifelse(s5_aud3_fct == "Non-drinker", NA, s5_audscorefin_num)) %>%
  tbl_summary(by = ie_ptype_fct, missing = "no",
              label = list(
                ie_sex_fct ~ "Sex",
                s0_ageyrs_num ~ "Age, years",
                Education ~ "Highest educational level",
                r3_medaid_bin ~ "Medical aid cover",
                s0_preg_bin ~ "Currently pregnant",
                HistoryMining ~ "History of mining",
                HistorySA ~ "History of living in South Africa",
                HistoryPrison ~ "History of imprisonment",
                s0_tobacsmokever_bin ~ "Current/former tobacco smoker",
                s5_aud3_fct ~ "Alcohol use category (AUDIT)",
                s5_audscorefin_num ~ "AUDIT score",
                s5_sub2_fct ~ "Use of other substances",
                s0_covvacc_bin ~ "≥1 COVID-19 vaccination received",
                s1_tbsxcough_bin ~ "Cough",
                s1_tbsxcough_dur_wk ~ "Cough duration, weeks",
                s1_tbsxscr_fct ~ "TB symptom screen positive"))

# Joint Wald p value for each characteristic (single coefficient for binary /
# continuous, joint test for multi-level), TB status ~ characteristic + sex + age,
# household cluster-robust. Pregnancy is women-only and not sex-adjusted, as published.
# substances: the published p is the level-specific Wald p for "Yes" from the
# 3-level model (Don't-know kept), not a joint test - reproduce that exactly
sub_p <- function() {
  m <- glm(ie_ptype_bin ~ s5_subever_fct + ie_sex_fct + agecat4, df, family = binomial)
  ct <- coeftest(m, vcov = vcovCL, cluster = df$hhid[as.integer(rownames(model.frame(m)))])
  ct[grep("s5_subever_fctYes", rownames(ct)), 4]
}
tab1_p_vars <- setdiff(tab1_vars, c("ie_sex_fct", "s0_ageyrs_num"))
tab1_pvals <- lapply(tab1_p_vars, function(v) {
  if (v == "s5_sub2_fct") return(tibble(variable = v, p = sub_p()))
  d <- df; sexadj <- TRUE
  if (v == "s0_preg_bin") { d <- filter(df, ie_sex_fct == "Female"); sexadj <- FALSE }
  f <- as.formula(paste0("ie_ptype_bin ~ ", v, if (sexadj) " + ie_sex_fct" else "", " + agecat4"))
  tibble(variable = v, p = wald_p(f, d, v))
}) %>% bind_rows()

# verify against the published Table 1 p values
tab1_targets <- tibble::tribble(
  ~variable,~target, "Education",0.39,"r3_medaid_bin",0.22,"s0_preg_bin",0.48,
  "HistoryMining",0.09,"HistorySA",0.95,"HistoryPrison",0.22,"s0_tobacsmokever_bin",0.61,
  "s5_aud3_fct",0.57,"s5_audscorefin_num",0.24,"s5_sub2_fct",0.26,"s0_covvacc_bin",0.75,
  "s1_tbsxcough_bin",0.001,"s1_tbsxcough_dur_wk",0.033,"s1_tbsxscr_fct",0.002)
cat("\n===== TABLE 1: adjusted p values (computed vs published) =====\n")
print(as.data.frame(tab1_pvals %>% left_join(tab1_targets, by = "variable") %>%
  mutate(p = signif(p, 2), match = ifelse(abs(p - target) <= 0.011, "ok", "**DIFF**"))), row.names = FALSE)

# =============================================================================
# TABLE 2. Odds ratios for chronic conditions, recent TB vs comparators,
#          adjusted for age and sex (and, in a second model, additionally HIV).
# =============================================================================
tab2_outcomes <- tibble::tribble(
  ~label,             ~outcome,
  "HIV",              "ie_hivcurr_fct",
  "Diabetes",         "ie_diabcurr_fct",
  "Hypertension",     "ie_htncurr_fct",
  "Memory difficulty","r4_cogn_bin",
  "Mental health",    "ie_mhcurr_fct",
  "Vision impairment","ie_viscurr_fct",
  "Impaired lung fn", "ImpLungFn_post_Status_AfrAm",
  "Underweight",      "ie_undwcurr_fct",
  "Anaemia",          "ie_anaemcurr_fct")

# prevalence n (%) by group
prevalence <- function(out) {
  df %>% filter(!is.na(.data[[out]])) %>%
    group_by(ie_ptype_fct) %>%
    summarise(n = sum(.data[[out]] == "Yes"), N = n(),
              pct = round(100 * n / N), .groups = "drop") %>%
    mutate(np = paste0(n, " (", pct, "%)")) %>%
    select(ie_ptype_fct, np) %>%
    pivot_wider(names_from = ie_ptype_fct, values_from = np)
}

# adjusted OR for TB vs comparator (optionally additionally adjusted for HIV)
adjusted_or <- function(out, hiv = FALSE) {
  rhs <- paste0("factor(ie_ptype_bin) + ie_sex_fct + ", age_term(out),
                if (hiv) " + ie_hivcurr_fct" else "")
  or_cluster(as.formula(paste0(out, " ~ ", rhs)), df) %>%
    filter(str_detect(term, "ie_ptype_bin")) %>%
    tidy_table()
}

table2 <- tab2_outcomes %>%
  rowwise() %>%
  mutate(prev = list(prevalence(outcome)),
         aOR  = adjusted_or(outcome, hiv = FALSE)$est_ci,
         p    = adjusted_or(outcome, hiv = FALSE)$p_val,
         aOR_HIV = if (outcome == "ie_hivcurr_fct") NA_character_
                   else adjusted_or(outcome, hiv = TRUE)$est_ci) %>%
  ungroup() %>%
  unnest(prev) %>%
  select(label, `Household contact`, `Index case`, aOR, p, aOR_HIV)

cat("\n===== TABLE 2: prevalence and adjusted odds ratios =====\n")
print(as.data.frame(table2), row.names = FALSE)

# Multimorbidity odds ratios (primary: memory included; sensitivity: excluded)
mm_models <- bind_rows(
  prevalence("ie_mmone_bin")  %>% mutate(label = ">=1 condition (memory incl, primary)"),
  prevalence("ie_mmcurr_bin") %>% mutate(label = "Multimorbidity >=2 (memory incl, primary)"),
  prevalence("ie_mmone_bin_nomem")  %>% mutate(label = ">=1 condition (memory excl, sensitivity)"),
  prevalence("ie_mmcurr_bin_nomem") %>% mutate(label = "Multimorbidity >=2 (memory excl, sensitivity)"))
mm_or <- sapply(c("ie_mmone_bin", "ie_mmcurr_bin", "ie_mmone_bin_nomem", "ie_mmcurr_bin_nomem"),
                function(o) adjusted_or(o)$est_ci)
cat("\n===== Multimorbidity =====\n")
print(as.data.frame(mm_models %>% select(label, `Household contact`, `Index case`)), row.names = FALSE)
cat("\nAdjusted ORs (TB vs comparator):\n")
print(data.frame(model = names(mm_or), aOR = unname(mm_or)), row.names = FALSE)

# =============================================================================
# TABLE 3. Age- and sex-adjusted predicted probabilities (balanced sex,
#          modal age category 30-39 years).
# =============================================================================
table3 <- tab2_outcomes %>%
  rowwise() %>%
  mutate(pp = list(predicted_prob(outcome, df, age = if (outcome %in% age_linear) "cont" else "cat"))) %>%
  unnest(pp) %>%
  mutate(`Household comparators` = sprintf("%.1f%% (%.1f–%.1f)", 100*hhc, 100*hhc_lo, 100*hhc_hi),
         `People with recent TB`  = sprintf("%.1f%% (%.1f–%.1f)", 100*tb, 100*tb_lo, 100*tb_hi)) %>%
  select(label, `Household comparators`, `People with recent TB`)

cat("\n===== TABLE 3: predicted probabilities =====\n")
print(as.data.frame(table3), row.names = FALSE)

# =============================================================================
# Health-related quality of life and physical activity
# =============================================================================
cat("\n===== HRQoL (EQ-5D value) and physical activity =====\n")

# EQ-5D utility index (linear regression)
eq5d <- coef_cluster(r4_eq5lval_num ~ factor(ie_ptype_bin) + ie_sex_fct + agecat4, df) %>%
  filter(str_detect(term, "ie_ptype_bin"))
cat(sprintf("EQ-5D value, adjusted difference: %.3f (%.3f, %.3f), p = %.3f\n",
            eq5d$estimate, eq5d$conf.low, eq5d$conf.high, eq5d$p.value))

# Sensitivity: EQ-5D severity category (ordinal logistic regression)
eq5d_ord <- polr(r4_eq5lcat_fct ~ factor(ie_ptype_bin) + ie_sex_fct + agecat4,
                 data = df, Hess = TRUE)
cat(sprintf("EQ-5D ordinal sensitivity OR (TB vs comparator): %.2f\n",
            exp(coef(eq5d_ord)["factor(ie_ptype_bin)1"])))

# Physical activity (MET-minutes/week, linear regression)
mets <- coef_cluster(r8_overallmets_num ~ factor(ie_ptype_bin) + ie_sex_fct + agecat4, df) %>%
  filter(str_detect(term, "ie_ptype_bin"))
cat(sprintf("Physical activity (MET-min/week), adjusted difference: %.0f (%.0f, %.0f), p = %.2f\n",
            mets$estimate, mets$conf.low, mets$conf.high, mets$p.value))

cat("\nDone. See README.md for notes on model specification and reproducibility.\n")
