# =============================================================================
# IMBA Hutano - Multimorbidity and HRQoL after TB treatment in Zimbabwe
# 02b: SUPPLEMENTARY ANALYSIS
# -----------------------------------------------------------------------------
# Reproduces the supplementary analyses from the minimal dataset:
#   S5  ORs for TB by demographics/health behaviours (age/sex-adjusted)
#   S8  Unadjusted proportions and associated clinical measures
#   S9  Memory difficulty by age category, stratified by recent TB
#   S10 Memory difficulty by mental-health symptoms (SSQ)
#   S13 EQ-5D component breakdown (with Fisher's exact tests)
#   S14 Ordinal-logistic sensitivity analyses (HRQoL, physical activity)
# Each result is checked against the published value. The formatted tables are
# produced in 03b_tables_supplement.R.
#
# Author: Claire J Calderwood
# =============================================================================

library(tidyverse)
library(gtsummary)
library(MASS, include.only = "polr")

# main analysis provides df (incl. derived variables) and the shared helpers
if (!exists("table2", inherits = FALSE)) source("R/02_analysis.R")

cat("\n##################  SUPPLEMENTARY ANALYSIS  ##################\n")

# =============================================================================
# S5. Odds ratios for the outcome of recent TB by demographic and behavioural
#     exposures, adjusted for age (4-level) and sex, household cluster-robust.
# =============================================================================
exp <- c("Education", "r3_medaid_bin", "HistoryMining", "HistorySA", "HistoryPrison",
         "s0_tobacsmok_bin", "s0_tobacother_bin", "s5_aud3_fct", "s5_audscorefin_num",
         "s5_subever_fct", "s1_tbsxcough_bin", "s1_tbsxscr_fct")
cov <- "ie_sex_fct + agecat4"

s5_models <- lapply(exp, function(i) {
  mod <- as.formula(paste0("ie_ptype_bin ~ ", i, " + ", cov))
  or_cluster(mod, df) %>% filter(str_detect(term, fixed(i))) %>% mutate(variable = i)
}) %>% bind_rows()

# pregnancy is reported among women only and is not sex-adjusted
s5_preg <- or_cluster(ie_ptype_bin ~ s0_preg_bin + agecat4, filter(df, ie_sex_fct == "Female")) %>%
  filter(str_detect(term, "s0_preg_bin")) %>% mutate(variable = "s0_preg_bin")

# variable + level label (for merging into the descriptive table) and formatted OR.
# Match the keys used by the descriptive table: substance use is shown binary
# (Don't-know not shown); AUDIT score is a continuous (label) row.
s5_or <- bind_rows(s5_models, s5_preg) %>%
  mutate(label = str_remove(term, fixed(variable))) %>%
  bind_cols(tidy_table(.) %>% select(est_ci)) %>%
  transmute(variable, label, OR = est_ci) %>%
  mutate(variable = recode(variable, "s5_subever_fct" = "s5_sub2_fct"),
         label = ifelse(variable == "s5_audscorefin_num", "AUDIT score", label)) %>%
  filter(!(variable == "s5_sub2_fct" & label == "Don't know"))

cat("\n----- S5: ORs for TB by exposure -----\n")
print(as.data.frame(s5_or), row.names = FALSE)
cat("Published: education 0.73, smoker 0.23, cough 2.58, symptom 2.47, pregnant 0.57\n")

# =============================================================================
# S8. Unadjusted proportion with each chronic condition and associated measures.
# =============================================================================
s8_summary <- df %>%
  mutate(s3_hivartcurr_fct = factor(ifelse(s3_hivartcurr_fct == "Currently on ART", "Yes", "No"))) %>%
  select(ie_ptype_fct, ie_hivcurr_fct, s3_hivartcurr_fct, ie_diabcurr_fct,
         ie_diaba1cresult_num, ie_htncurr_fct, s2_bpfins_num, s2_bpfinb_num,
         ie_mhcurr_fct, s5_ssqscore_num, s5_ssqredflag_fct, s5_ssqimpact_fct,
         r4_cogn_bin, ie_undwcurr_fct, s2_bmiadult_fct, s2_bmi_num,
         ie_anaemcurr_fct, ie_anaemresult_num, ie_viscurr_fct,
         ImpLungFn_post_Status_AfrAm, ImpLungFn_post_Type_AfrAm,
         FEV1_post_Z_AfrAm, FVC_post_Z_AfrAm, FEV1FVC_post_Z_AfrAm) %>%
  tbl_summary(by = ie_ptype_fct, missing = "no")

cat("\n----- S8: associated measures (comparator / recent TB) -----\n")
s8_med <- function(v) df %>% group_by(ie_ptype_fct) %>%
  summarise(m = median(.data[[v]], na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(ie_ptype_fct)) %>% pull(m)
cat(sprintf("HbA1c %%: %.2f / %.2f (5.80 / 5.60); BMI: %.1f / %.1f (24.1 / 20.7); Hb: %.1f / %.1f (14.0 / 14.0)\n",
            s8_med("ie_diaba1cresult_num")[1], s8_med("ie_diaba1cresult_num")[2],
            s8_med("s2_bmi_num")[1], s8_med("s2_bmi_num")[2],
            s8_med("ie_anaemresult_num")[1], s8_med("ie_anaemresult_num")[2]))

# =============================================================================
# S9. Memory difficulty by age category, stratified by recent TB status.
#     logit r4_cogn_bin i.agecat4 i.ie_sex_fct if <group>, vce(cluster hhid)
# =============================================================================
s9 <- lapply(c("Household contact", "Index case"), function(g) {
  or_cluster(r4_cogn_bin ~ agecat4 + ie_sex_fct, filter(df, ie_ptype_fct == g)) %>%
    filter(str_detect(term, "agecat4")) %>% tidy_table() %>%
    mutate(group = g, age = str_remove(term, "agecat4"))
}) %>% bind_rows()
cat("\n----- S9: memory x age (ref 18-29) -----\n")
cat("Published HHC 0.52/1.80/2.11; TB 1.25/1.85/2.20\n")
print(as.data.frame(s9 %>% select(group, age, est_ci)), row.names = FALSE)

# =============================================================================
# S10. Memory difficulty by mental-health disorder, stratified by recent TB.
#      Exposure = ie_mhcurr_fct (SSQ-based common mental-health disorder; the
#      "SSQ +ve" classification). The published table mislabelled this as
#      "SSQ +ve/-ve" and transposed the odds-ratio columns between groups; this
#      recreation uses the correct exposure and the consistent cluster-robust
#      stratified model. logit r4_cogn_bin i.ie_mhcurr_fct i.sex i.agecat4 if
#      <group>, vce(cluster hhid).
# =============================================================================
s10_or <- lapply(c("Household contact", "Index case"), function(g) {
  or_cluster(r4_cogn_bin ~ ie_mhcurr_fct + ie_sex_fct + agecat4, filter(df, ie_ptype_fct == g)) %>%
    filter(str_detect(term, "ie_mhcurr_fct")) %>% tidy_table() %>% mutate(group = g)
}) %>% bind_rows()

s10 <- df %>%
  count(group = ie_ptype_fct, mh = ie_mhcurr_fct, mem = r4_cogn_bin) %>%
  group_by(group, mh) %>% mutate(cell = sprintf("%d (%.1f%%)", n, 100 * n / sum(n))) %>% ungroup() %>%
  select(group, mh, mem, cell) %>%
  pivot_wider(names_from = mem, values_from = cell, names_prefix = "memory_") %>%
  left_join(s10_or %>% select(group, est_ci), by = "group") %>%
  mutate(aOR = ifelse(mh == "Yes", est_ci, "Ref"),
         mh = factor(ifelse(mh == "Yes", "Mental health disorder", "No mental health disorder"),
                     levels = c("No mental health disorder", "Mental health disorder"))) %>%
  arrange(group, mh) %>%
  select(group, mh, memory_No, memory_Yes, aOR)

cat("\n----- S10: memory x mental-health disorder (recreated) -----\n")
cat("Counts (published): HHC 156/31 & 60/38; TB 35/16 & 22/19\n")
cat("aOR (corrected, columns un-swapped): HHC 2.97 (1.64-5.37); TB 2.23 (0.89-5.57)\n")
print(as.data.frame(s10), row.names = FALSE)

# =============================================================================
# S13. EQ-5D component breakdown, with Fisher's exact tests.
# =============================================================================
s13_summary <- df %>%
  transmute(ie_ptype_fct,
            `Anxiety/depression` = r4_eq5ldep_fct, Mobility = r4_eq5lmob_fct,
            `Pain/discomfort` = r4_eq5lpain_fct, `Usual activities` = r4_eq5lusu_fct,
            `Self-care` = r4_eq5lwash_fct) %>%
  tbl_summary(by = ie_ptype_fct, missing = "no") %>%
  add_p(test = everything() ~ "fisher.test")
cat("\n----- S13: EQ-5D domains, Fisher exact p -----\n")
cat("Published 0.002 / 0.074 / 0.054 / 0.046 / 0.12\n")
for (v in c("r4_eq5ldep_fct", "r4_eq5lmob_fct", "r4_eq5lpain_fct", "r4_eq5lusu_fct", "r4_eq5lwash_fct"))
  cat(sprintf("  %-16s %.3f\n", v, fisher.test(table(df[[v]], df$ie_ptype_fct))$p.value))

# =============================================================================
# S14. Ordinal-logistic sensitivity analyses (proportional-odds OR for TB).
#      ologit <outcome> i.ie_ptype_bin i.ie_sex_fct i.agecat4
# =============================================================================
ord_or <- function(outcome) {
  m <- polr(as.formula(paste0(outcome, " ~ factor(ie_ptype_bin) + ie_sex_fct + agecat4")),
            data = df, Hess = TRUE)
  # household cluster-robust SE (consistent with all other models)
  cl <- df$hhid[as.integer(rownames(model.frame(m)))]
  V <- sandwich::vcovCL(m, cluster = cl)
  b  <- coef(m)["factor(ie_ptype_bin)1"]
  se <- sqrt(diag(V)["factor(ie_ptype_bin)1"])
  p  <- 2 * pnorm(abs(b / se), lower.tail = FALSE)
  tibble(est_ci = sprintf("%.2f (%.2f–%.2f)", exp(b), exp(b - 1.96*se), exp(b + 1.96*se)),
         p_val = ifelse(p < 0.001, "<0.001", as.character(signif(p, 2))))
}
s14 <- bind_rows(
  mutate(ord_or("r4_eq5lcat_fct"), outcome = "EQ-5D value"),
  mutate(ord_or("r8_cat_fct"),     outcome = "Physical activity (IPAQ-SF)"))
cat("\n----- S14: ordinal sensitivity -----\n")
cat("Published EQ-5D 2.33 (1.45-3.75) p<0.001; IPAQ 1.01 (0.61-1.69) p=0.96\n")
print(as.data.frame(s14 %>% select(outcome, est_ci, p_val)), row.names = FALSE)

# =============================================================================
# Impaired lung function by reference equation: GLI African-American (primary)
# vs GLI Global (race-neutral) sensitivity. Prevalence and age/sex-adjusted OR
# (linear age, household cluster-robust), among the spirometry subgroup.
# =============================================================================
gli_row <- function(statusvar) {
  prev <- df %>% filter(!is.na(.data[[statusvar]])) %>% group_by(ie_ptype_fct) %>%
    summarise(v = sprintf("%d/%d (%.0f%%)", sum(.data[[statusvar]] == "Yes"), n(),
                          100 * mean(.data[[statusvar]] == "Yes")), .groups = "drop") %>%
    pivot_wider(names_from = ie_ptype_fct, values_from = v)
  orr <- or_cluster(as.formula(paste0(statusvar, " ~ factor(ie_ptype_bin) + ie_sex_fct + s0_ageyrs_num")), df) %>%
    filter(str_detect(term, "ie_ptype_bin")) %>% tidy_table()
  tibble(Comparators = prev$`Household contact`, `Recent TB` = prev$`Index case`,
         `aOR (95% CI)` = orr$est_ci, p = orr$p_val)
}
s_gli <- bind_rows(
  mutate(gli_row("ImpLungFn_post_Status_AfrAm"), Equation = "GLI African-American (primary)"),
  mutate(gli_row("ImpLungFn_post_Status_GLIgl"), Equation = "GLI Global")) %>%
  select(Equation, everything())
cat("\n----- Impaired lung function by reference equation -----\n")
print(as.data.frame(s_gli), row.names = FALSE)

# =============================================================================
# Supplementary figure 2: age distribution by group
#   (NB: the published figure used all n=96 participants; the analysis cohort
#   used here is n=92, so counts differ slightly.)
# =============================================================================
fig_s2 <- df %>%
  ggplot(aes(s0_ageyrs_num)) +
  geom_density(aes(col = ie_ptype_fct)) +
  geom_histogram(aes(y = after_stat(density), fill = ie_ptype_fct), alpha = 0.2,
                 position = "identity", binwidth = 5) +
  labs(x = "Age, years", y = "Density", col = NULL, fill = NULL) +
  theme_minimal()

cat("\nSupplementary analysis complete.\n")
