# =============================================================================
# IMBA Hutano - 03b: FORMATTED SUPPLEMENTARY TABLES (Word)
# -----------------------------------------------------------------------------
# Renders the supplementary tables (S5, S8, S9, S10, S13, S14) and figure S2 to
# a Word document, using the ERASE draft-styles reference for the document theme.
# Output: output/imba_supplementary_tables.docx
# =============================================================================

library(officer)
library(flextable)
library(gtsummary)

# run the supplementary analysis first (computes all result objects)
if (!exists("s5_or", inherits = FALSE)) source("R/02b_analysis_supplement.R")

template <- "templates/ERASE_draftstyles_reference01.docx"
set_flextable_defaults(font.family = "Arial", font.size = 10,
                       padding.top = 1, padding.bottom = 1, padding.left = 2, padding.right = 2)
note <- function(txt) fpar(ftext(txt, fp_text(font.size = 8, font.family = "Arial")))

# -----------------------------------------------------------------------------
# S5 - characteristics with the odds ratio for recent TB on the relevant level
# -----------------------------------------------------------------------------
s5_vars <- c("Education", "r3_medaid_bin", "s0_preg_bin", "HistoryMining", "HistorySA",
             "HistoryPrison", "s0_tobacsmok_bin", "s0_tobacother_bin", "s5_aud3_fct",
             "s5_audscorefin_num", "s5_sub2_fct", "s1_tbsxcough_bin", "s1_tbsxscr_fct")
ft_s5 <- df %>%
  select(ie_ptype_fct, all_of(s5_vars)) %>%
  # AUDIT score summarised among people who drink (non-drinkers excluded)
  mutate(s5_audscorefin_num = ifelse(s5_aud3_fct == "Non-drinker", NA, s5_audscorefin_num)) %>%
  tbl_summary(by = ie_ptype_fct, missing = "no",
              type = all_dichotomous() ~ "categorical",   # show levels so ORs align
              label = list(Education ~ "Highest educational level", r3_medaid_bin ~ "Medical aid cover",
                s0_preg_bin ~ "Currently pregnant", HistoryMining ~ "History of mining",
                HistorySA ~ "History of living in South Africa", HistoryPrison ~ "History of imprisonment",
                s0_tobacsmok_bin ~ "Current tobacco smoker", s0_tobacother_bin ~ "Current other tobacco",
                s5_aud3_fct ~ "Alcohol use category (AUDIT)", s5_audscorefin_num ~ "AUDIT score",
                s5_sub2_fct ~ "Use of other substances", s1_tbsxcough_bin ~ "Cough",
                s1_tbsxscr_fct ~ "TB symptom screen positive")) %>%
  modify_table_body(~ .x %>%
    dplyr::left_join(s5_or %>% dplyr::rename(label = label), by = c("variable", "label"))) %>%
  modify_header(OR ~ "**OR (95% CI)***") %>%
  modify_caption("Supplementary table 5. Odds ratios for TB by demographics or health behaviours, adjusted for age and sex") %>%
  as_flex_table() %>% autofit()

# -----------------------------------------------------------------------------
# S8 - unadjusted proportions and associated measures
# -----------------------------------------------------------------------------
ft_s8 <- s8_summary %>%
  modify_caption("Supplementary table 8. Unadjusted proportion of people with chronic conditions and associated measures (N=377)") %>%
  as_flex_table() %>% autofit()

# -----------------------------------------------------------------------------
# S9 / S10 - memory difficulty associations (stratified ORs)
# -----------------------------------------------------------------------------
ft_s9 <- s9 %>% select(Group = group, `Age category` = age, `aOR (95% CI)` = est_ci) %>%
  flextable() %>% merge_v(j = "Group") %>%
  set_caption("Supplementary table 9. Reported memory difficulty by age category, stratified by recent TB (aOR, sex-adjusted, ref 18-29 years)") %>%
  autofit()

ft_s10 <- s10 %>%
  select(Group = group, `Mental health disorder (SSQ)` = mh,
         `Memory: No` = memory_No, `Memory: Yes` = memory_Yes, `aOR (95% CI)*` = aOR) %>%
  flextable() %>% merge_v(j = "Group") %>%
  set_caption("Supplementary table 10. Reported memory difficulty by mental-health disorder (SSQ), stratified by recent TB") %>%
  autofit()

# -----------------------------------------------------------------------------
# S13 - EQ-5D component breakdown (Fisher's exact p)
# -----------------------------------------------------------------------------
ft_s13 <- s13_summary %>%
  modify_caption("Supplementary table 13. Breakdown of EQ-5D components") %>%
  as_flex_table() %>% autofit()

# -----------------------------------------------------------------------------
# S14 - ordinal-logistic sensitivity analyses
# -----------------------------------------------------------------------------
ft_s14 <- s14 %>% select(Outcome = outcome, `OR (95% CI)` = est_ci, p = p_val) %>%
  flextable() %>%
  set_caption("Supplementary table 14. Ordinal-logistic sensitivity analyses (recent TB vs comparators)") %>%
  autofit()

# Impaired lung function by reference equation (GLI African-American vs GLI Global)
ft_gli <- s_gli %>%
  flextable() %>%
  set_caption("Supplementary table 19. Impaired lung function by reference equation: GLI African-American (primary) and GLI Global (sensitivity)") %>%
  autofit()

# Supplementary figure 2
dir.create("output", showWarnings = FALSE)
ggsave("output/supp_fig2_age_distribution.png", fig_s2, width = 6, height = 4, dpi = 300)

# -----------------------------------------------------------------------------
# Assemble the supplementary document
# -----------------------------------------------------------------------------
doc <- read_docx(template)
while (length(doc) > 1) doc <- body_remove(doc)

doc <- doc %>%
  body_add_par("IMBA Hutano: supplementary tables", style = "Title") %>%
  body_add_flextable(ft_s5) %>%
  body_add_fpar(note("Odds ratios from logistic regression for the outcome of recent TB, adjusted for age (4-level) and sex, household cluster-robust. Pregnancy among women only. Alcohol category p uses a joint Wald test; substance-use 'Don't know' responses are not shown.")) %>%
  body_add_break() %>%
  body_add_flextable(ft_s8) %>% body_add_break() %>%
  body_add_flextable(ft_s9) %>%
  body_add_flextable(ft_s10) %>%
  body_add_fpar(note("* aOR for reported memory difficulty by mental-health disorder (SSQ-based classification), from logistic regression adjusted for age (4-level) and sex, household cluster-robust. Reference = no mental-health disorder.")) %>%
  body_add_break() %>%
  body_add_flextable(ft_s13) %>%
  body_add_fpar(note("p values from Fisher's exact test.")) %>%
  body_add_flextable(ft_s14) %>% body_add_break() %>%
  body_add_flextable(ft_gli) %>%
  body_add_fpar(note("n (%) with impaired lung function (post-bronchodilator FVC or FEV1/FVC z-score below the lower limit of normal) and age/sex-adjusted odds ratio (recent TB vs comparators), household cluster-robust, among the spirometry subgroup. GLI African-American equations are the primary analysis; GLI Global (race-neutral) equations are shown as a sensitivity analysis.")) %>%
  body_add_break() %>%
  body_add_par("Supplementary figure 2: age distribution by group", style = "heading 2") %>%
  body_add_img("output/supp_fig2_age_distribution.png", width = 6, height = 4)

doc <- cursor_begin(doc)
doc <- body_remove(doc)

print(doc, target = "output/imba_supplementary_tables.docx")
cat("Written: output/imba_supplementary_tables.docx\n")
