# =============================================================================
# IMBA Hutano - 03: FORMATTED TABLES (Word)
# -----------------------------------------------------------------------------
# Renders Tables 1-3 to a Word document using officer + flextable, taking
# document styles from the ERASE draft-styles reference (templates/).
# Output: output/imba_tables.docx
# =============================================================================

library(officer)
library(flextable)
library(gtsummary)

# Run the analysis first (computes all table objects) unless already in memory
if (!exists("table2", inherits = FALSE)) source("R/02_analysis.R")

template <- "templates/ERASE_draftstyles_reference01.docx"

# flextable house style to match the manuscript tables
set_flextable_defaults(font.family = "Arial", font.size = 10,
                       padding.top = 1, padding.bottom = 1,
                       padding.left = 2, padding.right = 2)

# -----------------------------------------------------------------------------
# TABLE 1 - characteristics, with age/sex-adjusted p values
# -----------------------------------------------------------------------------
ft1 <- table1_desc %>%
  modify_table_body(~ .x %>%
    dplyr::left_join(
      tab1_pvals %>% dplyr::transmute(variable, p_adj = style_pvalue(p, digits = 2)),
      by = "variable") %>%
    dplyr::mutate(p_adj = ifelse(row_type == "label", p_adj, NA_character_))) %>%
  modify_header(p_adj ~ "**p value***") %>%
  modify_caption("Table 1. Characteristics of people with recent TB and household comparators") %>%
  as_flex_table() %>%
  autofit()

# -----------------------------------------------------------------------------
# TABLE 2 - prevalence + adjusted odds ratios (conditions + multimorbidity + QoL)
# -----------------------------------------------------------------------------
or_row <- function(out, label) {
  pr <- prevalence(out); a <- adjusted_or(out)
  ah <- if (out == "ie_hivcurr_fct") "–" else adjusted_or(out, hiv = TRUE)$est_ci
  tibble::tibble(Characteristic = label,
                 `Household comparators (N=285)` = pr$`Household contact`,
                 `People with recent TB (N=92)`  = pr$`Index case`,
                 `aOR (95% CI)` = a$est_ci, `p` = a$p_val, `aOR + HIV (95% CI)` = ah)
}
coef_row <- function(fit, label) {
  tibble::tibble(Characteristic = label,
                 `Household comparators (N=285)` = "–",
                 `People with recent TB (N=92)`  = "–",
                 `aOR (95% CI)` = sprintf("%.2f (%.2f, %.2f)", fit$estimate, fit$conf.low, fit$conf.high),
                 `p` = ifelse(fit$p.value < 0.001, "<0.001", as.character(signif(fit$p.value, 2))),
                 `aOR + HIV (95% CI)` = "–")
}

tab2_body <- dplyr::bind_rows(
  or_row("ie_mmone_bin",  "≥1 chronic condition"),
  or_row("ie_mmcurr_bin", "Multimorbidity (≥2 conditions)†"),
  purrr::pmap_dfr(tab2_outcomes, function(label, outcome) or_row(outcome, label)),
  coef_row(eq5d, "EQ-5D value (regression coefficient)**"),
  coef_row(mets, "Physical activity, MET-min/week**"))

ft2 <- flextable(tab2_body) %>%
  add_header_lines("Table 2. Odds ratios for chronic conditions, recent TB vs household comparators, adjusted for age and sex") %>%
  bold(part = "header") %>% autofit()

# -----------------------------------------------------------------------------
# TABLE 3 - age/sex-adjusted predicted probabilities
# -----------------------------------------------------------------------------
ft3 <- flextable(table3) %>%
  set_header_labels(label = "") %>%
  add_header_lines("Table 3. Predicted probability of each chronic condition after adjustment for age and sex") %>%
  bold(part = "header") %>% autofit()

# -----------------------------------------------------------------------------
# Assemble the Word document from the styles template
# -----------------------------------------------------------------------------
note <- function(txt) fpar(ftext(txt, fp_text(font.size = 8, font.family = "Arial")))

# start from the template (for its styles) but clear its placeholder content
# (officer keeps at least one block, so clear to 1 and drop the residual later)
doc <- read_docx(template)
while (length(doc) > 1) doc <- body_remove(doc)

doc <- doc %>%
  body_add_par("IMBA Hutano: tables", style = "Title") %>%

  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft1) %>%
  body_add_fpar(note("* Wald p values from logistic regression with TB status as the outcome, adjusted for age (4-level categorical) and sex, with household cluster-robust standard errors. Pregnancy reported among women only.")) %>%
  body_add_break() %>%

  body_add_par("Table 2", style = "heading 1") %>%
  body_add_flextable(ft2) %>%
  body_add_fpar(note("Presented as n (%). aOR adjusted for age and sex; aOR + HIV additionally adjusted for HIV. 95% CIs use household cluster-robust standard errors. A linear age term is used for diabetes, impaired lung function, underweight and anaemia. Impaired lung function: n=39 recent TB and n=155 comparators with good-quality post-bronchodilator spirometry.")) %>%
  body_add_fpar(note("† Multimorbidity defined as ≥2 of HIV, diabetes, hypertension, mental health disorder, memory difficulty, vision impairment, impaired lung function, underweight or anaemia (primary analysis; memory-excluded sensitivity analysis reported in text). ** Regression coefficient (linear regression), not an odds ratio.")) %>%
  body_add_break() %>%

  body_add_par("Table 3", style = "heading 1") %>%
  body_add_flextable(ft3) %>%
  body_add_fpar(note("Predicted probabilities at the mean covariate profile, balanced sex and modal age category (30–39 years)."))

# remove the single residual template paragraph left at the top
doc <- cursor_begin(doc)
doc <- body_remove(doc)

dir.create("output", showWarnings = FALSE)
print(doc, target = "output/imba_tables.docx")
cat("Written: output/imba_tables.docx\n")
