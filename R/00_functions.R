# =============================================================================
# IMBA Hutano - 00: SHARED FUNCTIONS
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(sandwich)   # cluster-robust variance
  library(lmtest)     # coeftest
})

# -----------------------------------------------------------------------------
# Odds ratios with household cluster-robust standard errors.
# Reproduces the Stata models:
#   logit <outcome> i.ie_ptype_bin <covariates>, vce(cluster s0_hhid_cha) or
# Returns a tidy tibble of OR, 95% CI and Wald p for every model term.
# -----------------------------------------------------------------------------
or_cluster <- function(formula, data, cluster = "hhid") {
  model <- glm(formula, data = data, family = binomial)
  cl <- data[[cluster]][as.integer(rownames(model.frame(model)))]  # rows actually used
  ct <- lmtest::coeftest(model, vcov = sandwich::vcovCL, cluster = cl)
  tibble(term      = rownames(ct),
         estimate  = exp(ct[, 1]),
         conf.low  = exp(ct[, 1] - 1.96 * ct[, 2]),
         conf.high = exp(ct[, 1] + 1.96 * ct[, 2]),
         p.value   = ct[, 4])
}

# Linear regression with household cluster-robust standard errors.
#   regress <outcome> i.ie_ptype_bin <covariates>, vce(cluster s0_hhid_cha)
coef_cluster <- function(formula, data, cluster = "hhid") {
  model <- lm(formula, data = data)
  cl <- data[[cluster]][as.integer(rownames(model.frame(model)))]
  ct <- lmtest::coeftest(model, vcov = sandwich::vcovCL, cluster = cl)
  tibble(term      = rownames(ct),
         estimate  = ct[, 1],
         conf.low  = ct[, 1] - 1.96 * ct[, 2],
         conf.high = ct[, 1] + 1.96 * ct[, 2],
         p.value   = ct[, 4])
}

# -----------------------------------------------------------------------------
# Joint Wald p-value (household cluster-robust) for all coefficients matching
# `term`. Reproduces Stata: logit ie_ptype_bin i.<x> ..., vce(cluster); test <x>
# (single coefficient for binary/continuous x; joint test for multi-level x).
# -----------------------------------------------------------------------------
wald_p <- function(formula, data, term, cluster = "hhid") {
  m <- glm(formula, data = data, family = binomial)
  cl <- data[[cluster]][as.integer(rownames(model.frame(m)))]
  V <- sandwich::vcovCL(m, cluster = cl)
  idx <- grep(term, names(coef(m)), fixed = TRUE)
  b <- coef(m)[idx]
  W <- as.numeric(t(b) %*% solve(V[idx, idx, drop = FALSE]) %*% b)
  pchisq(W, length(idx), lower.tail = FALSE)
}

# -----------------------------------------------------------------------------
# Format an estimate + 95% CI table (estimate, conf.low, conf.high, p.value)
# into "est (low-high)" with rounded p values.
# -----------------------------------------------------------------------------
tidy_table <- function(table, digits = 2) {
  table %>%
    mutate(est_ci = paste0(trimws(format(round(estimate, digits), nsmall = digits)), " (",
                           trimws(format(round(conf.low, digits), nsmall = digits)), "–",
                           trimws(format(round(conf.high, digits), nsmall = digits)), ")"),
           p_val = case_when(p.value >= 0.01 ~ paste0(signif(p.value, 2)),
                             p.value < 0.01 & p.value > 0.001 ~ paste0(signif(p.value, 1)),
                             p.value <= 0.001 ~ "<0.001")) %>%
    select(term, est_ci, p_val)
}

# -----------------------------------------------------------------------------
# Age- and sex-adjusted predicted probability of an outcome for each group,
# at balanced sex and the modal age category of people with TB (30-39 years),
# reproducing the Stata margins call:
#   margins ie_ptype_bin, at((means) _all (asbalanced) ie_sex_fct)
# (Published 95% CIs were obtained from Stata margins; point estimates only here.)
# -----------------------------------------------------------------------------
predicted_prob <- function(outcome, data, age = c("cat", "cont")) {
  age <- match.arg(age)
  d <- data %>%
    mutate(.y = ifelse(.data[[outcome]] == "Yes", 1L, 0L)) %>%
    filter(!is.na(.y), !is.na(ie_sex_fct))
  aterm <- if (age == "cat") "agecat4" else "s0_ageyrs_num"
  d <- d %>% filter(!is.na(.data[[aterm]]))
  model <- glm(as.formula(paste0(".y ~ factor(ie_ptype_bin) + ie_sex_fct + ", aterm)),
               data = d, family = binomial)
  # Evaluate the linear predictor at the mean of each covariate (so age is held
  # at its sample distribution), with sex balanced and exposure set to 0 / 1.
  # 95% CIs by the delta method on the household cluster-robust variance.
  V    <- sandwich::vcovCL(model, cluster = d[["hhid"]])
  prof <- colMeans(model.matrix(model))
  prof[grep("ie_sex_fct", names(prof))] <- 0.5             # balanced sex
  ptype_col <- grep("ie_ptype_bin", names(prof))
  est <- function(g) {
    v <- prof; v[ptype_col] <- g
    p <- plogis(sum(coef(model) * v))
    grad <- p * (1 - p) * v                                # d p / d beta
    se <- sqrt(as.numeric(t(grad) %*% V %*% grad))
    c(p = p, lo = p - 1.96 * se, hi = p + 1.96 * se)
  }
  h <- est(0); t <- est(1)
  tibble(hhc = h["p"], hhc_lo = h["lo"], hhc_hi = h["hi"],
         tb = t["p"], tb_lo = t["lo"], tb_hi = t["hi"])
}
