library(bayesplot)
library(posterior)
library(bayestestR)
library(mcmcse)
library(loo)
library(MASS)
library(knitr)
library(kableExtra)
library(emmeans)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lme4)
library(grid)
library(mediation)

hivdat <- read.csv("C:/Users/stein/OneDrive/Documents/School/2026 Spring/Advanced Methods/BIOS6624/Project 1/Data Raw/hiv_6624_final.csv")


#####################################################################
# TABLE 1
####################################################################

# Continuous: Mean (SD)
cont_row <- function(var, label, data) {
  overall  <- data[[var]]
  no_drugs <- data[[var]][data$hard_drugs_baseline == "No Hard Drugs"]
  drugs    <- data[[var]][data$hard_drugs_baseline == "Hard Drug User"]
  
  data.frame(
    Characteristic = label,
    Overall        = paste0(round(mean(overall,  na.rm=TRUE), 1), 
                            " (", round(sd(overall,  na.rm=TRUE), 1), ")"),
    No_Hard_Drugs  = paste0(round(mean(no_drugs, na.rm=TRUE), 1), 
                            " (", round(sd(no_drugs, na.rm=TRUE), 1), ")"),
    Hard_Drug_User = paste0(round(mean(drugs,    na.rm=TRUE), 1), 
                            " (", round(sd(drugs,    na.rm=TRUE), 1), ")")
  )
}

# Categorical: n (%)
cat_row <- function(var, level, label, data) {
  overall  <- data[[var]]
  no_drugs <- data[[var]][data$hard_drugs_baseline == "No Hard Drugs"]
  drugs    <- data[[var]][data$hard_drugs_baseline == "Hard Drug User"]
  
  n_overall  <- sum(overall  == level, na.rm = TRUE)
  n_no_drugs <- sum(no_drugs == level, na.rm = TRUE)
  n_drugs    <- sum(drugs    == level, na.rm = TRUE)
  
  pct_overall  <- round(100 * n_overall  / length(overall),  1)
  pct_no_drugs <- round(100 * n_no_drugs / length(no_drugs), 1)
  pct_drugs    <- round(100 * n_drugs    / length(drugs),    1)
  
  data.frame(
    Characteristic = label,
    Overall        = paste0(n_overall,  " (", pct_overall,  "%)"),
    No_Hard_Drugs  = paste0(n_no_drugs, " (", pct_no_drugs, "%)"),
    Hard_Drug_User = paste0(n_drugs,    " (", pct_drugs,    "%)")
  )
}

# Categorical block: header row with p-value + indented level rows
cat_block <- function(var, levels, labels, label_header, data) {
  # Chi-square p-value for the whole variable
  tbl <- table(data[[var]], data$hard_drugs_baseline)
  
  # Header row (variable name + p-value, no counts)
  header <- data.frame(
    Characteristic = label_header,
    Overall        = "",
    No_Hard_Drugs  = "",
    Hard_Drug_User = ""
  )
  
  # One row per level, indented with spaces
  rows <- do.call(rbind, mapply(function(lv, lb) {
    cat_row(var, lv, paste0("  ", lb), data)
  }, levels, labels, SIMPLIFY = FALSE))
  
  rbind(header, rows)
}

# Sample sizes
n_overall  <- nrow(analytic)
n_no_drugs <- sum(analytic$hard_drugs_baseline == "No Hard Drugs")
n_drugs    <- sum(analytic$hard_drugs_baseline == "Hard Drug User")

# Build table rows
table1_df <- rbind(
  
  # Continuous variables
  cont_row("age", "Age (years)", analytic),
  cont_row("BMI", "BMI (kg/m²)", analytic),
  
  # Race/Ethnicity
  cat_block(
    var          = "RACE_bin",
    levels       = c("Non-Hispanic White", "Other"),
    labels       = c("Non-Hispanic White", "Other"),
    label_header = "Race/Ethnicity",
    data         = analytic
  ),
  
  # Education
  cat_block(
    var          = "EDUC_bin",
    levels       = c(">=4 Yr College", "<4 Yr College"),
    labels       = c("≥4 Years College", "<4 Years College"),
    label_header = "Education",
    data         = analytic
  ),
  
  # Smoking
  cat_block(
    var          = "SMOKE_bin",
    levels       = c("Current", "Not Current"),
    labels       = c("Current Smoker", "Not Current Smoker"),
    label_header = "Smoking Status",
    data         = analytic
  )
)

# Render kable
kable(table1_df,
      row.names = FALSE,
      booktabs  = TRUE,
      caption   = paste0(
        "Baseline Characteristics of Analytic Sample by Hard Drug Use at Baseline. ",
        "Mean (SD) for continuous variables; n (%) for categorical variables. "
      ),
      col.names = c(
        "Characteristic",
        paste0("Overall (N=", n_overall, ")"),
        paste0("No Hard Drugs (N=", n_no_drugs, ")"),
        paste0("Hard Drug User (N=", n_drugs, ")")
      ),
      align = c("l", "c", "c", "c", "r")) %>%
  kable_styling(
    latex_options = c("striped", "hold_position"),
    full_width    = FALSE
  ) %>%
  row_spec(0, bold = TRUE)




# Creating graphics to visualize them

# Viral load
mean_vl <- tapply(hivdat$VLOAD,
                  list(hivdat$year, hivdat$hard_drugs),
                  mean)

matplot(as.numeric(rownames(mean_vl)), mean_vl,
        type = "l", lwd = 2, lty = 1,
        col = c("cornflowerblue", "red"),
        xlab = "Years Since HAART Initiation",
        ylab = "Mean Viral Load",
        main = "Viral Load Over Time by Drug Use")

legend("right", inset = -0.30, legend = colnames(mean_vl),
       col = c("cornflowerblue", "red"), lwd = 2, xpd = TRUE)



# CD4 
mean_cd4 <- tapply(hivdat$LEU3N,
                  list(hivdat$year, hivdat$hard_drugs),
                  mean)

matplot(as.numeric(rownames(mean_cd4)), mean_cd4,
        type = "l", lwd = 2, lty = 1,
        col = c("aquamarine2", "red"),
        xlab = "Years Since HAART Initiation",
        ylab = "Mean CD4 Count",
        main = "CD4 Count Over Time by Drug Use")

legend("right", inset = -0.30, legend = colnames(mean_cd4),
       col = c("aquamarine2", "red"), lwd = 2, xpd = TRUE)


# Physical Quality of Life Score
mean_pqol <- tapply(hivdat$AGG_PHYS,
                   list(hivdat$year, hivdat$hard_drugs),
                   mean)

matplot(as.numeric(rownames(mean_pqol)), mean_pqol,
        type = "l", lwd = 2, lty = 1,
        col = c("darkorchid", "red"),
        xlab = "Years Since HAART Initiation",
        ylab = "Mean Physical Quality of Life",
        main = "Physical Quality of Life Over Time by Drug Use")

legend("right", inset = -0.30, legend = colnames(mean_pqol),
       col = c("darkorchid", "red"), lwd = 2, xpd = TRUE)


# Mental Quality of Life Score
mean_mqol <- tapply(hivdat$AGG_MENT,
                    list(hivdat$year, hivdat$hard_drugs),
                    mean)

matplot(as.numeric(rownames(mean_mqol)), mean_mqol,
        type = "l", lwd = 2, lty = 1,
        col = c("deeppink4", "red"),
        xlab = "Years Since HAART Initiation",
        ylab = "Mean Mental Quality of Life",
        main = "Mental Quality of Life Over Time by Drug Use")

legend("right", inset = -0.30, legend = colnames(mean_mqol),
       col = c("deeppink4", "red"), lwd = 2, xpd = TRUE)


# Frequentist Analysis

# Prelim Data Preparation

# subject number tracking
n_start <- nrow(hivdat)

# fixing covariates
#BMI
n_bad_bmi <- sum(hivdat$BMI < 0 | hivdat$BMI > 250, na.rm = TRUE)
hivdat$BMI[hivdat$BMI < 0 | hivdat$BMI > 250] <- NA

# COnverting  to a factor for analysis
hivdat$hard_drugs <- factor(hivdat$hard_drugs,
                            levels = c(0,1),
                            labels = c("No Hard Drugs", "Hard Drug User"))
hivdat$SMOKE   <- factor(hivdat$SMOKE)
hivdat$EDUCBAS <- factor(hivdat$EDUCBAS)
hivdat$RACE    <- factor(hivdat$RACE)
hivdat$ADH     <- factor(hivdat$ADH)



# Creating baseline variables for the analysis
baseline_drugs <- hivdat[hivdat$year == 0, c("newid", "hard_drugs")]
names(baseline_drugs)[2] <- "hard_drugs_baseline"
hivdat <- merge(hivdat, baseline_drugs, by = "newid", all.x = TRUE)

# Pull Year 0 covariates and baseline outcome values
# rename outcomes to _base so don't get confusing
dat_yr0 <- hivdat[hivdat$year == 0,
                  c("newid", "hard_drugs_baseline",
                    "age", "BMI", "SMOKE", "EDUCBAS", "RACE",
                    "VLOAD", "LEU3N", "AGG_PHYS", "AGG_MENT")]

names(dat_yr0)[names(dat_yr0) == "VLOAD"]    <- "VLOAD_base"
names(dat_yr0)[names(dat_yr0) == "LEU3N"]    <- "LEU3N_base"
names(dat_yr0)[names(dat_yr0) == "AGG_PHYS"] <- "AGG_PHYS_base"
names(dat_yr0)[names(dat_yr0) == "AGG_MENT"] <- "AGG_MENT_base"

# Year 2: keep only the id and outcome columns
dat_yr2 <- hivdat[hivdat$year == 2,
                  c("newid", "VLOAD", "LEU3N", "AGG_PHYS", "AGG_MENT", "ADH")]

# Verify no overlapping names before merging (should only show "newid")
intersect(names(dat_yr2), names(dat_yr0))

# Now merge into one usable set
# Continue tracking the n values for analysis
n_yr0 <- nrow(dat_yr0)
n_yr2 <- nrow(dat_yr2)
analytic_all <- merge(dat_yr2, dat_yr0, by = "newid")
n_both <- nrow(analytic_all)
n_unmatched <- (n_yr0 + n_yr2) - (2 * n_both)


# Remove the missing data 
n_before_missing <- nrow(analytic_all)
analytic <- analytic_all[complete.cases(analytic_all), ] # removes any missing
n_after_missing <- nrow(analytic)
n_missing <- n_before_missing - n_after_missing

#Reset factors
analytic$SMOKE   <- droplevels(factor(analytic$SMOKE))
analytic$EDUCBAS <- droplevels(factor(analytic$EDUCBAS))
analytic$RACE    <- droplevels(factor(analytic$RACE))
analytic$ADH     <- droplevels(factor(analytic$ADH))
analytic$hard_drugs_baseline <- droplevels(factor(analytic$hard_drugs_baseline))



# Determining what needs to be log transformed with diagnostic plots
# Creating a function to do it for each variable rather than individually

# histogram + density + QQ for one variable
check_distribution <- function(x, label, color) {
  x_clean <- x[!is.na(x)]
  par(mfrow = c(1, 2))
  
  # Histogram with density overlay
  hist(x_clean,
       freq  = FALSE,
       main  = paste("Histogram:", label),
       xlab  = label,
       col   = adjustcolor(color, alpha.f = 0.5),
       border = "white")
  lines(density(x_clean), col = color, lwd = 2)
  
  # QQ plot
  qqnorm(x_clean,
         main = paste("QQ Plot:", label),
         pch  = 16, cex = 0.7,
         col  = adjustcolor(color, alpha.f = 0.6))
  qqline(x_clean, col = "black", lwd = 2, lty = 2)
  
  par(mfrow = c(1, 1))
  
  # Numeric skewness and excess kurtosis
  n    <- length(x_clean)
  mn   <- mean(x_clean)
  s    <- sd(x_clean)
  skew <- (sum((x_clean - mn)^3) / n) / s^3
  kurt <- (sum((x_clean - mn)^4) / n) / s^4 - 3
  cat(sprintf("%-30s  Skewness: %6.3f  Excess Kurtosis: %6.3f  N: %d\n",
              label, skew, kurt, n))
}



# Checking for each variable using similar colors as above

cat("=== VIRAL LOAD ===\n")
check_distribution(analytic$VLOAD,        "Viral Load (raw)",   "steelblue")
analytic$log10_VLOAD      <- log10(analytic$VLOAD + 1)
analytic$log10_VLOAD_base <- log10(analytic$VLOAD_base + 1)
check_distribution(analytic$log10_VLOAD,  "Viral Load (log10)", "steelblue")

# --- CD4 Count: raw vs log ---
cat("\n=== CD4 COUNT ===\n")
check_distribution(analytic$LEU3N,        "CD4 Count (raw)",    "aquamarine4")
analytic$log_LEU3N      <- log(analytic$LEU3N)
analytic$log_LEU3N_base <- log(analytic$LEU3N_base)
check_distribution(analytic$log_LEU3N,    "CD4 Count (log)",    "aquamarine4")

# --- Physical QoL (check raw only first) ---
cat("\n=== PHYSICAL QoL ===\n")
check_distribution(analytic$AGG_PHYS,     "Physical QoL (raw)", "darkorchid3")

# --- Mental QoL ---
cat("\n=== MENTAL QoL ===\n")
check_distribution(analytic$AGG_MENT,     "Mental QoL (raw)",   "deeppink4")


# From these use log_viral load, regular CD4, and log both mental/physical QoL
analytic$log10_VLOAD      <- log10(analytic$VLOAD)
analytic$log10_VLOAD_base <- log10(analytic$VLOAD_base)
analytic$refl_log_PHYS      <- log(101 - analytic$AGG_PHYS)
analytic$refl_log_PHYS_base <- log(101 - analytic$AGG_PHYS_base)
analytic$refl_log_MENT      <- log(101 - analytic$AGG_MENT)
analytic$refl_log_MENT_base <- log(101 - analytic$AGG_MENT_base)

# Convert categoricals to factors in the analytic dataset
analytic$SMOKE   <- factor(analytic$SMOKE)
analytic$EDUCBAS <- factor(analytic$EDUCBAS)
analytic$RACE    <- factor(analytic$RACE)
analytic$ADH     <- factor(analytic$ADH)


############################################################################
# Collapse groups here
############################################################################

# ADH codes: 1=100%, 2=95-99%, 3=75-94%, 4=<75%
# >95% adherent = codes 1 and 2; <=95% = codes 3 and 4
analytic$ADH_bin <- factor(
  ifelse(as.numeric(as.character(analytic$ADH)) %in% c(1, 2), ">95%", "<=95%"),
  levels = c(">95%", "<=95%")   # >95% is reference (better adherence)
)

# SMOKE codes: 1=Never, 2=Former, 3=Current
# "Not current" includes never (1) and former (2)
analytic$SMOKE_bin <- factor(
  ifelse(as.numeric(as.character(analytic$SMOKE)) == 3, "Current", "Not Current"),
  levels = c("Not Current", "Current")   # Not current is reference
)

# EDUCBAS codes: 1-4 = less than 4yr degree; 5=4yr degree, 6=some grad, 7=postgrad
analytic$EDUC_bin <- factor(
  ifelse(as.numeric(as.character(analytic$EDUCBAS)) >= 5,
         ">=4 Yr College", "<4 Yr College"),
  levels = c("<4 Yr College", ">=4 Yr College")   # <4yr is reference
)

# RACE codes: 1=White non-Hispanic; 2-8=all others
analytic$RACE_bin <- factor(
  ifelse(as.numeric(as.character(analytic$RACE)) == 1,
         "Non-Hispanic White", "Other"),
  levels = c("Non-Hispanic White", "Other")   # NHW is reference
)


# Frequentist models

# a is WITHOUT adherence
# b is WITH adherence

# Viral Load (log10 transformed) 
mod1a <- lm(log10_VLOAD ~ hard_drugs_baseline + log10_VLOAD_base +
              age + BMI + SMOKE_bin + EDUC_bin + RACE_bin,
            data = analytic)

mod1b <- lm(log10_VLOAD ~ hard_drugs_baseline + log10_VLOAD_base +
              age + BMI + SMOKE_bin + EDUC_bin + RACE_bin + ADH_bin,
            data = analytic)

par(mfrow = c(2, 2)); plot(mod1a, main = "Viral Load - No ADH"); par(mfrow = c(1, 1))
par(mfrow = c(2, 2)); plot(mod1b, main = "Viral Load - With ADH"); par(mfrow = c(1, 1))

#  CD4 Count (untransformed)
mod2a <- lm(LEU3N ~ hard_drugs_baseline + LEU3N_base +
              age + BMI + SMOKE_bin + EDUC_bin + RACE_bin,
            data = analytic)

mod2b <- lm(LEU3N ~ hard_drugs_baseline + LEU3N_base +
              age + BMI + SMOKE_bin + EDUC_bin + RACE_bin + ADH_bin,
            data = analytic)


par(mfrow = c(2, 2)); plot(mod2a, main = "CD4 - No ADH"); par(mfrow = c(1, 1))
par(mfrow = c(2, 2)); plot(mod2b, main = "CD4 - With ADH"); par(mfrow = c(1, 1))


# Physical QoL (reflected log)
mod3a <- lm(refl_log_PHYS ~ hard_drugs_baseline + refl_log_PHYS_base +
              age + BMI + SMOKE_bin + EDUC_bin + RACE_bin,
            data = analytic)

mod3b <- lm(refl_log_PHYS ~ hard_drugs_baseline + refl_log_PHYS_base +
              age + BMI + SMOKE_bin + EDUC_bin + RACE_bin + ADH_bin,
            data = analytic)


par(mfrow = c(2, 2)); plot(mod3a, main = "Physical QoL - No ADH"); par(mfrow = c(1, 1))
par(mfrow = c(2, 2)); plot(mod3b, main = "Physical QoL - With ADH"); par(mfrow = c(1, 1))


# Mental QoL (reflected log) 
mod4a <- lm(refl_log_MENT ~ hard_drugs_baseline + refl_log_MENT_base +
              age + BMI + SMOKE_bin + EDUC_bin + RACE_bin,
            data = analytic)

mod4b <- lm(refl_log_MENT ~ hard_drugs_baseline + refl_log_MENT_base +
              age + BMI + SMOKE_bin + EDUC_bin + RACE_bin + ADH_bin,
            data = analytic)

par(mfrow = c(2, 2)); plot(mod4a, main = "Mental QoL - No ADH"); par(mfrow = c(1, 1))
par(mfrow = c(2, 2)); plot(mod4b, main = "Mental QoL - With ADH"); par(mfrow = c(1, 1))

# Summary of all 4 frequentist models

# extract results from one model function
extract_results <- function(mod, outcome_label) {
  coef_row <- summary(mod)$coefficients["hard_drugs_baselineHard Drug User", ]
  ci_row   <- confint(mod)["hard_drugs_baselineHard Drug User", ]
  data.frame(
    Outcome   = outcome_label,
    Estimate  = round(coef_row[1], 3),
    SE        = round(coef_row[2], 3),
    p         = round(coef_row[4], 4),
    CI_Lower  = round(ci_row[1], 3),
    CI_Upper  = round(ci_row[2], 3)
  )
}

results_table <- rbind(
  extract_results(mod1a, "Viral Load (log10) - No ADH"),
  extract_results(mod1b, "Viral Load (log10) - With ADH"),
  extract_results(mod2a, "CD4 Count - No ADH"),
  extract_results(mod2b, "CD4 Count - With ADH"),
  extract_results(mod3a, "Physical QoL (refl log) - No ADH"),
  extract_results(mod3b, "Physical QoL (refl log) - With ADH"),
  extract_results(mod4a, "Mental QoL (refl log) - No ADH"),
  extract_results(mod4b, "Mental QoL (refl log) - With ADH")
)

# Format p-values so very small ones display nicely
results_table$p <- ifelse(results_table$p < 0.001, "<0.001",
                          as.character(results_table$p))

# Combine CI into one column for cleaner table
results_table$`95% CI` <- paste0("(", results_table$CI_Lower,
                                 ", ", results_table$CI_Upper, ")")
results_table$CI_Lower <- NULL
results_table$CI_Upper <- NULL

kable(results_table,
      row.names = FALSE,
      caption   = "Effect of Baseline Hard Drug Use on Year 2 Outcomes: With and Without Adherence Adjustment",
      booktabs  = TRUE,
      align     = c("l", "r", "r", "r", "r")) %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width    = FALSE) %>%
  pack_rows("Viral Load (log10)", 1, 2) %>%
  pack_rows("CD4 Count", 3, 4) %>%
  pack_rows("Physical Quality of Life (reflected log)", 5, 6) %>%
  pack_rows("Mental Quality of Life (reflected log)", 7, 8)


### ADH Mediation ###

# ADH_bin is binary so we use a logistic regression (glm) for the mediator model

# predicting the mediator new model
# This is the same for all 4 outcomes
med_model <- glm(
  ADH_bin ~ hard_drugs_baseline + age + BMI + SMOKE_bin + EDUC_bin + RACE_bin,
  data = analytic, family = binomial(link = "logit")
)

# Then just plug in existing b models
set.seed(42)
med_vload <- mediate(med_model, mod1b,
                     treat = "hard_drugs_baseline", mediator = "ADH_bin",
                     treat.value = "Hard Drug User", control.value = "No Hard Drugs",
                     sims = 1000, boot = F)

set.seed(42)
med_cd4 <- mediate(med_model, mod2b,
                   treat = "hard_drugs_baseline", mediator = "ADH_bin",
                   treat.value = "Hard Drug User", control.value = "No Hard Drugs",
                   sims = 1000, boot = F)

set.seed(42)
med_phys <- mediate(med_model, mod3b,
                    treat = "hard_drugs_baseline", mediator = "ADH_bin",
                    treat.value = "Hard Drug User", control.value = "No Hard Drugs",
                    sims = 1000, boot = F)

set.seed(42)
med_ment <- mediate(med_model, mod4b,
                    treat = "hard_drugs_baseline", mediator = "ADH_bin",
                    treat.value = "Hard Drug User", control.value = "No Hard Drugs",
                    sims = 1000, boot = F)

############################################################################
# MEDIATION RESULTS TABLE
############################################################################

extract_mediation <- function(med_obj, label) {
  s <- summary(med_obj)
  data.frame(
    Outcome   = label,
    ACME_est  = round(s$d1,        3),
    ACME_CI   = paste0("(", round(s$d1.ci[[1]], 3), ", ", round(s$d1.ci[[2]], 3), ")"),
    ACME_p    = ifelse(s$d1.p < 0.001, "<0.001", as.character(round(s$d1.p, 3))),
    ADE_est   = round(s$z1,        3),
    ADE_CI    = paste0("(", round(s$z1.ci[[1]], 3), ", ", round(s$z1.ci[[2]], 3), ")"),
    ADE_p     = ifelse(s$z1.p < 0.001, "<0.001", as.character(round(s$z1.p, 3))),
    Total_est = round(s$tau.coef,  3),
    Total_CI  = paste0("(", round(s$tau.ci[[1]], 3), ", ", round(s$tau.ci[[2]], 3), ")"),
    Total_p   = ifelse(s$tau.p < 0.001, "<0.001", as.character(round(s$tau.p, 3))),
    Prop_Med  = round(s$n1,        3)
  )
}

med_table <- rbind(
  extract_mediation(med_vload, "Viral Load (log10)"),
  extract_mediation(med_cd4,   "CD4 Count"),
  extract_mediation(med_phys,  "Physical QoL (refl log)"),
  extract_mediation(med_ment,  "Mental QoL (refl log)")
)

kable(med_table,
      row.names = FALSE,
      caption   = "Mediation Analysis: Effect of Baseline Hard Drug Use on Year 2 Outcomes Mediated Through Medication Adherence",
      booktabs  = TRUE,
      col.names = c("Outcome",
                    "Estimate", "95% CI", "p",
                    "Estimate", "95% CI", "p",
                    "Estimate", "95% CI", "p",
                    "Prop. Mediated")) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                full_width = FALSE) %>%
  add_header_above(c(" "               = 1,
                     "Indirect (ACME)" = 3,
                     "Direct (ADE)"    = 3,
                     "Total Effect"    = 3,
                     " "               = 1)) %>%
  footnote(general = paste(
    "ACME = Average Causal Mediation Effect (indirect path through adherence).",
    "ADE = Average Direct Effect (path not through adherence).",
    "Prop. Mediated = ACME / Total Effect.",
    "CIs based on quasi-Bayesian approximation with 1000 simulations.",
    "Quasi-Bayesian method used due to sparse cell counts in bootstrapped resamples.",
    "For reflected log QoL outcomes, positive estimates indicate worse quality of life.",
    "Hard drug users vs. non-users at baseline."
  ),
  general_title     = "Note: ",
  footnote_as_chunk = TRUE)

# Estimates for log-transformed outcomes represent differences on the log scale. 
# For viral load, exp(estimate) gives the fold-change in copies/mL.", 
# For QoL outcomes, positive estimates indicate worse quality of life due to reflection 
# of scale.

# Bayesian Analysis
# Used Gleason Worksheet as a template

############################################################################
# BAYESIAN ANALYSIS
# Dependencies
############################################################################

library(cmdstanr)
library(bayesplot)
library(posterior)
library(bayestestR)
library(mcmcse)
library(loo)
library(dplyr)
library(tibble)


############################################################################
# STEP 1: Define and compile the Stan model
############################################################################

stan_file <- write_stan_file("
data {
  int<lower=0> N;
  int<lower=0> P;
  matrix[N, P] X;
  vector[N] y;
  vector[P] prior_mean;
  vector<lower=0>[P] prior_sd;
  real<lower=0> sigma_prior_sd;
}
parameters {
  vector[P] beta;
  real<lower=0> sigma;
}
model {
  beta ~ normal(prior_mean, prior_sd);
  sigma ~ normal(0, sigma_prior_sd);
  y ~ normal(X * beta, sigma);
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(y[n] | X[n] * beta, sigma);
  }
}",
dir = "STAN", basename = "linear_regression_half_normal")

stan_mod <- cmdstan_model("STAN/linear_regression_half_normal.stan")


############################################################################
# STEP 2: Helper functions
############################################################################

# run_bayes_model()
# Builds design matrix, sets priors, fits Stan model, returns fit + LOO/WAIC
#
# Arguments:
#   y          : numeric outcome vector
#   formula    : one-sided formula for model.matrix (e.g. ~ x1 + x2)
#   data       : analytic dataframe
#   coef_sd    : prior SD for all non-intercept coefficients
#   sigma_sd   : SD for half-normal prior on sigma
#   seed       : random seed
#   chains/iter_warmup/iter_sampling : Stan sampling settings

run_bayes_model <- function(y,
                            formula,
                            data,
                            coef_sd       = 2,
                            sigma_sd      = 5,
                            seed          = 123,
                            chains        = 4,
                            iter_warmup   = 1000,
                            iter_sampling = 1000) {
  
  X <- model.matrix(formula, data = data)
  N <- nrow(X)
  P <- ncol(X)
  
  # Intercept gets outcome mean and wider SD; all others get 0 and coef_sd
  prior_mean <- c(mean(y), rep(0,       P - 1))
  prior_sd   <- c(10,      rep(coef_sd, P - 1))
  
  data_list <- list(
    N             = N,
    P             = P,
    X             = X,
    y             = y,
    prior_mean    = prior_mean,
    prior_sd      = prior_sd,
    sigma_prior_sd = sigma_sd
  )
  
  fit <- stan_mod$sample(
    data          = data_list,
    chains        = chains,
    iter_warmup   = iter_warmup,
    iter_sampling = iter_sampling,
    seed          = seed,
    refresh       = 500
  )
  
  # LOO and WAIC
  loglik   <- as_draws_matrix(fit$draws("log_lik"))
  loo_out  <- loo(loglik)
  waic_out <- waic(loglik)
  
  list(fit = fit, loo = loo_out, waic = waic_out, X = X, y = y)
}


# --- summarize_bayes_model() ---
# Returns a tibble with posterior mean, MCSE, SD, 95% HPDI, ESS, Rhat
# for all parameters, and prints convergence diagnostics

summarize_bayes_model <- function(model_list, label = "") {
  
  fit         <- model_list$fit
  draws_mat   <- as_draws_matrix(fit$draws())
  params      <- colnames(draws_mat)
  params      <- params[!grepl("lp__|log_lik", params)]
  
  summary_tbl <- lapply(params, function(p) {
    vals     <- as.numeric(draws_mat[, p])
    hpd      <- hdi(vals, ci = 0.95)
    tibble(
      Parameter = p,
      Estimate  = mean(vals),
      MCSE      = mcmcse::mcse(vals)$se,
      Std_Dev   = sd(vals),
      HPDI_low  = hpd$CI_low,
      HPDI_high = hpd$CI_high,
      ESS       = ess_bulk(vals),
      Rhat      = rhat(vals)
    )
  }) %>% bind_rows()
  
  if (nchar(label) > 0)
    cat("\n===", label, "===\n")
  
  cat("MCSE < 6% of SD: ", all((100 * summary_tbl$MCSE / summary_tbl$Std_Dev) < 6), "\n")
  cat("ESS > 1000:      ", all(summary_tbl$ESS > 1000), "\n")
  cat("Rhat < 1.01:     ", all(summary_tbl$Rhat < 1.01), "\n")
  
  print(summary_tbl)
  invisible(summary_tbl)
}


############################################################################
# STEP 3: Fit the 4 Bayesian models
############################################################################

# Viral Load (log10)
# coef_sd = 2 per analysis plan
result_vload <- run_bayes_model(
  y       = analytic$log10_VLOAD,
  formula = ~ hard_drugs_baseline + log10_VLOAD_base +
    age + BMI + SMOKE_bin + EDUC_bin + RACE_bin,
  data    = analytic,
  coef_sd = 2,
  sigma_sd = 5
)
summ_vload <- summarize_bayes_model(result_vload, "Viral Load (log10)")
print(result_vload$loo)
print(result_vload$waic)
result_vload$fit$cmdstan_diagnose()


# CD4 Count
# coef_sd = 100 per analysis plan
result_cd4 <- run_bayes_model(
  y       = analytic$LEU3N,
  formula = ~ hard_drugs_baseline + LEU3N_base +
    age + BMI + SMOKE_bin + EDUC_bin + RACE_bin,
  data    = analytic,
  coef_sd = 100,
  sigma_sd = 5
)
summ_cd4 <- summarize_bayes_model(result_cd4, "CD4 Count")
print(result_cd4$loo)
print(result_cd4$waic)
result_cd4$fit$cmdstan_diagnose()


# Physical QoL (reflected log) 
# coef_sd = 2 per analysis plan
result_phys <- run_bayes_model(
  y       = analytic$refl_log_PHYS,
  formula = ~ hard_drugs_baseline + refl_log_PHYS_base +
    age + BMI + SMOKE_bin + EDUC_bin + RACE_bin,
  data    = analytic,
  coef_sd = 2,
  sigma_sd = 5
)
summ_phys <- summarize_bayes_model(result_phys, "Physical QoL (refl log)")
print(result_phys$loo)
print(result_phys$waic)
result_phys$fit$cmdstan_diagnose()


# Mental QoL (reflected log) 
# coef_sd = 2 per analysis plan
result_ment <- run_bayes_model(
  y       = analytic$refl_log_MENT,
  formula = ~ hard_drugs_baseline + refl_log_MENT_base +
    age + BMI + SMOKE_bin + EDUC_bin + RACE_bin,
  data    = analytic,
  coef_sd = 2,
  sigma_sd = 5
)
summ_ment <- summarize_bayes_model(result_ment, "Mental QoL (refl log)")
print(result_ment$loo)
print(result_ment$waic)
result_ment$fit$cmdstan_diagnose()

############################################################################
# BAYESIAN RESULTS TABLE: With and Without ADH, all 4 outcomes
# Matches structure of frequentist results table
# Requires: result_vload, result_cd4, result_phys, result_ment (no ADH)
#           result_vload_b, result_cd4_b, result_phys_b, result_ment_b (with ADH)
############################################################################

extract_bayes_results <- function(result_list, outcome_label) {
  draws_mat <- as_draws_matrix(result_list$fit$draws())
  
  # beta[2] is always hard_drugs_baseline in our design matrix
  drug_coef <- as.numeric(draws_mat[, "beta[2]"])
  hpd       <- hdi(drug_coef, ci = 0.95)
  
  # Posterior probability of direction (prob effect is in the direction of estimate)
  est <- mean(drug_coef)
  p_dir <- ifelse(est > 0, mean(drug_coef > 0), mean(drug_coef < 0))
  
  data.frame(
    Outcome   = outcome_label,
    Estimate  = round(est, 3),
    Std_Dev   = round(sd(drug_coef), 3),
    HPDI      = paste0("(", round(hpd$CI_low, 3), ", ", round(hpd$CI_high, 3), ")"),
    P_dir     = round(p_dir, 3),
    ESS       = round(ess_bulk(drug_coef), 0),
    Rhat      = round(rhat(drug_coef), 3)
  )
}

bayes_results_table <- rbind(
  # No ADH models
  extract_bayes_results(result_vload, "Viral Load (log10) - No ADH"),
  extract_bayes_results(result_vload_b, "Viral Load (log10) - With ADH"),
  extract_bayes_results(result_cd4,   "CD4 Count - No ADH"),
  extract_bayes_results(result_cd4_b, "CD4 Count - With ADH"),
  extract_bayes_results(result_phys,  "Physical QoL (refl log) - No ADH"),
  extract_bayes_results(result_phys_b,"Physical QoL (refl log) - With ADH"),
  extract_bayes_results(result_ment,  "Mental QoL (refl log) - No ADH"),
  extract_bayes_results(result_ment_b,"Mental QoL (refl log) - With ADH")
)

kable(bayes_results_table,
      row.names = FALSE,
      caption   = "Bayesian Results: Effect of Baseline Hard Drug Use on Year 2 Outcomes, With and Without Adherence Adjustment",
      booktabs  = TRUE,
      col.names = c("Outcome", "Estimate", "Posterior SD", "95% HPDI", 
                    "P(Direction)", "ESS", "Rhat")) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                full_width = FALSE) %>%
  pack_rows("Viral Load (log10)",                  1, 2) %>%
  pack_rows("CD4 Count",                           3, 4) %>%
  pack_rows("Physical QoL (reflected log)",        5, 6) %>%
  pack_rows("Mental QoL (reflected log)",          7, 8) %>%
  footnote(general = paste(
    "Estimate = posterior mean. Posterior SD = posterior standard deviation.",
    "HPDI = 95% Highest Posterior Density Interval.",
    "P(Direction) = posterior probability that the effect is in the direction of the estimate.",
    "ESS = Effective Sample Size; values > 1000 indicate adequate sampling.",
    "Rhat < 1.01 indicates chain convergence.",
    "For reflected log QoL outcomes, positive estimates indicate worse quality of life.",
    "Hard drug users vs. non-users at baseline."
  ),
  general_title = "Note: ",
  footnote_as_chunk = TRUE)


############################################################################
# STEP 4: Bayesian Mediation Analysis
#
# Approach: fit mediator model (logistic) and outcome model (linear, with
# ADH_bin included) separately in Stan, then combine posterior draws to
# compute mediation quantities manually.
#
# For each posterior draw s:
#   - From mediator model: get P(ADH=1 | drug=1) and P(ADH=1 | drug=0)
#   - From outcome model:  get coefficients for drug and ADH
#   - ACME(s)  = beta_ADH * (P(ADH=1|drug=1) - P(ADH=1|drug=0))
#   - ADE(s)   = beta_drug (direct effect)
#   - Total(s) = ADE(s) + ACME(s)
#   - Prop(s)  = ACME(s) / Total(s)
#
# This gives full posterior distributions for all mediation quantities.
############################################################################

# Stan model for logistic regression (mediator model)
stan_logistic_file <- write_stan_file("
data {
  int<lower=0> N;
  int<lower=0> P;
  matrix[N, P] X;
  array[N] int<lower=0, upper=1> y;
  vector[P] prior_mean;
  vector<lower=0>[P] prior_sd;
}
parameters {
  vector[P] beta;
}
model {
  beta ~ normal(prior_mean, prior_sd);
  y ~ bernoulli_logit(X * beta);
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(y[n] | X[n] * beta);
  }
}",
dir = "STAN", basename = "logistic_regression")

stan_logistic_mod <- cmdstan_model("STAN/logistic_regression.stan")


# --- Helper: fit logistic mediator model ---
run_bayes_logistic <- function(y_binary, formula, data,
                               coef_sd = 2, seed = 123,
                               chains = 4,
                               iter_warmup = 1000,
                               iter_sampling = 1000) {
  X <- model.matrix(formula, data = data)
  N <- nrow(X)
  P <- ncol(X)
  
  data_list <- list(
    N          = N,
    P          = P,
    X          = X,
    y          = as.integer(y_binary),
    prior_mean = rep(0, P),
    prior_sd   = c(10, rep(coef_sd, P - 1))
  )
  
  fit <- stan_logistic_mod$sample(
    data          = data_list,
    chains        = chains,
    iter_warmup   = iter_warmup,
    iter_sampling = iter_sampling,
    seed          = seed,
    refresh       = 500
  )
  
  list(fit = fit, X = X)
}


# --- Helper: compute Bayesian mediation quantities from posterior draws ---
# med_fit   : output of run_bayes_logistic()
# out_fit   : output of run_bayes_model() WITH ADH_bin in formula
# data      : analytic dataframe
# drug_col  : name of treatment column in design matrices
# adh_col   : name of mediator column in outcome design matrix

compute_bayes_mediation <- function(med_fit, out_fit, data,
                                    drug_col = "hard_drugs_baselineHard Drug User",
                                    adh_col  = "ADH_bin<=95%") {
  
  # Posterior draws as matrices
  med_draws <- as_draws_matrix(med_fit$fit$draws())
  out_draws <- as_draws_matrix(out_fit$fit$draws())
  
  # Column indices for beta parameters only
  med_beta_cols <- grep("^beta\\[", colnames(med_draws))
  out_beta_cols <- grep("^beta\\[", colnames(out_draws))
  
  med_betas <- med_draws[, med_beta_cols]
  out_betas <- out_draws[, out_beta_cols]
  
  # Column names from design matrices for indexing
  med_X_names <- colnames(med_fit$X)
  out_X_names <- colnames(out_fit$X)
  
  # Index of drug variable in each model
  med_drug_idx <- which(med_X_names == drug_col)
  out_drug_idx <- which(out_X_names == drug_col)
  out_adh_idx  <- which(out_X_names == adh_col)
  
  if (length(med_drug_idx) == 0) stop("drug_col not found in mediator design matrix. Check column name.")
  if (length(out_drug_idx) == 0) stop("drug_col not found in outcome design matrix. Check column name.")
  if (length(out_adh_idx)  == 0) stop("adh_col not found in outcome design matrix. Check column name.")
  
  n_draws <- nrow(med_betas)
  
  ACME  <- numeric(n_draws)
  ADE   <- numeric(n_draws)
  Total <- numeric(n_draws)
  Prop  <- numeric(n_draws)
  
  for (s in seq_len(n_draws)) {
    # Mediator model betas for this draw
    b_med <- as.numeric(med_betas[s, ])
    
    # Build covariate vectors for drug=1 and drug=0 at mean of other covariates
    # Use mean covariate values across the sample
    X_med <- med_fit$X
    
    # Average predictor values (for computing marginal probabilities)
    x_mean <- colMeans(X_med)
    
    x_drug1        <- x_mean
    x_drug1[med_drug_idx] <- 1   # drug user
    
    x_drug0        <- x_mean
    x_drug0[med_drug_idx] <- 0   # non drug user
    
    # P(ADH = "<=95%" | drug = 1 or 0) via inverse logit
    p_adh_drug1 <- plogis(sum(b_med * x_drug1))
    p_adh_drug0 <- plogis(sum(b_med * x_drug0))
    
    # Outcome model betas for this draw
    b_out      <- as.numeric(out_betas[s, ])
    beta_drug  <- b_out[out_drug_idx]
    beta_adh   <- b_out[out_adh_idx]
    
    # Mediation quantities
    ACME[s]  <- beta_adh * (p_adh_drug1 - p_adh_drug0)
    ADE[s]   <- beta_drug
    Total[s] <- ADE[s] + ACME[s]
    Prop[s]  <- ifelse(abs(Total[s]) > 1e-10, ACME[s] / Total[s], NA)
  }
  
  list(ACME = ACME, ADE = ADE, Total = Total, Prop = Prop)
}


# --- Helper: summarize mediation posterior ---
summarize_bayes_mediation <- function(med_quantities, label) {
  
  summarize_qty <- function(x, name) {
    x     <- x[!is.na(x)]
    hpd   <- hdi(x, ci = 0.95)
    p_pos <- mean(x > 0)   # posterior probability of positive effect
    tibble(
      Quantity  = name,
      Estimate  = round(mean(x), 3),
      Std_Dev   = round(sd(x),   3),
      HPDI_low  = round(hpd$CI_low,  3),
      HPDI_high = round(hpd$CI_high, 3),
      P_positive = round(p_pos, 3)
    )
  }
  
  tbl <- bind_rows(
    summarize_qty(med_quantities$ACME,  "ACME (Indirect)"),
    summarize_qty(med_quantities$ADE,   "ADE (Direct)"),
    summarize_qty(med_quantities$Total, "Total Effect"),
    summarize_qty(med_quantities$Prop,  "Prop. Mediated")
  )
  
  cat("\n=== Bayesian Mediation:", label, "===\n")
  print(tbl)
  invisible(tbl)
}


############################################################################
# Fit outcome models WITH ADH_bin for mediation (matching mod_b)
############################################################################

result_vload_b <- run_bayes_model(
  y       = analytic$log10_VLOAD,
  formula = ~ hard_drugs_baseline + ADH_bin + log10_VLOAD_base +
    age + BMI + SMOKE_bin + EDUC_bin + RACE_bin,
  data    = analytic,
  coef_sd = 2, sigma_sd = 5
)

result_cd4_b <- run_bayes_model(
  y       = analytic$LEU3N,
  formula = ~ hard_drugs_baseline + ADH_bin + LEU3N_base +
    age + BMI + SMOKE_bin + EDUC_bin + RACE_bin,
  data    = analytic,
  coef_sd = 100, sigma_sd = 5
)

result_phys_b <- run_bayes_model(
  y       = analytic$refl_log_PHYS,
  formula = ~ hard_drugs_baseline + ADH_bin + refl_log_PHYS_base +
    age + BMI + SMOKE_bin + EDUC_bin + RACE_bin,
  data    = analytic,
  coef_sd = 2, sigma_sd = 5
)

result_ment_b <- run_bayes_model(
  y       = analytic$refl_log_MENT,
  formula = ~ hard_drugs_baseline + ADH_bin + refl_log_MENT_base +
    age + BMI + SMOKE_bin + EDUC_bin + RACE_bin,
  data    = analytic,
  coef_sd = 2, sigma_sd = 5
)


############################################################################
# Fit mediator model (same for all 4 outcomes)
############################################################################

# ADH_bin: >95% = reference, <=95% = 1
# model P(ADH = "<=95%") — poor adherence
med_fit_adh <- run_bayes_logistic(
  y_binary = as.integer(analytic$ADH_bin == "<=95%"),
  formula  = ~ hard_drugs_baseline + age + BMI + SMOKE_bin + EDUC_bin + RACE_bin,
  data     = analytic,
  coef_sd  = 2
)


############################################################################
# Compute and summarize Bayesian mediation for all 4 outcomes
############################################################################

med_bayes_vload <- compute_bayes_mediation(
  med_fit_adh, result_vload_b, data = analytic,
  drug_col = "hard_drugs_baselineHard Drug User",
  adh_col  = "ADH_bin<=95%"
)
summ_med_vload <- summarize_bayes_mediation(med_bayes_vload, "Viral Load (log10)")


med_bayes_cd4 <- compute_bayes_mediation(
  med_fit_adh, result_cd4_b, data = analytic,
  drug_col = "hard_drugs_baselineHard Drug User",
  adh_col  = "ADH_bin<=95%"
)
summ_med_cd4 <- summarize_bayes_mediation(med_bayes_cd4, "CD4 Count")


med_bayes_phys <- compute_bayes_mediation(
  med_fit_adh, result_phys_b, data = analytic,
  drug_col = "hard_drugs_baselineHard Drug User",
  adh_col  = "ADH_bin<=95%"
)
summ_med_phys <- summarize_bayes_mediation(med_bayes_phys, "Physical QoL (refl log)")


med_bayes_ment <- compute_bayes_mediation(
  med_fit_adh, result_ment_b, data = analytic,
  drug_col = "hard_drugs_baselineHard Drug User",
  adh_col  = "ADH_bin<=95%"
)
summ_med_ment <- summarize_bayes_mediation(med_bayes_ment, "Mental QoL (refl log)")


############################################################################
# Bayesian Mediation Summary Table (all 4 outcomes, ACME row only)
# Full tables are in the individual 
############################################################################

build_bayes_med_row <- function(med_quantities, label) {
  acme  <- med_quantities$ACME
  ade   <- med_quantities$ADE
  total <- med_quantities$Total
  prop  <- med_quantities$Prop[!is.na(med_quantities$Prop)]
  
  hpd_acme  <- hdi(acme,  ci = 0.95)
  hpd_ade   <- hdi(ade,   ci = 0.95)
  hpd_total <- hdi(total, ci = 0.95)
  
  data.frame(
    Outcome    = label,
    ACME_est   = round(mean(acme),  3),
    ACME_HPDI  = paste0("(", round(hpd_acme$CI_low,  3), ", ",
                        round(hpd_acme$CI_high, 3), ")"),
    ACME_Ppos  = round(mean(acme > 0), 3),
    ADE_est    = round(mean(ade),   3),
    ADE_HPDI   = paste0("(", round(hpd_ade$CI_low,  3), ", ",
                        round(hpd_ade$CI_high, 3), ")"),
    ADE_Ppos   = round(mean(ade > 0), 3),
    Total_est  = round(mean(total), 3),
    Total_HPDI = paste0("(", round(hpd_total$CI_low,  3), ", ",
                        round(hpd_total$CI_high, 3), ")"),
    Total_Ppos = round(mean(total > 0), 3),
    Prop_Med   = round(mean(prop),  3)
  )
}

bayes_med_table <- rbind(
  build_bayes_med_row(med_bayes_vload, "Viral Load (log10)"),
  build_bayes_med_row(med_bayes_cd4,   "CD4 Count"),
  build_bayes_med_row(med_bayes_phys,  "Physical QoL (refl log)"),
  build_bayes_med_row(med_bayes_ment,  "Mental QoL (refl log)")
)

kable(bayes_med_table,
      row.names = FALSE,
      caption   = "Bayesian Mediation Analysis: Effect of Baseline Hard Drug Use on Year 2 Outcomes Mediated Through Medication Adherence",
      booktabs  = TRUE,
      col.names = c("Outcome",
                    "Estimate", "95% HPDI", "P(>0)",
                    "Estimate", "95% HPDI", "P(>0)",
                    "Estimate", "95% HPDI", "P(>0)",
                    "Prop. Mediated")) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                full_width = FALSE) %>%
  add_header_above(c(" "               = 1,
                     "Indirect (ACME)" = 3,
                     "Direct (ADE)"    = 3,
                     "Total Effect"    = 3,
                     " "               = 1)) %>%
  footnote(general = paste(
    "ACME = Average Causal Mediation Effect (indirect path through adherence).",
    "ADE = Average Direct Effect (path not through adherence).",
    "P(>0) = posterior probability that the effect is positive.",
    "Prop. Mediated = posterior mean of ACME / Total Effect.",
    "Mediation quantities computed by combining posterior draws from",
    "separate logistic (mediator) and linear (outcome) Stan models.",
    "For reflected log QoL outcomes, positive estimates indicate worse quality of life.",
    "Hard drug users vs. non-users at baseline."
  ),
  general_title     = "Note: ",
  footnote_as_chunk = TRUE)


