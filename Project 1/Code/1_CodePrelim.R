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

hivdat <- read.csv("C:/Users/stein/OneDrive/Documents/School/2026 Spring/Advanced Methods/BIOS6624/Project 1/Data Raw/hiv_6624_final.csv")

# Some Prelim Data Preparation

# COnverting hard drugs to a factor 
hivdat$hard_drugs <- factor(hivdat$hard_drugs,
                          levels = c(0,1),
                          labels = c("No Hard Drugs", "Hard Drug User"))

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
analytic <- merge(dat_yr2, dat_yr0, by = "newid")




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


# Convert categoricals to factors in the analytic dataset
analytic$SMOKE   <- factor(analytic$SMOKE)
analytic$EDUCBAS <- factor(analytic$EDUCBAS)
analytic$RACE    <- factor(analytic$RACE)
analytic$ADH     <- factor(analytic$ADH)




# Frequentist models

# a is WITHOUT adherence
# b is WITH adherence

# Viral Load (log10 transformed) 
mod1a <- lm(log10(VLOAD) ~ hard_drugs_baseline + log10(VLOAD_base) +
             age + BMI + SMOKE + EDUCBAS + RACE,
           data = analytic)
mod1b <- lm(log10(VLOAD) ~ hard_drugs_baseline + log10(VLOAD_base) +
              age + BMI + SMOKE + EDUCBAS + RACE + ADH,
            data = analytic)
summary(mod1)
par(mfrow = c(2, 2)); plot(mod1); par(mfrow = c(1, 1))

#  CD4 Count (untransformed)
mod2a <- lm(LEU3N ~ hard_drugs_baseline + LEU3N_base +
             age + BMI + SMOKE + EDUCBAS + RACE,
           data = analytic)
mod2b <- lm(LEU3N ~ hard_drugs_baseline + LEU3N_base +
             age + BMI + SMOKE + EDUCBAS + RACE + ADH,
           data = analytic)
summary(mod2)
par(mfrow = c(2, 2)); plot(mod2); par(mfrow = c(1, 1))

# Physical QoL (reflected log)
mod3a <- lm(log(101 - AGG_PHYS) ~ hard_drugs_baseline + log(101 - AGG_PHYS_base) +
             age + BMI + SMOKE + EDUCBAS + RACE,
           data = analytic)
mod3b <- lm(log(101 - AGG_PHYS) ~ hard_drugs_baseline + log(101 - AGG_PHYS_base) +
             age + BMI + SMOKE + EDUCBAS + RACE + ADH,
           data = analytic)
summary(mod3)
par(mfrow = c(2, 2)); plot(mod3); par(mfrow = c(1, 1))

# Mental QoL (reflected log) 
mod4a <- lm(log(101 - AGG_MENT) ~ hard_drugs_baseline + log(101 - AGG_MENT_base) +
             age + BMI + SMOKE + EDUCBAS + RACE,
           data = analytic)
mod4b <- lm(log(101 - AGG_MENT) ~ hard_drugs_baseline + log(101 - AGG_MENT_base) +
             age + BMI + SMOKE + EDUCBAS + RACE + ADH,
           data = analytic)
summary(mod4)
par(mfrow = c(2, 2)); plot(mod4); par(mfrow = c(1, 1))


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



# Estimates for log-transformed outcomes represent differences on the log scale. 
# For viral load, exp(estimate) gives the fold-change in copies/mL.", 
# For QoL outcomes, positive estimates indicate worse quality of life due to reflection of scale.







