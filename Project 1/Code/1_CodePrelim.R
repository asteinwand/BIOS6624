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

hivdat <gridhivdat <- read.csv("C:/Users/stein/OneDrive/Documents/School/2026 Spring/Advanced Methods/BIOS6624/Project 1/Data Raw/hiv_6624_final.csv")


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
analytic <- analytic_all[complete.cases(analytic_all), ]
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


# =============================================================================
# CONSORT FLOW DIAGRAM
# =============================================================================

library(ggplot2)
library(grid)

# Create the flow data
flow_data <- data.frame(
  step = 1:6,
  label = c(
    paste0("Total subjects in dataset\nN = ", n_start),
    paste0("Subjects with Year 0 data\nN = ", n_yr0),
    paste0("Subjects with Year 2 data\nN = ", n_yr2),
    paste0("Matched subjects\n(have both Year 0 and Year 2)\nN = ", n_both),
    paste0("After removing missing data\nN = ", n_after_missing),
    paste0("Final analytic sample\nN = ", n_after_missing)
  ),
  excluded = c(
    "",
    "",
    "",
    paste0("Excluded: ", n_unmatched, "\n(unmatched visits)"),
    paste0("Excluded: ", n_missing, "\n(missing data)"),
    ""
  ),
  x = c(2, 2, 2, 2, 2, 2),
  y = c(6, 5, 4, 3, 2, 1),
  excluded_x = c(NA, NA, NA, 4, 4, NA),
  excluded_y = c(NA, NA, NA, 3, 2, NA)
)

# Create plot
png("consort_flow.png", width = 800, height = 1000, res = 120)

plot(NULL, xlim = c(0, 6), ylim = c(0, 7), xlab = "", ylab = "", 
     axes = FALSE, asp = 1)

# Draw boxes
for(i in 1:nrow(flow_data)) {
  rect(flow_data$x[i] - 0.8, flow_data$y[i] - 0.3, 
       flow_data$x[i] + 0.8, flow_data$y[i] + 0.3,
       col = "lightblue", border = "black", lwd = 2)
  text(flow_data$x[i], flow_data$y[i], flow_data$label[i], cex = 0.9)
}

# Draw exclusion boxes
for(i in 1:nrow(flow_data)) {
  if(!is.na(flow_data$excluded_x[i]) && flow_data$excluded[i] != "") {
    rect(flow_data$excluded_x[i] - 0.6, flow_data$excluded_y[i] - 0.2,
         flow_data$excluded_x[i] + 0.6, flow_data$excluded_y[i] + 0.2,
         col = "lightcoral", border = "black", lwd = 1.5)
    text(flow_data$excluded_x[i], flow_data$excluded_y[i], 
         flow_data$excluded[i], cex = 0.8)
  }
}

# Draw arrows
for(i in 1:5) {
  arrows(flow_data$x[i], flow_data$y[i] - 0.3, 
         flow_data$x[i+1], flow_data$y[i+1] + 0.3,
         length = 0.15, lwd = 2)
}

# Draw exclusion arrows
arrows(2.8, 3, 3.4, 3, length = 0.1, lwd = 1.5, col = "red")
arrows(2.8, 2, 3.4, 2, length = 0.1, lwd = 1.5, col = "red")

title(main = "CONSORT Flow Diagram: Sample Selection", 
      cex.main = 1.3, font.main = 2)

dev.off()

cat("\nFlow diagram saved as 'consort_flow.png'\n")




