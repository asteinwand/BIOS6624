library(MASS)
library(knitr)
library(kableExtra)
library(emmeans)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lme4)
library(grid)
library(powertools)

### Load Data

memdata <- read.csv("C:/Users/stein/OneDrive/Documents/School/2026 Spring/Advanced Methods/BIOS6624/Project 2/Data Raw/PrelimData.csv",
                    header = TRUE, stringsAsFactors = FALSE)

### Initial data inspection

# How many observations?
nrow(memdata) # 30

# Check variable types
sapply(memdata, class) # Continuous variables should be numeric

### Missing data

missingcount <- colSums(is.na(memdata))
missingcount # Noting missing

### Descriptive statistics

describe <- function(x, varname = "") {
  cat("Variable:", varname, "\n")
  cat("  N:      ", sum(!is.na(x)), "\n")
  cat("  Mean:   ", round(mean(x, na.rm = TRUE), 3), "\n")
  cat("  SD:     ", round(sd(x,   na.rm = TRUE), 3), "\n")
  cat("  Min:    ", round(min(x,  na.rm = TRUE), 3), "\n")
  cat("  Max:    ", round(max(x,  na.rm = TRUE), 3), "\n\n")
}

# Apply to each variable individually
describe(memdata$IL_6,      "IL_6")
describe(memdata$MCP_1,     "MCP_1")
describe(memdata$CVLT_CNG3, "CVLT_CNG3")
describe(memdata$CORT_CNG3, "CORT_CNG3")


# outliers

flag_outliers <- function(x, varname = "") {
  m   <- mean(x, na.rm = TRUE)
  s   <- sd(x,   na.rm = TRUE)
  out <- which(abs(x - m) > 3 * s)
  if (length(out) > 0) {
    cat("Outliers in", varname, "at rows:", out,
        "\nValues:", round(x[out], 3), "\n\n")
  } else {
    cat("No outliers detected in", varname, "\n\n")
  }
}

# Apply to each variable individually
flag_outliers(memdata$IL_6,      "IL_6")
flag_outliers(memdata$MCP_1,     "MCP_1")
flag_outliers(memdata$CVLT_CNG3, "CVLT_CNG3")
flag_outliers(memdata$CORT_CNG3, "CORT_CNG3")
# No outliers detected



### Assess normality
par(mfrow = c(2, 2))

hist(memdata$IL_6,
     main = "IL-6 Distribution",
     xlab = "IL-6", col = "lightblue", breaks = 20)

hist(memdata$MCP_1,
     main = "MCP-1 Distribution",
     xlab = "MCP-1", col = "blueviolet", breaks = 20)

hist(memdata$CVLT_CNG3,
     main = "CVLT Distribution",
     xlab = "CVLT", col = "lightgreen", breaks = 20)

hist(memdata$CORT_CNG3,
     main = "Cortical Thickness Distribution",
     xlab = "Cortical Thickness (mm)", col = "coral", breaks = 20)

par(mfrow = c(1, 1))


### Boxplots

boxplot(memdata$CVLT_CNG3,
        main  = "CVLT Follow-up",
        ylab  = "Score",
        col   = "lightgreen",
        outline = TRUE)

boxplot(memdata$CORT_CNG3,
        main  = "Cortical Thickness Follow-up",
        ylab  = "mm",
        col   = "coral",
        outline = TRUE)

boxplot(memdata$IL_6,
        main = "IL-6", ylab = "Plasma Level",
        col = "lightblue", outline = TRUE)

boxplot(memdata$MCP_1,
        main = "MCP-1", ylab = "Plasma Level",
        col = "blueviolet", outline = TRUE)



#### Power analysis ####

#### Aim 1 ####

# Tests the partial contribution of each cytokine over and
# above covariates in predicting each outcome change score
#
# Parameters:
#   N     = 175  projected analytic sample (125 aMCI + 50 HC
#           after ~10% attrition from 192 enrolled)
#   p     = 10   covariates (age, sex, APOE-e4, education,
#                BMI, hypercholesterolemia, NSAID use, immune
#                conditions, group membership, baseline score)
#   q     = 1    one cytokine tested at a time
#   pc    = partial correlation 
#   alpha = 0.05 (FDR correction applied at analysis stage
#                 via p.adjust; alpha not adjusted here)



cor_matrix <- cor(memdata[, c("IL_6", "MCP_1", "CVLT_CNG3", "CORT_CNG3")],
                  use = "complete.obs")
round(cor_matrix, 3)

sd_CVLT <- sd(memdata$CVLT_CNG3, na.rm = TRUE)
sd_CORT <- sd(memdata$CORT_CNG3, na.rm = TRUE)
sd_IL6  <- sd(memdata$IL_6,      na.rm = TRUE)
sd_MCP1 <- sd(memdata$MCP_1,     na.rm = TRUE)

# Key correlations 
r_IL6_CVLT  <- abs(cor_matrix["IL_6",  "CVLT_CNG3"])
r_IL6_CORT  <- abs(cor_matrix["IL_6",  "CORT_CNG3"])
r_MCP1_CVLT <- abs(cor_matrix["MCP_1", "CVLT_CNG3"])
r_MCP1_CORT <- abs(cor_matrix["MCP_1", "CORT_CNG3"])

# IL-6 -> CVLT change
mlrF.partial(N = 175, p = 10, q = 1,
             pc = r_IL6_CVLT, alpha = 0.05, v = TRUE)

# IL-6 -> cort change
mlrF.partial(N = 175, p = 10, q = 1,
             pc = r_IL6_CORT, alpha = 0.05, v = TRUE)

# MCP-1 -> CVLT change
mlrF.partial(N = 175, p = 10, q = 1,
             pc = r_MCP1_CVLT, alpha = 0.05, v = TRUE)

# MCP-1 -> cort change
mlrF.partial(N = 175, p = 10, q = 1,
             pc = r_MCP1_CORT, alpha = 0.05, v = TRUE)



#### Aim 2 ####

# No preliminary data available for the cytokine x amyloid
# interaction. Instead we estimate the minimum detectable
# partial correlation for the interaction term at 80% power.
#
# N     = 175
# p     = 12   (10 covariates + cytokine main effect +
#               amyloid SUVR main effect)
# q     = 1    (the interaction term)
# alpha = 0.05

mlrF.partial(N = 175, p = 12, q = 1,
             pc = NULL, alpha = 0.05, power = 0.80)


pc_values <- seq(0.10, 0.40, by = 0.01)

for (pc_val in pc_values) {
  result <- mlrF.partial(N = 175, p = 12, q = 1,
                         pc = pc_val, alpha = 0.05, v = FALSE)
  if (result >= 0.80) {
    cat("Minimum detectable partial correlation at 80% power:", pc_val, "\n")
    mlrF.partial(N = 175, p = 12, q = 1,
                 pc = pc_val, alpha = 0.05, v = TRUE)
    break
  }
}



