library(MASS)
library(knitr)
library(kableExtra)
library(emmeans)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lme4)
library(grid)

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










