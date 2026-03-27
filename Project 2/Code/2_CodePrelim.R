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











